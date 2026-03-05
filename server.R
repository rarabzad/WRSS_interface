library(shiny)
library(jsonlite)

`%||%` <- function(a, b) if (is.null(a) || identical(a, NA) || identical(a, "")) b else a

scalar_str <- function(x, default = "") {
  if (is.null(x) || length(x) == 0) return(default)
  v <- tryCatch(as.character(x[[1]]), error = function(e) default)
  if (is.null(v) || is.na(v) || !nzchar(v)) return(default)
  v
}

to_num <- function(x) {
  if (is.null(x) || length(x) == 0) return(numeric(0))
  v <- suppressWarnings(as.numeric(unlist(x)))
  v[!is.na(v)]
}

to_mat <- function(lst, c1, c2) {
  if (is.null(lst) || length(lst) == 0) return(NULL)
  rows <- Filter(function(r) !is.null(r) && length(r) >= 2, lst)
  if (length(rows) == 0) return(NULL)
  v1 <- suppressWarnings(as.numeric(sapply(rows, function(r) r[[1]])))
  v2 <- suppressWarnings(as.numeric(sapply(rows, function(r) r[[2]])))
  ok <- !is.na(v1) & !is.na(v2)
  if (!any(ok)) return(NULL)
  mat <- cbind(v1[ok], v2[ok])
  colnames(mat) <- c(c1, c2)
  mat
}

safe_extract <- function(x, depth = 0) {
  if (depth > 12) return(NULL)
  if (is.environment(x) || is.function(x)) return(NULL)
  if (is.data.frame(x)) {
    out <- lapply(x, function(col) {
      if (is.numeric(col) || is.integer(col) || is.logical(col) || is.character(col))
        return(as.list(col))
      NULL
    })
    return(Filter(Negate(is.null), out))
  }
  if (is.list(x)) {
    out <- lapply(x, safe_extract, depth = depth + 1)
    return(Filter(Negate(is.null), out))
  }
  if (is.numeric(x) || is.integer(x) || is.character(x) || is.logical(x)) return(x)
  NULL
}

server <- function(input, output, session) {
  
  # ── Receive messages from iframe (relayed by ui.R's JS glue) ────────────
  observeEvent(input$iframe_msg, {
    raw <- input$iframe_msg$raw
    if (is.null(raw) || !nzchar(raw)) return()
    
    msg <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE),
                    error = function(e) NULL)
    if (is.null(msg) || is.null(msg$type)) return()
    
    # Helper: send message back to iframe via the JS glue
    send_to_iframe <- function(obj) {
      session$sendCustomMessage("to_builder", obj)
    }
    
    if (msg$type == "ping") {
      send_to_iframe(list(type = "pong",
                          wrss = requireNamespace("WRSS", quietly = TRUE)))
      return()
    }
    
    if (msg$type != "run_model") return()
    
    model_def <- msg$model
    
    send_status <- function(txt) {
      send_to_iframe(list(type = "run_status", status = "running", message = txt))
    }
    send_error <- function(txt) {
      send_to_iframe(list(type = "run_status", status = "error", message = txt))
    }
    
    send_status("Loading WRSS...")
    sim_error_reported <- FALSE
    
    result_obj <- tryCatch({
      if (!requireNamespace("WRSS", quietly = TRUE))
        stop("WRSS not installed. Run: install.packages('WRSS')")
      suppressPackageStartupMessages(library(WRSS))
      
      sim_cfg <- model_def$simulation
      simulation <- list(
        start    = as.character(sim_cfg$start    %||% "2010-01-01"),
        end      = as.character(sim_cfg$end      %||% "2019-12-01"),
        interval = as.character(sim_cfg$interval %||% "month")
      )
      
      area_name <- if (!is.null(model_def$name) && nzchar(model_def$name))
        as.character(model_def$name) else "Basin"
      
      send_status(paste0("Creating area '", area_name, "'..."))
      Area <- createArea(name = area_name, simulation = simulation)
      
      env <- new.env(parent = emptyenv())
      resolve <- function(nm) {
        if (is.null(nm) || !nzchar(nm)) return(NULL)
        nm2 <- gsub("[^a-zA-Z0-9_.]", "_", nm)
        if (exists(nm2, envir = env, inherits = FALSE))
          get(nm2, envir = env, inherits = FALSE) else NULL
      }
      reg <- function(nm, obj)
        assign(gsub("[^a-zA-Z0-9_.]", "_", nm), obj, envir = env)
      
      nodes_list <- model_def$nodes
      send_status(paste0("Building ", length(nodes_list), " node(s)..."))
      
      for (nd in nodes_list) {
        ntype <- as.character(nd$type)
        vn    <- gsub("[^a-zA-Z0-9_.]", "_", as.character(nd$varName))
        p     <- nd$props
        
        down_obj    <- resolve(nd$downstreamVar)
        seepage_obj <- resolve(nd$seepageObjectVar)
        leakage_obj <- resolve(nd$leakageObjectVar)
        divert_obj  <- resolve(nd$divertObjectVar)
        sup_vars    <- nd$supplierVars
        
        node_name <- if (!is.null(p$name) && nzchar(as.character(p$name)))
          as.character(p$name) else vn
        
        get_priority <- function() {
          pv <- if (!is.null(p$priority)) as.character(p$priority) else ""
          if (nzchar(pv)) suppressWarnings(as.numeric(pv)) else Inf
        }
        
        if (ntype == "river") {
          Q  <- to_num(p$discharge)
          sf <- suppressWarnings(as.numeric(p$seepageFraction %||% 0))
          args <- list(name = node_name, seepageFraction = sf, priority = get_priority())
          if (length(Q) > 0)         args$discharge     <- Q
          if (!is.null(down_obj))    args$downstream    <- down_obj
          if (!is.null(seepage_obj)) args$seepageObject <- seepage_obj
          reg(vn, do.call(createRiver, args))
          
        } else if (ntype == "reservoir") {
          E   <- to_num(p$netEvaporation)
          SA  <- to_mat(p$storageAreaTable,        "volume_MCM", "area_Km2")
          SE  <- to_mat(p$storageElevationTable,   "volume_MCM", "elev_m")
          DE  <- to_mat(p$dischargeElevationTable, "disch_cms",  "elev_m")
          EFF <- to_mat(p$efficiency,              "disch_cms",  "efficiency")
          res_type <- if (!is.null(p$type)) as.character(p$type) else "storage"
          geo <- list(
            deadStorage = suppressWarnings(as.numeric(p$deadStorage %||% 0)),
            capacity    = suppressWarnings(as.numeric(p$capacity    %||% 0))
          )
          if (!is.null(SA))  geo$storageAreaTable        <- SA
          if (!is.null(SE))  geo$storageElevationTable   <- SE
          if (!is.null(DE))  geo$dischargeElevationTable <- DE
          plant_list <- NULL
          if (identical(res_type, "hydropower")) {
            plant_list <- list(
              submerged = identical(as.character(p$submerged %||% "FALSE"), "TRUE"),
              loss      = suppressWarnings(as.numeric(p$loss %||% 0))
            )
            ic <- as.character(p$installedCapacity %||% "")
            if (nzchar(ic)) plant_list$installedCapacity <- as.numeric(ic)
            if (!is.null(EFF)) plant_list$efficiency <- EFF
            dh1 <- as.character(p$designHeadMin %||% ""); dh2 <- as.character(p$designHeadMax %||% "")
            if (nzchar(dh1) && nzchar(dh2)) plant_list$designHead <- c(as.numeric(dh1), as.numeric(dh2))
            df1 <- as.character(p$designFlowMin %||% ""); df2 <- as.character(p$designFlowMax %||% "")
            if (nzchar(df1) && nzchar(df2)) plant_list$designFlow <- c(as.numeric(df1), as.numeric(df2))
            tae <- as.character(p$turbineAxisElevation %||% "")
            if (nzchar(tae)) plant_list$turbineAxisElevation <- as.numeric(tae)
          }
          penstock_list <- NULL
          pd <- as.character(p$penstockDiameter %||% "")
          pl <- as.character(p$penstockLength   %||% "")
          pr <- as.character(p$penstockRoughness %||% "")
          if (nzchar(pd) || nzchar(pl) || nzchar(pr)) {
            penstock_list <- list()
            if (nzchar(pd)) penstock_list$diameter  <- as.numeric(pd)
            if (nzchar(pl)) penstock_list$length    <- as.numeric(pl)
            if (nzchar(pr)) penstock_list$roughness <- as.numeric(pr)
          }
          args <- list(type = res_type, name = node_name, geometry = geo,
                       seepageFraction = suppressWarnings(as.numeric(p$seepageFraction %||% 0)),
                       priority = get_priority())
          if (length(E) > 0)           args$netEvaporation <- E
          if (!is.null(down_obj))      args$downstream     <- down_obj
          if (!is.null(seepage_obj))   args$seepageObject  <- seepage_obj
          is_v <- as.character(p$initialStorage %||% "")
          if (nzchar(is_v))            args$initialStorage <- as.numeric(is_v)
          if (!is.null(plant_list))    args$plant    <- plant_list
          if (!is.null(penstock_list)) args$penstock <- penstock_list
          reg(vn, do.call(createReservoir, args))
          
        } else if (ntype == "junction") {
          args <- list(name = node_name)
          if (!is.null(down_obj)) args$downstream <- down_obj
          reg(vn, do.call(createJunction, args))
          
        } else if (ntype == "diversion") {
          args <- list(name = node_name,
                       capacity = suppressWarnings(as.numeric(p$capacity %||% 0)),
                       priority = get_priority())
          if (!is.null(down_obj))   args$downstream   <- down_obj
          if (!is.null(divert_obj)) args$divertObject <- divert_obj
          reg(vn, do.call(createDiversion, args))
          
        } else if (ntype == "aquifer") {
          RTS <- to_num(p$rechargeTS)
          args <- list(name = node_name,
                       area   = suppressWarnings(as.numeric(p$area   %||% 0)),
                       volume = suppressWarnings(as.numeric(p$volume %||% 0)),
                       Sy     = suppressWarnings(as.numeric(p$Sy     %||% 0.1)),
                       leakageFraction = suppressWarnings(as.numeric(p$leakageFraction %||% 0)),
                       priority = get_priority())
          if (length(RTS) > 0)       args$rechargeTS    <- RTS
          is_v <- as.character(p$initialStorage %||% "")
          if (nzchar(is_v))          args$initialStorage <- as.numeric(is_v)
          if (!is.null(leakage_obj)) args$leakageObject  <- leakage_obj
          reg(vn, do.call(createAquifer, args))
          
        } else if (ntype == "demand") {
          sup_objs <- list()
          if (!is.null(sup_vars) && length(sup_vars) > 0)
            for (sv in sup_vars) { so <- resolve(as.character(sv)); if (!is.null(so)) sup_objs <- c(sup_objs, list(so)) }
          args <- list(name = node_name, suppliers = sup_objs, priority = get_priority())
          mode <- as.character(p$demandInputMode %||% "demandTS")
          if (mode == "demandTS") {
            D <- to_num(p$demandTS)
            if (length(D) > 0) args$demandTS <- D
          } else {
            WV <- to_num(p$waterVariation)
            args$demandParams <- list(
              waterUseRate   = suppressWarnings(as.numeric(p$waterUseRate %||% 0)),
              waterVariation = if (length(WV) > 0) WV else NULL,
              cropArea       = suppressWarnings(as.numeric(p$cropArea %||% 0))
            )
          }
          rff <- as.character(p$returnFlowFraction %||% "")
          if (nzchar(rff))        args$returnFlowFraction <- as.numeric(rff)
          if (!is.null(down_obj)) args$downstream <- down_obj
          reg(vn, do.call(createDemandSite, args))
        }
        
        node_obj <- get(gsub("[^a-zA-Z0-9_.]", "_", vn), envir = env, inherits = FALSE)
        Area <- addObjectToArea(Area, node_obj)
      }
      
      send_status("Running sim()...")
      withCallingHandlers(
        suppressMessages(sim(Area)),
        warning = function(w) invokeRestart("muffleWarning")
      )
      
    }, error = function(e) {
      send_error(conditionMessage(e))
      sim_error_reported <<- TRUE
      NULL
    })
    
    if (!is.null(result_obj)) {
      send_status("Serialising results...")
      tryCatch({
        clean    <- safe_extract(result_obj)
        json_str <- toJSON(clean, auto_unbox = TRUE, null = "null", na = "null", digits = 6, force = TRUE)
        parsed   <- fromJSON(json_str, simplifyVector = FALSE)
        send_to_iframe(list(type = "run_result", status = "success", result = parsed))
      }, error = function(e) {
        send_error(paste("Serialisation error:", conditionMessage(e)))
      })
    } else if (!sim_error_reported) {
      send_error("sim() returned no results. Check node connections and time-series lengths.")
    }
  })
}