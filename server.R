library(shiny)
library(jsonlite)

# ── helper: safe coerce to numeric vector, drop NAs
to_num <- function(x) {
  if (is.null(x) || length(x) == 0) return(numeric(0))
  suppressWarnings(as.numeric(unlist(x)))
}

# ── helper: build cbind matrix from list of two numeric vectors
to_mat <- function(lst, c1, c2) {
  if (is.null(lst) || length(lst) == 0) return(NULL)
  if (is.data.frame(lst) || is.matrix(lst)) return(lst)
  # expect list with two equal-length numeric vectors
  v1 <- to_num(lst[[1]]); v2 <- to_num(lst[[2]])
  if (length(v1) == 0 || length(v2) == 0) return(NULL)
  n <- min(length(v1), length(v2))
  mat <- cbind(v1[seq_len(n)], v2[seq_len(n)])
  colnames(mat) <- c(c1, c2)
  mat
}

# ── helper: safely serialise sim() result to JSON for the browser
serialise_result <- function(result) {
  tryCatch(
    toJSON(result, auto_unbox = TRUE, null = "null",
           na = "null", digits = 6, force = TRUE),
    error = function(e) {
      toJSON(list(error = conditionMessage(e)), auto_unbox = TRUE)
    }
  )
}

# ══════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════
server <- function(input, output, session) {

  # ── Receive messages from the builder iframe
  observeEvent(input$iframe_msg, {
    msg <- input$iframe_msg
    if (is.null(msg$type)) return()

    # ────────────────────────────────────────────
    # RUN MODEL
    # ────────────────────────────────────────────
    if (msg$type == "run_model") {
      model_def <- msg$model   # full model JSON from the builder

      # Send "running" status immediately
      session$sendCustomMessage("to_builder", list(
        type    = "run_status",
        status  = "running",
        message = "Building simulation objects…"
      ))

      result <- tryCatch({

        # ── 1. Require WRSS
        if (!requireNamespace("WRSS", quietly = TRUE))
          stop("WRSS package is not installed. Run: install.packages('WRSS')")
        library(WRSS)

        # ── 2. Simulation period
        sim_cfg <- model_def$simulation
        simulation <- list(
          start    = sim_cfg$start    %||% "2010-01-01",
          end      = sim_cfg$end      %||% "2019-12-01",
          interval = sim_cfg$interval %||% "month"
        )

        # ── 3. Create area
        area_name <- model_def$name %||% "Basin"
        Area <- createArea(name = area_name, simulation = simulation)

        # ── 4. Build an environment so nodes can reference each other by varName
        env <- new.env(parent = emptyenv())

        # ── Helper: resolve a node reference (name → R object in env)
        resolve <- function(nm) {
          if (is.null(nm) || nm == "" || identical(nm, NA)) return(NULL)
          nm_clean <- gsub("[^a-zA-Z0-9_.]", "_", nm)
          if (exists(nm_clean, envir = env)) return(get(nm_clean, envir = env))
          NULL
        }

        # ── Helper: register a node
        reg <- function(varName, obj) {
          nm <- gsub("[^a-zA-Z0-9_.]", "_", varName)
          assign(nm, obj, envir = env)
          obj
        }

        # ── 5. Process nodes in the topological order provided by the builder
        nodes_list <- model_def$nodes   # ordered list from topoSort in JS

        session$sendCustomMessage("to_builder", list(
          type    = "run_status",
          status  = "running",
          message = paste0("Creating ", length(nodes_list), " nodes…")
        ))

        for (nd in nodes_list) {
          type    <- nd$type
          vn      <- gsub("[^a-zA-Z0-9_.]", "_", nd$varName)
          p       <- nd$props

          # Resolve topology links
          down_obj    <- resolve(nd$downstreamVar)
          seepage_obj <- resolve(nd$seepageObjectVar)
          leakage_obj <- resolve(nd$leakageObjectVar)
          divert_obj  <- resolve(nd$divertObjectVar)
          supplier_vars <- nd$supplierVars

          # ── RIVER
          if (type == "river") {
            Q <- to_num(p$discharge)
            obj <- createRiver(
              name             = p$name %||% vn,
              downstream       = down_obj,
              discharge        = if (length(Q) > 0) Q else NULL,
              seepageFraction  = as.numeric(p$seepageFraction %||% 0),
              seepageObject    = seepage_obj,
              priority         = if (!is.null(p$priority) && p$priority != "") as.numeric(p$priority) else Inf
            )
            reg(vn, obj)
          }

          # ── RESERVOIR
          else if (type == "reservoir") {
            E   <- to_num(p$netEvaporation)
            SA  <- to_mat(p$storageAreaTable,       "volume_MCM", "area_Km2")
            SE  <- to_mat(p$storageElevationTable,  "volume_MCM", "elev_m")
            DE  <- to_mat(p$dischargeElevationTable,"disch_cms",  "elev_m")
            EFF <- to_mat(p$efficiency,             "disch_cms",  "efficiency")

            geo <- list(
              deadStorage = as.numeric(p$deadStorage %||% 0),
              capacity    = as.numeric(p$capacity    %||% 0)
            )
            if (!is.null(SA))  geo$storageAreaTable       <- SA
            if (!is.null(SE))  geo$storageElevationTable  <- SE
            if (!is.null(DE))  geo$dischargeElevationTable <- DE

            # plant (hydropower)
            plant_list <- NULL
            if (!is.null(p$type) && p$type == "hydropower") {
              plant_list <- list(
                submerged = identical(p$submerged, "TRUE"),
                loss      = as.numeric(p$loss %||% 0)
              )
              if (!is.null(p$installedCapacity) && p$installedCapacity != "")
                plant_list$installedCapacity <- as.numeric(p$installedCapacity)
              if (!is.null(EFF)) plant_list$efficiency <- EFF
              dh_min <- as.numeric(p$designHeadMin %||% NA)
              dh_max <- as.numeric(p$designHeadMax %||% NA)
              if (!is.na(dh_min) && !is.na(dh_max))
                plant_list$designHead <- c(dh_min, dh_max)
              df_min <- as.numeric(p$designFlowMin %||% NA)
              df_max <- as.numeric(p$designFlowMax %||% NA)
              if (!is.na(df_min) && !is.na(df_max))
                plant_list$designFlow <- c(df_min, df_max)
              if (!is.null(p$turbineAxisElevation) && p$turbineAxisElevation != "")
                plant_list$turbineAxisElevation <- as.numeric(p$turbineAxisElevation)
            }

            # penstock
            penstock_list <- NULL
            has_pen <- !is.null(p$penstockDiameter)  && p$penstockDiameter  != "" ||
                       !is.null(p$penstockLength)     && p$penstockLength    != "" ||
                       !is.null(p$penstockRoughness)  && p$penstockRoughness != ""
            if (has_pen) {
              penstock_list <- list()
              if (!is.null(p$penstockDiameter)  && p$penstockDiameter  != "") penstock_list$diameter  <- as.numeric(p$penstockDiameter)
              if (!is.null(p$penstockLength)     && p$penstockLength    != "") penstock_list$length    <- as.numeric(p$penstockLength)
              if (!is.null(p$penstockRoughness)  && p$penstockRoughness != "") penstock_list$roughness <- as.numeric(p$penstockRoughness)
            }

            args <- list(
              type            = p$type %||% "storage",
              name            = p$name %||% vn,
              geometry        = geo,
              seepageFraction = as.numeric(p$seepageFraction %||% 0),
              priority        = if (!is.null(p$priority) && p$priority != "") as.numeric(p$priority) else Inf
            )
            if (length(E) > 0)       args$netEvaporation <- E
            if (!is.null(down_obj))   args$downstream     <- down_obj
            if (!is.null(seepage_obj))args$seepageObject  <- seepage_obj
            if (!is.null(p$initialStorage) && p$initialStorage != "")
              args$initialStorage <- as.numeric(p$initialStorage)
            if (!is.null(plant_list))   args$plant    <- plant_list
            if (!is.null(penstock_list))args$penstock <- penstock_list

            obj <- do.call(createReservoir, args)
            reg(vn, obj)
          }

          # ── JUNCTION
          else if (type == "junction") {
            obj <- createJunction(
              name       = p$name %||% vn,
              downstream = down_obj
            )
            reg(vn, obj)
          }

          # ── DIVERSION
          else if (type == "diversion") {
            obj <- createDiversion(
              name       = p$name %||% vn,
              capacity   = as.numeric(p$capacity %||% 0),
              downstream = down_obj,
              divertObject = divert_obj,
              priority   = if (!is.null(p$priority) && p$priority != "") as.numeric(p$priority) else Inf
            )
            reg(vn, obj)
          }

          # ── AQUIFER
          else if (type == "aquifer") {
            RTS <- to_num(p$rechargeTS)
            args <- list(
              name            = p$name %||% vn,
              area            = as.numeric(p$area   %||% 0),
              volume          = as.numeric(p$volume %||% 0),
              Sy              = as.numeric(p$Sy     %||% 0.1),
              leakageFraction = as.numeric(p$leakageFraction %||% 0),
              priority        = if (!is.null(p$priority) && p$priority != "") as.numeric(p$priority) else Inf
            )
            if (length(RTS) > 0) args$rechargeTS <- RTS
            if (!is.null(p$initialStorage) && p$initialStorage != "")
              args$initialStorage <- as.numeric(p$initialStorage)
            if (!is.null(leakage_obj)) args$leakageObject <- leakage_obj
            obj <- do.call(createAquifer, args)
            reg(vn, obj)
          }

          # ── DEMAND SITE
          else if (type == "demand") {
            # Resolve suppliers
            sup_objs <- list()
            if (!is.null(supplier_vars) && length(supplier_vars) > 0) {
              for (sv in supplier_vars) {
                so <- resolve(sv)
                if (!is.null(so)) sup_objs <- c(sup_objs, list(so))
              }
            }

            args <- list(
              name      = p$name %||% vn,
              suppliers = sup_objs,
              priority  = if (!is.null(p$priority) && p$priority != "") as.numeric(p$priority) else Inf
            )

            mode <- p$demandInputMode %||% "demandTS"
            if (mode == "demandTS") {
              D <- to_num(p$demandTS)
              if (length(D) > 0) args$demandTS <- D
            } else {
              WV <- to_num(p$waterVariation)
              args$demandParams <- list(
                waterUseRate    = as.numeric(p$waterUseRate %||% 0),
                waterVariation  = if (length(WV) > 0) WV else NULL,
                cropArea        = as.numeric(p$cropArea %||% 0)
              )
            }
            if (!is.null(p$returnFlowFraction) && p$returnFlowFraction != "")
              args$returnFlowFraction <- as.numeric(p$returnFlowFraction)
            if (!is.null(down_obj)) args$downstream <- down_obj

            obj <- do.call(createDemandSite, args)
            reg(vn, obj)
          }

          # ── Add to area
          Area <- addObjectToArea(Area, get(vn, envir = env))
        }

        # ── 6. Simulate
        session$sendCustomMessage("to_builder", list(
          type    = "run_status",
          status  = "running",
          message = "Running simulation (sim)…"
        ))

        result_obj <- sim(Area)
        result_obj

      }, error = function(e) {
        session$sendCustomMessage("to_builder", list(
          type    = "run_status",
          status  = "error",
          message = conditionMessage(e)
        ))
        NULL
      })

      if (!is.null(result)) {
        session$sendCustomMessage("to_builder", list(
          type    = "run_status",
          status  = "running",
          message = "Serialising results…"
        ))

        json_str <- serialise_result(result)

        session$sendCustomMessage("to_builder", list(
          type   = "run_result",
          status = "success",
          result = jsonlite::fromJSON(json_str, simplifyVector = FALSE)
        ))
      }
    }

    # ────────────────────────────────────────────
    # PING (health check from builder)
    # ────────────────────────────────────────────
    else if (msg$type == "ping") {
      session$sendCustomMessage("to_builder", list(
        type   = "pong",
        wrss   = requireNamespace("WRSS", quietly = TRUE)
      ))
    }
  })
}

# Null-coalescing helper (base R doesn't have %||%)
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !identical(a, "") && !identical(a, NA)) a else b
