#' Save a WRSS Basin Object to a JSON File
#'
#' This function serializes a WRSS (Water Resources Simulation System) \code{basin} object
#' into a JSON file format. It handles nested lists, formulas, and references between components
#' (e.g., reservoirs, rivers, junctions, demands, diversions, and aquifers), resolving numeric
#' labels to object names for key reference fields.
#'
#' The JSON output is human-readable (pretty-printed), with automatic handling of NULLs and
#' unboxing of single-element vectors. Reference fields that link to other components
#' (e.g., downstream nodes, suppliers, seepage objects) are resolved from numeric labels
#' to the component names to improve readability and maintain referential integrity.
#'
#' @param obj A \code{basin} object from the WRSS package, typically containing a
#'        nested \code{operation} list with components like \code{reservoirs}, \code{rivers},
#'        \code{junctions}, \code{demands}, \code{diversions}, and \code{aquifers}.
#' @param file A character string specifying the file path where the JSON output will be saved.
#'
#' @details
#' The function performs the following steps internally:
#' \enumerate{
#'   \item Constructs a label-to-name map for all components that have a numeric label and a name.
#'   \item Defines a safe resolver function to convert numeric labels into their corresponding names
#'         in reference fields.
#'   \item Recursively sanitizes the basin object, removing functions, environments, and certain
#'         internal attributes, while converting formulas and expressions to character.
#'   \item Resolves key reference fields (\code{downstream}, \code{suppliers}, \code{seepageObject},
#'         \code{leakageObject}, \code{divertObject}) from numeric labels to names.
#'   \item Writes the sanitized object to a JSON file using \code{jsonlite::write_json} with
#'         pretty formatting and null handling.
#' }
#'
#' @return Invisibly returns \code{NULL}. The main output is the JSON file written to disk.
#'
#' @examples
#' \dontrun{
#' # Save a WRSS basin object to a JSON file
#' basin_obj <- wrss_load_basin("example_basin.wrss")
#' save_basin_json(basin_obj, "basin_output.json")
#' }
#'
#' @seealso \code{\link[jsonlite]{write_json}}, \code{\link[WRSS]{WRSS-package}}
#'
#' @export
save_basin_json <- function(obj, file)
{
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    install.packages("jsonlite")
  }
  library(jsonlite)  # --- Helper: consistent float -> string key ---
  label_key <- function(x) formatC(x, digits = 6, format = "f")
  
  # --- Fields to resolve from numeric label -> object name ---
  REFERENCE_FIELDS <- c(
    "downstream",
    "suppliers",
    "seepageObject",
    "leakageObject",
    "divertObject"
  )
  
  # --- Step 1: Build label -> name lookup ---
  op <- obj$operation
  label_map <- list()
  
  for (component_type in c("reservoirs", "rivers", "junctions",
                           "demands", "diversions", "aquifers")) {
    items <- op[[component_type]]
    if (length(items) == 0) next
    for (item in items) {
      o <- item$operation
      if (!is.null(o$label) && !is.null(o$name) && is.numeric(o$label)) {
        label_map[[label_key(o$label)]] <- o$name
      }
    }
  }
  
  message("Label map: ", length(label_map), " entries")
  for (k in names(label_map)) message("  ", k, " -> ", label_map[[k]])
  
  # --- Step 2: Safe label resolver ---
  resolve_label <- function(val) {
    if (!is.numeric(val) || length(val) != 1 || is.na(val)) return(val)
    key <- label_key(val)
    if (key %in% names(label_map)) return(label_map[[key]])
    return(val)
  }
  
  # --- Step 3: Recursive sanitizer ---
  sanitize <- function(x) {
    
    if (inherits(x, c("call", "name", "expression"))) return(as.character(x))
    if (inherits(x, "formula"))                       return(deparse(x))
    if (is.function(x) || is.environment(x))          return(NULL)
    
    if (is.list(x)) {
      nms <- names(x)
      attr(x, "class")        <- NULL
      attr(x, ".Environment") <- NULL
      
      x        <- lapply(x, sanitize)
      names(x) <- nms
      
      # Resolve reference fields
      for (field in REFERENCE_FIELDS) {
        if (!field %in% names(x) || is.null(x[[field]])) next
        val <- x[[field]]
        x[[field]] <- if (is.numeric(val) && length(val) > 1) {
          sapply(val, resolve_label, USE.NAMES = FALSE)  # vector (e.g. suppliers)
        } else {
          resolve_label(val)                              # scalar
        }
      }
      
      return(x)
    }
    
    return(x)
  }
  
  # --- Step 4: Clean and write ---
  jsonlite::write_json(
    sanitize(obj),
    path       = file,
    pretty     = TRUE,
    auto_unbox = TRUE,
    null       = "null"
  )
  
  message("Saved to: ", file)
}