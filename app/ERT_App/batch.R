#!/usr/bin/env Rscript
#
# Headless ERT batch processor.
#
# Runs the same processing pipeline as the Shiny app, with no UI: point it
# at a directory of ERT images, get a CSV of per-image metrics out.
#
# Usage:
#   Rscript batch.R --input PATH --output PATH [options]
#
# Options:
#   --input PATH         Directory containing .jpg/.jpeg/.png ERT images (required)
#   --output PATH        Directory to write results CSV into (required)
#   --transform TYPE     log | linear | power   (default: log)
#   --erosion N          Edge-erosion in pixels (default: 2)
#   --calibration FILE   CSV with columns: position,value  (default: PiCUS-style
#                        scale 30, 61, 125, 254, 518, 1000 at 0, 0.2, ..., 1.0)
#   --pattern REGEX      Filename regex to match (default: \\.(jpg|jpeg|png)$)
#   --help               Show this message
#
# Examples:
#   Rscript batch.R --input ./scans --output ./results
#   Rscript batch.R --input ./scans --output ./results --transform linear --erosion 3
#   Rscript batch.R --input ./scans --output ./results --calibration my_scale.csv

# Resolve script directory so processing.R is loaded from the right place
# regardless of where Rscript is invoked from.
get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(dirname(sub("^--file=", "", file_arg[1]))))
  }
  normalizePath(".")
}
SCRIPT_DIR <- get_script_dir()
source(file.path(SCRIPT_DIR, "processing.R"))

# --- Argument parsing -------------------------------------------------------

parse_args <- function(args) {
  out <- list()
  i <- 1
  while (i <= length(args)) {
    a <- args[i]
    if (a == "--help" || a == "-h") {
      out$help <- TRUE
      i <- i + 1
      next
    }
    if (startsWith(a, "--")) {
      key <- sub("^--", "", a)
      if (i + 1 > length(args) || startsWith(args[i + 1], "--")) {
        stop("Missing value for --", key)
      }
      out[[key]] <- args[i + 1]
      i <- i + 2
    } else {
      stop("Unexpected argument: ", a)
    }
  }
  out
}

print_help <- function() {
  cat(readLines(file.path(SCRIPT_DIR, "batch.R"))[2:24], sep = "\n")
}

args <- parse_args(commandArgs(trailingOnly = TRUE))

if (isTRUE(args$help) || length(args) == 0) {
  print_help()
  quit(status = if (isTRUE(args$help)) 0 else 1)
}

if (is.null(args$input) || is.null(args$output)) {
  cat("Error: --input and --output are required.\n\n")
  print_help()
  quit(status = 1)
}

input_dir  <- normalizePath(args$input, mustWork = TRUE)
output_dir <- args$output
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
output_dir <- normalizePath(output_dir)

transform <- if (is.null(args$transform)) "log" else args$transform
if (!transform %in% c("log", "linear", "power")) {
  stop("--transform must be one of: log, linear, power")
}

erosion <- if (is.null(args$erosion)) 2L else as.integer(args$erosion)
if (is.na(erosion) || erosion < 0) {
  stop("--erosion must be a non-negative integer")
}

pattern <- if (is.null(args$pattern)) "\\.(jpg|jpeg|png)$" else args$pattern

# Calibration: load CSV if provided, otherwise use defaults from processing.R
if (!is.null(args$calibration)) {
  calib_df <- read.csv(args$calibration, stringsAsFactors = FALSE)
  if (!all(c("position", "value") %in% names(calib_df))) {
    stop("--calibration CSV must have columns: position, value")
  }
  calib_positions <- calib_df$position
  calib_values    <- calib_df$value
} else {
  calib_positions <- DEFAULT_CALIB_POSITIONS
  calib_values    <- DEFAULT_CALIB_VALUES
}

# --- Run pipeline -----------------------------------------------------------

image_files <- list.files(input_dir, pattern = pattern,
                          ignore.case = TRUE, full.names = TRUE)

if (length(image_files) == 0) {
  cat(sprintf("No images matched pattern '%s' in %s\n", pattern, input_dir))
  quit(status = 1)
}

cat(sprintf("Found %d images in %s\n", length(image_files), input_dir))
cat(sprintf("Transform: %s | Erosion: %d px\n", transform, erosion))
cat(sprintf("Calibration: %d points (%s ... %s)\n",
            length(calib_positions),
            calib_values[1], calib_values[length(calib_values)]))
cat("\n")

results <- list()
failures <- character(0)

for (i in seq_along(image_files)) {
  fpath <- image_files[i]
  fname <- basename(fpath)
  cat(sprintf("[%d/%d] %s ... ", i, length(image_files), fname))

  res <- tryCatch(
    run_auto_pipeline(
      image_path = fpath,
      image_name = fname,
      calib_positions = calib_positions,
      calib_values    = calib_values,
      transform = transform,
      erosion   = erosion
    ),
    error = function(e) e
  )

  if (inherits(res, "error")) {
    cat("FAILED: ", conditionMessage(res), "\n", sep = "")
    failures <- c(failures, sprintf("%s\t%s", fname, conditionMessage(res)))
    next
  }

  cat(sprintf("OK (mean=%.1f, n=%d)\n", res$mean, res$n_pixels))
  results[[length(results) + 1]] <- result_to_row(res)
}

# --- Write outputs ----------------------------------------------------------

if (length(results) > 0) {
  out_df <- do.call(rbind, results)
  out_csv <- file.path(output_dir,
                       sprintf("ERT_results_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")))
  write.csv(out_df, out_csv, row.names = FALSE)
  cat(sprintf("\nWrote %d rows to %s\n", nrow(out_df), out_csv))
} else {
  cat("\nNo successful results to write.\n")
}

if (length(failures) > 0) {
  fail_log <- file.path(output_dir,
                        sprintf("ERT_failures_%s.tsv", format(Sys.time(), "%Y%m%d_%H%M%S")))
  writeLines(c("filename\terror", failures), fail_log)
  cat(sprintf("Wrote %d failures to %s\n", length(failures), fail_log))
}

quit(status = if (length(failures) > 0 && length(results) == 0) 1 else 0)
