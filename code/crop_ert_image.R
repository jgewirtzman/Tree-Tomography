#!/usr/bin/env Rscript
# Crop a single ERT tomogram to its blue PiCUS boundary polygon.
# Reuses auto_detect_polygon_jpeg() logic from fig_ert_workflow.R / ERT_App.R.
#
# Usage: Rscript code/crop_ert_image.R <input_jpg> <output_jpg>
# Default: crops images/main_ERT_normalized/309_18vii24.jpg

suppressPackageStartupMessages({
  library(jpeg)
  library(png)
  library(sp)
})

auto_detect_polygon_jpeg <- function(img) {
  H <- nrow(img); W <- ncol(img)
  colorbar_cutoff <- 40

  rows <- (colorbar_cutoff + 1):H
  r_vals <- as.vector(img[rows, , 1])
  g_vals <- as.vector(img[rows, , 2])
  b_vals <- as.vector(img[rows, , 3])
  grid_xy <- expand.grid(row = rows, col = 1:W)
  is_blue <- (b_vals > 0.4) & (r_vals < 0.3) & (g_vals < 0.4) & ((b_vals - g_vals) > 0.15)
  blue_pts <- data.frame(row = grid_xy$row[is_blue], col = grid_xy$col[is_blue])
  cat("Blue polygon pixels detected:", nrow(blue_pts), "\n")

  cx <- median(blue_pts$col); cy <- median(blue_pts$row)
  angles <- atan2(blue_pts$row - cy, blue_pts$col - cx)
  dists  <- sqrt((blue_pts$col - cx)^2 + (blue_pts$row - cy)^2)

  n_bins <- 180
  bin_edges <- seq(-pi, pi, length.out = n_bins + 1)
  bin_mid   <- (bin_edges[-1] + bin_edges[-(n_bins + 1)]) / 2

  # Per-bin max distance (NA if bin empty or label-occluded)
  bin_dist <- rep(NA_real_, n_bins)
  for (i in seq_len(n_bins)) {
    in_bin <- angles >= bin_edges[i] & angles < bin_edges[i + 1]
    if (any(in_bin)) bin_dist[i] <- max(dists[in_bin])
  }

  # Rolling median (circular) is robust to inward dips from label-occluded bins.
  # Use it as a baseline to flag occluded bins as "invalid", then circular-
  # interpolate over those bins. Bins with intact boundary keep their original
  # max distance, so the polygon hugs the true boundary without overshooting.
  win <- 6
  roll_med <- numeric(n_bins)
  for (i in seq_len(n_bins)) {
    idx <- ((i + (-win:win)) - 1) %% n_bins + 1
    vals <- bin_dist[idx]
    vals <- vals[!is.na(vals)]
    roll_med[i] <- if (length(vals)) median(vals) else NA_real_
  }
  valid <- !is.na(bin_dist) & !is.na(roll_med) & bin_dist >= 0.90 * roll_med
  poly_dist <- bin_dist
  if (any(!valid)) {
    ang_v <- bin_mid[valid]; d_v <- bin_dist[valid]
    ang_ext <- c(ang_v - 2*pi, ang_v, ang_v + 2*pi)
    d_ext   <- c(d_v, d_v, d_v)
    poly_dist[!valid] <- approx(ang_ext, d_ext, xout = bin_mid[!valid])$y
  }

  poly_col <- cx + poly_dist * cos(bin_mid)
  poly_row <- cy + poly_dist * sin(bin_mid)

  # Circular k=3 smoothing
  n <- length(poly_col)
  k <- 3
  pc_s <- pr_s <- numeric(n)
  for (i in seq_len(n)) {
    idx <- ((i + (-k:k)) - 1) %% n + 1
    pc_s[i] <- mean(poly_col[idx]); pr_s[i] <- mean(poly_row[idx])
  }
  list(col = pc_s, row = pr_s)
}

args <- commandArgs(trailingOnly = TRUE)
in_path  <- if (length(args) >= 1) args[1] else "images/main_ERT_normalized/309_18vii24.jpg"
out_path <- if (length(args) >= 2) args[2] else "output/cropped/309_18vii24_cropped.png"

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

img <- readJPEG(in_path)
H <- nrow(img); W <- ncol(img)
cat("Input:", in_path, "  size:", W, "x", H, "\n")

poly <- auto_detect_polygon_jpeg(img)

# Build inside-polygon mask
grid_all <- expand.grid(row = 1:H, col = 1:W)
pip <- point.in.polygon(grid_all$col, grid_all$row, poly$col, poly$row)
mask_mat <- matrix(FALSE, nrow = H, ncol = W)
mask_mat[cbind(grid_all$row[pip > 0], grid_all$col[pip > 0])] <- TRUE

# Composite RGBA: outside polygon -> alpha = 0
alpha <- ifelse(mask_mat, 1, 0)
rgba <- array(0, dim = c(H, W, 4))
rgba[, , 1:3] <- img[, , 1:3]
rgba[, , 4]   <- alpha

# Crop to polygon bounding box (with 2-px pad)
pad <- 2
r_min <- max(1, floor(min(poly$row)) - pad)
r_max <- min(H, ceiling(max(poly$row)) + pad)
c_min <- max(1, floor(min(poly$col)) - pad)
c_max <- min(W, ceiling(max(poly$col)) + pad)
rgba <- rgba[r_min:r_max, c_min:c_max, ]

cat("Cropped size:", dim(rgba)[2], "x", dim(rgba)[1],
    "(rows ", r_min, "-", r_max, ", cols ", c_min, "-", c_max, ")\n")

writePNG(rgba, target = out_path)
cat("Wrote:", out_path, "\n")
