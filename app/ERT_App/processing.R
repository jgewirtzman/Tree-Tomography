# ERT Image Processing Functions
#
# Shared between the Shiny app (app.R) and the headless batch CLI (batch.R).
# All functions here are pure (no Shiny session state).

library(imager)
library(dplyr)
library(DescTools)
library(sp)

# Cumulative arc-length parameter (robust to uneven sampling)
arc_length_param <- function(col_df) {
  if (nrow(col_df) < 2) return(rep(0, nrow(col_df)))
  d <- sqrt(diff(col_df$r)^2 + diff(col_df$g)^2 + diff(col_df$b)^2)
  s <- c(0, cumsum(d))
  if (max(s) > 0) s / max(s) else s
}

# Extract horizontal colorbar from rows 3:8 of the image
extract_colorbar <- function(img) {
  W <- width(img)
  rows <- 3:8

  r_mat <- matrix(NA_real_, nrow = length(rows), ncol = W)
  g_mat <- matrix(NA_real_, nrow = length(rows), ncol = W)
  b_mat <- matrix(NA_real_, nrow = length(rows), ncol = W)

  for (i in seq_along(rows)) {
    r_mat[i, ] <- as.vector(img[, rows[i], 1, 1])
    g_mat[i, ] <- as.vector(img[, rows[i], 1, 2])
    b_mat[i, ] <- as.vector(img[, rows[i], 1, 3])
  }

  r <- colMeans(r_mat, na.rm = TRUE)
  g <- colMeans(g_mat, na.rm = TRUE)
  b <- colMeans(b_mat, na.rm = TRUE)

  cb <- data.frame(r = r, g = g, b = b)

  if (nrow(cb) > 6) {
    cb <- cb[3:(nrow(cb)-2), ]
  }

  if (nrow(cb) >= 5) {
    smooth <- function(v) {
      n <- length(v)
      result <- numeric(n)
      for(i in 1:n) {
        start_idx <- max(1, i - 2)
        end_idx <- min(n, i + 2)
        result[i] <- mean(v[start_idx:end_idx])
      }
      result
    }
    cb$r <- smooth(cb$r)
    cb$g <- smooth(cb$g)
    cb$b <- smooth(cb$b)
  }

  if (nrow(cb) > 256) {
    idx <- round(seq(1, nrow(cb), length.out = 256))
    cb <- cb[idx, , drop = FALSE]
  }

  left_blue_score  <- cb$b[1] - cb$r[1]
  right_red_score  <- cb$r[nrow(cb)] - cb$b[nrow(cb)]
  if ( (left_blue_score + right_red_score) < 0 ) {
    cb <- cb[nrow(cb):1, , drop = FALSE]
  }

  attr(cb, "t") <- arc_length_param(cb)
  cb
}

# Build a calibration function from position-value pairs
create_calibration_function <- function(calib_positions, calib_values, transform = "log") {
  valid <- !is.na(calib_positions) & !is.na(calib_values)
  if(sum(valid) < 2) {
    stop("Need at least 2 calibration points")
  }

  positions <- calib_positions[valid]
  values <- calib_values[valid]

  ord <- order(positions)
  positions <- positions[ord]
  values <- values[ord]

  if(transform == "log") {
    log_values <- log10(values)

    if(length(positions) <= 3) {
      interp_func <- approxfun(positions, log_values, rule = 2)
      calib_func <- function(x) {
        10^interp_func(x)
      }
    } else {
      fit <- smooth.spline(positions, log_values, spar = 0.5)
      calib_func <- function(x) {
        10^predict(fit, x)$y
      }
    }
  } else if(transform == "linear") {
    calib_func <- approxfun(positions, values, rule = 2)
  } else if(transform == "power") {
    if(length(positions) >= 2) {
      log_pos <- log10(positions[positions > 0])
      log_val <- log10(values[positions > 0])
      fit <- lm(log_val ~ log_pos)
      a <- 10^coef(fit)[1]
      b <- coef(fit)[2]
      calib_func <- function(x) {
        a * x^b
      }
    } else {
      calib_func <- approxfun(positions, values, rule = 2)
    }
  }

  return(calib_func)
}

# Map pixel colors to calibrated values via nearest neighbour in colorbar RGB space
map_colors_calibrated <- function(pixel_colors, colorbar_colors, calib_func) {
  if (is.null(colorbar_colors) || nrow(colorbar_colors) < 2) {
    stop("Colorbar not available or too short")
  }

  cb_mat <- as.matrix(colorbar_colors[, c("r","g","b")])
  t_cb   <- attr(colorbar_colors, "t")
  if (is.null(t_cb)) {
    t_cb <- seq(0, 1, length.out = nrow(colorbar_colors))
  }

  px <- as.matrix(pixel_colors)
  n  <- nrow(px)
  t_hat <- numeric(n)

  chunk <- 2000L
  for (start in seq(1L, n, by = chunk)) {
    end <- min(n, start + chunk - 1L)
    x <- px[start:end, , drop = FALSE]

    for (i in 1:nrow(x)) {
      d <- (cb_mat[,1] - x[i,1])^2 + (cb_mat[,2] - x[i,2])^2 + (cb_mat[,3] - x[i,3])^2
      k <- which.min(d)
      t_hat[start + i - 1L] <- t_cb[k]
    }
  }

  vals <- calib_func(t_hat)
  vals
}

process_ert_with_mask <- function(img, mask_vector, image_name,
                                  calib_positions = NULL, calib_values = NULL,
                                  transform = "log") {

  W <- width(img)
  H <- height(img)

  colorbar <- extract_colorbar(img)

  img_df <- as.data.frame(img, wide = "c") %>%
    mutate(
      inside = mask_vector,
      r = c.1,
      g = c.2,
      b = c.3
    ) %>%
    filter(inside == TRUE)

  if(nrow(img_df) == 0) {
    stop("No pixels found in mask")
  }

  pixel_colors <- as.matrix(img_df[, c("r", "g", "b")])

  if(!is.null(colorbar) && nrow(colorbar) > 10) {
    if(!is.null(calib_positions) && !is.null(calib_values)) {
      calib_func <- create_calibration_function(calib_positions, calib_values, transform)
      values <- map_colors_calibrated(pixel_colors, colorbar, calib_func)
    } else {
      warning("No calibration provided, using 0-1 scale")
      cb_mat <- as.matrix(colorbar[, c("r","g","b")])
      t_cb <- attr(colorbar, "t")
      if(is.null(t_cb)) t_cb <- seq(0, 1, length.out = nrow(colorbar))

      px <- as.matrix(pixel_colors)
      values <- numeric(nrow(px))
      for(i in 1:nrow(px)) {
        d <- (cb_mat[,1] - px[i,1])^2 + (cb_mat[,2] - px[i,2])^2 + (cb_mat[,3] - px[i,3])^2
        k <- which.min(d)
        values[i] <- t_cb[k]
      }
    }
  } else {
    stop("Could not extract colorbar")
  }

  valid_idx <- !is.na(values)
  values <- values[valid_idx]
  img_df <- img_df[valid_idx,]

  if(length(values) == 0) {
    stop("No valid values after processing")
  }

  cx <- mean(img_df$x)
  cy <- mean(img_df$y)
  r <- sqrt((img_df$x - cx)^2 + (img_df$y - cy)^2)
  r_norm <- r / max(r)

  n_rings <- 8
  ring_breaks <- seq(0, 1, length.out = n_rings + 1)
  ring_ids <- cut(r_norm, breaks = ring_breaks, include.lowest = TRUE, labels = FALSE)
  ring_means <- tapply(values, ring_ids, mean, na.rm = TRUE)
  ring_means[is.na(ring_means)] <- 0

  q_low <- quantile(values, probs = 0.30, na.rm = TRUE)
  is_low <- values <= q_low
  is_inner <- r_norm <= 0.33

  if(sum(is_low) > 0) {
    CMA <- sum(is_low & is_inner) / sum(is_low)
  } else {
    CMA <- 0
  }

  center_vals <- values[r_norm <= 0.33]
  edge_vals <- values[r_norm >= 0.67]

  center_mean <- if(length(center_vals) > 0) mean(center_vals) else mean(values)
  edge_mean <- if(length(edge_vals) > 0) mean(edge_vals) else mean(values)

  list(
    file = image_name,
    mean = mean(values),
    median = median(values),
    sd = sd(values),
    cv = sd(values) / mean(values),
    gini = Gini(values),
    entropy = Entropy(values, method = "ML"),
    CMA = CMA,
    center_mean = center_mean,
    edge_mean = edge_mean,
    radial_gradient = edge_mean - center_mean,
    n_pixels = length(values),
    ring_profile = as.numeric(ring_means),
    values = values,
    spatial_data = img_df
  )
}

# Auto-detect PiCUS blue boundary polygon
auto_detect_polygon <- function(img) {
  W <- width(img)
  H <- height(img)
  colorbar_cutoff <- min(40, round(H * 0.08))

  rows <- (colorbar_cutoff + 1):H
  r_vals <- as.vector(img[, rows, 1, 1])
  g_vals <- as.vector(img[, rows, 1, 2])
  b_vals <- as.vector(img[, rows, 1, 3])

  grid <- expand.grid(x = 1:W, row_idx = seq_along(rows))
  grid$y <- rows[grid$row_idx]
  grid$r <- r_vals
  grid$g <- g_vals
  grid$b <- b_vals

  grid$is_blue <- (grid$b > 0.4) & (grid$r < 0.3) & (grid$g < 0.4) &
                  ((grid$b - grid$g) > 0.15)

  blue_pts <- grid[grid$is_blue, c("x", "y")]

  if (nrow(blue_pts) < 20) {
    stop("Could not detect blue boundary polygon (only ", nrow(blue_pts),
         " blue pixels found). Use manual polygon instead.")
  }

  cx <- median(blue_pts$x)
  cy <- median(blue_pts$y)

  angles <- atan2(blue_pts$y - cy, blue_pts$x - cx)
  dists  <- sqrt((blue_pts$x - cx)^2 + (blue_pts$y - cy)^2)

  n_bins <- 180
  bin_edges <- seq(-pi, pi, length.out = n_bins + 1)
  poly_x <- numeric(n_bins)
  poly_y <- numeric(n_bins)
  valid   <- logical(n_bins)

  for (i in 1:n_bins) {
    in_bin <- angles >= bin_edges[i] & angles < bin_edges[i + 1]
    if (any(in_bin)) {
      max_idx <- which(in_bin)[which.max(dists[in_bin])]
      poly_x[i] <- blue_pts$x[max_idx]
      poly_y[i] <- blue_pts$y[max_idx]
      valid[i]  <- TRUE
    }
  }

  poly_x <- poly_x[valid]
  poly_y <- poly_y[valid]

  n <- length(poly_x)
  if (n > 10) {
    k <- 3
    px_s <- numeric(n); py_s <- numeric(n)
    for (i in 1:n) {
      idx <- ((i + (-k:k)) - 1) %% n + 1
      px_s[i] <- mean(poly_x[idx])
      py_s[i] <- mean(poly_y[idx])
    }
    poly_x <- px_s; poly_y <- py_s
  }

  cbind(poly_x, poly_y)
}

# Build a logical mask vector from polygon points, optionally eroded
build_mask <- function(polygon_points, W, H, erosion = 0) {
  grid <- expand.grid(x = 1:W, y = 1:H)
  mask <- point.in.polygon(grid$x, grid$y,
                           polygon_points[,1],
                           polygon_points[,2])
  mask <- mask > 0

  if (erosion > 0) {
    mask_array <- array(mask, dim = c(W, H, 1, 1))
    mask_cimg <- as.cimg(mask_array)
    for (i in 1:erosion) {
      mask_cimg <- erode_square(mask_cimg, size = 3)
    }
    mask <- as.logical(as.vector(mask_cimg))
  }

  mask
}

# Full headless pipeline: load image, auto-detect polygon, process.
# Returns the process_ert_with_mask result list, or stops with an error.
run_auto_pipeline <- function(image_path, image_name = basename(image_path),
                              calib_positions, calib_values,
                              transform = "log", erosion = 2) {
  img <- load.image(image_path)
  poly <- auto_detect_polygon(img)
  mask <- build_mask(poly, width(img), height(img), erosion = erosion)
  if (sum(mask) == 0) {
    stop("Mask is empty after erosion")
  }
  process_ert_with_mask(
    img = img,
    mask_vector = mask,
    image_name = image_name,
    calib_positions = calib_positions,
    calib_values = calib_values,
    transform = transform
  )
}

# Default calibration matching the Shiny app's pre-filled values
DEFAULT_CALIB_POSITIONS <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0)
DEFAULT_CALIB_VALUES    <- c(30, 61, 125, 254, 518, 1000)

# Convert a result list (from process_ert_with_mask) to a one-row data frame
# matching the columns used in the Shiny results table.
result_to_row <- function(result) {
  data.frame(
    Filename = result$file,
    Mean = round(result$mean, 2),
    Median = round(result$median, 2),
    SD = round(result$sd, 2),
    CV = round(result$cv, 4),
    Gini = round(result$gini, 4),
    Entropy = round(result$entropy, 4),
    CMA = round(result$CMA, 4),
    RadialGradient = round(result$radial_gradient, 2),
    NPixels = result$n_pixels,
    stringsAsFactors = FALSE
  )
}
