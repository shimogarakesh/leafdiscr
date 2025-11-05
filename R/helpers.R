# Helper Functions for leafdiscr Package
# Internal functions - not exported

#' Process a single leaf disc image
#' @keywords internal
#' @noRd
process_single_image <- function(img_path, roi_coordinates_default, disc_diameter = 500, verbose = TRUE) {
  
  # Load image
  img <- magick::image_read(img_path)
  
  # Extract metadata from filename
  filename <- basename(img_path)
  filename_base <- sub("\\.jpg$", "", filename, ignore.case = TRUE)
  
  sp <- substr(filename_base, 1, 4)
  temp_treatment <- substr(filename_base, 5, 6)
  leafside <- substr(filename_base, 7, 8)
  
  if (verbose) {
    cat("────────────────────────────────────────\n")
    cat(sprintf("Processing: %s\n", filename))
    cat(sprintf("  Species: %s | Temperature: %s | Leaf Side: %s\n", sp, temp_treatment, leafside))
    cat("────────────────────────────────────────\n")
  }
  
  # Try automated ROI detection
  roi_coordinates <- tryCatch(
    detect_marked_squares(img),
    error = function(e) {
      if (verbose) cat("  [AUTO-DETECT FAILED - Using manual coordinates]\n")
      return(NULL)
    }
  )
  
  if (is.null(roi_coordinates)) {
    if (verbose) cat("  [Using default ROI coordinates]\n")
    roi_coordinates <- roi_coordinates_default
  } else {
    if (verbose) cat("  [Automated ROI detection successful]\n")
  }
  
  # Extract color data
  if (verbose) cat("  Extracting discs:\n")
  
  all_color_data <- purrr::map_df(roi_coordinates, function(roi) {
    tryCatch(
      extract_disc_colors(img, roi$x_min, roi$y_min, roi$x_max, roi$y_max,
                         roi$tree, roi$disc, sp, temp_treatment, leafside, disc_diameter),
      error = function(e) return(NULL)
    )
  })
  
  if (nrow(all_color_data) == 0) {
    if (verbose) cat(sprintf("  WARNING: No valid color data extracted\n"))
    return(NULL)
  }
  
  # Aggregate to disc summary
  disc_summary <- all_color_data %>%
    dplyr::group_by(sp, temp_treatment, leafside, tree_id, disc_id) %>%
    dplyr::summarise(
      n_pixels = dplyr::n(),
      n_green = sum(color_type == "green"),
      n_brown = sum(color_type == "brown"),
      pct_green = 100 * n_green / n_pixels,
      pct_brown = 100 * n_brown / n_pixels,
      mean_hue = mean(hue, na.rm = TRUE),
      sd_hue = sd(hue, na.rm = TRUE),
      mean_saturation = mean(saturation, na.rm = TRUE),
      mean_value = mean(value, na.rm = TRUE),
      mean_red = mean(red, na.rm = TRUE),
      mean_green_channel = mean(green, na.rm = TRUE),
      mean_blue = mean(blue, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (verbose) cat(sprintf("  ✓ Extracted %d discs with data\n", nrow(disc_summary)))
  
  return(disc_summary)
}


#' Detect marked squares in image
#' @keywords internal
#' @noRd
detect_marked_squares <- function(img, n_rows = 5, n_cols = 3) {
  
  temp_file <- tempfile(fileext = ".jpg")
  magick::image_write(img, path = temp_file, format = "jpeg")
  img_array <- EBImage::readImage(temp_file)
  unlink(temp_file)
  
  gray_img <- (img_array[, , 1] + img_array[, , 2] + img_array[, , 3]) / 3
  
  row_sums <- colSums(gray_img)
  col_sums <- rowSums(gray_img)
  
  row_sums_smooth <- caTools::runmean(row_sums, k = 50)
  col_sums_smooth <- caTools::runmean(col_sums, k = 50)
  
  row_diffs <- abs(diff(row_sums_smooth))
  col_diffs <- abs(diff(col_sums_smooth))
  
  row_peaks <- which(row_diffs > stats::quantile(row_diffs, 0.90))
  col_peaks <- which(col_diffs > stats::quantile(col_diffs, 0.90))
  
  row_clusters <- stats::kmeans(row_peaks, centers = n_rows + 1)$centers %>% sort()
  col_clusters <- stats::kmeans(col_peaks, centers = n_cols + 1)$centers %>% sort()
  
  roi_list <- list()
  idx <- 1
  
  for (tree in 1:n_rows) {
    for (disc in 1:n_cols) {
      y_min <- as.integer(row_clusters[tree])
      y_max <- as.integer(row_clusters[tree + 1])
      x_min <- as.integer(col_clusters[disc])
      x_max <- as.integer(col_clusters[disc + 1])
      
      roi_list[[idx]] <- list(
        tree = tree,
        disc = disc,
        x_min = x_min,
        y_min = y_min,
        x_max = x_max,
        y_max = y_max
      )
      idx <- idx + 1
    }
  }
  
  return(roi_list)
}


#' Extract color data from single disc
#' @keywords internal
#' @noRd
extract_disc_colors <- function(img, x_min, y_min, x_max, y_max, tree_id, disc_id,
                                sp, temp_treatment, leafside, disc_diameter = 500) {
  
  # Crop and convert
  width <- x_max - x_min
  height <- y_max - y_min
  cropped <- magick::image_crop(img, geometry = sprintf("%dx%d+%d+%d",
                                                        width, height, x_min, y_min))
  cropped <- magick::image_convert(cropped, colorspace = "RGB")
  
  # Read with EBImage
  temp_file <- tempfile(fileext = ".jpg")
  magick::image_write(cropped, path = temp_file, format = "jpeg")
  img_ebimage <- EBImage::readImage(temp_file)
  unlink(temp_file)
  
  img_width <- dim(img_ebimage)[1]
  img_height <- dim(img_ebimage)[2]
  
  # Scale to 0-255
  img_array <- img_ebimage
  if (max(img_array) <= 1) img_array <- img_array * 255
  
  # Circular mask
  center_x <- img_width / 2
  center_y <- img_height / 2
  radius <- disc_diameter / 2
  
  x_coords <- matrix(rep(seq_len(img_width), img_height), nrow = img_width, byrow = FALSE)
  y_coords <- matrix(rep(seq_len(img_height), each = img_width), nrow = img_width, byrow = FALSE)
  distances <- sqrt((x_coords - center_x)^2 + (y_coords - center_y)^2)
  circ_mask <- distances <= radius
  
  # Extract RGB
  red <- img_array[, , 1]
  green <- img_array[, , 2]
  blue <- img_array[, , 3]
  
  # Background filtering
  brightness <- (red + green + blue) / 3
  max_val <- pmax(red, green, blue)
  min_val <- pmin(red, green, blue)
  saturation <- ifelse(max_val > 0, (max_val - min_val) / max_val, 0)
  
  background_mask <- (brightness > 200) & (saturation < 0.1)
  foreground_mask <- circ_mask & !background_mask
  
  if (sum(foreground_mask) == 0) return(NULL)
  
  # Extract foreground pixels
  red_fg <- red[foreground_mask]
  green_fg <- green[foreground_mask]
  blue_fg <- blue[foreground_mask]
  
  # Compute HSV
  r_norm <- red_fg / 255
  g_norm <- green_fg / 255
  b_norm <- blue_fg / 255
  
  max_rgb <- pmax(r_norm, g_norm, b_norm)
  min_rgb <- pmin(r_norm, g_norm, b_norm)
  delta <- max_rgb - min_rgb
  
  # Hue calculation
  hue <- rep(NA, length(red_fg))
  
  idx_red <- max_rgb == r_norm & delta > 0
  hue[idx_red] <- (((g_norm[idx_red] - b_norm[idx_red]) / delta[idx_red]) %% 6) / 6
  
  idx_grn <- max_rgb == g_norm & delta > 0
  hue[idx_grn] <- (((b_norm[idx_grn] - r_norm[idx_grn]) / delta[idx_grn]) + 2) / 6
  
  idx_blu <- max_rgb == b_norm & delta > 0
  hue[idx_blu] <- (((r_norm[idx_blu] - g_norm[idx_blu]) / delta[idx_blu]) + 4) / 6
  
  hue[is.na(hue)] <- 0
  
  value <- max_rgb
  saturation_fg <- ifelse(max_rgb > 0, delta / max_rgb, 0)
  hue_deg <- hue * 360
  
  # Classify colors
  is_green <- (hue_deg >= 60 & hue_deg <= 180) | is.na(hue_deg)
  is_brown <- (hue_deg < 60 & hue_deg >= 0) | (hue_deg > 330 & hue_deg <= 360)
  
  # Binning
  hue_bins <- cut(hue_deg, breaks = seq(0, 360, by = 10), include.lowest = TRUE)
  sat_bins <- cut(saturation_fg, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)
  val_bins <- cut(value, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)
  
  # Create output
  color_dist <- tibble::tibble(
    sp = sp,
    temp_treatment = temp_treatment,
    leafside = leafside,
    tree_id = tree_id,
    disc_id = disc_id,
    hue_bin = hue_bins,
    saturation_bin = sat_bins,
    value_bin = val_bins,
    hue = hue_deg,
    saturation = saturation_fg,
    value = value,
    color_type = dplyr::case_when(
      is_green ~ "green",
      is_brown ~ "brown",
      TRUE ~ "other"
    ),
    red = red_fg,
    green = green_fg,
    blue = blue_fg
  ) %>%
    tidyr::drop_na(hue_bin, saturation_bin, value_bin)
  
  return(color_dist)
}