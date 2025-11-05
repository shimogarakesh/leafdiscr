#' Process Leaf Disc Images for Color Analysis
#'
#' @description
#' Processes all JPEG files in the working directory to extract RGB color data
#' from leaf disc scans. Quantifies green vs. brown tissue proportions for
#' thermal tolerance analysis.
#'
#' @param path Character. Path to directory containing JPEG files. Default is
#'   current working directory (".").
#' @param disc_diameter Numeric. Diameter of circular leaf discs in pixels.
#'   Default is 500.
#' @param roi_coordinates List. Optional custom ROI coordinates for each disc.
#'   If NULL (default), uses automated detection with fallback to default coordinates.
#' @param output_file Character. Name of output CSV file. Default is
#'   "leaf_disc_color_master_summary.csv".
#' @param verbose Logical. If TRUE, prints processing progress. Default is TRUE.
#'
#' @return A tibble containing summary statistics for each leaf disc across all
#'   processed images. Columns include species (sp), temperature treatment
#'   (temp_treatment), leaf side (leafside), tree_id, disc_id, pixel counts,
#'   color percentages, and mean color metrics.
#'
#' @details
#' The function expects JPEG filenames in the format: [species][temp][side]_*.jpg
#' where species is 4 characters, temperature is 2 characters, and side is 2 characters.
#' Example: saal50ls_20251101_0001.jpg
#'
#' Color classification uses HSV color space:
#' - Green: Hue 60-180 degrees
#' - Brown: Hue 0-60 or 330-360 degrees
#'
#' Background pixels (brightness > 200 and saturation < 0.1) are automatically excluded.
#'
#' @examples
#' \dontrun{
#' # Process all JPEG files in current directory
#' results <- process_leaf_discs()
#'
#' # Process with custom disc diameter
#' results <- process_leaf_discs(disc_diameter = 450)
#'
#' # Process files in specific directory
#' results <- process_leaf_discs(path = "data/leaf_scans")
#' }
#'
#' @export
#' @import magick
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import readr
#' @import tibble
#' @importFrom EBImage readImage
#' @importFrom caTools runmean
process_leaf_discs <- function(path = ".",
                               disc_diameter = 500,
                               roi_coordinates = NULL,
                               output_file = "leaf_disc_color_master_summary.csv",
                               verbose = TRUE) {
  
  # Find all JPEG files
  jpeg_files <- list.files(
    path = path,
    pattern = "\\.(jpg|jpeg)$",
    ignore.case = TRUE,
    full.names = FALSE
  )
  
  if (length(jpeg_files) == 0) {
    stop("No JPEG files found in specified directory!")
  }
  
  if (verbose) {
    cat("========================================\n")
    cat("BULK PROCESSING: LEAF DISC RGB ANALYSIS\n")
    cat("========================================\n\n")
    cat(sprintf("Found %d JPEG file(s) to process:\n", length(jpeg_files)))
    for (i in seq_along(jpeg_files)) {
      cat(sprintf("  %d. %s\n", i, jpeg_files[i]))
    }
    cat("\n")
  }
  
  # Initialize master dataframe
  master_disc_summary <- tibble::tibble()
  
  # Default ROI coordinates if not provided
  if (is.null(roi_coordinates)) {
    roi_coordinates_default <- list(
      list(tree = 1, disc = 1, x_min = 580,  y_min = 5647, x_max = 1350, y_max = 6465),
      list(tree = 1, disc = 2, x_min = 2080, y_min = 5647, x_max = 2840, y_max = 6465),
      list(tree = 1, disc = 3, x_min = 3565, y_min = 5647, x_max = 4332, y_max = 6465),
      list(tree = 2, disc = 1, x_min = 580,  y_min = 4375, x_max = 1350, y_max = 5185),
      list(tree = 2, disc = 2, x_min = 2080, y_min = 4375, x_max = 2840, y_max = 5185),
      list(tree = 2, disc = 3, x_min = 3565, y_min = 4375, x_max = 4332, y_max = 5185),
      list(tree = 3, disc = 1, x_min = 580,  y_min = 3100, x_max = 1350, y_max = 3916),
      list(tree = 3, disc = 2, x_min = 2080, y_min = 3100, x_max = 2840, y_max = 3916),
      list(tree = 3, disc = 3, x_min = 3565, y_min = 3100, x_max = 4332, y_max = 3916),
      list(tree = 4, disc = 1, x_min = 580,  y_min = 1847, x_max = 1350, y_max = 2623),
      list(tree = 4, disc = 2, x_min = 2080, y_min = 1847, x_max = 2840, y_max = 2623),
      list(tree = 4, disc = 3, x_min = 3565, y_min = 1847, x_max = 4332, y_max = 2623),
      list(tree = 5, disc = 1, x_min = 580,  y_min = 580,  x_max = 1350, y_max = 1350),
      list(tree = 5, disc = 2, x_min = 2080, y_min = 580,  x_max = 2840, y_max = 1350),
      list(tree = 5, disc = 3, x_min = 3565, y_min = 580,  x_max = 4332, y_max = 1350)
    )
  } else {
    roi_coordinates_default <- roi_coordinates
  }
  
  # Process each file
  for (i in seq_along(jpeg_files)) {
    
    img_path <- file.path(path, jpeg_files[i])
    
    tryCatch(
      {
        result <- process_single_image(
          img_path = img_path,
          roi_coordinates_default = roi_coordinates_default,
          disc_diameter = disc_diameter,
          verbose = verbose
        )
        
        if (!is.null(result)) {
          master_disc_summary <- dplyr::bind_rows(master_disc_summary, result)
          if (verbose) {
            cat(sprintf("✓ Successfully processed: %s\n\n", jpeg_files[i]))
          }
        } else {
          if (verbose) {
            cat(sprintf("✗ Failed to extract data from: %s\n\n", jpeg_files[i]))
          }
        }
      },
      error = function(e) {
        if (verbose) {
          cat(sprintf("✗ ERROR processing %s: %s\n\n", jpeg_files[i], e$message))
        }
      }
    )
  }
  
  # Final summary
  if (verbose) {
    cat("\n")
    cat("========================================\n")
    cat("BULK PROCESSING COMPLETE\n")
    cat("========================================\n\n")
    cat(sprintf("Total files processed: %d\n", length(jpeg_files)))
    cat(sprintf("Total discs in master summary: %d\n", nrow(master_disc_summary)))
    cat(sprintf("Total unique samples: %d\n\n", 
                dplyr::n_distinct(master_disc_summary %>% 
                                    dplyr::select(sp, temp_treatment, leafside))))
  }
  
  # Export to CSV
  readr::write_csv(master_disc_summary, file.path(path, output_file))
  
  if (verbose) {
    cat(sprintf("✓ Exported: %s\n", output_file))
    cat(sprintf("  Dimensions: %d rows × %d columns\n", 
                nrow(master_disc_summary), ncol(master_disc_summary)))
    cat("\n✓ Processing pipeline complete!\n")
  }
  
  return(master_disc_summary)
}