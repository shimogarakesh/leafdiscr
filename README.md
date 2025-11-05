# leafdiscr <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/shimogarakesh/leafdiscr/workflows/R-CMD-check/badge.svg)](https://github.com/shimogarakesh/leafdiscr/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Overview

`leafdiscr` provides automated tools for extracting and analyzing RGB color data from scanned leaf disc images in thermal tolerance experiments. The package quantifies the proportion of green (healthy) vs. brown (damaged) tissue, enabling rapid assessment of plant stress responses to temperature treatments.

## Key Features

- **Circular disc masking** — Automatically extracts only pixels within circular leaf disc boundaries (default 500px diameter)
- **Background filtering** — Removes white background and non-leaf pixels based on brightness and saturation thresholds
- **Automated ROI detection** — Detects marked grid squares automatically, with fallback to manual coordinates
- **Bulk processing** — Processes all JPEG files in a directory and consolidates results into a single dataframe
- **Metadata extraction** — Parses species, temperature treatment, and leaf side from standardized filenames
- **HSV color classification** — Classifies pixels as green (60-180° hue) or brown (0-60° or 330-360° hue)
- **Compact output** — Exports summary statistics per disc rather than individual pixel data

## Installation

### From GitHub (Development Version)

```r
# Install devtools if needed
if (!require("devtools")) install.packages("devtools")

# Install leafdiscr
devtools::install_github("shimogarakesh/leafdiscr")
```

### Bioconductor Dependency

The package requires `EBImage` from Bioconductor:

```r
if (!require("BiocManager")) install.packages("BiocManager")
BiocManager::install("EBImage")
```

## Quick Start

### Filename Convention

Images must follow this naming convention:
```
[species][temp][side]_[date]_[scan].jpg

Example: saal50ls_20251101_0001.jpg
  - saal: species code (4 characters)
  - 50: temperature treatment (2 characters)
  - ls: leaf side (2 characters)
```

### Basic Usage

```r
library(leafdiscr)

# Set working directory to folder containing JPEG scans
setwd("/path/to/your/jpeg/files")

# Process all images and get combined results
results <- process_leaf_discs()

# View summary
head(results)

# Export to CSV (automatically saved as "leaf_disc_color_master_summary.csv")
```

### Customization

```r
# Adjust disc diameter (in pixels)
results <- process_leaf_discs(disc_diameter = 450)

# Provide custom ROI coordinates if auto-detection fails
custom_roi <- list(
  list(tree = 1, disc = 1, x_min = 580, y_min = 5647, x_max = 1350, y_max = 6465),
  # ... additional coordinates
)

results <- process_leaf_discs(roi_coordinates = custom_roi)
```

## Output Format

The function returns a tibble with the following columns:

| Column | Description |
|--------|-------------|
| `sp` | Species code (4-character) |
| `temp_treatment` | Temperature treatment (2-character) |
| `leafside` | Leaf side (2-character) |
| `tree_id` | Tree/individual ID (1-5) |
| `disc_id` | Disc ID within tree (1-3) |
| `n_pixels` | Total foreground pixels |
| `n_green` | Count of green pixels |
| `n_brown` | Count of brown pixels |
| `pct_green` | Percentage of green pixels |
| `pct_brown` | Percentage of brown pixels |
| `mean_hue` | Mean hue (0-360°) |
| `sd_hue` | Standard deviation of hue |
| `mean_saturation` | Mean saturation (0-1) |
| `mean_value` | Mean brightness value (0-1) |
| `mean_red` | Mean red channel (0-255) |
| `mean_green_channel` | Mean green channel (0-255) |
| `mean_blue` | Mean blue channel (0-255) |

## Example Workflow

```r
library(leafdiscr)
library(dplyr)
library(ggplot2)

# Process all leaf disc scans
results <- process_leaf_discs()

# Calculate treatment means
treatment_summary <- results %>%
  group_by(sp, temp_treatment, leafside) %>%
  summarise(
    mean_pct_green = mean(pct_green),
    se_pct_green = sd(pct_green) / sqrt(n()),
    .groups = "drop"
  )

# Visualize green percentage by temperature
ggplot(treatment_summary, aes(x = temp_treatment, y = mean_pct_green, color = sp)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_pct_green - se_pct_green, 
                    ymax = mean_pct_green + se_pct_green), width = 0.2) +
  labs(title = "Thermal Tolerance: Green Tissue by Temperature",
       x = "Temperature Treatment (°C)",
       y = "Green Tissue (%)",
       color = "Species") +
  theme_minimal()
```

## Methodology

### Color Classification

The package uses HSV (Hue-Saturation-Value) color space for robust classification:

- **Green pixels**: Hue between 60° and 180°
- **Brown pixels**: Hue between 0°-60° or 330°-360°
- **Other**: Remaining hues (typically minor contributions)

### Background Removal

Pixels are excluded if:
- Brightness > 200 (on 0-255 scale) AND
- Saturation < 0.1 (on 0-1 scale)

This effectively removes white/light background while retaining leaf tissue.

### Circular Masking

Only pixels within a circular region (default 500px diameter) centered on each marked square are analyzed, eliminating corner artifacts from the square bounding boxes.

## Citation

If you use `leafdiscr` in your research, please cite:

```
Rakesh Tiwari (2025). leafdiscr: Automated Leaf Disc Color Analysis for Thermal 
Tolerance Research. R package version 0.1.0. 
https://github.com/shimogarakesh/leafdiscr
```

## Contributing

Contributions are welcome! Please open an issue or submit a pull request on [GitHub](https://github.com/yourusername/leafdiscr).

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Support

For questions or issues:
- Open an issue: https://github.com/shimogarakesh/leafdiscr/issues
- Email: shimogarakesh@gmail.com
