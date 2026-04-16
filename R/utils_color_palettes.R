# ────────────────────────────────────────────────
# IPHRA Color Palettes for Data Visualization
# ────────────────────────────────────────────────

#' REACH Color Palette
#'
#' Provides access to the REACH brand color palettes including primary and secondary colors
#' organized into 4 distinct color ramps.
#'
#' @param ramp Integer from 1 to 4 specifying which color ramp to use.
#'   - Ramp 1: Red/Pink tones (primary)
#'   - Ramp 2: Gray tones (primary)
#'   - Ramp 3: Beige tones (secondary)
#'   - Ramp 4: Light gray tones (secondary)
#' @param n Number of colors to return. If NULL, returns all 5 colors from the ramp.
#'   If n is less than 5, interpolates to return exactly n colors.
#'   If n is greater than 5, interpolates to return exactly n colors.
#' @param reverse Logical indicating whether to reverse the color order. Default is FALSE.
#'
#' @return A character vector of hex color codes.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get all colors from ramp 1 (red/pink)
#'   reach_colors(ramp = 1)
#'
#'   # Get 3 colors from ramp 2 (gray)
#'   reach_colors(ramp = 2, n = 3)
#'
#'   # Get colors in reverse order
#'   reach_colors(ramp = 1, reverse = TRUE)
#' }
reach_colors <- function(ramp = 1, n = NULL, reverse = FALSE) {
  origin <- "reach_colors"

  # Define REACH color ramps
  reach_palettes <- list(
    # PRIMARY COLOURS
    # Color Ramp 1: Red/Pink tones
    ramp1 = c(
      "#EE5859",  # RGB(238,90,89)
      "#F1797A",  # RGB(243,121,122)
      "#F49A9B",  # RGB(243,155,156)
      "#F8BCBC",  # RGB(249,189,188)
      "#FBDDDD"   # RGB(252,222,220)
    ),
    # Color Ramp 2: Gray tones
    ramp2 = c(
      "#58585A",  # RGB(88,88,90)
      "#79797B",  # RGB(122,121,123)
      "#9A9A9C",  # RGB(154,153,154)
      "#BCBCBD",  # RGB(187,188,189)
      "#DDDDDE"   # RGB(221,221,222)
    ),
    # SECONDARY COLOURS
    # Color Ramp 3: Beige tones
    ramp3 = c(
      "#D2CBB8",  # RGB(209,202,184)
      "#DBD5C6",  # RGB(221,214,198)
      "#E4DFD4",  # RGB(227,222,211)
      "#EDEAE2",  # RGB(237,234,227)
      "#F6F4F0"   # RGB(247,245,240)
    ),
    # Color Ramp 4: Light gray tones
    ramp4 = c(
      "#C7C8CA",  # RGB(198,200,202)
      "#D2D3D4",  # RGB(209,211,213)
      "#DDDEDF",  # RGB(221,221,222)
      "#E8E9E9",  # RGB(231,233,232)
      "#F3F4F4"   # RGB(244,244,244)
    )
  )

  # Validate ramp parameter
  phr_assert(ramp %in% 1:4,
               phr_txt("ramp must be an integer between 1 and 4"),
               origin = origin)

  # Get selected ramp
  colors <- reach_palettes[[paste0("ramp", ramp)]]

  # Reverse if requested
  if (reverse) {
    colors <- rev(colors)
  }

  # If n is specified, interpolate to get exactly n colors
  if (!is.null(n)) {
    phr_assert(is.numeric(n) && n > 0,
                 phr_txt("n must be a positive integer"),
                 origin = origin)

    if (n == 1) {
      # Return middle color for single color request
      colors <- colors[3]
    } else if (n != length(colors)) {
      # Use colorRampPalette to interpolate
      colors <- grDevices::colorRampPalette(colors)(n)
    }
  }

  return(colors)
}


#' Traffic Light Color Palette
#'
#' Provides a traffic light color palette with red, orange/yellow, green, and light grey
#' in that order (worst to best). Useful for quality indicators, status displays, and
#' categorical data where the first level is the worst outcome.
#'
#' @param n Number of colors to return. If NULL, returns all 4 colors (red, orange, green, grey).
#'   If n is less than 4, returns the first n colors in order.
#'   If n is greater than 4, cycles through the colors or interpolates.
#' @param include_grey Logical indicating whether to include the light grey color. Default is TRUE.
#' @param reverse Logical indicating whether to reverse the color order. Default is FALSE.
#'
#' @return A character vector of hex color codes.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get all traffic light colors
#'   traffic_light_colors()
#'
#'   # Get only red, orange, green (no grey)
#'   traffic_light_colors(include_grey = FALSE)
#'
#'   # Get 6 colors by cycling
#'   traffic_light_colors(n = 6)
#' }
traffic_light_colors <- function(n = NULL, include_grey = TRUE, reverse = FALSE) {
  origin <- "traffic_light_colors"

  # Define traffic light palette (worst to best: red, orange/yellow, green, grey for NA)
  colors <- c(
    "#E74C3C",  # Red (critical/fail - worst)
    "#F39C12",  # Yellow/Orange (warning)
    "#2ECC71",  # Green (good/pass - best)
    "#BDC3C7"   # Light grey (neutral/NA)
  )

  # Remove grey if not included
  if (!include_grey) {
    colors <- colors[1:3]
  }

  # Reverse if requested
  if (reverse) {
    colors <- rev(colors)
  }

  # If n is specified, adjust the color vector
  if (!is.null(n)) {
    phr_assert(is.numeric(n) && n > 0,
                 phr_txt("n must be a positive integer"),
                 origin = origin)

    if (n <= length(colors)) {
      # Return first n colors
      colors <- colors[1:n]
    } else {
      # Cycle through colors to reach n
      colors <- rep(colors, length.out = n)
    }
  }

  return(colors)
}


#' Sex Comparison Color Palette
#'
#' Provides a standard 2-color palette for sex/gender comparisons (e.g., Male vs Female).
#' Uses colors from REACH brand palette that provide good contrast.
#'
#' @param reverse Logical indicating whether to reverse the color order. Default is FALSE.
#'   When FALSE, returns [Male color, Female color].
#'   When TRUE, returns [Female color, Male color].
#'
#' @return A character vector of 2 hex color codes: Male and Female.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get sex comparison colors (Male, Female)
#'   sex_colors()
#'
#'   # Get reversed order (Female, Male)
#'   sex_colors(reverse = TRUE)
#' }
sex_colors <- function(reverse = FALSE) {
  # Define sex comparison palette using REACH colors
  # Using darker shades for good contrast and readability
  colors <- c(
    "#58585A",  # Male - Dark gray (from REACH ramp 2)
    "#EE5859"   # Female - Red/Pink (from REACH ramp 1)
  )

  # Reverse if requested
  if (reverse) {
    colors <- rev(colors)
  }

  return(colors)
}


#' Group Comparison Color Palette
#'
#' Provides a set of highly distinguishable colors for comparing multiple groups
#' (e.g., administrative regions, strata, program areas). Colors are selected to maximize
#' visual contrast and avoid light or similar tones that make distinctions difficult.
#'
#' @param n Number of colors to return. If NULL, returns all 10 colors from the palette.
#'   If n is less than or equal to 10, returns the first n colors.
#'   If n is greater than 10, interpolates to generate exactly n colors.
#' @param reverse Logical indicating whether to reverse the color order. Default is FALSE.
#'
#' @return A character vector of hex color codes.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get all group comparison colors
#'   group_colors()
#'
#'   # Get 5 colors for 5 groups
#'   group_colors(n = 5)
#'
#'   # Get colors in reverse order
#'   group_colors(reverse = TRUE)
#' }
group_colors <- function(n = NULL, reverse = FALSE) {
  origin <- "group_colors"

  # Define group comparison palette with highly distinguishable colors
  # Selected to maximize contrast and avoid light/similar tones
  colors <- c(
    "#E63946",  # Red - Strong, warm
    "#1D3557",  # Dark blue - Cool, deep
    "#F77F00",  # Orange - Bright, energetic
    "#06A77D",  # Teal/green - Fresh, distinct
    "#9B59B6",  # Purple - Rich, unique
    "#D4A373",  # Tan/brown - Earthy, warm
    "#2E86AB",  # Medium blue - Professional
    "#D62828",  # Crimson - Bold, striking
    "#588157",  # Olive green - Natural
    "#BC4749"   # Burgundy - Deep, sophisticated
  )

  # Reverse if requested
  if (reverse) {
    colors <- rev(colors)
  }

  # If n is specified, adjust the color vector
  if (!is.null(n)) {
    phr_assert(is.numeric(n) && n > 0,
                 phr_txt("n must be a positive integer"),
                 origin = origin)

    if (n == 1) {
      # Return first color for single group
      colors <- colors[1]
    } else if (n <= length(colors)) {
      # Return first n colors
      colors <- colors[1:n]
    } else {
      # Use colorRampPalette to interpolate for more colors
      colors <- grDevices::colorRampPalette(colors)(n)
    }
  }

  return(colors)
}


#' IYCF Area Graph Color Palette
#'
#' Provides a color palette specifically designed for IYCF (Infant and Young Child Feeding) area graphs.
#' This palette uses a gradient from light gray through yellows and oranges to dark brown, representing
#' the progression from unknown/not breastfed states to exclusive breastfeeding.
#'
#' The palette is optimized for visualizing the following IYCF feeding categories in order from
#' lightest to darkest:
#' - Unknown
#' - Not Breastfed
#' - Breastfed & Solid or Semi-Solid Foods
#' - Breastfed & Animal Milk or Formula
#' - Breastfed & Non-Milk Liquids
#' - Breastfed & Plain Water
#' - Exclusive Breastfed
#'
#' @param reverse Logical indicating whether to reverse the color order. Default is FALSE.
#'   When FALSE, returns colors from lightest (Unknown) to darkest (Exclusive Breastfed).
#'   When TRUE, returns colors from darkest to lightest.
#'
#' @return A named character vector of 7 hex color codes for the IYCF feeding categories.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get IYCF area graph colors (light to dark)
#'   iycf_area_colors()
#'
#'   # Get reversed order (dark to light)
#'   iycf_area_colors(reverse = TRUE)
#' }
iycf_area_colors <- function(reverse = FALSE) {
  # Define IYCF area graph palette
  # Colors progress from light gray (Unknown) through yellow/orange gradient to dark brown (Exclusive Breastfed)
  colors <- c(
    "Unknown" = "#CCCCCC",                                    # Light gray
    "Not Breastfed" = "#FFF7BC",                             # Very light yellow
    "Breastfed & Solid or Semi-Solid Foods" = "#FEE391",     # Light yellow
    "Breastfed & Animal Milk or Formula" = "#FEC44F",        # Medium yellow
    "Breastfed & Non-Milk Liquids" = "#FE9929",              # Orange-yellow
    "Breastfed & Plain Water" = "#EC7014",                   # Orange
    "Exclusive Breastfed" = "#8C2D04"                        # Dark brown (darkest)
  )

  # Reverse if requested
  if (reverse) {
    colors <- rev(colors)
  }

  return(colors)
}


#' IPC Color Palette
#'
#' Provides the standard IPC (Integrated Food Security Phase Classification) color palette
#' representing the 5 food security phases ordered from worst (Phase 5 / Famine) to best
#' (Phase 1 / Minimal). This ordering ensures the first color maps to the worst outcome,
#' consistent with factor variables ordered worst-to-best.
#'
#' @param n Number of colors to return. If NULL, returns all 5 phase colors.
#'   If n is less than or equal to 5, returns the first n phase colors (worst n phases).
#'   If n is greater than 5, interpolates to generate exactly n colors.
#' @param reverse Logical indicating whether to reverse the color order. Default is FALSE.
#'   When FALSE, returns colors from Phase 5 (Famine/worst) to Phase 1 (Minimal/best).
#'
#' @return A character vector of hex color codes.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get all IPC phase colors (P5 to P1, worst to best)
#'   ipc_colors()
#'
#'   # Get colors for worst 4 phases (P5 to P2)
#'   ipc_colors(n = 4)
#'
#'   # Get colors in reverse order (P1 to P5, best to worst)
#'   ipc_colors(reverse = TRUE)
#' }
ipc_colors <- function(n = NULL, reverse = FALSE) {
  origin <- "ipc_colors"

  # Define IPC phase colors ordered worst to best (P5 Famine → P1 Minimal)
  colors <- c(
    "#8C0000",  # P5 Famine       RGB(140, 0, 0)
    "#FF0000",  # P4 Emergency    RGB(255, 0, 0)
    "#FF9900",  # P3 Crisis       RGB(255, 153, 0)
    "#FFEE00",  # P2 Stressed     RGB(255, 238, 0)
    "#A1FE8D"   # P1 Minimal      RGB(161, 254, 141)
  )

  # Reverse if requested
  if (reverse) {
    colors <- rev(colors)
  }

  # If n is specified, adjust the color vector
  if (!is.null(n)) {
    phr_assert(is.numeric(n) && n > 0,
                 phr_txt("n must be a positive integer"),
                 origin = origin)

    if (n <= length(colors)) {
      colors <- colors[1:n]
    } else {
      colors <- grDevices::colorRampPalette(colors)(n)
    }
  }

  return(colors)
}


#' Water Theme Color Palette
#'
#' Provides a color palette based on the Water theme color (#28A1D2), using tints
#' blended with white at 80\%, 55\%, and 30\%. Can be scaled to any number of colors
#' by interpolating the full gradient from the primary color to the lightest tint.
#'
#' @param n Number of colors to return. If NULL, returns the 3 standard tints (80\%, 55\%, 30\%).
#'   If n is less than or equal to 3, returns the first n standard tints.
#'   If n is greater than 3, interpolates across the full gradient to return exactly n colors.
#' @param reverse Logical indicating whether to reverse the color order. Default is FALSE.
#'   When FALSE, returns colors from darkest (80\%) to lightest (30\%).
#'
#' @return A named character vector of hex color codes (unnamed when interpolated).
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get the 3 standard Water tints
#'   water_colors()
#'
#'   # Get 6 interpolated colors across the Water gradient
#'   water_colors(n = 6)
#'
#'   # Get colors in reverse order (lightest to darkest)
#'   water_colors(reverse = TRUE)
#' }
water_colors <- function(n = NULL, reverse = FALSE) {
  origin <- "water_colors"

  # Standard tints: Water primary (#28A1D2, RGB 40/161/210) blended with white
  colors <- c(
    "#53B4DB",  # RGB(83, 180, 219)  — 80% primary + 20% white
    "#89CBE6",  # RGB(137, 203, 230) — 55% primary + 45% white
    "#BEE3F2"   # RGB(190, 227, 242) — 30% primary + 70% white
  )

  # Reverse if requested
  if (reverse) {
    colors <- rev(colors)
  }

  # If n is specified, adjust the color vector
  if (!is.null(n)) {
    phr_assert(is.numeric(n) && n > 0,
                 phr_txt("n must be a positive integer"),
                 origin = origin)

    if (n <= length(colors)) {
      colors <- colors[1:n]
    } else {
      # Include primary color for a richer gradient range
      gradient <- c("#28A1D2", "#53B4DB", "#89CBE6", "#BEE3F2")
      if (reverse) gradient <- rev(gradient)
      colors <- grDevices::colorRampPalette(gradient)(n)
    }
  }

  return(colors)
}


#' Sanitation Theme Color Palette
#'
#' Provides a color palette based on the Sanitation theme color (#532F87), using tints
#' blended with white at 80\%, 55\%, and 30\%. Can be scaled to any number of colors
#' by interpolating the full gradient from the primary color to the lightest tint.
#'
#' @param n Number of colors to return. If NULL, returns the 3 standard tints (80\%, 55\%, 30\%).
#'   If n is less than or equal to 3, returns the first n standard tints.
#'   If n is greater than 3, interpolates across the full gradient to return exactly n colors.
#' @param reverse Logical indicating whether to reverse the color order. Default is FALSE.
#'   When FALSE, returns colors from darkest (80\%) to lightest (30\%).
#'
#' @return A named character vector of hex color codes (unnamed when interpolated).
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get the 3 standard Sanitation tints
#'   sanitation_colors()
#'
#'   # Get 6 interpolated colors across the Sanitation gradient
#'   sanitation_colors(n = 6)
#'
#'   # Get colors in reverse order (lightest to darkest)
#'   sanitation_colors(reverse = TRUE)
#' }
sanitation_colors <- function(n = NULL, reverse = FALSE) {
  origin <- "sanitation_colors"

  # Standard tints: Sanitation primary (#532F87, RGB 83/47/135) blended with white
  colors <- c(
    "#75599F",  # RGB(117, 89, 159) — 80% primary + 20% white
    "#A08DBD",  # RGB(160, 141, 189) — 55% primary + 45% white
    "#CBC1DB"   # RGB(203, 193, 219) — 30% primary + 70% white
  )

  # Reverse if requested
  if (reverse) {
    colors <- rev(colors)
  }

  # If n is specified, adjust the color vector
  if (!is.null(n)) {
    phr_assert(is.numeric(n) && n > 0,
                 phr_txt("n must be a positive integer"),
                 origin = origin)

    if (n <= length(colors)) {
      colors <- colors[1:n]
    } else {
      # Include primary color for a richer gradient range
      gradient <- c("#532F87", "#75599F", "#A08DBD", "#CBC1DB")
      if (reverse) gradient <- rev(gradient)
      colors <- grDevices::colorRampPalette(gradient)(n)
    }
  }

  return(colors)
}


#' Hygiene Theme Color Palette
#'
#' Provides a color palette based on the Hygiene theme color (#008D48), using tints
#' blended with white at 80\%, 55\%, and 30\%. Can be scaled to any number of colors
#' by interpolating the full gradient from the primary color to the lightest tint.
#'
#' @param n Number of colors to return. If NULL, returns the 3 standard tints (80\%, 55\%, 30\%).
#'   If n is less than or equal to 3, returns the first n standard tints.
#'   If n is greater than 3, interpolates across the full gradient to return exactly n colors.
#' @param reverse Logical indicating whether to reverse the color order. Default is FALSE.
#'   When FALSE, returns colors from darkest (80\%) to lightest (30\%).
#'
#' @return A named character vector of hex color codes (unnamed when interpolated).
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get the 3 standard Hygiene tints
#'   hygiene_colors()
#'
#'   # Get 6 interpolated colors across the Hygiene gradient
#'   hygiene_colors(n = 6)
#'
#'   # Get colors in reverse order (lightest to darkest)
#'   hygiene_colors(reverse = TRUE)
#' }
hygiene_colors <- function(n = NULL, reverse = FALSE) {
  origin <- "hygiene_colors"

  # Standard tints: Hygiene primary (#008D48, RGB 0/141/72) blended with white
  colors <- c(
    "#33A46D",  # RGB(51, 164, 109)  — 80% primary + 20% white
    "#73C09A",  # RGB(115, 192, 154) — 55% primary + 45% white
    "#B2DDC8"   # RGB(178, 221, 200) — 30% primary + 70% white
  )

  # Reverse if requested
  if (reverse) {
    colors <- rev(colors)
  }

  # If n is specified, adjust the color vector
  if (!is.null(n)) {
    phr_assert(is.numeric(n) && n > 0,
                 phr_txt("n must be a positive integer"),
                 origin = origin)

    if (n <= length(colors)) {
      colors <- colors[1:n]
    } else {
      # Include primary color for a richer gradient range
      gradient <- c("#008D48", "#33A46D", "#73C09A", "#B2DDC8")
      if (reverse) gradient <- rev(gradient)
      colors <- grDevices::colorRampPalette(gradient)(n)
    }
  }

  return(colors)
}


#' Get Color Palette for Plotting
#'
#' A convenience function that returns an appropriate color palette based on the specified type.
#' Supports REACH brand colors, traffic light colors, sex comparison colors, group comparison colors,
#' IYCF area graph colors, IPC phase colors, WASH theme colors (water, sanitation, hygiene),
#' and ggplot2 default colors.
#'
#' @param type Character string specifying the palette type.
#'   Options: "reach1", "reach2", "reach3", "reach4", "traffic_light", "sex", "group", "iycf_area",
#'   "ipc", "water", "sanitation", "hygiene", "default".
#'   Default is "reach1".
#' @param n Number of colors to return. If NULL, returns all available colors for the palette.
#'   Note: For "sex" palette, n is ignored as it always returns 2 colors.
#'   Note: For "iycf_area" palette, n is ignored as it always returns 7 named colors.
#' @param reverse Logical indicating whether to reverse the color order. Default is FALSE.
#'
#' @return A character vector of hex color codes.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get REACH ramp 1 colors
#'   get_color_palette("reach1", n = 5)
#'
#'   # Get traffic light colors
#'   get_color_palette("traffic_light", n = 3)
#'
#'   # Get sex comparison colors
#'   get_color_palette("sex")
#'
#'   # Get group comparison colors
#'   get_color_palette("group", n = 8)
#'
#'   # Get IYCF area graph colors
#'   get_color_palette("iycf_area")
#'
#'   # Get IPC phase colors
#'   get_color_palette("ipc")
#'
#'   # Get Water theme colors (6 interpolated shades)
#'   get_color_palette("water", n = 6)
#'
#'   # Get Sanitation theme colors
#'   get_color_palette("sanitation")
#'
#'   # Get Hygiene theme colors
#'   get_color_palette("hygiene")
#'
#'   # Get default ggplot2 colors
#'   get_color_palette("default", n = 6)
#' }
get_color_palette <- function(type = "reach1", n = NULL, reverse = FALSE) {
  origin <- "get_color_palette"

  # Validate type
  valid_types <- c("reach1", "reach2", "reach3", "reach4", "traffic_light", "sex", "group", "iycf_area", "ipc", "water", "sanitation", "hygiene", "default")
  phr_assert(type %in% valid_types,
               phr_txt("type must be one of: {paste(valid_types, collapse = ', ')}"),
               origin = origin)

  # Return appropriate palette
  if (type == "traffic_light") {
    return(traffic_light_colors(n = n, reverse = reverse))
  } else if (type == "sex") {
    return(sex_colors(reverse = reverse))
  } else if (type == "group") {
    return(group_colors(n = n, reverse = reverse))
  } else if (type == "iycf_area") {
    return(iycf_area_colors(reverse = reverse))
  } else if (type == "ipc") {
    return(ipc_colors(n = n, reverse = reverse))
  } else if (type == "water") {
    return(water_colors(n = n, reverse = reverse))
  } else if (type == "sanitation") {
    return(sanitation_colors(n = n, reverse = reverse))
  } else if (type == "hygiene") {
    return(hygiene_colors(n = n, reverse = reverse))
  } else if (grepl("^reach[1-4]$", type)) {
    ramp_num <- as.integer(substr(type, 6, 6))
    return(reach_colors(ramp = ramp_num, n = n, reverse = reverse))
  } else if (type == "default") {
    # Return ggplot2 default color palette
    if (is.null(n)) n <- 6
    colors <- scales::hue_pal()(n)
    if (reverse) colors <- rev(colors)
    return(colors)
  }
}
