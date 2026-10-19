# ---------------------------------------------------------------------------
# Shared chart/map helpers
# Author: Lorcan Carnegie
# Date created: 2026-10-19
# R version: R 2026.06.0
# ---------------------------------------------------------------------------

# Compared text -> shape code (24 better, 21 similar, 25 worse, 22 not compared/NA)
shape_compared_code <- function(x) {
  x <- tolower(trimws(as.character(x)))
  is_na_x <- is.na(x) | x == ""
  out <- rep(22L, length(x))
  out[grepl("better|higher", x)] <- 24L
  out[grepl("worse|lower", x)] <- 25L
  out[grepl("similar|no diff|not different", x)] <- 21L
  out[is_na_x] <- 22L
  out
}

NOT_COMPARED_COLOUR <- "#AA9767"

override_not_compared_colour <- function(compared_vec, colour_vec) {
  compared_vec <- tolower(trimws(as.character(compared_vec)))
  colour_vec <- as.character(colour_vec)
  colour_vec[grepl("not compared", compared_vec)] <- NOT_COMPARED_COLOUR
  colour_vec
}

indicator_info_details <- function(value_type, benchmarking_method, source,
                                    original_geography = NA, known_caveats = NA,
                                    additional_notes = NA, data_years = NA) {
  if (all(is.na(c(value_type, benchmarking_method, source)))) return(NULL)
  tags$details(
    style = "margin-top: 0.5rem; font-size:0.8rem; color: #555;",
    tags$summary(style = "cursor: pointer; color: #6F2277;", "About this indicator"),
    tags$div(
      style = "margin-top: 0.35rem;",
      tags$div("Office for National Statistics licensed under the Open Government Licence v.3.0. Contains OS data © Crown copyright and database right."),
      if (!is.na(value_type)) tags$div(tags$strong("Value type: "), value_type),
      if (!is.na(original_geography)) tags$div(tags$strong("Original geography: "), original_geography),
      if (!is.na(data_years)) tags$div(tags$strong("Data available: "), data_years, " (the chart shows the most recent period only)"),
      if (!is.na(benchmarking_method)) tags$div(tags$strong("Benchmarking method: "), benchmarking_method),
      if (!is.na(source)) tags$div(tags$strong("Source: "), source),
      if (!is.na(known_caveats)) tags$div(tags$strong("Known caveats: "), known_caveats),
      if (!is.na(additional_notes)) tags$div(tags$strong("Additional notes: "), additional_notes)
    )
  )
}

short_axis_label <- function(value_type) {
  if (is.null(value_type) || length(value_type) == 0 || is.na(value_type)) return(NULL)
  vt <- trimws(value_type)
  if (grepl("%", vt, fixed = TRUE)) return("%")
  if (grepl("^Crude rate per", vt)) return(sub("^Crude rate ", "", vt))
  if (grepl("^Mean average", vt)) return(sub("^Mean average ", "", vt))
  if (grepl("^Decile", vt)) return("Decile")
  if (nchar(vt) > 20) return(NULL)
  vt
}

build_indicator_legend <- function(compared_vec, colour_vec) {
  compared_vec <- as.character(compared_vec)
  colour_vec <- as.character(colour_vec)
  keep <- !is.na(compared_vec) & !duplicated(paste(compared_vec, colour_vec))
  compared_vec <- compared_vec[keep]
  colour_vec <- colour_vec[keep]
  if (length(compared_vec) == 0) return(NULL)

  swatch <- function(label, colour) {
    colour <- if (is.na(colour) || colour == "") "#CCCCCC" else colour
    tags$div(
      style = "display:inline-block; margin-right:1rem; margin-bottom:0.15rem; white-space:nowrap;",
      tags$span(style = paste0(
        "display:inline-block; width:10px; height:10px; background-color:",
        colour, "; border:1px solid #999; border-radius:2px; margin-right:5px; vertical-align:middle;"
      ), HTML("&nbsp;")),
      tags$span(label, style = "font-size:0.8rem; vertical-align:middle; color:#555;")
    )
  }

  tags$div(
    style = "margin-top:0.4rem;",
    tags$div("Compared to England", style = "font-weight:600; color:#6F2277; font-size:0.8rem; margin-bottom:0.2rem;"),
    lapply(seq_along(compared_vec), function(i) swatch(compared_vec[i], colour_vec[i]))
  )
}

add_rbwm_outline <- function(map, shp_ward) {
  if (is.null(shp_ward)) return(map)
  outline <- sf::st_union(sf::st_make_valid(sf::st_transform(shp_ward, 4326)))
  map |> leaflet::addPolygons(
    data = outline, fill = FALSE,
    color = "#1A1A1A", weight = 3, opacity = 1
  )
}

rbwm_default_bbox <- function(shp_ward, selected_bbox = NULL) {
  if (!is.null(selected_bbox)) return(selected_bbox)
  if (is.null(shp_ward)) return(NULL)
  sf::st_bbox(sf::st_transform(shp_ward, 4326))
}
