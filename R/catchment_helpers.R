# ---------------------------------------------------------------------------
# Shared helpers: GP practice catchment heat maps (registered patients by area)
# Author: Lorcan Carnegie
# Date created: 2026-10-19
# R version: R 2026.06.0
# ---------------------------------------------------------------------------

catchment_practice_table <- function(d, catchment) {
  g <- d[!is.na(d$oseast1m) & !is.na(d$osnrth1m) &
           d$AreaCode %in% catchment$lsoa$PracticeCode, c("AreaCode", "AreaName")]
  g <- g[!duplicated(g$AreaCode), ]
  names(g) <- c("code", "name")
  pcn <- if (!is.null(catchment$pcn)) catchment$pcn$PCN[match(g$code, catchment$pcn$PracticeCode)] else NA
  g$pcn <- ifelse(is.na(pcn), "Not in a PCN", pcn)
  g$list_size <- catchment$lsoa$ListSize[match(g$code, catchment$lsoa$PracticeCode)]
  g[order(g$pcn, g$name), ]
}

catchment_practice_choices <- function(d, catchment, codes = NULL) {
  if (is.null(catchment)) return(NULL)
  g <- catchment_practice_table(d, catchment)
  if (!is.null(codes)) g <- g[g$code %in% codes, ]
  flat <- stats::setNames(g$code, g$name)
  if (is.null(catchment$pcn)) return(flat)
  split(flat, g$pcn)
}

catchment_first_choice <- function(choices) {
  unlist(choices, use.names = FALSE)[1]
}

catchment_pcn_colour <- function(pcn, catchment) {
  palette <- c("#B26B00", "#2A7F8E", "#C0392B", "#2E7D32")
  levels <- sort(unique(catchment$pcn$PCN[!grepl("unallocated", catchment$pcn$PCN, ignore.case = TRUE)]))
  cols <- stats::setNames(rep_len(palette, length(levels)), levels)
  out <- unname(cols[pcn])
  out[is.na(out)] <- "#555555"
  out
}

catchment_caption <- function(d, catchment, code) {
  g <- catchment_practice_table(d, catchment)
  r <- g[g$code == code, ]
  if (nrow(r) == 0) return(NULL)
  col <- catchment_pcn_colour(r$pcn, catchment)
  tags$p(
    style = "font-size:0.9rem; margin:0.25rem 0 0;",
    tags$strong(r$name), tags$br(),
    tags$span(style = paste0("color:", col, "; font-weight:600;"),
              r$pcn, tags$br(),
              format(r$list_size, big.mark = ","), " registered patients"),
    tags$br(),
    tags$span(
      style = "color:#888; font-size:0.8rem;",
      "Source: NHS England, Patients Registered at a GP Practice (July 2026)."
    )
  )
}

catchment_map_controls <- function(ns, choices, selected_view = "points", selected_practice = NULL) {
  tags$div(
    style = "display:flex; gap:1rem; flex-wrap:wrap; align-items:flex-end; margin-bottom:0.25rem;",
    radioButtons(
      ns("map_view"), NULL, inline = TRUE, selected = selected_view,
      choices = c("Practice locations" = "points",
                  "Catchment by LSOA" = "lsoa",
                  "Catchment by ward" = "ward")
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] != 'points'", ns("map_view")),
      tags$div(
        style = "display:flex; gap:1rem; flex-wrap:wrap; align-items:flex-end;",
        selectInput(ns("catchment_practice"), NULL, choices = choices,
                    selected = selected_practice, width = "280px"),
        radioButtons(ns("catchment_measure"), NULL, inline = TRUE, selected = "pct",
                     choices = c("% of practice's patients" = "pct",
                                 "Number of patients" = "n"))
      )
    )
  )
}

catchment_polygons <- function(catchment, view, practice_code, shp_lsoa, shp_ward) {
  if (is.null(catchment) || is.null(practice_code) || !nzchar(practice_code)) return(NULL)
  tbl <- if (view == "lsoa") catchment$lsoa else catchment$ward
  shp <- if (view == "lsoa") shp_lsoa else shp_ward
  if (is.null(tbl) || is.null(shp)) return(NULL)
  tbl <- tbl[tbl$PracticeCode == practice_code, ]
  shp$Pct <- tbl$Pct[match(shp$AreaCode, tbl$AreaCode)]
  shp$Patients <- tbl$Patients[match(shp$AreaCode, tbl$AreaCode)]
  shp$Pct[is.na(shp$Pct)] <- 0
  shp$Patients[is.na(shp$Patients)] <- 0
  sf::st_transform(shp, 4326)
}

add_catchment_layer <- function(map, shp, measure = "pct") {
  as_count <- identical(measure, "n")
  shp$Value <- if (as_count) shp$Patients else shp$Pct
  top <- max(shp$Value, na.rm = TRUE)
  if (!is.finite(top) || top <= 0) top <- 1
  pal <- leaflet::colorNumeric(c("#F3EEF5", "#6F2277"), domain = c(0, top))
  shp$Label <- paste0(shp$AreaName, ": ", format(shp$Patients, big.mark = ","), " patients (",
                      round(shp$Pct * 100, 1), "% of the practice's registered patients)")
  map |>
    leaflet::addPolygons(
      data = shp,
      fillColor = ~pal(Value), fillOpacity = 0.85,
      color = "#999999", weight = 0.6,
      label = ~Label,
      highlightOptions = leaflet::highlightOptions(weight = 2, color = "#333333", bringToFront = TRUE)
    ) |>
    leaflet::addLegend(
      pal = pal, values = c(0, top), position = "bottomright",
      title = if (as_count) "Registered<br>patients" else "% of practice's<br>registered patients",
      labFormat = if (as_count) {
        leaflet::labelFormat(big.mark = ",", digits = 0)
      } else {
        leaflet::labelFormat(transform = function(x) x * 100, suffix = "%", digits = 0)
      }
    )
}

add_practice_marker <- function(map, pts_wgs, label) {
  map |>
    leaflet::addCircleMarkers(
      data = pts_wgs, fillColor = "white", color = "black",
      weight = 2, fillOpacity = 1, radius = 7, label = label
    )
}
