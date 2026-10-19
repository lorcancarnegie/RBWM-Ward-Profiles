# ---------------------------------------------------------------------------
# Shiny module: grouped GP-practice-level indicator card with a condition
# dropdown (e.g. Disease prevalence - CHD, Cancer, Asthma)
# Author: Lorcan Carnegie
# Date created: 2026-10-19
# R version: R 2026.06.0
# ---------------------------------------------------------------------------

mod_condition_group_ui <- function(id, display_name, condition_choices) {
  ns <- NS(id)

  bslib::card(
    full_screen = TRUE,
    bslib::card_header(display_name),
    tags$div(
      style = "display:flex; gap:1rem; flex-wrap:wrap; margin-bottom:0.5rem;",
      selectizeInput(ns("ward"), "Ward", choices = NULL, multiple = TRUE, width = "320px",
                     options = list(placeholder = "All wards (select to compare)",
                                    plugins = list("remove_button"))),
      selectInput(ns("condition"), "Condition", choices = condition_choices, width = "300px")
    ),
    uiOutput(ns("no_practice_note")),
    bslib::layout_columns(
      col_widths = c(6, 6),
      tags$div(
        plotly::plotlyOutput(ns("chart"), height = "300px"),
        uiOutput(ns("chart_legend"))
      ),
      tags$div(
        uiOutput(ns("map_controls")),
        leaflet::leafletOutput(ns("map"), height = "300px"),
        uiOutput(ns("catchment_caption")),
        uiOutput(ns("map_legend"))
      )
    ),
    uiOutput(ns("info_details"))
  )
}

mod_condition_group_server <- function(id, full_dataset, condition_sources,
                                        ward_names, shp_ward = NULL,
                                        catchment = NULL, shp_lsoa = NULL) {
  moduleServer(id, function(input, output, session) {

    normalize_ward_name <- function(x) {
      trimws(gsub("\\s+", " ", gsub("&", "and", x, fixed = TRUE)))
    }

    updateSelectizeInput(session, "ward",
                          choices = sort(ward_names),
                          selected = character(0),
                          server = TRUE)

    gp_data <- reactive({
      req(full_dataset(), input$condition)
      d <- full_dataset()
      d <- d[d$ShortIndicatorName == input$condition, ]
      d <- d[!is.na(d$oseast1m) & !is.na(d$osnrth1m), ]
      if ("Colour" %in% names(d)) d$Colour <- as.character(d$Colour)
      req(nrow(d) > 0)
      d <- d[order(d$Timeperiod, decreasing = TRUE), ]
      d <- d[!duplicated(d$AreaCode), ]
      if (all(c("Compared", "Colour") %in% names(d))) {
        d$Colour <- override_not_compared_colour(d$Compared, d$Colour)
      }
      d
    })

    gp_with_ward <- reactive({
      t0 <- Sys.time()
      d <- gp_data()
      req(nrow(d) > 0, shp_ward)
      pts <- sf::st_as_sf(d, coords = c("oseast1m", "osnrth1m"), crs = 27700,
                           remove = FALSE)
      joined <- sf::st_join(pts, shp_ward[, "AreaName"], join = sf::st_within,
                             suffix = c("", "_ward"))
      joined$WardName <- normalize_ward_name(joined$AreaName_ward)
      message("[", id, "] st_join took ", round(as.numeric(Sys.time() - t0, units = "secs"), 3), "s for ", nrow(d), " rows")
      joined
    })

    filtered_data <- reactive({
      d <- gp_with_ward()
      req(nrow(d) > 0)
      selected_wards <- input$ward
      if (!is.null(selected_wards) && length(selected_wards) > 0) {
        sel_norm <- normalize_ward_name(selected_wards)
        d <- d[!is.na(d$WardName) & d$WardName %in% sel_norm, ]
      }
      d
    })

    output$no_practice_note <- renderUI({
      selected_wards <- input$ward
      if (is.null(selected_wards) || length(selected_wards) == 0) return(NULL)
      d <- sf::st_drop_geometry(gp_with_ward())
      sel_norm <- normalize_ward_name(selected_wards)
      missing_wards <- selected_wards[!sel_norm %in% unique(d$WardName)]
      if (length(missing_wards) == 0) return(NULL)
      tags$p(
        paste0("No GP practice is located in ", paste(missing_wards, collapse = ", "),
               ", so there is no practice-level data to show for ",
               if (length(missing_wards) > 1) "these wards" else "this ward",
               ". Residents may be registered at practices in neighbouring wards; try selecting those wards as well, or clear the ward selection to see all practices."),
        style = "color:#a94442; background:rgba(169,68,66,0.08); border-radius:6px; padding:0.5rem 0.75rem; font-size:0.9rem; margin-top:0.5rem;"
      )
    })

    map_view <- reactive({
      v <- input$map_view
      if (is.null(v)) "points" else v
    })

    output$catchment_caption <- renderUI({
      if (map_view() == "points") return(NULL)
      req(input$catchment_practice, !is.null(catchment))
      catchment_caption(full_dataset(), catchment, input$catchment_practice)
    })

    observe({
      req(!is.null(catchment), input$map_view)
      codes <- unique(sf::st_drop_geometry(filtered_data())$AreaCode)
      ch <- catchment_practice_choices(full_dataset(), catchment, codes)
      valid <- unlist(ch, use.names = FALSE)
      cur <- isolate(input$catchment_practice)
      sel <- if (!is.null(cur) && cur %in% valid) cur else catchment_first_choice(ch)
      updateSelectInput(session, "catchment_practice",
                        choices = if (length(valid) > 0) ch else character(0),
                        selected = sel)
    })

    output$map_controls <- renderUI({
      req(!is.null(catchment))
      ch <- catchment_practice_choices(full_dataset(), catchment)
      req(length(ch) > 0)
      catchment_map_controls(session$ns, ch, selected_practice = catchment_first_choice(ch))
    })

    output$info_details <- renderUI({
      req(input$condition)
      src <- condition_sources[[input$condition]]
      indicator_info_details(
        value_type = "Proportion (%)",
        benchmarking_method = "Wilson Score method (95%)",
        source = if (!is.null(src)) src else NA,
        original_geography = "GP"
      )
    })

    output$chart_legend <- renderUI({
      d <- sf::st_drop_geometry(filtered_data())
      req(nrow(d) > 0)
      if ("Compared" %in% names(d)) {
        build_indicator_legend(d$Compared, d$Colour)
      }
    })

    output$chart <- plotly::renderPlotly({
      d <- sf::st_drop_geometry(filtered_data())
      req(nrow(d) > 0)

      d <- d[order(d$Value, decreasing = TRUE, na.last = TRUE), ]
      d$area_f <- factor(d$AreaName, levels = d$AreaName)
      d$is_suppressed <- is.na(d$Value)
      d$tooltip_text <- ifelse(d$is_suppressed,
                               paste0(d$AreaName, ": data suppressed (small numbers)"),
                               paste0(d$AreaName, ": ", round(d$Value, 1)))
      if ("Compared" %in% names(d)) {
        d$shape_code <- shape_compared_code(d$Compared)
      }
      d$bar_colour <- ifelse(!is.na(d$Colour), d$Colour, "#4472C4")
      d$plot_value <- ifelse(d$is_suppressed, 0, d$Value)

      p <- ggplot2::ggplot(d, ggplot2::aes(x = area_f, y = plot_value, text = tooltip_text)) +
        ggplot2::geom_col(fill = ifelse(d$is_suppressed, "grey85", d$bar_colour)) +
        ggplot2::labs(x = NULL, y = "%") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          axis.title.y = ggplot2::element_text(size = 8, colour = "grey40"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          legend.position = "none"
        )

      if (all(c("LowerCI95Limit", "UpperCI95Limit") %in% names(d)) &&
          any(!is.na(d$LowerCI95Limit))) {
        p <- p + ggplot2::geom_errorbar(
          ggplot2::aes(ymin = LowerCI95Limit, ymax = UpperCI95Limit),
          width = 0.2, na.rm = TRUE
        )
      }

      if (any(d$is_suppressed)) {
        p <- p + ggplot2::geom_text(
          data = d[d$is_suppressed, ],
          ggplot2::aes(x = area_f, y = 0, label = "suppressed"),
          angle = 90, hjust = -0.1, size = 2.8, colour = "grey50"
        )
      }

      if ("shape_code" %in% names(d)) {
        max_val <- suppressWarnings(max(d$Value, na.rm = TRUE))
        if (is.finite(max_val)) {
          p <- p + ggplot2::geom_point(
            data = d[!d$is_suppressed, ],
            ggplot2::aes(y = -max_val * 0.05, shape = shape_code,
                         colour = bar_colour, fill = bar_colour),
            size = 1.6, na.rm = TRUE
          ) + ggplot2::scale_shape_identity() +
            ggplot2::scale_colour_identity() +
            ggplot2::scale_fill_identity() +
            ggplot2::expand_limits(y = -max_val * 0.08)
        }
      }

      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(autosize = TRUE,
                        xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE)) |>
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    })

    output$map_legend <- renderUI({
      if (map_view() != "points") return(NULL)
      d <- filtered_data()
      req(nrow(d) > 0)
      d_flat <- sf::st_drop_geometry(d)
      if ("Compared" %in% names(d_flat)) {
        build_indicator_legend(d_flat$Compared, d_flat$Colour)
      }
    })

    output$map <- leaflet::renderLeaflet({
      view <- map_view()
      d <- filtered_data()

      ward_lines <- function(m, colour, base_weight) {
        if (is.null(shp_ward)) return(m)
        wards_wgs <- sf::st_transform(shp_ward, 4326)
        border_weight <- rep(base_weight, nrow(wards_wgs))
        selected_wards <- input$ward
        if (!is.null(selected_wards) && length(selected_wards) > 0) {
          border_weight[normalize_ward_name(wards_wgs$AreaName) %in%
                          normalize_ward_name(selected_wards)] <- 3
        }
        m |> leaflet::addPolygons(
          data = wards_wgs, fill = FALSE, color = colour,
          weight = border_weight
        )
      }

      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas)

      cat_shp <- NULL
      if (view != "points") {
        cat_shp <- catchment_polygons(catchment, view, input$catchment_practice, shp_lsoa, shp_ward)
      }

      d_wgs <- NULL
      if (!is.null(cat_shp)) {
        pts <- gp_with_ward()
        pts <- pts[pts$AreaCode == input$catchment_practice, ]
        m <- m |> add_catchment_layer(cat_shp, if (is.null(input$catchment_measure)) "pct" else input$catchment_measure) |> ward_lines("#555555", 1) |> add_rbwm_outline(shp_ward)
        if (nrow(pts) > 0) m <- m |> add_practice_marker(sf::st_transform(pts, 4326), pts$AreaName)
      } else {
        req(nrow(d) > 0)
        d_wgs <- sf::st_transform(d, 4326)
        d_wgs$Colour <- as.character(d_wgs$Colour)
        d_wgs$is_suppressed <- is.na(d_wgs$Value)
        fill_colours <- ifelse(d_wgs$is_suppressed, "grey70",
                               ifelse(!is.na(d_wgs$Colour), d_wgs$Colour, "#4472C4"))
        map_labels <- ifelse(d_wgs$is_suppressed,
                             paste0(d_wgs$AreaName, ": data suppressed (small numbers)"),
                             paste0(d_wgs$AreaName, ": ", round(d_wgs$Value, 1)))
        m <- m |> ward_lines("#999999", 1) |>
          leaflet::addCircleMarkers(
            data = d_wgs,
            fillColor = fill_colours,
            color = "#333333", weight = 1, fillOpacity = 0.9, radius = 8,
            label = map_labels
          ) |>
          add_rbwm_outline(shp_ward)
      }

      selected_wards <- input$ward
      selected_bbox <- NULL
      if (!is.null(selected_wards) && length(selected_wards) > 0 && !is.null(shp_ward)) {
        ward_shape <- shp_ward[normalize_ward_name(shp_ward$AreaName) %in%
                                  normalize_ward_name(selected_wards), ]
        if (nrow(ward_shape) > 0) selected_bbox <- sf::st_bbox(sf::st_transform(ward_shape, 4326))
      }
      bbox <- rbwm_default_bbox(shp_ward, selected_bbox)
      if (is.null(bbox)) bbox <- sf::st_bbox(if (!is.null(d_wgs)) d_wgs else cat_shp)
      m |> leaflet::fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
                               lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]) |>
        leaflet::setMaxBounds(lng1 = bbox[["xmin"]] - 0.15, lat1 = bbox[["ymin"]] - 0.15,
                              lng2 = bbox[["xmax"]] + 0.15, lat2 = bbox[["ymax"]] + 0.15)
    })
  })
}
