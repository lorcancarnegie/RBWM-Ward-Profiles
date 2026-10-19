# ---------------------------------------------------------------------------
# Shiny module: MSOA-level indicator card (bar per MSOA + choropleth map)
# Author: Lorcan Carnegie
# Date created: 2026-10-19
# R version: R 2026.06.0
# ---------------------------------------------------------------------------

mod_msoa_indicator_ui <- function(id, display_name, value_type = NA, benchmarking_method = NA,
                                   source = NA, original_geography = NA, known_caveats = NA,
                                   additional_notes = NA, data_years = NA) {
  ns <- NS(id)
  ward_selector <- selectizeInput(ns("ward"), label = NULL, choices = NULL,
                                   multiple = TRUE, width = "320px",
                                   options = list(placeholder = "All wards (select to compare)",
                                                  plugins = list("remove_button")))

  bslib::card(
    full_screen = TRUE,
    bslib::card_header(display_name),
    ward_selector,
    bslib::navset_tab(
      bslib::nav_panel("Chart",
        plotly::plotlyOutput(ns("chart"), height = "300px"),
        uiOutput(ns("chart_legend"))
      ),
      bslib::nav_panel("Map",
        leaflet::leafletOutput(ns("map"), height = "300px"),
        uiOutput(ns("map_legend"))
      )
    ),
    indicator_info_details(value_type, benchmarking_method, source,
                            original_geography, known_caveats, additional_notes,
                            data_years)
  )
}

mod_msoa_indicator_server <- function(id, full_dataset, short_name, ward_names = NULL,
                                       rbwm_code, se_code, eng_code,
                                       shp_msoa = NULL, shp_ward = NULL, value_type = NA) {
  moduleServer(id, function(input, output, session) {

    normalize_ward_name <- function(x) {
      trimws(gsub("\\s+", " ", gsub("&", "and", x, fixed = TRUE)))
    }

    updateSelectizeInput(session, "ward",
                          choices = sort(ward_names),
                          selected = character(0),
                          server = TRUE)

    ind_data <- reactive({
      req(full_dataset())
      d <- full_dataset()
      d <- d[d$ShortIndicatorName == short_name, ]
      req(nrow(d) > 0)
      if ("Colour" %in% names(d)) d$Colour <- as.character(d$Colour)
      d <- d[order(d$Timeperiod, decreasing = TRUE), ]
      d <- d[!duplicated(d$AreaCode), ]
      if (all(c("Compared", "Colour") %in% names(d))) {
        d$Colour <- override_not_compared_colour(d$Compared, d$Colour)
      }
      d
    })

    msoa_data <- reactive({
      d <- ind_data()
      d[d$AreaType == "MSOA", ]
    })

    ref_data <- reactive({
      d <- ind_data()
      d <- d[d$AreaCode %in% unique(c(rbwm_code, se_code, eng_code)), ]
      d[!duplicated(d$AreaName), ]
    })

    msoa_with_ward <- reactive({
      req(shp_msoa, shp_ward)
      centroids <- sf::st_centroid(shp_msoa)
      joined <- sf::st_join(centroids, shp_ward[, "AreaName"],
                             join = sf::st_within, suffix = c("", "_ward"))
      sf::st_drop_geometry(joined)[, c("AreaCode", "AreaName_ward")]
    })

    filtered_data <- reactive({
      d <- msoa_data()
      mw <- msoa_with_ward()
      d <- merge(d, mw, by = "AreaCode", all.x = TRUE)
      selected_wards <- input$ward
      if (!is.null(selected_wards) && length(selected_wards) > 0) {
        sel_norm <- normalize_ward_name(selected_wards)
        d <- d[!is.na(d$AreaName_ward) &
                 normalize_ward_name(d$AreaName_ward) %in% sel_norm, ]
      }
      d
    })

    chart_data <- reactive({
      msoas <- filtered_data()
      refs <- ref_data()
      req(nrow(msoas) > 0 || nrow(refs) > 0)
      shared_cols <- intersect(names(msoas), names(refs))
      rbind(refs[, shared_cols, drop = FALSE], msoas[, shared_cols, drop = FALSE])
    })

    output$chart_legend <- renderUI({
      d <- chart_data()
      req(nrow(d) > 0)
      if ("Compared" %in% names(d)) {
        build_indicator_legend(d$Compared, d$Colour)
      }
    })

    output$chart <- plotly::renderPlotly({
      d <- chart_data()
      req(nrow(d) > 0)

      is_ref <- d$AreaCode %in% unique(c(rbwm_code, se_code, eng_code))
      ward_levels <- d$AreaName[!is_ref][order(d$Value[!is_ref], decreasing = TRUE, na.last = TRUE)]
      d$area_f <- factor(d$AreaName, levels = c(sort(unique(d$AreaName[is_ref])), ward_levels))
      d$tooltip_text <- paste0(d$AreaName, ": ", round(d$Value, 1))
      if ("Compared" %in% names(d)) {
        d$shape_code <- shape_compared_code(d$Compared)
      }
      d$bar_colour <- ifelse(!is.na(d$Colour), d$Colour, "#4472C4")

      p <- ggplot2::ggplot(d, ggplot2::aes(x = area_f, y = Value, text = tooltip_text)) +
        ggplot2::geom_col(fill = d$bar_colour, position = "identity") +
        ggplot2::labs(x = NULL, y = short_axis_label(value_type)) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          axis.title.y = ggplot2::element_text(size = 8, colour = "grey40"),
          legend.position = "none"
        )

      if (all(c("LowerCI95Limit", "UpperCI95Limit") %in% names(d)) &&
          any(!is.na(d$LowerCI95Limit))) {
        p <- p + ggplot2::geom_errorbar(
          ggplot2::aes(ymin = LowerCI95Limit, ymax = UpperCI95Limit),
          width = 0.2, na.rm = TRUE
        )
      }

      if ("shape_code" %in% names(d)) {
        max_val <- suppressWarnings(max(d$Value, na.rm = TRUE))
        if (is.finite(max_val)) {
          p <- p + ggplot2::geom_point(
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
      d <- filtered_data()
      req(nrow(d) > 0)
      if ("Compared" %in% names(d)) {
        build_indicator_legend(d$Compared, d$Colour)
      }
    })

    output$map <- leaflet::renderLeaflet({
      d <- filtered_data()
      req(nrow(d) > 0, shp_msoa)

      merged <- merge(shp_msoa, d[, c("AreaCode", "Value", "Colour", "Compared")],
                       by = "AreaCode", all.x = FALSE)
      req(nrow(merged) > 0)
      merged <- sf::st_transform(merged, 4326)
      merged$Colour <- as.character(merged$Colour)
      fill_colours <- ifelse(!is.na(merged$Colour), merged$Colour, "#4472C4")
      bbox <- sf::st_bbox(merged)

      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas)

      if (!is.null(shp_ward)) {
        wards_wgs <- sf::st_transform(shp_ward, 4326)
        border_weight <- rep(1, nrow(wards_wgs))
        selected_wards <- input$ward
        if (!is.null(selected_wards) && length(selected_wards) > 0) {
          border_weight[normalize_ward_name(wards_wgs$AreaName) %in%
                          normalize_ward_name(selected_wards)] <- 3
        }
        m <- m |> leaflet::addPolygons(
          data = wards_wgs, fill = FALSE, color = "#999999",
          weight = border_weight
        )
      }

      m <- m |> leaflet::addPolygons(
        data = merged,
        fillColor = fill_colours,
        fillOpacity = 0.75,
        color = "#333333",
        weight = 1,
        label = ~paste0(AreaName, ": ", round(Value, 1)),
        highlightOptions = leaflet::highlightOptions(weight = 3, bringToFront = TRUE)
      ) |>
        add_rbwm_outline(shp_ward)

      m |> leaflet::fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
                               lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
    })
  })
}
