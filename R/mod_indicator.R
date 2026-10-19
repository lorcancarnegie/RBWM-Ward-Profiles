# ---------------------------------------------------------------------------
# Shiny module: single indicator card (ward toggle + chart + map)
# Author: Lorcan Carnegie
# Date created: 2026-10-19
# R version: R 2026.06.0
# ---------------------------------------------------------------------------

mod_indicator_ui <- function(id, short_name, display_name = short_name,
                              has_map = FALSE, has_trend = FALSE, available_at_ward = TRUE,
                              value_type = NA, benchmarking_method = NA, source = NA,
                              original_geography = NA, known_caveats = NA,
                              additional_notes = NA, data_years = NA) {
  ns <- NS(id)

  if (!available_at_ward) {
    return(bslib::card(
      bslib::card_header(display_name),
      tags$p("Placeholder", style = "color:#888; font-style:italic; margin: 2rem 0; text-align:center;")
    ))
  }

  ward_selector <- selectizeInput(ns("ward"), label = NULL, choices = NULL,
                                   multiple = TRUE, width = "320px",
                                   options = list(placeholder = "All wards (select to compare)",
                                                  plugins = list("remove_button")))
  breakdown_selector <- uiOutput(ns("breakdown_ui"))
  info_details <- indicator_info_details(value_type, benchmarking_method, source,
                                          original_geography, known_caveats, additional_notes,
                                          data_years)

  chart_only_body <- tagList(
    plotly::plotlyOutput(ns("chart"), height = "300px"),
    uiOutput(ns("chart_legend"))
  )

  tabs <- list(bslib::nav_panel("Chart",
    plotly::plotlyOutput(ns("chart"), height = "300px"),
    uiOutput(ns("chart_legend"))
  ))
  if (has_map) {
    tabs <- c(tabs, list(bslib::nav_panel("Map",
      leaflet::leafletOutput(ns("map"), height = "300px"),
      uiOutput(ns("map_legend"))
    )))
  }
  if (has_trend) {
    tabs <- c(tabs, list(bslib::nav_panel("Trend",
      plotly::plotlyOutput(ns("trend"), height = "300px"),
      tags$p("RBWM (purple, solid), England (grey, dotted), selected ward(s) (dashed), over time.",
             style = "color:#888; font-size:0.8rem; margin-top:0.3rem;")
    )))
  }
  body <- if (has_map || has_trend) do.call(bslib::navset_tab, tabs) else chart_only_body

  bslib::card(
    full_screen = TRUE,
    bslib::card_header(display_name),
    ward_selector,
    breakdown_selector,
    body,
    info_details
  )
}

mod_indicator_server <- function(id, full_dataset, short_name,
                                  ward_names, rbwm_code, se_code, eng_code,
                                  map_geography = "None", has_trend = FALSE,
                                  shp_ward = NULL, shp_lsoa = NULL,
                                  available_at_ward = TRUE, value_type = NA) {
  moduleServer(id, function(input, output, session) {

    if (!available_at_ward) return(invisible(NULL))

    normalize_ward_name <- function(x) {
      trimws(gsub("\\s+", " ", gsub("&", "and", x, fixed = TRUE)))
    }
    ward_names_norm <- normalize_ward_name(ward_names)

    updateSelectizeInput(session, "ward",
                          choices = sort(ward_names),
                          selected = character(0),
                          server = TRUE)

    ind_data <- reactive({
      req(full_dataset())
      d <- full_dataset()
      d <- d[d$ShortIndicatorName == short_name, ]
      if ("Colour" %in% names(d)) d$Colour <- as.character(d$Colour)
      if ("Colour" %in% names(d) && "Compared" %in% names(d)) {
        d$Colour <- override_not_compared_colour(d$Compared, d$Colour)
      }
      if ("Group" %in% names(d)) d$Group <- as.character(d$Group)
      if (nrow(d) > 0 && all(is.na(d$Value))) {
        d$Value <- d$Count
      }
      d
    })

    is_grouped <- reactive({
      d <- ind_data()
      g <- unique(d$Group[!is.na(d$Group) & d$Group != ""])
      length(g) > 1
    })

    breakdown_type <- reactive({
      d <- ind_data()
      req(nrow(d) > 0)
      sex_levels <- unique(d$Sex[!is.na(d$Sex) & d$Sex != ""])
      age_levels <- unique(d$Age[!is.na(d$Age) & d$Age != ""])
      if (length(sex_levels) > 1) {
        default <- if ("persons" %in% sex_levels) "persons" else sort(sex_levels)[1]
        return(list(field = "Sex", levels = sort(sex_levels), default = default))
      }
      if (length(age_levels) > 1) {
        default <- if ("All ages" %in% age_levels) "All ages" else sort(age_levels)[1]
        return(list(field = "Age", levels = sort(age_levels), default = default))
      }
      NULL
    })

    output$breakdown_ui <- renderUI({
      bd <- breakdown_type()
      if (is.null(bd)) return(NULL)
      selectInput(session$ns("breakdown"), paste0(bd$field, ":"),
                  choices = bd$levels, selected = bd$default, width = "200px")
    })

    plot_data <- reactive({
      d <- ind_data()
      req(nrow(d) > 0)

      bd <- breakdown_type()
      if (!is.null(bd) && !is.null(input$breakdown)) {
        d <- d[d[[bd$field]] == input$breakdown, ]
      }

      keep_codes <- unique(c(rbwm_code, se_code, eng_code))
      is_ref <- d$AreaCode %in% keep_codes
      is_ward <- d$AreaType == "Ward" & normalize_ward_name(d$AreaName) %in% ward_names_norm

      selected_wards <- input$ward
      if (is.null(selected_wards) || length(selected_wards) == 0) {
        d <- d[is_ref | is_ward, ]
      } else {
        sel_norm <- normalize_ward_name(selected_wards)
        d <- d[is_ref | (is_ward & normalize_ward_name(d$AreaName) %in% sel_norm), ]
      }

      d <- d[order(d$Timeperiod, decreasing = TRUE), ]
      if (is_grouped()) {
        d <- d[!duplicated(d[, c("AreaName", "Group")]), ]
      } else {
        d <- d[!duplicated(d[, "AreaName"]), ]
      }
      d
    })

    output$chart_legend <- renderUI({
      d <- plot_data()
      req(nrow(d) > 0)
      if ("Compared" %in% names(d)) {
        build_indicator_legend(d$Compared, d$Colour)
      }
    })

    output$chart <- plotly::renderPlotly({
      d <- plot_data()
      req(nrow(d) > 0)

      is_ref <- d$AreaCode %in% unique(c(rbwm_code, se_code, eng_code))
      if (is_grouped()) {
        ward_levels <- sort(unique(d$AreaName[!is_ref]))
      } else {
        ward_levels <- d$AreaName[!is_ref][order(d$Value[!is_ref], decreasing = TRUE)]
      }
      d$area_f <- factor(
        d$AreaName,
        levels = c(sort(unique(d$AreaName[is_ref])), ward_levels)
      )

      if (is_grouped()) {
        d$tooltip_text <- paste0(d$AreaName, " - ", d$Group, ": ", round(d$Value, 1))
        p <- ggplot2::ggplot(d, ggplot2::aes(x = area_f, y = Value, fill = Group,
                                              text = tooltip_text)) +
          ggplot2::geom_col(position = "dodge")
      } else {
        d$tooltip_text <- paste0(d$AreaName, ": ", round(d$Value, 1))
        if ("Compared" %in% names(d)) {
          d$shape_code <- shape_compared_code(d$Compared)
        }

        fill_vals <- if (!is.null(d$Colour) && any(!is.na(d$Colour))) d$Colour else "#4472C4"
        d$bar_colour <- if (length(fill_vals) == nrow(d)) fill_vals else rep("#4472C4", nrow(d))

        p <- ggplot2::ggplot(d, ggplot2::aes(x = area_f, y = Value, text = tooltip_text)) +
          ggplot2::geom_col(fill = d$bar_colour)

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
      }

      p <- p +
        ggplot2::labs(x = NULL, y = if (is_grouped()) NULL else short_axis_label(value_type)) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          axis.title.y = ggplot2::element_text(size = 8, colour = "grey40"),
          legend.position = if (is_grouped()) "bottom" else "none"
        )

      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(legend = list(orientation = "h", y = -0.3), autosize = TRUE,
                        xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE)) |>
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    })

    if (has_trend) {
      output$trend <- plotly::renderPlotly({
        d <- ind_data()
        req(nrow(d) > 0)

        is_rbwm <- d$AreaCode == rbwm_code
        is_eng <- d$AreaCode == eng_code
        selected_wards <- input$ward
        has_ward <- !is.null(selected_wards) && length(selected_wards) > 0
        is_ward <- if (has_ward) {
          d$AreaType == "Ward" & normalize_ward_name(d$AreaName) %in% normalize_ward_name(selected_wards)
        } else {
          rep(FALSE, nrow(d))
        }
        td <- d[is_rbwm | is_eng | is_ward, ]
        req(nrow(td) > 0)

        td$AreaLabel <- ifelse(td$AreaCode == rbwm_code, "RBWM",
                         ifelse(td$AreaCode == eng_code, "England", td$AreaName))
        td <- td[order(td$Timeperiod), ]
        td$Timeperiod_f <- factor(td$Timeperiod, levels = sort(unique(td$Timeperiod)))
        td$tooltip_text <- paste0(td$AreaLabel, " - ", td$Timeperiod, ": ", round(td$Value, 1))

        ward_palette <- c("#AA9767", "#4472C4", "#70AD47", "#ED7D31", "#255E91", "#C00000")
        area_colours <- c("RBWM" = "#6F2277", "England" = "#999999")
        area_linetypes <- c("RBWM" = "solid", "England" = "dotted")
        ward_labels_present <- sort(unique(td$AreaLabel[!td$AreaLabel %in% c("RBWM", "England")]))
        if (length(ward_labels_present) > 0) {
          ward_cols <- rep(ward_palette, length.out = length(ward_labels_present))
          area_colours[ward_labels_present] <- ward_cols
          area_linetypes[ward_labels_present] <- "dashed"
        }

        p <- ggplot2::ggplot(td, ggplot2::aes(x = Timeperiod_f, y = Value, colour = AreaLabel,
                                               linetype = AreaLabel, group = AreaLabel, text = tooltip_text)) +
          ggplot2::geom_line(linewidth = 1) +
          ggplot2::geom_point(size = 1.8) +
          ggplot2::scale_colour_manual(values = area_colours) +
          ggplot2::scale_linetype_manual(values = area_linetypes) +
          ggplot2::labs(x = NULL, y = short_axis_label(value_type), colour = NULL, linetype = NULL) +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            axis.title.y = ggplot2::element_text(size = 8, colour = "grey40")
          )

        plotly::ggplotly(p, tooltip = "text") |>
          plotly::layout(showlegend = FALSE, autosize = TRUE,
                          xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE)) |>
          plotly::config(displayModeBar = FALSE, responsive = TRUE)
      })
    }

    if (map_geography != "None") {

      map_shape <- switch(map_geography, "Ward" = shp_ward, "LSOA" = shp_lsoa)

      map_data <- reactive({
        req(map_shape)
        d <- ind_data()
        d <- d[d$AreaType == map_geography, ]
        req(nrow(d) > 0)

        if (is_grouped()) {
          overall <- d$Group[grepl("overall", d$Group, ignore.case = TRUE)]
          if (length(overall) > 0) d <- d[d$Group == overall[1], ]
        }
        d <- d[order(d$Timeperiod, decreasing = TRUE), ]
        d <- d[!duplicated(d$AreaCode), ]

        merge(map_shape, d[, c("AreaCode", "Value", "Colour", "Compared")],
              by = "AreaCode", all.x = FALSE)
      })

      output$map_legend <- renderUI({
        d <- map_data()
        req(nrow(d) > 0)
        if ("Compared" %in% names(d)) {
          build_indicator_legend(d$Compared, d$Colour)
        }
      })

      output$map <- leaflet::renderLeaflet({
        d <- map_data()
        req(nrow(d) > 0)

        d <- sf::st_transform(d, 4326)

        d$Colour <- as.character(d$Colour)
        fill_colours <- ifelse(!is.na(d$Colour), d$Colour, "#4472C4")

        selected_wards <- input$ward
        border_weight <- rep(1, nrow(d))
        fill_opacity <- rep(0.75, nrow(d))
        ward_outline <- NULL

        bbox <- sf::st_bbox(d)
        if (!is.null(selected_wards) && length(selected_wards) > 0 && !is.null(shp_ward)) {
          sel_norm <- normalize_ward_name(selected_wards)
          ward_shape <- shp_ward[normalize_ward_name(shp_ward$AreaName) %in% sel_norm, ]
          if (nrow(ward_shape) > 0) {
            ward_outline <- sf::st_transform(ward_shape, 4326)
            bbox <- sf::st_bbox(ward_outline)

            if (map_geography == "Ward") {
              is_selected <- normalize_ward_name(d$AreaName) %in% sel_norm
            } else {
              centroids <- sf::st_centroid(d)
              is_selected <- lengths(sf::st_within(centroids, ward_outline)) > 0
            }

            border_weight[is_selected] <- 4
            fill_opacity[!is_selected] <- 0.15
          }
        }

        m <- leaflet::leaflet(d) |>
          leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) |>
          leaflet::addPolygons(
            fillColor = fill_colours,
            fillOpacity = fill_opacity,
            color = "#333333",
            weight = border_weight,
            label = ~paste0(AreaName, ": ", round(Value, 1)),
            highlightOptions = leaflet::highlightOptions(weight = 4, bringToFront = TRUE)
          )

        if (!is.null(ward_outline)) {
          m <- m |> leaflet::addPolygons(
            data = ward_outline, fill = FALSE,
            color = "#000000", weight = 3
          )
        }

        m <- m |> add_rbwm_outline(shp_ward)

        m |>
          leaflet::fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
                              lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
      })
    }
  })
}
