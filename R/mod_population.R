# ---------------------------------------------------------------------------
# Shiny module: Population pyramid + age profile
# Author: Lorcan Carnegie
# Date created: 2026-10-19
# R version: R 2026.06.0
# ---------------------------------------------------------------------------

mod_population_ui <- function(id) {
  ns <- NS(id)
  ward_selector <- selectInput(ns("ward"), label = NULL, choices = NULL,
                                width = "260px")
  bslib::card(
    full_screen = TRUE,
    bslib::card_header("Population"),
    ward_selector,
    bslib::navset_tab(
      bslib::nav_panel("Population pyramid",
        uiOutput(ns("pyramid_totals")),
        uiOutput(ns("pyramid_note")),
        tags$div(
          style = "max-width:420px; margin:0 auto;",
          plotly::plotlyOutput(ns("pyramid"), height = "300px")
        )
      ),
      bslib::nav_panel("Age profile",
        plotly::plotlyOutput(ns("age_profile"), height = "310px")
      ),
      bslib::nav_panel("Map",
        leaflet::leafletOutput(ns("pop_map"), height = "310px")
      )
    ),
    tags$p(
      "Source: ONS Population estimates mid-2024. Office for National Statistics licensed under the Open Government Licence v.3.0.",
      style = "color:#888; font-size:0.8rem; margin-top:0.5rem; margin-bottom:0;"
    )
  )
}

mod_population_server <- function(id, full_dataset, ward_names, rbwm_code, eng_code, shp_ward = NULL) {
  moduleServer(id, function(input, output, session) {

    normalize_ward_name <- function(x) {
      trimws(gsub("\\s+", " ", gsub("&", "and", x, fixed = TRUE)))
    }

    updateSelectInput(session, "ward",
                       choices = c("All wards" = "__all__", sort(ward_names)),
                       selected = "__all__")

    ind_data <- reactive({
      req(full_dataset())
      d <- full_dataset()
      d <- d[d$ShortIndicatorName == "LA population", ]
      req(nrow(d) > 0)
      d$SexClean <- tolower(trimws(as.character(d$Sex)))
      d$AgeNum <- suppressWarnings(as.numeric(gsub("[^0-9]", "", as.character(d$Age))))
      d
    })

    has_ward <- reactive({
      !is.null(input$ward) && input$ward != "__all__"
    })

    all_areas_data <- reactive({
      d <- ind_data()
      is_rbwm <- d$AreaCode == rbwm_code
      is_eng  <- d$AreaCode == eng_code

      if (!has_ward()) {
        d <- d[is_rbwm | is_eng, ]
        d$AreaLabel <- ifelse(d$AreaCode == rbwm_code, "RBWM", "England")
      } else {
        is_ward <- !(d$AreaCode %in% c(rbwm_code, eng_code)) &
          normalize_ward_name(d$AreaName) == normalize_ward_name(input$ward)
        d <- d[is_ward | is_rbwm | is_eng, ]
        d$AreaLabel <- ifelse(d$AreaCode == rbwm_code, "RBWM",
                        ifelse(d$AreaCode == eng_code, "England", input$ward))
      }
      req(nrow(d) > 0)
      d <- d[order(d$Timeperiod, decreasing = TRUE), ]
      d <- d[!duplicated(d[, c("AreaLabel", "SexClean", "AgeNum")]), ]
      d
    })

    age_breaks <- seq(0, 90, 5)
    age_labels <- c(paste0(head(age_breaks, -1), "-", head(age_breaks, -1) + 4), "90+")

    banded_data <- reactive({
      d <- all_areas_data()
      d <- d[!is.na(d$AgeNum), ]
      req(nrow(d) > 0)
      d$AgeBand <- cut(d$AgeNum, breaks = c(age_breaks, Inf), labels = age_labels,
                        right = FALSE, include.lowest = TRUE)
      d
    })

    output$pyramid_totals <- renderUI({
      d <- all_areas_data()
      persons <- d[d$SexClean %in% c("persons", "person", "all persons", "total"), ]
      req(nrow(persons) > 0)
      totals <- stats::aggregate(Count ~ AreaLabel, data = persons, FUN = sum)
      txt <- paste(paste0(totals$AreaLabel, ": ", format(round(totals$Count), big.mark = ",")),
                   collapse = "   |   ")
      tags$p(paste("Total population -", txt), style = "font-weight:500; margin-bottom:0.25rem;")
    })

    output$pyramid_note <- renderUI({
      txt <- if (has_ward()) {
        "Bars: selected ward. Solid grey line: RBWM. Dashed line: England."
      } else {
        "Bars: RBWM. Dashed line: England. Select a ward to compare it against RBWM."
      }
      tags$p(txt, style = "color:#888; font-size:0.8rem; margin-bottom:0.5rem;")
    })

    output$pyramid <- plotly::renderPlotly({
      d <- banded_data()
      d <- d[d$SexClean %in% c("male", "males", "female", "females"), ]
      req(nrow(d) > 0)
      d$SexLabel <- ifelse(grepl("^m", d$SexClean), "Male", "Female")

      agg <- stats::aggregate(Count ~ AreaLabel + AgeBand + SexLabel, data = d, FUN = sum)
      totals <- stats::aggregate(Count ~ AreaLabel, data = agg, FUN = sum)
      agg <- merge(agg, totals, by = "AreaLabel", suffixes = c("", "Total"))
      agg$Pct <- agg$Count / agg$CountTotal * 100
      agg$PlotPct <- ifelse(agg$SexLabel == "Male", -agg$Pct, agg$Pct)
      agg$tooltip_text <- paste0(agg$AreaLabel, " - ", agg$AgeBand, " ", agg$SexLabel,
                                  ": ", round(agg$Pct, 1), "%")

      bars_label <- if (has_ward()) input$ward else "RBWM"
      bars_data <- agg[agg$AreaLabel == bars_label, ]
      req(nrow(bars_data) > 0)
      rbwm_data <- if (has_ward()) agg[agg$AreaLabel == "RBWM", ] else agg[0, ]
      eng_data  <- agg[agg$AreaLabel == "England", ]

      sex_colours <- c("Male" = "#1B5E20", "Female" = "#81C784")

      p <- ggplot2::ggplot() +
        ggplot2::geom_col(
          data = bars_data,
          ggplot2::aes(x = AgeBand, y = PlotPct, fill = SexLabel, text = tooltip_text)
        ) +
        ggplot2::scale_fill_manual(values = sex_colours) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(labels = function(x) paste0(abs(x), "%")) +
        ggplot2::labs(x = NULL,
                      y = paste0("Male (left) / Female (right) - % of ", bars_label, " population"),
                      fill = NULL) +
        ggplot2::theme_minimal(base_size = 11)

      if (nrow(rbwm_data) > 0) {
        p <- p + ggplot2::geom_path(
          data = rbwm_data[order(rbwm_data$SexLabel, rbwm_data$AgeBand), ],
          ggplot2::aes(x = AgeBand, y = PlotPct, group = SexLabel),
          colour = "#333333", linewidth = 0.8
        )
      }
      if (nrow(eng_data) > 0) {
        p <- p + ggplot2::geom_path(
          data = eng_data[order(eng_data$SexLabel, eng_data$AgeBand), ],
          ggplot2::aes(x = AgeBand, y = PlotPct, group = SexLabel),
          colour = "#333333", linewidth = 0.8, linetype = "dashed"
        )
      }

      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(
          legend = list(orientation = "h", y = 1.12, x = 0.5, xanchor = "center", yanchor = "bottom"),
          margin = list(t = 50), autosize = TRUE,
          xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE)
        ) |>
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    })

    output$age_profile <- plotly::renderPlotly({
      d <- banded_data()
      d <- d[d$SexClean %in% c("persons", "person", "all persons", "total"), ]
      req(nrow(d) > 0)

      agg <- stats::aggregate(Count ~ AreaLabel + AgeBand, data = d, FUN = sum)
      totals <- stats::aggregate(Count ~ AreaLabel, data = agg, FUN = sum)
      agg <- merge(agg, totals, by = "AreaLabel", suffixes = c("", "Total"))
      agg$Pct <- agg$Count / agg$CountTotal * 100
      agg$tooltip_text <- paste0(agg$AreaLabel, " - ", agg$AgeBand, ": ", round(agg$Pct, 1), "%")

      area_colours <- c("RBWM" = "#A6A6A6", "England" = "#D9D9D9")
      ward_lbl <- unique(agg$AreaLabel[!agg$AreaLabel %in% c("RBWM", "England")])
      if (length(ward_lbl) > 0) area_colours[ward_lbl] <- "#4B2E6F"

      p <- ggplot2::ggplot(agg, ggplot2::aes(x = AgeBand, y = Pct, fill = AreaLabel,
                                              text = tooltip_text)) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::scale_fill_manual(values = area_colours) +
        ggplot2::labs(x = NULL, y = "% of total population", fill = NULL) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(
          legend = list(orientation = "h", y = 1.12, x = 0.5, xanchor = "center", yanchor = "bottom"),
          margin = list(t = 50, b = 90), autosize = TRUE,
          xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE)
        ) |>
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    })
    output$pop_map <- leaflet::renderLeaflet({
      req(shp_ward)
      d <- ind_data()
      persons <- d[d$SexClean %in% c("persons", "person", "all persons", "total") &
                     !d$AreaCode %in% c(rbwm_code, eng_code), ]
      req(nrow(persons) > 0)

      totals <- stats::aggregate(Count ~ AreaCode, data = persons, FUN = sum)

      map_d <- merge(shp_ward, totals, by = "AreaCode", all.x = TRUE)
      map_d <- sf::st_transform(map_d, 4326)

      pal <- leaflet::colorNumeric(palette = "Purples", domain = map_d$Count)

      leaflet::leaflet(map_d) |>
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) |>
        leaflet::addPolygons(
          fillColor = ~pal(Count), fillOpacity = 0.75,
          color = "#666666", weight = 1,
          label = ~paste0(AreaName, ": ", format(round(Count), big.mark = ",")),
          highlightOptions = leaflet::highlightOptions(weight = 3, color = "#6F2277", bringToFront = TRUE)
        ) |>
        add_rbwm_outline(shp_ward) |>
        leaflet::addLegend(pal = pal, values = ~Count, title = "Population",
                          position = "bottomright",
                          labFormat = leaflet::labelFormat(big.mark = ","))
    })
  })
}
