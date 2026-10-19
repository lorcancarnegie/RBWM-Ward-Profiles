# ---------------------------------------------------------------------------
# Shiny modules: IMD / deprivation (ward-scoped chart+map) and a separate
# LSOA ranking card (table+map of top/bottom N LSOAs across all of RBWM)
# Author: Lorcan Carnegie
# Date created: 2026-10-19
# R version: R 2026.06.0
# ---------------------------------------------------------------------------

mod_lsoa_indicator_ui <- function(id, display_name) {
  ns <- NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(display_name),
    layout_columns(
      col_widths = c(6, 6),
      selectizeInput(ns("ward"), "Ward", choices = NULL, multiple = TRUE,
                     options = list(placeholder = "All wards (select to compare)",
                                    plugins = list("remove_button"))),
      selectInput(ns("domain"), "Deprivation domain", choices = NULL)
    ),
    tags$p(
      "Each ward is scored on a scale from 1 (most deprived nationally) to 10 (least deprived nationally) for the selected domain below, based on its position relative to every other small area in England. A lower score means the ward falls among the more deprived areas nationally for that domain; a higher score means it falls among the less deprived.",
      style = "font-size:0.9rem; margin-top:0.5rem; margin-bottom:0.5rem;"
    ),
    tags$p(
      "IMD = Index of Multiple Deprivation. IDACI = Income Deprivation Affecting Children Index. IDAOPI = Income Deprivation Affecting Older People Index.",
      style = "color:#888; font-size:0.8rem; margin-top:0.5rem; margin-bottom:0.25rem;"
    ),
    tags$p(
      "Source: Ministry of Housing, Communities and Local Government. English Indices of Deprivation 2025. Office for National Statistics licensed under the Open Government Licence v.3.0. Contains OS data \u00A9 Crown copyright and database right.",
      style = "color:#888; font-size:0.8rem; margin-top:0; margin-bottom:0.75rem;"
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      plotly::plotlyOutput(ns("chart"), height = "300px"),
      leaflet::leafletOutput(ns("map"), height = "300px")
    )
  )
}

mod_lsoa_indicator_server <- function(id, full_dataset, short_name,
                                       ward_names, shp_ward = NULL, shp_lsoa = NULL) {
  moduleServer(id, function(input, output, session) {

    normalize_ward_name <- function(x) {
      trimws(gsub("\\s+", " ", gsub("&", "and", x, fixed = TRUE)))
    }

    updateSelectizeInput(session, "ward",
                          choices = sort(ward_names),
                          selected = character(0),
                          server = TRUE)

    raw_data <- reactive({
      req(full_dataset())
      d <- full_dataset()
      d[d$ShortIndicatorName == short_name, ]
    })

    observe({
      d <- raw_data()
      req(nrow(d) > 0)
      domains <- sort(unique(d$GroupingVariable))
      req(length(domains) > 0)
      overall <- domains[grepl("overall", domains, ignore.case = TRUE)]
      ordered_domains <- c(overall, domains[!domains %in% overall])
      default <- if (length(overall) > 0) overall[1] else domains[1]
      updateSelectInput(session, "domain", choices = ordered_domains, selected = default)
    })

    lsoa_with_ward <- reactive({
      req(shp_lsoa, shp_ward)
      centroids <- sf::st_centroid(shp_lsoa)
      joined <- sf::st_join(centroids, shp_ward[, "AreaName"],
                             join = sf::st_within, suffix = c("", "_ward"))
      sf::st_drop_geometry(joined)[, c("AreaCode", "AreaName_ward")]
    })

    ind_data <- reactive({
      req(input$domain)
      d <- raw_data()
      d <- d[d$GroupingVariable == input$domain, ]
      d$Decile <- suppressWarnings(as.numeric(gsub("\\D", "", d$Group)))
      d
    })

    ward_lsoa_data <- reactive({
      d <- ind_data()
      lw <- lsoa_with_ward()
      d <- merge(d, lw, by = "AreaCode", all.x = FALSE)
      selected_wards <- input$ward
      if (!is.null(selected_wards) && length(selected_wards) > 0) {
        sel_norm <- normalize_ward_name(selected_wards)
        d <- d[!is.na(d$AreaName_ward) &
                 normalize_ward_name(d$AreaName_ward) %in% sel_norm, ]
      }
      d
    })

    decile_pal <- leaflet::colorNumeric(palette = "RdYlGn", domain = 1:10)

    output$chart <- plotly::renderPlotly({
      d <- ward_lsoa_data()
      req(nrow(d) > 0)

      d <- d[order(d$Decile), ]
      d$area_f <- factor(d$AreaCode, levels = d$AreaCode)
      d$tooltip_text <- paste0(d$AreaCode, ": decile ", d$Decile, " (1 = most deprived)")
      showing_all <- nrow(d) > 20

      p <- ggplot2::ggplot(d, ggplot2::aes(x = area_f, y = Decile, text = tooltip_text)) +
        ggplot2::geom_col(fill = decile_pal(d$Decile)) +
        ggplot2::labs(x = NULL, y = "Decile (1 = most deprived)") +
        ggplot2::scale_y_continuous(limits = c(0, 10), breaks = 1:10) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(legend.position = "none")

      p <- p + if (showing_all) {
        ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
      } else {
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      }

      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(autosize = TRUE,
                        xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE)) |>
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    })

    output$map <- leaflet::renderLeaflet({
      d <- ward_lsoa_data()
      req(nrow(d) > 0, shp_lsoa)

      merged <- merge(shp_lsoa, d[, c("AreaCode", "Decile")], by = "AreaCode", all.x = FALSE)
      req(nrow(merged) > 0)
      merged <- sf::st_transform(merged, 4326)
      bbox <- sf::st_bbox(merged)

      leaflet::leaflet(merged) |>
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) |>
        leaflet::addPolygons(
          fillColor = ~decile_pal(Decile),
          fillOpacity = 0.8,
          color = "#333333",
          weight = 1,
          label = ~paste0(AreaName, ": decile ", Decile),
          highlightOptions = leaflet::highlightOptions(weight = 3, bringToFront = TRUE)
        ) |>
        add_rbwm_outline(shp_ward) |>
        leaflet::addLegend(
          pal = decile_pal, values = 1:10, title = input$domain,
          opacity = 0.8
        ) |>
        leaflet::fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
                            lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
    })
  })
}

mod_lsoa_ranking_ui <- function(id, display_name = "Deprivation: LSOA ranking") {
  ns <- NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(display_name),
    selectInput(ns("domain"), "Deprivation domain", choices = NULL, width = "300px"),
    tags$p(
      "Ranks all RBWM LSOAs by the deprivation domain selected above.",
      style = "color:#888; font-size:0.8rem; margin-top:0.5rem; margin-bottom:0.5rem;"
    ),
    layout_columns(
      col_widths = c(6, 6),
      numericInput(ns("top_n"), "Number of LSOAs to show:", value = 10, min = 1, max = 89),
      radioButtons(ns("rank_direction"), NULL,
                  choices = c("Most deprived" = "asc", "Least deprived" = "desc"),
                  inline = TRUE)
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      DT::dataTableOutput(ns("rank_table")),
      leaflet::leafletOutput(ns("rank_map"), height = "500px")
    )
  )
}

mod_lsoa_ranking_server <- function(id, full_dataset, short_name,
                                    shp_ward = NULL, shp_lsoa = NULL) {
  moduleServer(id, function(input, output, session) {

    raw_data <- reactive({
      req(full_dataset())
      d <- full_dataset()
      d[d$ShortIndicatorName == short_name, ]
    })

    observe({
      d <- raw_data()
      req(nrow(d) > 0)
      domains <- sort(unique(d$GroupingVariable))
      req(length(domains) > 0)
      overall <- domains[grepl("overall", domains, ignore.case = TRUE)]
      ordered_domains <- c(overall, domains[!domains %in% overall])
      default <- if (length(overall) > 0) overall[1] else domains[1]
      updateSelectInput(session, "domain", choices = ordered_domains, selected = default)
    })

    lsoa_with_ward <- reactive({
      req(shp_lsoa, shp_ward)
      centroids <- sf::st_centroid(shp_lsoa)
      joined <- sf::st_join(centroids, shp_ward[, "AreaName"],
                             join = sf::st_within, suffix = c("", "_ward"))
      sf::st_drop_geometry(joined)[, c("AreaCode", "AreaName_ward")]
    })

    ind_data <- reactive({
      req(input$domain)
      d <- raw_data()
      d <- d[d$GroupingVariable == input$domain, ]
      d$Decile <- suppressWarnings(as.numeric(gsub("\\D", "", d$Group)))
      d
    })

    decile_pal <- leaflet::colorNumeric(palette = "RdYlGn", domain = 1:10)

    ranked_data <- reactive({
      req(input$top_n, input$rank_direction)
      d <- ind_data()
      lw <- lsoa_with_ward()
      d <- merge(d, lw, by = "AreaCode", all.x = TRUE)
      d <- d[!is.na(d$Decile), ]
      req(nrow(d) > 0)
      d <- d[order(d$Decile, decreasing = (input$rank_direction == "desc")), ]
      n <- min(input$top_n, nrow(d))
      head(d, n)
    })

    output$rank_table <- DT::renderDataTable({
      d <- ranked_data()
      req(nrow(d) > 0)
      tbl <- d[, c("AreaCode", "AreaName_ward", "Decile")]
      colnames(tbl) <- c("LSOA code", "Ward", "Decile")
      DT::datatable(
        tbl, rownames = FALSE, class = "compact stripe hover",
        options = list(pageLength = 20, dom = "t"),
        selection = "none"
      )
    })

    output$rank_map <- leaflet::renderLeaflet({
      d <- ranked_data()
      req(nrow(d) > 0, shp_lsoa)

      merged <- merge(shp_lsoa, d[, c("AreaCode", "Decile")], by = "AreaCode", all.x = FALSE)
      req(nrow(merged) > 0)
      merged <- sf::st_transform(merged, 4326)
      bbox <- sf::st_bbox(merged)

      leaflet::leaflet(merged) |>
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) |>
        leaflet::addPolygons(
          fillColor = ~decile_pal(Decile),
          fillOpacity = 0.8,
          color = "#6F2277",
          weight = 2,
          label = ~paste0(AreaName, ": decile ", Decile),
          highlightOptions = leaflet::highlightOptions(weight = 4, bringToFront = TRUE)
        ) |>
        add_rbwm_outline(shp_ward) |>
        leaflet::addLegend(
          pal = decile_pal, values = 1:10, title = input$domain,
          opacity = 0.8
        ) |>
        leaflet::fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
                            lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
    })
  })
}
