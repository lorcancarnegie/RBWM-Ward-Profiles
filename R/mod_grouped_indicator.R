# ---------------------------------------------------------------------------
# Shiny module: composition breakdown card (Ethnicity, Main language spoken)
# Author: Lorcan Carnegie
# Date created: 2026-10-19
# R version: R 2026.06.0
# ---------------------------------------------------------------------------

mod_grouped_indicator_ui <- function(id, display_name) {
  ns <- NS(id)
  ward_selector <- selectizeInput(ns("ward"), label = NULL, choices = NULL,
                                   multiple = TRUE, width = "320px",
                                   options = list(placeholder = "All wards (select to compare)",
                                                  plugins = list("remove_button")))
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(display_name),
    ward_selector,
    plotly::plotlyOutput(ns("chart"), height = "420px"),
    uiOutput(ns("no_ward_note")),
    tags$p(
      "Source: Nomis. 2021 Census.",
      style = "color:#888; font-size:0.8rem; margin-top:0.5rem; margin-bottom:0;"
    )
  )
}

mod_grouped_indicator_server <- function(id, full_dataset, short_name,
                                          ward_names, rbwm_code) {
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
      if ("Group" %in% names(d)) d$Group <- as.character(d$Group)
      d
    })

    labelled_data <- reactive({
      d <- ind_data()
      d$AreaLabel <- d$AreaName
      d$AreaLabel[d$AreaCode == rbwm_code] <- "RBWM"
      d
    })

    plot_data <- reactive({
      d <- labelled_data()
      req(nrow(d) > 0)
      is_rbwm <- d$AreaCode == rbwm_code

      selected_wards <- input$ward
      if (is.null(selected_wards) || length(selected_wards) == 0) {
        d <- d[is_rbwm, ]
      } else {
        sel_norm <- normalize_ward_name(selected_wards)
        is_ward <- d$AreaType == "Ward" &
          normalize_ward_name(d$AreaName) %in% sel_norm
        d <- d[is_rbwm | is_ward, ]
      }
      d <- d[order(d$Timeperiod, decreasing = TRUE), ]
      d <- d[!duplicated(d[, c("AreaLabel", "Group")]), ]
      d
    })

    output$no_ward_note <- renderUI({
      selected_wards <- input$ward
      if (is.null(selected_wards) || length(selected_wards) == 0) return(NULL)
      d <- plot_data()
      ward_present <- unique(d$AreaLabel[d$AreaCode != rbwm_code])
      missing_wards <- selected_wards[!normalize_ward_name(selected_wards) %in% normalize_ward_name(ward_present)]
      if (length(missing_wards) > 0) {
        tags$p(
          paste0("Ward-level data not available for ", paste(missing_wards, collapse = ", "), "."),
          style = "color:#888; font-style:italic; font-size:0.8rem; margin-top:0.5rem;"
        )
      }
    })

    output$chart <- plotly::renderPlotly({
      d <- plot_data()
      req(nrow(d) > 0)

      ward_area <- sort(unique(d$AreaLabel[d$AreaCode != rbwm_code]))
      area_levels <- c(ward_area, "RBWM")
      d$AreaLabel <- factor(d$AreaLabel, levels = area_levels[area_levels %in% d$AreaLabel])

      d$tooltip_text <- paste0(d$AreaLabel, " - ", d$Group, ": ", round(d$Value, 1), "%")

      d$Group <- factor(d$Group, levels = rev(sort(unique(d$Group))))

      ward_palette <- c("#6F2277", "#AA9767", "#4472C4", "#70AD47", "#ED7D31", "#255E91", "#C00000")
      area_colours <- c("RBWM" = "grey")
      if (length(ward_area) > 0) {
        area_colours[ward_area] <- rep(ward_palette, length.out = length(ward_area))
      }

      p <- ggplot2::ggplot(d, ggplot2::aes(x = Group, y = Value, fill = AreaLabel, text = tooltip_text)) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::scale_fill_manual(values = area_colours) +
        ggplot2::labs(x = NULL, y = "Proportion (%)", fill = NULL) +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal(base_size = 11)

      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(
          legend = list(orientation = "h", y = 1.12, x = 0.5, xanchor = "center", yanchor = "bottom"),
          margin = list(t = 50), autosize = TRUE,
          xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE)
        ) |>
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    })
  })
}
