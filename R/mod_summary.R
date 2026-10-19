# ---------------------------------------------------------------------------
# Shiny module: Summary (RAG/BOB overview across all indicators, ward toggle)
# Author: Lorcan Carnegie
# Date created: 2026-10-19
# R version: R 2026.06.0
# ---------------------------------------------------------------------------

# The original PPTX has a "Summary" slide using phiTools's
# summary_table_list(), but only ever shows the first 3 of its tables per
# ward due to physical slide space (tables <- x[1:3]) - the rest has never
# actually been seen, and it never had an all-wards-at-once view at all
# since each deck is generated per ward. This builds both: a per-ward table
# and an "All wards" matrix (one row per indicator, one column per ward),
# plus a RAG/BOB key built dynamically from whatever Compared/Colour pairs
# actually exist in the data, rather than assuming fixed category names.
#
# Population, Ethnicity, Main language, IMD, and the two GP-practice-level
# indicators (QOF Obesity, % Caring responsibility) are excluded - none of
# them have a single RAG/BOB-compared ward value the way the others do.

mod_summary_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header("Summary"),
    tags$p("This summary covers ward-level indicators only. GP-practice-level and LSOA-level indicators (screening, QOF prevalence, Deprivation, and related measures) are not included here - see their individual theme tabs.",
           style = "color:#555; font-size:0.9rem; margin-top:0.5rem;"),
    tags$div(
      class = "summary-filters",
      tags$div(style = "flex:1 1 200px; min-width:0;",
               selectInput(ns("ward"), "Ward", choices = NULL, width = "100%")),
      tags$div(style = "flex:1 1 220px; min-width:0;",
               selectizeInput(ns("theme_filter"), "Theme", choices = NULL,
                              multiple = TRUE, width = "100%",
                              options = list(placeholder = "All themes"))),
      tags$div(style = "flex:2 1 300px; min-width:0;",
               selectizeInput(ns("indicator_filter"), "Indicators", choices = NULL,
                              multiple = TRUE, width = "100%",
                              options = list(placeholder = "All indicators"))),
      tags$div(style = "flex:0 0 auto; padding-bottom:1rem;",
               checkboxInput(ns("compact_view"), "Short ward names", value = FALSE, width = "auto")),
      tags$div(style = "flex:0 0 auto; padding-bottom:1rem;",
               actionButton(ns("clear_filters"), "Clear filters", class = "btn-outline-primary"))
    ),
    tags$p("Selecting one or more indicators above also shows a map for each below the table.",
          style = "color:#555; font-size:0.8rem; font-style:italic; margin:0.25rem 0 0.75rem;"),
    uiOutput(ns("rag_key")),
    tags$details(
      style = "margin-bottom:0.75rem; padding:0.25rem 0.25rem;",
      tags$summary(style = "cursor:pointer; font-weight:700; color:#6F2277;", "What do these comparisons mean?"),
      tags$div(
        style = "margin-top:0.5rem; font-size:0.9rem;",
        p("The profiles use several methods to compare indicators and highlight variation between areas:"),
        tags$ul(
          tags$li(strong("RAG comparison (Red, Amber, Green)"), " \u2013 Indicates how an area's value compares with a benchmark, usually England. Comparisons are based on statistical testing, typically using confidence intervals. Green indicates the area is performing significantly better than the benchmark, red indicates it is performing significantly worse, and amber indicates there is no statistically significant difference."),
          tags$li(strong("BOB comparison (Blue, Orange, Blue)"), " \u2013 Used where a higher or lower value should not be interpreted as inherently better or worse. Light blue indicates the area's value is higher than the benchmark, while dark blue indicates it is lower."),
          tags$li(strong("Quintiles"), " \u2013 Divide data into five equal groups to show the relative position of areas and highlight inequalities. Quintiles are used where indicators are not population-based (for example, household data or survey data where weighted estimates are unavailable), making statistical comparisons inappropriate."),
          tags$li(strong("Brown (Not compared)"), " \u2013 Indicates that a comparison cannot be made, typically because no data are available for the area or the underlying count is too small (usually fewer than 10) to produce a reliable estimate.")
        ),
        uiOutput(ns("benchmark_examples"))
      )
    ),
    tags$style(HTML("
      .summary-filters { display:flex; flex-wrap:wrap; gap:1rem; align-items:flex-end; padding:0.75rem 1rem 0; border:1px solid #E4DBE8; border-radius:8px; background:#fff; }
      .summary-filters label.control-label { font-weight:700; color:#6F2277; }
      .summary-filters .form-group, .summary-filters .shiny-input-container { margin-bottom:1rem; }
      .all-wards-table td, .all-wards-table th { white-space:nowrap; }
      .summary-dt table.dataTable thead th:nth-child(n+3) { text-align:center !important; }
    ")),
    tags$p(textOutput(ns("n_shown"), inline = TRUE), style = "color:#555; font-size:0.9rem; margin-bottom:0.25rem;"),
    tags$div(class = "summary-dt", DT::dataTableOutput(ns("summary_table"))),
    uiOutput(ns("selected_maps"))
  )
}

mod_summary_server <- function(id, full_dataset, indicator_lookup, ward_names, rbwm_code, shp_ward = NULL) {
  moduleServer(id, function(input, output, session) {

    normalize_ward_name <- function(x) {
      trimws(gsub("\\s+", " ", gsub("&", "and", x, fixed = TRUE)))
    }

    contrast_text_colour <- function(hex) {
      rgb <- grDevices::col2rgb(hex)
      luminance <- 0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]
      ifelse(luminance < 128, "#FFFFFF", "#000000")
    }

    updateSelectInput(session, "ward",
                       choices = c("All wards" = "__all__", sort(ward_names)),
                       selected = "__all__")

    excluded_names <- c("LA population", "ethnicity", "Main language", "IMD")
    all_summary_indicators <- indicator_lookup[
      !indicator_lookup$ShortIndicatorName %in% excluded_names &
        (is.na(indicator_lookup$OriginalGeography) | !indicator_lookup$OriginalGeography %in% c("GP", "MSOA")),
    ]

    updateSelectizeInput(session, "theme_filter",
                        choices = sort(unique(as.character(all_summary_indicators$Theme))),
                        server = TRUE)
    updateSelectizeInput(session, "indicator_filter",
                        choices = setNames(all_summary_indicators$ShortIndicatorName,
                                          all_summary_indicators$DisplayName),
                        server = TRUE)

    # narrows the indicator set by theme and/or explicitly selected
    # indicators - both filters apply together when both are set
    observeEvent(input$clear_filters, {
      updateSelectizeInput(session, "theme_filter", selected = character(0))
      updateSelectizeInput(session, "indicator_filter", selected = character(0))
    })

    output$n_shown <- renderText({
      paste0("Showing ", nrow(summary_indicators()), " ward-level indicators")
    })

    theme_cell <- function(x) {
      shown <- c(TRUE, x[-1] != x[-length(x)])
      ifelse(shown, sprintf('<strong style="color:#6F2277;">%s</strong>', x), "")
    }

    summary_indicators <- reactive({
      d <- all_summary_indicators
      if (!is.null(input$theme_filter) && length(input$theme_filter) > 0) {
        d <- d[as.character(d$Theme) %in% input$theme_filter, ]
      }
      if (!is.null(input$indicator_filter) && length(input$indicator_filter) > 0) {
        d <- d[d$ShortIndicatorName %in% input$indicator_filter, ]
      }
      d
    })

    # latest ward-level row per indicator per ward, shared by both the
    # per-ward table and the all-wards matrix
    ward_names_norm <- normalize_ward_name(ward_names)

    # generic initials-based abbreviation, safe regardless of the exact
    # ward list - words like "and"/"&" are dropped, and any resulting
    # collision (e.g. two wards starting with the same initials) is broken
    # by appending increasing letters of the full first word until unique
    ward_short_labels <- local({
      make_initials <- function(name) {
        words <- strsplit(name, "[\\s&,]+", perl = TRUE)[[1]]
        words <- words[!tolower(words) %in% c("and", "&", "")]
        paste(toupper(substring(words, 1, 1)), collapse = "")
      }
      initials <- vapply(ward_names, make_initials, character(1))
      names(initials) <- ward_names

      for (n in unique(initials)) {
        dup_names <- names(initials)[initials == n]
        if (length(dup_names) > 1) {
          for (extra_len in 2:10) {
            candidates <- vapply(dup_names, function(w) {
              first_word <- strsplit(w, "\\s+")[[1]][1]
              paste0(toupper(substring(first_word, 1, extra_len)))
            }, character(1))
            if (!anyDuplicated(candidates)) {
              initials[dup_names] <- candidates
              break
            }
          }
        }
      }
      initials
    })

    latest_ward_data <- reactive({
      d <- full_dataset()
      req(d)
      si <- summary_indicators()
      # AreaType == "Ward" alone isn't enough - several indicators (e.g.
      # the Journey Time Statistics access ones) carry unfiltered ward rows
      # for the whole of England/Wales, not just RBWM's 19 - same bug
      # already fixed in mod_indicator.R, just missed here when this
      # module was built
      d <- d[d$ShortIndicatorName %in% si$ShortIndicatorName &
               d$AreaType == "Ward" &
               normalize_ward_name(d$AreaName) %in% ward_names_norm, ]
      req(nrow(d) > 0)
      d <- d[order(d$Timeperiod, decreasing = TRUE), ]
      d <- d[!duplicated(d[, c("ShortIndicatorName", "AreaName")]), ]
      d$Compared <- as.character(d$Compared)
      d$Colour <- as.character(d$Colour)
      d$Colour <- override_not_compared_colour(d$Compared, d$Colour)
      d
    })

    # RBWM-level value per indicator - the borough-wide picture, shown
    # alongside the ward breakdown rather than only ward-by-ward
    rbwm_data <- reactive({
      d <- full_dataset()
      req(d)
      si <- summary_indicators()
      d <- d[d$ShortIndicatorName %in% si$ShortIndicatorName &
               d$AreaCode == rbwm_code, ]
      req(nrow(d) > 0)
      d <- d[order(d$Timeperiod, decreasing = TRUE), ]
      d <- d[!duplicated(d$ShortIndicatorName), ]
      d$Compared <- as.character(d$Compared)
      d$Colour <- as.character(d$Colour)
      d$Colour <- override_not_compared_colour(d$Compared, d$Colour)
      d
    })

    # classify each indicator by which comparison method it actually uses,
    # from the Compared values it produces across all wards - not just
    # text-matching "Compared" in isolation, since "Similar" appears in
    # both RAG and BOB output and can carry a different colour in each.
    # Shared by the legend and the "which indicators use which method" list.
    indicator_comparison_types <- reactive({
      d <- latest_ward_data()
      req(nrow(d) > 0)
      si <- summary_indicators()

      ind_type <- aggregate(Compared ~ ShortIndicatorName, data = d, FUN = function(x) {
        x <- tolower(as.character(x))
        if (any(grepl("better|worse", x))) "RAG"
        else if (any(grepl("higher|lower", x))) "BOB"
        else "Other"
      })
      names(ind_type)[2] <- "CompType"

      is_quintile <- si$ShortIndicatorName[
        grepl("quintile", si$BenchmarkingMethod, ignore.case = TRUE)
      ]
      ind_type$CompType[ind_type$ShortIndicatorName %in% is_quintile] <- "Quintile"
      merge(ind_type, si[, c("ShortIndicatorName", "DisplayName")], by = "ShortIndicatorName")
    })

    output$rag_key <- renderUI({
      d <- latest_ward_data()
      req(nrow(d) > 0)
      si <- summary_indicators()
      ind_type <- indicator_comparison_types()[, c("ShortIndicatorName", "CompType")]

      is_quintile <- si$ShortIndicatorName[
        grepl("quintile", si$BenchmarkingMethod, ignore.case = TRUE)
      ]

      d2 <- merge(d, ind_type, by = "ShortIndicatorName", all.x = TRUE)
      d2$CompType[d2$ShortIndicatorName %in% is_quintile] <- "Quintile"

      # dedupe by (Compared, Colour, CompType) - not Compared alone, so a
      # shared label like "Similar" with a different colour in RAG vs BOB
      # isn't silently dropped
      key <- d2[!duplicated(d2[, c("Compared", "Colour", "CompType")]) & !is.na(d2$Compared) & d2$CompType != "Quintile",
                c("Compared", "Colour", "CompType")]

      key_row <- function(label, colour) {
        swatch_colour <- if (is.na(colour) || colour == "") "#CCCCCC" else colour
        tags$span(
          style = "display:inline-flex; align-items:center; white-space:nowrap; margin-right:1rem;",
          tags$span(style = paste0(
            "display:inline-block; width:12px; height:12px; background-color:",
            swatch_colour, "; border:1px solid #999; border-radius:2px; margin-right:5px;"
          )),
          tags$span(label, style = "font-size:0.8rem;")
        )
      }

      group_inline <- function(title, rows) {
        tags$span(
          style = "display:inline-flex; align-items:center; flex-wrap:nowrap; white-space:nowrap; margin-right:1.5rem;",
          tags$span(paste0(title, ":"), style = "font-weight:700; color:#6F2277; font-size:0.8rem; margin-right:0.5rem; white-space:nowrap;"),
          tags$span(
            style = "display:inline-flex; align-items:center; flex-wrap:nowrap;",
            lapply(seq_len(nrow(rows)), function(i) key_row(rows$Compared[i], rows$Colour[i]))
          )
        )
      }

      boxes <- list()
      if (nrow(key[key$CompType == "RAG", ]) > 0) {
        boxes <- c(boxes, list(group_inline("RAG", key[key$CompType == "RAG", ])))
      }
      if (nrow(key[key$CompType == "BOB", ]) > 0) {
        boxes <- c(boxes, list(group_inline("BOB", key[key$CompType == "BOB", ])))
      }

      # quintile legend: explicit 1-5, using the actual colours from the data
      if (length(is_quintile) > 0) {
        qd <- d2[d2$CompType == "Quintile" & !is.na(d2$Quintile), c("Quintile", "Colour")]
        qd$Quintile <- suppressWarnings(as.numeric(gsub("\\D", "", qd$Quintile)))
        qd <- qd[!is.na(qd$Quintile), ]
        qd <- qd[!duplicated(qd$Quintile), ]
        qd <- qd[order(qd$Quintile), ]
        if (nrow(qd) > 0) {
          qd$Compared <- ifelse(qd$Quintile == 1, "1 (worst)",
                          ifelse(qd$Quintile == 5, "5 (best)",
                                 as.character(qd$Quintile)))
          boxes <- c(boxes, list(group_inline("Quintiles", qd[, c("Compared", "Colour")])))
        }
      }

      tags$div(
        style = "display:flex; flex-wrap:wrap; align-items:center; border:1px solid #E4DBE8; border-radius:8px; padding:0.6rem 1rem; margin-bottom:0.5rem; background:#fff;",
        tags$span("Legend \u2013", style = "font-weight:700; margin-right:1rem;"),
        boxes
      )
    })

    output$benchmark_examples <- renderUI({
      ct <- indicator_comparison_types()
      req(nrow(ct) > 0)

      example_list <- function(type) {
        names_for_type <- sort(unique(ct$DisplayName[ct$CompType == type]))
        if (length(names_for_type) == 0) return(NULL)
        tags$li(strong(type, ": "), paste(names_for_type, collapse = ", "))
      }

      tags$div(
        style = "margin-top:0.5rem; font-size:0.9rem;",
        tags$p(strong("Which indicators use which method:")),
        tags$ul(
          example_list("RAG"), example_list("BOB"), example_list("Quintile")
        )
      )
    })

    output$summary_table <- DT::renderDataTable({
      req(input$ward)
      d <- latest_ward_data()
      si <- summary_indicators()
      req(nrow(si) > 0)

      if (input$ward == "__all__") {
        # all-wards matrix: RBWM first (the overall borough picture), then
        # one column per ward. Built as HTML cells directly rather than
        # DT::formatStyle, since that gets unreliable once every column
        # needs its own independent colour mapping.
        wards <- sort(unique(d$AreaName))
        req(length(wards) > 0)
        rbwm_d <- rbwm_data()

        wide_rows <- lapply(seq_len(nrow(si)), function(i) {
          short_name <- si$ShortIndicatorName[i]
          row_d <- d[d$ShortIndicatorName == short_name, ]
          rbwm_row <- rbwm_d[rbwm_d$ShortIndicatorName == short_name, ]
          if (nrow(row_d) == 0 && nrow(rbwm_row) == 0) return(NULL)

          cell_html <- function(v) {
            if (nrow(v) == 0 || is.na(v$Value[1])) return("")
            colour <- if (is.na(v$Colour[1])) "#EEEEEE" else v$Colour[1]
            text_colour <- contrast_text_colour(colour)
            sprintf('<div style="background-color:%s; color:%s; padding:4px; border-radius:5px; font-weight:600; text-align:center;">%s</div>',
                    colour, text_colour, round(v$Value[1], 1))
          }

          cells <- vapply(wards, function(w) cell_html(row_d[row_d$AreaName == w, ]),
                           character(1))
          if (isTRUE(input$compact_view)) names(cells) <- ward_short_labels[wards]

          c(Theme = as.character(si$Theme[i]),
            Indicator = si$DisplayName[i],
            RBWM = cell_html(rbwm_row),
            cells)
        })
        wide_rows <- wide_rows[!vapply(wide_rows, is.null, logical(1))]
        req(length(wide_rows) > 0)

        tbl <- as.data.frame(do.call(rbind, wide_rows),
                              stringsAsFactors = FALSE, check.names = FALSE)
        tbl$Theme <- theme_cell(tbl$Theme)

        DT::datatable(
          tbl, rownames = FALSE, escape = FALSE, class = "compact stripe hover all-wards-table",
          options = list(pageLength = 100, dom = "t", scrollX = TRUE,
                         scrollY = "65vh", scrollCollapse = TRUE, ordering = FALSE,
                         columnDefs = list(
                           list(width = "60px", targets = 2:(ncol(tbl) - 1)),
                           list(width = "220px", targets = 0),   # Theme
                           list(width = "360px", targets = 1)    # Indicator
                         )),
          selection = "none"
        )
      } else {
        rbwm_d <- rbwm_data()

        cell_html <- function(value, colour) {
          if (is.na(value)) return("")
          colour <- if (is.na(colour) || colour == "") "#EEEEEE" else colour
          text_colour <- contrast_text_colour(colour)
          sprintf('<div style="background-color:%s; color:%s; padding:4px; border-radius:5px; font-weight:600; text-align:center;">%s</div>',
                  colour, text_colour, round(value, 1))
        }

        rows <- lapply(seq_len(nrow(si)), function(i) {
          short_name <- si$ShortIndicatorName[i]
          ind_d <- d[d$ShortIndicatorName == short_name &
                       normalize_ward_name(d$AreaName) == normalize_ward_name(input$ward), ]
          if (nrow(ind_d) == 0 || is.na(ind_d$Value[1]) || is.na(ind_d$Compared[1])) return(NULL)
          rbwm_row <- rbwm_d[rbwm_d$ShortIndicatorName == short_name, ]

          data.frame(
            Theme = as.character(si$Theme[i]),
            Indicator = si$DisplayName[i],
            `Ward value` = cell_html(ind_d$Value[1], ind_d$Colour[1]),
            `RBWM value` = if (nrow(rbwm_row) > 0) cell_html(rbwm_row$Value[1], rbwm_row$Colour[1]) else "",
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
        })
        tbl <- do.call(rbind, rows)
        req(!is.null(tbl), nrow(tbl) > 0)
        tbl$Theme <- theme_cell(tbl$Theme)

        DT::datatable(
          tbl, rownames = FALSE, escape = FALSE, class = "compact stripe hover",
          options = list(pageLength = 100, dom = "t",
                         scrollY = "65vh", scrollCollapse = TRUE, ordering = FALSE,
                         columnDefs = list(
                           list(width = "110px", targets = 0),   # Theme
                           list(width = "200px", targets = 1),   # Indicator
                           list(width = "100px", targets = c(2, 3))  # Ward/RBWM values
                         )),
          selection = "none"
        )
      }
    })

    # Associated maps - one ward-level choropleth per explicitly selected
    # indicator (input$indicator_filter, not the theme filter), so the
    # summary page can carry a narrative rather than staying table-only.
    # Nothing renders until the user actually ticks at least one indicator.
    output$selected_maps <- renderUI({
      selected <- input$indicator_filter
      req(!is.null(selected), length(selected) > 0, shp_ward)

      map_cards <- lapply(selected, function(ind) {
        map_id <- paste0("map_", make.names(ind))
        display_name <- all_summary_indicators$DisplayName[all_summary_indicators$ShortIndicatorName == ind][1]
        tags$div(
          tags$h5(display_name, style = "color:#6F2277;"),
          leaflet::leafletOutput(session$ns(map_id), height = "350px")
        )
      })

      tags$div(
        style = "display:grid; grid-template-columns: repeat(auto-fit, minmax(400px, 1fr)); gap:1.5rem; margin-top:1rem;",
        map_cards
      )
    })

    observeEvent(input$indicator_filter, {
      selected <- input$indicator_filter
      req(!is.null(selected), length(selected) > 0, shp_ward)

      lapply(selected, function(ind) {
        local({
          this_ind <- ind
          map_id <- paste0("map_", make.names(this_ind))
          output[[map_id]] <- leaflet::renderLeaflet({
            d <- latest_ward_data()
            req(nrow(d) > 0)
            d <- d[d$ShortIndicatorName == this_ind, ]
            req(nrow(d) > 0)

            d_map <- merge(shp_ward, d[, c("AreaCode", "Value", "Colour", "Compared")],
                           by = "AreaCode", all.x = FALSE)
            req(nrow(d_map) > 0)
            d_map <- sf::st_transform(d_map, 4326)
            d_map$Colour <- as.character(d_map$Colour)
            fill_colours <- ifelse(!is.na(d_map$Colour), d_map$Colour, "#4472C4")

            leaflet::leaflet(d_map) |>
              leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) |>
              leaflet::addPolygons(
                fillColor = fill_colours, fillOpacity = 0.75,
                color = "#333333", weight = 1,
                label = ~paste0(AreaName, ": ", round(Value, 1))
              )
          })
        })
      })
    }, ignoreNULL = FALSE)
  })
}
