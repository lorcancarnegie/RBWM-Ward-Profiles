# ---------------------------------------------------------------------------
# RBWM Ward Profiles — Shiny app
# Author: Lorcan Carnegie
# Date created: 2026-10-19
# R version: R 2026.06.0
# ---------------------------------------------------------------------------

library(shiny)
library(bslib)
library(ggplot2)
library(leaflet)
library(sf)
library(plotly)
library(DT)

source("R/shape_helpers.R")
source("R/catchment_helpers.R")
source("R/indicator_lookup.R")
source("R/mod_indicator.R")
source("R/mod_lsoa_indicator.R")
source("R/mod_gp_indicator.R")
source("R/mod_msoa_indicator.R")
source("R/mod_condition_group.R")
source("R/mod_grouped_indicator.R")
source("R/mod_population.R")
source("R/mod_summary.R")

# ---- Geography codes (same as data.R) ----
rbwm_code <- "E06000040"
se_code   <- "E12000008"
eng_code  <- "E92000001"

# ---- Locate the most recent data bundle ----
bundle_dir <- "."

find_latest_bundle <- function(dir) {
  files <- list.files(dir, pattern = "^ward_profiles_data_bundle_.*\\.Rds$",
                       full.names = TRUE)
  if (length(files) == 0) {
    stop("No data bundle found in '", normalizePath(dir, mustWork = FALSE),
         "'. Check data.R has been run locally and bundle_dir points at ",
         "the right folder.")
  }
  files[order(file.info(files)$mtime, decreasing = TRUE)][1]
}

# ---- Data year(s) per indicator, for the "About this indicator" panel ----
data_years_lookup <- tryCatch({
  bundle_for_years <- readRDS(find_latest_bundle(bundle_dir))
  d <- bundle_for_years$dataset
  years_by_indicator <- tapply(d$Timeperiod, d$ShortIndicatorName, function(x) {
    u <- sort(unique(as.character(x[!is.na(x)])))
    if (length(u) == 0) return(NA_character_)
    if (length(u) == 1) return(u)
    paste0(u[1], " to ", u[length(u)])
  })
  years_by_indicator
}, error = function(e) character(0))

get_data_years <- function(short_name) {
  if (short_name %in% names(data_years_lookup)) data_years_lookup[[short_name]] else NA
}

# ---- Ward names ----
source("R/wards.R")  # provides `wards` (AreaCode/AreaName) and `ward_name`

shiny::addResourcePath("branding", "Branding")

# ---- Reusable accordion panels (shown on the Introduction page) ----
panel_indicator_measures <- bslib::accordion_panel(
      "Indicator measures",
      p("The indicators in this profile use a range of measures to present data appropriately. These include:"),
      tags$ul(
        tags$li(strong("Proportion or percentage (%)"), " \u2013 Shows the size of a group as a share of the whole. For example, if 25 out of 100 people have a particular condition, this is shown as 25%."),
        tags$li(strong("Crude rate"), " \u2013 Shows the total number of events (such as births, deaths or disease cases) in a population over a specified period. It is usually expressed per 1,000, 10,000 or 100,000 people to enable comparisons between populations of different sizes."),
        tags$li(strong("Indirectly standardised ratio (ISR)"), " \u2013 Compares the observed number of events in a population with the number that would be expected if that population had the same age-specific rates as a standard population. This adjusts for differences in age structure, making comparisons between areas more meaningful. An ISR of 100 indicates that the observed rate is equal to the expected rate, values above 100 indicate more events than expected, and values below 100 indicate fewer events than expected.")
      )
    )

panel_benchmarking_methods <- bslib::accordion_panel(
      "Benchmarking methods",
      p("The profiles use several methods to compare indicators and highlight variation between areas:"),
      tags$ul(
        tags$li(strong("RAG comparison (Red, Amber, Green)"), " \u2013 Indicates how an area's value compares with a benchmark, usually England. Comparisons are based on statistical testing, typically using confidence intervals. Green indicates the area is performing significantly better than the benchmark, red indicates it is performing significantly worse, and amber indicates there is no statistically significant difference."),
        tags$li(strong("BOB comparison (Blue, Orange, Blue)"), " \u2013 Used where a higher or lower value should not be interpreted as inherently better or worse. Light blue indicates the area's value is higher than the benchmark, while dark blue indicates it is lower."),
        tags$li(strong("Quintiles"), " \u2013 Divide data into five equal groups to show the relative position of areas and highlight inequalities. Quintiles are used where indicators are not population-based (for example, household data or survey data where weighted estimates are unavailable), making statistical comparisons inappropriate."),
        tags$li(strong("Brown (Not compared)"), " \u2013 Indicates that a comparison cannot be made, typically because no data are available for the area or the underlying count is too small (usually fewer than 10) to produce a reliable estimate.")
      )
    )

panel_accessibility <- bslib::accordion_panel(
      "Accessibility",
      p("All charts include alternative text (alt text) to improve accessibility. The alt text provides a description of the visual content and, where possible, includes the key values shown in the chart."),
      p("To view the alt text, right-click on a chart and select View Alt Text."),
      p("To ensure the profiles are accessible to users with colour vision deficiencies, symbols are used alongside colours to indicate comparisons:"),
      tags$p("\u25B2 Upward triangle \u2013 Better performance", style = "margin-left:1rem;"),
      tags$p("\u25CF Circle \u2013 Similar performance", style = "margin-left:1rem;"),
      tags$p("\u25BC Downward triangle \u2013 Worse performance", style = "margin-left:1rem;"),
      tags$p("\u25A0 Square \u2013 Comparison not available", style = "margin-left:1rem;")
    )

# ---- Intro page ----
intro_panel <- function() {
  rbwm_name <- tags$span("Royal Borough of Windsor & Maidenhead", style = "color: #6F2277; font-weight: 600;")
  rbwm_name_html <- as.character(rbwm_name)
  strategy_link <- as.character(tags$a(
    "Royal Borough of Windsor & Maidenhead Joint Health and Wellbeing Strategy 2026-2036",
    href = "https://www.rbwm.gov.uk/sites/default/files/2026-01/ph_joint_health_wellbeing_strategy_2026-2036_0.pdf",
    target = "_blank", style = "color: #6F2277; font-weight: 600;"
  ))

  link_style <- "color: #6F2277; font-weight: 600;"
  go_tab <- function(label) sprintf(
    'var t=document.querySelector("a[data-value=\'%s\']"); if(t){t.click(); window.scrollTo({top:0,behavior:"smooth"});} return false;', label)

  theme_tile <- function(label, blurb) {
    tags$a(href = "#", class = "intro-theme", onclick = go_tab(label),
           div(class = "intro-theme-t", label), div(class = "intro-theme-b", blurb))
  }
  step_card <- function(n, title, text) {
    div(class = "intro-step",
        div(class = "intro-step-n", n),
        div(tags$strong(title), tags$br(), text))
  }

  nav_panel(
    title = "Introduction",
    div(
      style = "width: 100%; margin: 1rem 0; text-align: left; font-size: 1rem;",
      class = "intro-page-content",
      tags$style(HTML("
        .intro-page-content p { margin-bottom: 0.6rem; }
        .intro-hero { background: linear-gradient(135deg,#6F2277 0%,#8E3A96 100%); color:#fff;
                      border-radius:10px; padding:1.5rem 1.75rem; margin-bottom:1.25rem; }
        .intro-hero h2 { color:#fff; margin:0 0 0.5rem; font-size:1.6rem; font-weight:700; }
        .intro-hero p { color:#fff; margin:0; font-size:1rem; }
        .intro-h { color:#6F2277; font-weight:700; font-size:1.15rem; margin:1.25rem 0 0.6rem; }
        .intro-themes { display:grid; grid-template-columns:repeat(auto-fit,minmax(190px,1fr)); gap:0.75rem; }
        .intro-theme { display:block; text-decoration:none; background:#fff; border:1px solid #D9CFDD;
                       border-left:5px solid #6F2277; border-radius:8px; padding:0.75rem 1rem; color:#333; }
        .intro-theme:hover { background:#F3EEF5; text-decoration:none; color:#333; }
        .intro-theme-t { font-weight:700; color:#6F2277; }
        .intro-theme-b { font-size:0.9rem; margin-top:0.15rem; }
        .intro-steps { display:grid; grid-template-columns:repeat(auto-fit,minmax(250px,1fr)); gap:0.75rem; }
        .intro-step { display:flex; gap:0.75rem; align-items:flex-start; background:#F8F5F9;
                      border-radius:8px; padding:0.75rem 1rem; font-size:0.9rem; }
        .intro-step-n { flex:0 0 auto; width:1.9rem; height:1.9rem; border-radius:50%; background:#6F2277;
                        color:#fff; font-weight:700; display:flex; align-items:center; justify-content:center; }
        .intro-further { font-size:0.9rem; margin-top:1rem; }
      ")),

      div(class = "intro-hero",
        tags$h2("Ward Profiles"),
        HTML(paste0("<p>These ward profiles bring together a range of indicators describing the wider determinants, or building blocks, of health and wellbeing across the wards of the Royal Borough of Windsor &amp; Maidenhead.</p>"))
      ),

      HTML(paste0("<p>The profiles are structured around the five themes of the ", strategy_link, ", which sets the long-term direction for improving health and wellbeing across the borough. Each theme includes the relevant Strategy focus areas alongside indicators covering topics such as population and demographics, food, employment, family and communities, transport, housing, the local environment, income and resources, and education and skills.</p>")),
      HTML(paste0("<p>Where possible, ward-level data are shown alongside ", rbwm_name_html, " and England comparators to provide context. Each indicator also includes its data source and relevant notes to support interpretation.</p>")),

      div(class = "intro-h", "Explore the five themes"),
      div(class = "intro-themes",
        theme_tile("Best Start in Life", "School readiness, healthy weight, oral health"),
        theme_tile("Living Well", "Prevention, screening and health behaviours"),
        theme_tile("Healthy Ageing", "Independence, early intervention, loneliness"),
        theme_tile("Healthy Place", "Active travel, housing, environment, work"),
        theme_tile("Cross-cutting themes", "Health inequalities and wider determinants")
      ),

      div(class = "intro-h", "How to use the profiles"),
      div(class = "intro-steps",
        step_card("1", "All wards by default", paste0("Charts show all wards alongside ", "Royal Borough of Windsor & Maidenhead and England reference values.")),
        step_card("2", "Pick a ward", "Use the dropdown menu on each chart or card to select an individual ward."),
        step_card("3", "Go deeper", "See the Summary and Data information tabs for further benchmarking and information on the indicators.")
      ),

      div(class = "intro-h", "Understanding the indicators"),
      bslib::accordion(
        open = FALSE,
        panel_indicator_measures,
        panel_benchmarking_methods,
        panel_accessibility
      ),

      tags$p(
        class = "intro-further",
        "Further reading: ",
        tags$a(href = "https://www.rbwm.gov.uk/community-and-living/public-health/joint-strategic-needs-assessment",
               target = "_blank", style = link_style,
               "RBWM Joint Strategic Needs Assessment (JSNA)"),
        " and ",
        tags$a(href = "https://www.rbwm.gov.uk/sites/default/files/2026-01/ph_joint_health_wellbeing_strategy_2026-2036_0.pdf",
               target = "_blank", style = link_style,
               "RBWM Joint Health and Wellbeing Strategy 2026-2036"),
        "."
      )
    )
  )
}

# ---- Data information & references page ----
data_information_panel <- function() {
  nav_panel(
    title = "Data information",
    div(
      style = "width: 100%; margin: 1.5rem 0;",
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Statistical, Census and electoral geographies",
          p("Data are presented at several small-area geographies throughout these profiles:"),
          tags$div(
            tags$p(strong("Output Areas (OAs)")),
            tags$p("The smallest statistical geography used by the Office for National Statistics, with populations of around 100 to 625 people. There are 458 2021 Output Areas in the Royal Borough of Windsor & Maidenhead.", style = "margin-left:1rem;"),
            tags$p(strong("Lower Super Output Areas (LSOAs)")),
            tags$p("Statistical areas with populations of around 1,000 to 3,000 people. There are 89 Census 2021 Lower Super Output Areas in the Royal Borough of Windsor & Maidenhead.", style = "margin-left:1rem;"),
            tags$p(strong("Electoral wards")),
            tags$p("Areas represented by one or more elected councillors on the local authority. There are 19 electoral wards in the Royal Borough of Windsor & Maidenhead.", style = "margin-left:1rem;"),
            tags$p(strong("Postcodes")),
            tags$p("Used to identify specific locations within the borough. There are approximately 7,190 postcodes in the Royal Borough of Windsor & Maidenhead.", style = "margin-left:1rem;"),
            tags$p(strong("GP practices")),
            tags$p("The profiles include data for the main GP practices serving residents of the borough. There are 38 GP practices in the Royal Borough of Windsor & Maidenhead.", style = "margin-left:1rem;")
          ),
          tags$p(strong("A note on geographies")),
          p("Lower Super Output Areas do not align exactly with electoral ward boundaries. Where ward-level estimates are presented using LSOA-based data, a best-fit geographic lookup has been used to assign LSOAs to wards.")
        ),
        bslib::accordion_panel(
          "References",
          p("The methodology for each indicator is based on published data and existing measures. Sources of methodology include the Office for Health Improvement and Disparities, the Office for National Statistics, and NHS Digital."),
          p("Some indicator values may be different from published figures. This may be due to slight differences in numbers extracted or data being grouped into different time periods."),
          p("Each indicator card states its own specific data source and licensing information. Map-specific attribution:"),
          tags$ul(
            tags$li("Map boundary data: Office for National Statistics, licensed under the Open Government Licence v.3.0. Contains OS data \u00A9 Crown copyright and database right."),
            tags$li("Map base layer: \u00A9 OpenStreetMap contributors, \u00A9 CARTO."),
            tags$li(
              "GP practice catchment maps: NHS England, ",
              tags$a("Patients Registered at a GP Practice",
                     href = "https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice",
                     target = "_blank"),
              " (July 2026), with practices assigned to Primary Care Networks using the practice mapping file published alongside it."
            )
          )
        ),
        bslib::accordion_panel(
          "Contact",
          p("For questions or additional information about these profiles, please contact:"),
          p("Public Health Intelligence Function", tags$br(),
            "Public Health", tags$br(),
            "Royal Borough of Windsor & Maidenhead", tags$br(),
            tags$a(href = "mailto:phi@rbwm.gov.uk", "phi@rbwm.gov.uk"))
        )
      )
    )
  )
}

app_header <- tags$div(
  style = "display:grid; grid-template-columns: 1fr auto 1fr; align-items:center; width:100vw; margin-left: calc(-50vw + 50%); margin-right: calc(-50vw + 50%); padding-left: calc(50vw - 50%); padding-right: calc(50vw - 50%); border-bottom:4px solid #6F2277; padding-bottom:4px; margin-bottom:0; box-sizing:border-box;",
  tags$span(),
  tags$span("Royal Borough of Windsor & Maidenhead: Ward Profiles",
            style = "font-size: 1.7rem; font-weight: 700; color: #6F2277; text-align:center;"),
  tags$img(src = "branding/logo.png", height = "80px", alt = "RBWM logo",
          style = "justify-self:end;")
)

footer_stripes <- tags$div(
  tags$div(style = "height:4px; background-color:#6F2277; width:100%;"),
  tags$div(style = "height:4px; background-color:#AA9767; width:100%;")
)

  qof_condition_sources <- list(
    "Mental Health: QOF prevalence" = "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 90581",
    "Depression: QOF prevalence" = "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 848",
    "Depression: QOF incidence - new diagnosis" = "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 90646",
    "CHD: QOF prevalence" = "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 273",
    "Cancer: QOF prevalence" = "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 276",
    "Atrial fibrillation: QOF prevalence" = "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 280",
    "Asthma: QOF prevalence" = "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 90933",
    "Diabetes: QOF prevalence" = "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 241",
    "COPD: QOF prevalence" = "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 253",
    "CKD: QOF prevalence" = "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 258",
    "Heart Failure: QOF prevalence" = "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 262",
    "Hypertension: QOF prevalence" = "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 219",
    "Smoking: QOF prevalence" = "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 91280",
    "Rheumatoid Arthritis: QOF prevalence" = "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 91269",
    "Osteoporosis: QOF prevalence" = "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 90443",
    "Dementia: QOF prevalence" = "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 247",
    "Stroke: QOF prevalence" = "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 212"
  )

  theme_strategy_info <- list(
    "Best Start in Life" = list(
      description = "The experiences and opportunities children and young people have as they grow up shape their health and wellbeing for life. In the borough, we see gaps in health outcomes emerging from the earliest years, which can continue into later childhood and adolescence. By ensuring the building blocks of good health and wellbeing are in place, we can give every young person the strongest foundation to thrive.",
      focus_points = c("School readiness", "Mental health and wellbeing", "Healthy weight", "Oral health", "Healthy adolescence")
    ),
    "Living Well" = list(
      description = "Adults in the borough face preventable health challenges that affect wellbeing across the life-course. Early intervention in young and middle adulthood increases the chances of being healthy in later life. We will take a prevention-first, neighbourhood-based approach, working with residents, health services, voluntary and community partners to create supportive environments for healthy living.",
      focus_points = c("Cardiovascular disease prevention", "Oral health", "Screening and immunisations", "Health behaviours (Healthy weight, Physical activity, Alcohol harm, Tobacco harm)")
    ),
    "Healthy Ageing" = list(
      description = "Although challenges to health and wellbeing are common in later life, living in poor health and experiencing poor wellbeing aren't an inevitable consequence of ageing. We will be proactive in supporting the health and wellbeing of older adults to ensure as many as possible can live independent, productive and fulfilling lives.",
      focus_points = c("Promoting independence", "Screening and early intervention", "Digital inclusion", "Loneliness and isolation")
    ),
    "Healthy Place" = list(
      description = "The environments where we live, work, learn and connect, including physical, social, cultural and digital, shape our health and wellbeing. While individual behaviours are often viewed through the lens of personal choice, strong evidence highlights the powerful role our surroundings play in influencing outcomes. Adopting a place-based approach, we aim to ensure the borough is designed to support the wellbeing of all residents, embedding health into the very fabric of our communities.",
      focus_points = c("Active travel", "Healthy housing", "Planning, infrastructure and licensing", "Sustainable and healthy environments", "Health and work", "Gambling harm")
    ),
    "Cross-cutting themes" = list(
      description = NULL,
      focus_points = c("Reducing health inequalities", "Addressing wider determinants of health", "Shifting from treatment to prevention", "Mental health and wellbeing")
    )
  )

anchor_link <- function(label, target) {
  target <- sub("^#", "", target)
  tags$a(
    label,
    href = paste0("#", target),
    onclick = sprintf('var el=document.querySelector("[data-anchor=\'%s\']"); if(el){el.scrollIntoView({behavior:"smooth",block:"start"});} return false;', target),
    style = "color:#6F2277; margin-right:1.25rem; font-size:0.9rem; white-space:nowrap;"
  )
}

toc_topics <- c(
  "LA population" = "Population", "ethnicity" = "Population", "Main language" = "Population",
  "IMD" = "Deprivation",
  "Reception: Prevalence of obesity" = "Child weight", "Reception: Prevalence of excess weight" = "Child weight",
  "Year 6: Prevalence of obesity" = "Child weight", "Year 6: Prevalence of excess weight" = "Child weight",
  "Children providing unpaid care" = "Families and income", "Lone parent with dependent children" = "Families and income",
  "Children in absolute low-income" = "Families and income", "Working families - absolute low-income" = "Families and income",
  "Lone parent - absolute low-income" = "Families and income",
  "No qualifications" = "Education", "Level 2 qualifications" = "Education", "Level 4 or above qualifications" = "Education",
  "Access to primary schools" = "Education", "Access to secondary school" = "Education",
  "Youth unemployment benefit claimants" = "Employment and benefits",
  "DTaP vaccination coverage (8 months)" = "Immunisation", "MMR vaccination coverage: 1st dose (1.5 yrs)" = "Immunisation",
  "MMR/DTaP vaccination coverage (5 yrs)" = "Immunisation", "Shingles vaccination coverage (70-79 yrs)" = "Immunisation",
  "QOF: Obesity" = "Health behaviours", "Alcohol-related admissions" = "Health behaviours",
  "Employment and Support Allowance" = "Employment and benefits", "Universal Credit - no work requirement" = "Employment and benefits",
  "Unemployment benefit claimants" = "Employment and benefits", "Unemployment benefit claimants (50+ yrs)" = "Employment and benefits",
  "Universal Credit" = "Employment and benefits", "PIP claimants" = "Employment and benefits",
  "Provides unpaid care" = "Caring", "% Caring responsibility" = "Caring", "Carers Allowance" = "Caring",
  "Disabled under Equality Act" = "Health and disability", "Not disabled under Equality Act - has LTC" = "Health and disability",
  "People with not good health" = "Health and disability", "Cannot speak English well or at all" = "Language",
  "Cervical screening (25-49 yrs)" = "Screening", "Cervical screening (50-64 yrs)" = "Screening",
  "Bowel cancer screening" = "Screening", "Breast cancer screening" = "Screening",
  "Bowel cancer screening uptake" = "Screening", "Breast cancer screening uptake" = "Screening",
  "One-person household (aged 66+)" = "Older people", "People on pension credit (65+ yrs)" = "Older people",
  "Hip fracture admissions (65+)" = "Older people",
  "Access to GP" = "Access", "Access to hospital" = "Access", "Access to greenspace" = "Access",
  "Carbon footprint per person" = "Transport and environment", "No cars or vans in household" = "Transport and environment",
  "Mean average house price" = "Housing", "Owner occupied households" = "Housing", "Private rented households" = "Housing",
  "Social rented housing" = "Housing", "Overcrowded households" = "Housing", "Caravan or other temporary accommodation" = "Housing",
  "Fuel poverty" = "Home energy", "No central heating" = "Home energy", "Not connected to gas network" = "Home energy"
)

toc_ui <- function(items) {
  topics_in_order <- unique(items$topic)
  tags$details(
    class = "toc-box",
    tags$summary("On this page", class = "toc-title"),
    tags$div(
      class = "toc-grid",
      lapply(topics_in_order, function(tp) {
        sub <- items[items$topic == tp, ]
        tagList(
          tags$div(tp, class = "toc-topic"),
          tags$div(lapply(seq_len(nrow(sub)), function(j) anchor_link(sub$label[j], sub$target[j])))
        )
      })
    )
  )
}

ui <- tagList(page_sidebar(
  title = app_header,
  theme = bslib::bs_theme(
    version       = 5,
    primary       = "#6F2277",  # RBWM Signal Violet
    secondary     = "#AA9767",  # RBWM Beige
    base_font     = "Arial, sans-serif",
    heading_font  = "Arial, sans-serif"
  ) |>
    bslib::bs_add_rules("
      body {
        line-height: 1.6;
      }
      p {
        margin-bottom: 1rem;
      }
      .card {
        border-radius: 8px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.08);
        border: 1px solid #e5e0d8;
      }
      .card-header {
        background-color: rgba(170, 151, 103, 0.15);
        border-bottom: 2px solid #6F2277;
        font-weight: 600;
        font-size: 1.4rem;
      }
      .nav-tabs .nav-link {
        font-size: 1.15rem;
        padding: 0.6rem 1rem;
      }
      .card-body .nav-tabs .nav-link {
        font-size: 0.9rem;
        padding: 0.4rem 0.75rem;
      }
      /* CSS Grid stretches items to the tallest in their row by default;
         override so each indicator card sizes to its own content instead
         of being padded out with blank space to match its row neighbour */
      .bslib-grid {
        align-items: start;
      }
      .toc-box { margin-bottom:1rem; padding:0.7rem 1rem; background:rgba(170,151,103,0.1); border-radius:6px; }
      .toc-title { font-weight:700; color:#6F2277; cursor:pointer; }
      .toc-box[open] .toc-title { margin-bottom:0.5rem; }
      .toc-grid { display:grid; grid-template-columns:max-content 1fr; column-gap:1.5rem; row-gap:0.3rem; align-items:baseline; }
      .toc-topic { font-size:0.9rem; color:#555; }
      [data-anchor] {
        scroll-margin-top: 0.5rem;
      }
      .card-body label,
      .card-body .control-label,
      .card-body .form-check-label,
      .card-body .selectize-input,
      .card-body .selectize-dropdown,
      .card-body .form-select {
        font-size: 0.9rem;
      }
      .nav-tabs .nav-link.active {
        color: #6F2277;
        border-bottom: 3px solid #6F2277;
        font-weight: 600;
      }
      .navbar {
        margin-bottom: 0 !important;
        padding-bottom: 0 !important;
      }
      table.dataTable {
        font-size: 0.9rem;
      }
      table.dataTable thead th {
        font-size: 0.9rem !important;
      }
      table.dataTable tbody td {
        font-size: 0.9rem !important;
        white-space: normal !important;
        word-wrap: break-word;
      }
      table.dataTable thead th {
        white-space: normal !important;
      }
      .accordion-button {
        background-color: rgba(170, 151, 103, 0.15);
        color: #6F2277;
        font-weight: 600;
      }
      .accordion-button:not(.collapsed) {
        background-color: rgba(111, 34, 119, 0.1);
        color: #6F2277;
        box-shadow: none;
      }
      .accordion-button:focus {
        box-shadow: 0 0 0 0.2rem rgba(111, 34, 119, 0.25);
      }
      .accordion-item {
        border-color: #AA9767;
      }
      .tab-content {
        margin-top: 1.25rem;
      }
      .card-body {
        padding: 0.6rem;
        gap: 0.25rem;
      }
      .card-body .form-group,
      .card-body .shiny-input-container {
        margin-bottom: 0.15rem;
      }
      .card-body .nav-tabs {
        margin-bottom: 0.25rem;
      }
    "),
  sidebar = sidebar(
    width = 260,
    open = "closed",
    tags$div(
      style = "display:flex; gap:8px; margin-bottom:1rem;",
      actionButton("back_btn", label = tags$img(src = "branding/back.png", height = "22px"),
                   title = "Back"),
      actionButton("home_btn", label = tags$img(src = "branding/home.png", height = "22px"),
                   title = "Home"),
      actionButton("menu_btn", label = tags$img(src = "branding/menu.png", height = "22px"),
                   title = "Menu")
    ),
    actionButton("refresh_data", "Reload latest data", icon = icon("rotate"),
                 class = "btn-primary")
  ),
  do.call(navset_tab, c(
    list(id = "main_tabs"),
    list(intro_panel()),
    list(nav_panel(
      title = "Summary",
      mod_summary_ui("summary")
    )),
    lapply(levels(indicator_lookup$Theme), function(theme_name) {
      inds <- indicator_lookup[indicator_lookup$Theme == theme_name, ]
      inds <- inds[order(inds$order), ]

      card_list <- lapply(seq_len(nrow(inds)), function(i) {
        this_name <- inds$ShortIndicatorName[i]
        if (grepl(": QOF (prevalence|incidence)", this_name)) return(NULL)
        anchor_id <- paste0("anchor_", make.names(this_name))
        card_ui <- if (this_name == "LA population") {
          mod_population_ui(id = paste0("ind_", make.names(this_name)))
        } else if (this_name == "IMD") {
          mod_lsoa_indicator_ui(
            id = paste0("ind_", make.names(this_name)),
            display_name = inds$DisplayName[i]
          )
        } else if (this_name %in% c("QOF: Obesity", "% Caring responsibility", "Cervical screening (25-49 yrs)", "Cervical screening (50-64 yrs)", "Bowel cancer screening", "Breast cancer screening", "Bowel cancer screening uptake", "Breast cancer screening uptake", "DTaP vaccination coverage (8 months)", "MMR vaccination coverage: 1st dose (1.5 yrs)", "MMR/DTaP vaccination coverage (5 yrs)", "Shingles vaccination coverage (70-79 yrs)")) {
          mod_gp_indicator_ui(
            id = paste0("ind_", make.names(this_name)),
            display_name = inds$DisplayName[i],
            value_type = inds$ValueType[i],
            benchmarking_method = inds$BenchmarkingMethod[i],
            source = inds$Source[i],
            original_geography = inds$OriginalGeography[i],
            known_caveats = inds$KnownCaveats[i],
            additional_notes = inds$AdditionalNotes[i],
            data_years = get_data_years(this_name)
          )
        } else if (this_name %in% c("Alcohol-related admissions", "Hip fracture admissions (65+)")) {
          mod_msoa_indicator_ui(
            id = paste0("ind_", make.names(this_name)),
            display_name = inds$DisplayName[i],
            value_type = inds$ValueType[i],
            benchmarking_method = inds$BenchmarkingMethod[i],
            source = inds$Source[i],
            original_geography = inds$OriginalGeography[i],
            known_caveats = inds$KnownCaveats[i],
            additional_notes = inds$AdditionalNotes[i],
            data_years = get_data_years(this_name)
          )
        } else if (this_name %in% c("ethnicity", "Main language")) {
          mod_grouped_indicator_ui(
            id = paste0("ind_", make.names(this_name)),
            display_name = inds$DisplayName[i]
          )
        } else {
          mod_indicator_ui(
            id = paste0("ind_", make.names(this_name)),
            short_name = this_name,
            display_name = inds$DisplayName[i],
            has_map = inds$MapGeography[i] != "None",
            has_trend = inds$HasTrend[i],
            available_at_ward = inds$AvailableAtWard[i],
            value_type = inds$ValueType[i],
            benchmarking_method = inds$BenchmarkingMethod[i],
            source = inds$Source[i],
            original_geography = inds$OriginalGeography[i],
            known_caveats = inds$KnownCaveats[i],
            additional_notes = inds$AdditionalNotes[i],
            data_years = get_data_years(this_name)
          )
        }
        list(name = this_name, ui = htmltools::tagAppendAttributes(card_ui, `data-anchor` = anchor_id))
      })
      card_list <- Filter(Negate(is.null), card_list)

      is_imd <- vapply(card_list, function(x) x$name == "IMD", logical(1))
      cards <- lapply(card_list[!is_imd], function(x) x$ui)

      is_qof <- grepl(": QOF (prevalence|incidence)", inds$ShortIndicatorName)

      wide_cards <- lapply(card_list[is_imd], function(x) {
        tags$div(style = "width:100%; margin-bottom:1rem;", x$ui)
      })
      if ("IMD" %in% inds$ShortIndicatorName) {
        ranking_anchor <- "anchor_IMD_ranking"
        ranking_ui <- htmltools::tagAppendAttributes(
          mod_lsoa_ranking_ui(id = "ind_IMD_ranking"),
          `data-anchor` = ranking_anchor
        )
        wide_cards <- c(wide_cards, list(tags$div(style = "width:100%; margin-bottom:1rem;", ranking_ui)))
      }

      if (any(is_qof)) {
        qof_names <- inds$ShortIndicatorName[is_qof]
        qof_display <- inds$DisplayName[is_qof]
        qof_anchor <- paste0("anchor_qof_group_", make.names(theme_name))
        qof_group_label <- if (theme_name == "Cross-cutting themes") "Mental health disease prevalence" else "Disease prevalence"
        qof_ui <- htmltools::tagAppendAttributes(
          mod_condition_group_ui(
            id = paste0("qof_group_", make.names(theme_name)),
            display_name = qof_group_label,
            condition_choices = setNames(qof_names, qof_display)
          ),
          `data-anchor` = qof_anchor
        )
        wide_cards <- c(wide_cards, list(tags$div(style = "width:100%; margin-bottom:1rem;", qof_ui)))
      }

      strategy_info <- theme_strategy_info[[theme_name]]
      strategy_box <- if (!is.null(strategy_info)) {
        tags$details(
          style = "margin-bottom:1rem; padding:0.6rem 0.9rem; background:rgba(170,151,103,0.1); border-radius:6px;",
          tags$summary(style = "cursor:pointer; font-weight:700; color:#6F2277;",
                      tags$a("Joint Health and Wellbeing Strategy Focus Points",
                            href = "https://www.rbwm.gov.uk/sites/default/files/2026-01/ph_joint_health_wellbeing_strategy_2026-2036_0.pdf",
                            target = "_blank",
                            style = "color:#6F2277;",
                            onclick = "event.stopPropagation()")),
          tags$div(
            style = "margin-top:0.5rem;",
            if (!is.null(strategy_info$description)) tags$p(strategy_info$description),
            tags$strong("Focus points:"),
            tags$ul(lapply(strategy_info$focus_points, tags$li))
          )
        )
      }

      non_imd_idx <- which(inds$ShortIndicatorName != "IMD")
      is_qof_idx <- grepl(": QOF (prevalence|incidence)", inds$ShortIndicatorName[non_imd_idx])
      toc_order <- non_imd_idx[which(!is_qof_idx)]

      toc_items <- data.frame(label = character(0), target = character(0), topic = character(0), stringsAsFactors = FALSE)
      add_item <- function(label, target, topic) {
        toc_items[nrow(toc_items) + 1, ] <<- c(label, target, topic)
      }
      topic_of <- function(nm) { t <- unname(toc_topics[nm]); if (is.na(t)) "Other" else t }
      if ("IMD" %in% inds$ShortIndicatorName) {
        i <- which(inds$ShortIndicatorName == "IMD")
        add_item(inds$DisplayName[i], paste0("#anchor_", make.names("IMD")), "Deprivation")
        add_item("LSOA ranking", "#anchor_IMD_ranking", "Deprivation")
      }
      if (any(is_qof_idx)) {
        qlab <- if (theme_name == "Cross-cutting themes") "Mental health disease prevalence" else "Disease prevalence"
        add_item(qlab, paste0("#anchor_qof_group_", make.names(theme_name)), if (theme_name == "Cross-cutting themes") "Mental health" else "Disease prevalence")
      }
      for (i in toc_order) {
        if (inds$ShortIndicatorName[i] == "IMD") next
        add_item(inds$DisplayName[i], paste0("#anchor_", make.names(inds$ShortIndicatorName[i])), topic_of(inds$ShortIndicatorName[i]))
      }
      toc <- toc_ui(toc_items)

      nav_panel(
        title = theme_name,
        strategy_box,
        toc,
        wide_cards,
        do.call(layout_column_wrap, c(list(width = "480px"), cards))
      )
    }),
    list(data_information_panel())
  ))
), footer_stripes)

server <- function(input, output, session) {

  # ---- Home / Back / Menu navigation ----
  all_tab_titles <- c("Introduction", "Summary", levels(indicator_lookup$Theme), "Data information")
  tab_history <- reactiveVal(character(0))
  suppress_history <- reactiveVal(FALSE)

  observeEvent(input$main_tabs, {
    if (isTRUE(suppress_history())) {
      suppress_history(FALSE)
    } else {
      tab_history(c(tab_history(), input$main_tabs))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$home_btn, {
    suppress_history(TRUE)
    bslib::nav_select("main_tabs", "Introduction", session = session)
  })

  observeEvent(input$back_btn, {
    h <- tab_history()
    if (length(h) > 1) {
      suppress_history(TRUE)
      bslib::nav_select("main_tabs", h[length(h) - 1], session = session)
      tab_history(h[-length(h)])
    }
  })

  observeEvent(input$menu_btn, {
    showModal(modalDialog(
      title = "Jump to section",
      lapply(all_tab_titles, function(t) {
        actionButton(paste0("jump_", make.names(t)), t, class = "btn-outline-secondary",
                     style = "display:block; width:100%; margin-bottom:6px; text-align:left;")
      }),
      footer = modalButton("Close"), easyClose = TRUE
    ))
  })

  lapply(all_tab_titles, function(t) {
    observeEvent(input[[paste0("jump_", make.names(t))]], {
      suppress_history(TRUE)
      bslib::nav_select("main_tabs", t, session = session)
      removeModal()
    }, ignoreInit = TRUE)
  })

  raw_bundle   <- reactiveVal(NULL)
  raw_shp_ward <- reactiveVal(NULL)
  raw_shp_lsoa <- reactiveVal(NULL)
  raw_shp_msoa <- reactiveVal(NULL)
  raw_catchment <- reactiveVal(NULL)

  load_bundle <- function() {
    path <- find_latest_bundle(bundle_dir)
    bundle <- readRDS(path)
    raw_bundle(bundle$dataset)
    raw_shp_ward(bundle$shp_ward)
    raw_shp_lsoa(bundle$shp_lsoa)
    raw_shp_msoa(bundle$shp_msoa)
    raw_catchment(bundle$gp_catchment)
  }

  observeEvent(input$refresh_data, load_bundle(), ignoreNULL = FALSE)
  load_bundle()  # load once on startup

  full_dataset <- reactive({
    req(raw_bundle())
    raw_bundle()
  })

  mod_summary_server(
    id             = "summary",
    full_dataset   = full_dataset,
    indicator_lookup = indicator_lookup,
    ward_names     = ward_name,
    rbwm_code      = rbwm_code,
    shp_ward       = isolate(raw_shp_ward())
  )

  lapply(seq_len(nrow(indicator_lookup)), function(i) {
    this_short_name <- indicator_lookup$ShortIndicatorName[i]

    if (this_short_name == "LA population") {
      mod_population_server(
        id           = paste0("ind_", make.names(this_short_name)),
        full_dataset = full_dataset,
        ward_names   = ward_name,
        rbwm_code    = rbwm_code,
        eng_code     = eng_code,
        shp_ward     = isolate(raw_shp_ward())
      )
    } else if (this_short_name == "IMD") {
      mod_lsoa_indicator_server(
        id           = paste0("ind_", make.names(this_short_name)),
        full_dataset = full_dataset,
        short_name   = this_short_name,
        ward_names   = ward_name,
        shp_ward     = isolate(raw_shp_ward()),
        shp_lsoa     = isolate(raw_shp_lsoa())
      )
      mod_lsoa_ranking_server(
        id           = "ind_IMD_ranking",
        full_dataset = full_dataset,
        short_name   = this_short_name,
        shp_ward     = isolate(raw_shp_ward()),
        shp_lsoa     = isolate(raw_shp_lsoa())
      )
    } else if (this_short_name %in% c("QOF: Obesity", "% Caring responsibility", "Cervical screening (25-49 yrs)", "Cervical screening (50-64 yrs)", "Bowel cancer screening", "Breast cancer screening", "Bowel cancer screening uptake", "Breast cancer screening uptake", "DTaP vaccination coverage (8 months)", "MMR vaccination coverage: 1st dose (1.5 yrs)", "MMR/DTaP vaccination coverage (5 yrs)", "Shingles vaccination coverage (70-79 yrs)")) {
      mod_gp_indicator_server(
        id           = paste0("ind_", make.names(this_short_name)),
        full_dataset = full_dataset,
        short_name   = this_short_name,
        ward_names   = ward_name,
        rbwm_code    = rbwm_code,
        se_code      = se_code,
        eng_code     = eng_code,
        shp_ward     = isolate(raw_shp_ward()),
        shp_lsoa     = isolate(raw_shp_lsoa()),
        catchment    = isolate(raw_catchment()),
        value_type   = indicator_lookup$ValueType[i]
      )
    } else if (this_short_name %in% c("Alcohol-related admissions", "Hip fracture admissions (65+)")) {
      mod_msoa_indicator_server(
        id           = paste0("ind_", make.names(this_short_name)),
        full_dataset = full_dataset,
        short_name   = this_short_name,
        ward_names   = ward_name,
        rbwm_code    = rbwm_code,
        se_code      = se_code,
        eng_code     = eng_code,
        shp_msoa     = isolate(raw_shp_msoa()),
        shp_ward     = isolate(raw_shp_ward()),
        value_type   = indicator_lookup$ValueType[i]
      )
    } else if (this_short_name %in% c("ethnicity", "Main language")) {
      mod_grouped_indicator_server(
        id           = paste0("ind_", make.names(this_short_name)),
        full_dataset = full_dataset,
        short_name   = this_short_name,
        ward_names   = ward_name,
        rbwm_code    = rbwm_code
      )
    } else {
      mod_indicator_server(
        id                = paste0("ind_", make.names(this_short_name)),
        full_dataset      = full_dataset,
        short_name        = this_short_name,
        ward_names        = ward_name,
        rbwm_code         = rbwm_code,
        se_code           = se_code,
        eng_code          = eng_code,
        map_geography     = indicator_lookup$MapGeography[i],
        has_trend         = indicator_lookup$HasTrend[i],
        shp_ward          = isolate(raw_shp_ward()),
        shp_lsoa          = isolate(raw_shp_lsoa()),
        available_at_ward = indicator_lookup$AvailableAtWard[i],
        value_type        = indicator_lookup$ValueType[i]
      )
    }
  })

  qof_themes <- unique(indicator_lookup$Theme[grepl(": QOF (prevalence|incidence)", indicator_lookup$ShortIndicatorName)])
  lapply(qof_themes, function(theme_name) {
    mod_condition_group_server(
      id               = paste0("qof_group_", make.names(theme_name)),
      full_dataset     = full_dataset,
      condition_sources = qof_condition_sources,
      ward_names       = ward_name,
      shp_ward         = isolate(raw_shp_ward()),
      shp_lsoa         = isolate(raw_shp_lsoa()),
      catchment        = isolate(raw_catchment())
    )
  })
}

shinyApp(ui, server)
