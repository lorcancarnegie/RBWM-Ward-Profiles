# ---------------------------------------------------------------------------
# Indicator -> theme lookup for RBWM Ward Profiles Shiny app
# Author: Lorcan Carnegie
# Date created: 2026-10-19
# R version: R 2026.06.0
# ---------------------------------------------------------------------------

# ---- Indicator -> Theme lookup ----
# This table mirrors the section structure in data.R / create_ppt.Rmd.
# ShortIndicatorName must match dataset$ShortIndicatorName exactly, since
# that's what the app uses to filter the master dataset per indicator card.
#
# If you add/remove/rename an indicator in data.R, update this table to
# match (the "order" column controls display order within each theme tab).

indicator_lookup <- data.frame(
  # Restructured around the RBWM Joint Health and Wellbeing Strategy
  # 2026-2036: Demographics stays first as an overview (population context
  # underpinning all four life-course themes, not specific to one), then
  # the four life-course themes in the strategy's own order. See mapping
  # notes below each block for indicators that could plausibly sit in more
  # than one theme.
  Theme = c(
    rep("Cross-cutting themes", 7),
    rep("Best Start in Life", 18),
    rep("Living Well", 27),
    rep("Healthy Ageing", 12),
    rep("Healthy Place", 14)
  ),
  ShortIndicatorName = c(
    "LA population", "ethnicity", "Main language", "IMD",
    "Mental Health: QOF prevalence", "Depression: QOF prevalence", "Depression: QOF incidence - new diagnosis",

    "Reception: Prevalence of obesity", "Reception: Prevalence of excess weight",
    "Year 6: Prevalence of obesity", "Year 6: Prevalence of excess weight",
    "Children providing unpaid care", "Lone parent with dependent children",
    "Children in absolute low-income", "Working families - absolute low-income",
    "Lone parent - absolute low-income",
    "No qualifications", "Level 2 qualifications", "Level 4 or above qualifications",
    "Youth unemployment benefit claimants", "Access to primary schools", "Access to secondary school",
    "DTaP vaccination coverage (8 months)", "MMR vaccination coverage: 1st dose (1.5 yrs)",
    "MMR/DTaP vaccination coverage (5 yrs)",

    "QOF: Obesity", "Employment and Support Allowance",
    "Universal Credit - no work requirement",
    "Provides unpaid care", "% Caring responsibility", "Carers Allowance",
    "Disabled under Equality Act", "Not disabled under Equality Act - has LTC",
    "People with not good health", "Cannot speak English well or at all",
    "PIP claimants",
    "Cervical screening (25-49 yrs)", "Cervical screening (50-64 yrs)",
    "Unemployment benefit claimants", "Unemployment benefit claimants (50+ yrs)", "Universal Credit",
    "CHD: QOF prevalence", "Cancer: QOF prevalence", "Atrial fibrillation: QOF prevalence",
    "Asthma: QOF prevalence", "Diabetes: QOF prevalence", "COPD: QOF prevalence",
    "CKD: QOF prevalence", "Heart Failure: QOF prevalence", "Hypertension: QOF prevalence",
    "Smoking: QOF prevalence",
    "Alcohol-related admissions",

    "One-person household (aged 66+)", "People on pension credit (65+ yrs)",
    "Bowel cancer screening", "Breast cancer screening",
    "Bowel cancer screening uptake", "Breast cancer screening uptake",
    "Rheumatoid Arthritis: QOF prevalence", "Osteoporosis: QOF prevalence",
    "Dementia: QOF prevalence",
    "Stroke: QOF prevalence",
    "Hip fracture admissions (65+)",
    "Shingles vaccination coverage (70-79 yrs)",

    "Access to GP", "Access to hospital", "Access to greenspace",
    "Carbon footprint per person", "No cars or vans in household",
    "Mean average house price", "Owner occupied households",
    "Private rented households", "Social rented housing",
    "Overcrowded households", "Caravan or other temporary accommodation",
    "Fuel poverty", "No central heating", "Not connected to gas network"
  ),
  stringsAsFactors = FALSE
)

# preserve theme + within-theme order for display
indicator_lookup$Theme <- factor(indicator_lookup$Theme, levels = c(
  "Best Start in Life", "Living Well", "Healthy Ageing",
  "Healthy Place", "Cross-cutting themes"
))
indicator_lookup$order <- ave(seq_len(nrow(indicator_lookup)),
                               indicator_lookup$Theme, FUN = seq_along)

# DisplayName is what shows on the card header - ShortIndicatorName stays
# untouched since it must exactly match dataset$ShortIndicatorName for
# filtering. Defaults to ShortIndicatorName; override below to match the
# original PPTX wording where it differs.
indicator_lookup$DisplayName <- indicator_lookup$ShortIndicatorName
indicator_lookup$DisplayName[indicator_lookup$ShortIndicatorName == "LA population"] <- "Population"
indicator_lookup$DisplayName[indicator_lookup$ShortIndicatorName == "ethnicity"] <- "Ethnicity"
indicator_lookup$DisplayName[indicator_lookup$ShortIndicatorName == "Main language"] <- "Main language spoken"
indicator_lookup$DisplayName[indicator_lookup$ShortIndicatorName == "IMD"] <- "Deprivation"

# MapGeography records which shapefile (if any) create_ppt.Rmd used for this
# indicator's map, extracted directly from its shape_file= argument:
# "Ward" -> shp_ward, "LSOA" -> shp_lsoa, "None" -> no map in the original
# (or a GP point-map, not a choropleth - not yet supported here).
indicator_lookup$MapGeography <- "LSOA"  # most indicators default to LSOA
indicator_lookup$MapGeography[indicator_lookup$ShortIndicatorName %in% c(
  "LA population", "ethnicity", "Main language",
  "Carbon footprint per person", "Mean average house price",
  "QOF: Obesity", "% Caring responsibility",
  "Cervical screening (25-49 yrs)", "Cervical screening (50-64 yrs)",
  "Bowel cancer screening", "Breast cancer screening",
  "DTaP vaccination coverage (8 months)", "MMR vaccination coverage: 1st dose (1.5 yrs)",
  "MMR/DTaP vaccination coverage (5 yrs)", "Shingles vaccination coverage (70-79 yrs)"
)] <- "None"
indicator_lookup$MapGeography[indicator_lookup$ShortIndicatorName %in% c(
  "Alcohol-related admissions",
  "Hip fracture admissions (65+)"
)] <- "MSOA"
indicator_lookup$MapGeography[indicator_lookup$ShortIndicatorName %in% c(
  "Reception: Prevalence of obesity", "Reception: Prevalence of excess weight",
  "Year 6: Prevalence of obesity", "Year 6: Prevalence of excess weight",
  "Youth unemployment benefit claimants", "Unemployment benefit claimants (50+ yrs)",
  "Children providing unpaid care"
)] <- "Ward"

# AvailableAtWard: FALSE for the two GP-practice-level indicators (QOF
# Obesity, % Caring responsibility). The original PPTX explicitly shows
# "Data unavailable for wards" for these - they're recorded per GP practice,
# not per ward, so the standard ward bar chart/map template doesn't apply.
indicator_lookup$AvailableAtWard <- TRUE
indicator_lookup$AvailableAtWard[indicator_lookup$ShortIndicatorName %in% c(
  "QOF: Obesity", "% Caring responsibility",
  "Cervical screening (25-49 yrs)", "Cervical screening (50-64 yrs)",
  "Bowel cancer screening", "Breast cancer screening",
  "DTaP vaccination coverage (8 months)", "MMR vaccination coverage: 1st dose (1.5 yrs)",
  "MMR/DTaP vaccination coverage (5 yrs)", "Shingles vaccination coverage (70-79 yrs)",
  "Alcohol-related admissions",
  "Hip fracture admissions (65+)"
)] <- FALSE

# match the original PPTX's title wording for QOF Obesity
indicator_lookup$DisplayName[indicator_lookup$ShortIndicatorName == "QOF: Obesity"] <- "Obesity: GP recorded prevalence"
indicator_lookup$DisplayName[indicator_lookup$ShortIndicatorName == "Bowel cancer screening"] <- "Bowel cancer screening (60 to 74 yrs)"
indicator_lookup$DisplayName[indicator_lookup$ShortIndicatorName == "Breast cancer screening"] <- "Breast cancer screening (53 to 70 yrs)"

# ---- Per-indicator metadata: value type, benchmarking method, source ----
# Extracted directly from create_ppt.Rmd's title/value_type/benchmarking_method/
# source variables for each indicator chunk, not guessed. Population and IMD
# aren't RAG/BOB-compared in the original either, so their entries are built
# from data.R's own source instead (no metadata chunk to extract from).
metadata <- data.frame(
  ShortIndicatorName = c(
    "LA population", "ethnicity", "Main language", "IMD",
    "Reception: Prevalence of obesity", "Reception: Prevalence of excess weight",
    "Year 6: Prevalence of obesity", "Year 6: Prevalence of excess weight",
    "QOF: Obesity", "Unemployment benefit claimants",
    "Youth unemployment benefit claimants", "Unemployment benefit claimants (50+ yrs)",
    "Employment and Support Allowance", "Universal Credit - no work requirement",
    "One-person household (aged 66+)", "Lone parent with dependent children",
    "% Caring responsibility", "Provides unpaid care", "Children providing unpaid care",
    "People with not good health", "Disabled under Equality Act",
    "Not disabled under Equality Act - has LTC", "Cannot speak English well or at all",
    "No cars or vans in household", "Access to primary schools",
    "Access to secondary school", "Access to GP", "Access to hospital",
    "Carbon footprint per person", "Access to greenspace", "Mean average house price",
    "Owner occupied households", "Private rented households", "Social rented housing",
    "Overcrowded households", "Caravan or other temporary accommodation", "Fuel poverty",
    "No central heating", "Not connected to gas network", "Universal Credit",
    "Children in absolute low-income", "Working families - absolute low-income",
    "Lone parent - absolute low-income", "PIP claimants", "Carers Allowance",
    "People on pension credit (65+ yrs)", "No qualifications", "Level 2 qualifications",
    "Level 4 or above qualifications",
    "Cervical screening (25-49 yrs)", "Cervical screening (50-64 yrs)",
    "Bowel cancer screening", "Breast cancer screening",
    "Mental Health: QOF prevalence", "Depression: QOF prevalence", "Depression: QOF incidence - new diagnosis",
    "CHD: QOF prevalence", "Cancer: QOF prevalence", "Atrial fibrillation: QOF prevalence",
    "Asthma: QOF prevalence", "Diabetes: QOF prevalence", "COPD: QOF prevalence",
    "CKD: QOF prevalence", "Heart Failure: QOF prevalence", "Hypertension: QOF prevalence",
    "Bowel cancer screening uptake", "Breast cancer screening uptake",
    "Rheumatoid Arthritis: QOF prevalence", "Osteoporosis: QOF prevalence",
    "Dementia: QOF prevalence",
    "Stroke: QOF prevalence",
    "Smoking: QOF prevalence",
    "DTaP vaccination coverage (8 months)", "MMR vaccination coverage: 1st dose (1.5 yrs)",
    "MMR/DTaP vaccination coverage (5 yrs)", "Shingles vaccination coverage (70-79 yrs)",
    "Alcohol-related admissions",
    "Hip fracture admissions (65+)"
  ),
  ValueType = c(
    "Count", "Proportion (%)", "Proportion (%)", "Decile (1 = most deprived, 10 = least)",
    "Proportion (%)", "Proportion (%)", "Proportion (%)", "Proportion (%)",
    "Proportion (%)", "Crude rate per 1,000", "Crude rate per 1,000", "Crude rate per 1,000",
    "Crude rate per 1,000", "Crude rate per 1,000", "Proportion (%) - Households", "Proportion (%) - Households",
    "Proportion (%)", "Proportion (%)", "Proportion (%)", "Proportion (%)",
    "Proportion (%)", "Proportion (%)", "Proportion (%)", "Proportion (%) - Households",
    "Proportion (%)", "Proportion (%)", "Proportion (%)", "Proportion (%)",
    "kg per person", "Proportion (%) - Addresses", "Mean average (\u00A3 - thousands)", "Proportion (%) - Households",
    "Proportion (%) - Households", "Proportion (%) - Households", "Proportion (%) - Households", "Proportion (%) - Households",
    "Proportion (%) - Households", "Proportion (%) - Households", "Proportion (%) - Properties", "Crude rate per 1,000",
    "Crude rate per 1,000", "Crude rate per 1,000", "Crude rate per 1,000", "Crude rate per 1,000",
    "Crude rate per 1,000", "Crude rate per 1,000", "Proportion (%)", "Proportion (%)",
    "Proportion (%)", "Proportion (%)", "Proportion (%)", "Proportion (%)",
    "Proportion (%)", "Proportion (%)", "Proportion (%)", "Proportion (%)",
    "Proportion (%)", "Proportion (%)", "Proportion (%)", "Proportion (%)",
    "Proportion (%)", "Proportion (%)", "Proportion (%)", "Proportion (%)",
    "Proportion (%)", "Proportion (%)", "Proportion (%)", "Proportion (%)",
    "Proportion (%)", "Proportion (%)", "Proportion (%)", "Proportion (%)",
    "Proportion (%)", "Proportion (%)", "Proportion (%)", "Proportion (%)",
    "Crude rate per 100,000", "Crude rate per 100,000"
  ),
  OriginalGeography = c(
    NA, NA, NA, "LSOA",
    "Ward", "Ward", "Ward", "Ward",
    "GP", "Output Area", "Output Area", "Output Area",
    "Output Area", "Postcode", "Household", "Household",
    "GP", "Output Area", "Output Area", "Output Area",
    "Output Area", "Output Area", "Household", "Household",
    "LSOA", "LSOA", "LSOA", "LSOA",
    "LSOA", "LSOA", "Address", "Household",
    "Household", "Household", "Household", "Household",
    "LSOA", "LSOA", "Domestic properties", "Postcode",
    "Output Area", "Output Area", "Output Area", "Output Area",
    "Output Area", "Output Area", "Output Area", "Output Area",
    "Output Area", "GP", "GP", "GP",
    "GP", "GP", "GP", "GP",
    "GP", "GP", "GP", "GP",
    "GP", "GP", "GP", "GP",
    "GP", "GP", "GP", "GP",
    "GP", "GP", "GP", "GP",
    "GP", "GP", "GP", "GP",
    "MSOA", "MSOA"
  ),
  BenchmarkingMethod = c(
    "Not compared", "Not compared", "Not compared", "Not compared",
    "Wilson Score CI method (95%)", "Wilson Score CI method (95%)", "Wilson Score CI method (95%)", "Wilson Score CI method (95%)",
    "Wilson Score method (95%)", "Byar's method (%)", "Byar's method (%)", "Byar's method (%)",
    "Byar's method (%)", "Byar's method (%)", "Wilson Score method (95%)", "Wilson Score method (95%)",
    "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)",
    "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)",
    "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)",
    "Quintiles created using sample quantiles, where quintile 1 is the worst performing", "Wilson Score method (95%)", "Quintiles created using sample quantiles, where quintile 1 is the worst performing", "Wilson Score method (95%)",
    "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)",
    "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)", "Byar's method (%)",
    "Byar's method (%)", "Byar's method (%)", "Byar's method (%)", "Byar's method (%)",
    "Byar's method (%)", "Byar's method (%)", "Wilson Score method (95%)", "Wilson Score method (95%)",
    "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)",
    "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)",
    "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)",
    "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)",
    "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)",
    "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)",
    "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)", "Wilson Score method (95%)",
    "Byar's method (%)", "Byar's method (%)"
  ),
  Source = c(
    "Office for National Statistics. Population estimates mid-2024", "Nomis. 2021 Census", "Nomis. 2021 Census", "Ministry of Housing, Communities and Local Government. English Indices of Deprivation 2025",
    "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 93105", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 93106", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 93107", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 93108",
    "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 94136", "Nomis. Claimant count by sex and age", "Nomis. Claimant count by sex and age", "Nomis. Claimant count by sex and age",
    "Department for Work and Pensions. Stat-Xplore. Employment and Support Allowance", "Department for Work and Pensions. Stat-Xplore. People on Universal Credit", "Office for National Statistics. Census 2021. Household composition", "Office for National Statistics. Census 2021. Household composition",
    "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 352", "Office for National Statistics. Census 2021. Provision of unpaid care by age", "Office for National Statistics. Census 2021. Provision of unpaid care by age", "Nomis. Census 2021. General health",
    "Nomis. Census 2021. TS038 - Disability", "Nomis. Census 2021. TS038 - Disability", "Office for National Statistics. Census 2021. Proficiency in English language", "Office for National Statistics. Census 2021. Car or van availability",
    "Department for Transport. Journey time statistics", "Department for Transport. Journey time statistics", "Department for Transport. Journey time statistics", "Department for Transport. Journey time statistics",
    "Carbon and Place. Place-Based Carbon Calculator", "Office for National Statistics. Access to gardens and public green space in Great Britain", "HM Land Registry. Price Paid Data", "Nomis. Census 2021. TS054 - Tenure",
    "Nomis. Census 2021. TS054 - Tenure", "Nomis. Census 2021. TS054 - Tenure", "Nomis. Census 2021. TS053 - Occupancy rating for rooms", "Nomis. Census 2021. TS044 - Accommodation type",
    "Department for Energy Security and Net Zero. Sub-regional fuel poverty data", "Nomis. Census 2021. TS046 - Central heating", "Department for Energy Security and Net Zero. Subnational gas consumption data", "Department for Work and Pensions. Stat-Xplore. People on Universal Credit",
    "Department for Work and Pensions. Stat-Xplore. Children in Low Income Families", "Department for Work and Pensions. Stat-Xplore. Children in Low Income Families", "Department for Work and Pensions. Stat-Xplore. Children in Low Income Families", "Department for Work and Pensions. Stat-Xplore. Personal Independence Payment: Cases with Entitlement from 2019",
    "Department for Work and Pensions. Stat-Xplore. Carer's Allowance: Cases in Payment - Data from May 2018", "Department for Work and Pensions. Stat-Xplore. Pension Credit", "Office for National Statistics. Census 2021. TS067 - Highest level of qualification", "Office for National Statistics. Census 2021. TS067 - Highest level of qualification",
    "Office for National Statistics. Census 2021. TS067 - Highest level of qualification", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 93725", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 93726", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 92600",
    "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 94063", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 90581", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 848", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 90646",
    "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 273", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 276", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 280", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 90933",
    "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 241", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 253", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 258", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 262",
    "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 219", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 92601", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 91340", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 91269",
    "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 90443", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 247", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 212", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 91280",
    "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 93924", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 93925", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 93926", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 93927",
    "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 93465", "Office for Health Improvement and Disparities. Fingertips. Indicator ID: 93241"
  ),
  KnownCaveats = c(
    NA, NA, NA, NA,
    "Data for local authorities may not match that published by NHS England which are based on the local authority of the school attended by the child or based on the local authority that submitted the data.", "Data for local authorities may not match that published by NHS England which are based on the local authority of the school attended by the child or based on the local authority that submitted the data.", "Data for local authorities may not match that published by NHS England which are based on the local authority of the school attended by the child or based on the local authority that submitted the data.", "Data for local authorities may not match that published by NHS England which are based on the local authority of the school attended by the child or based on the local authority that submitted the data.",
    "Local authority values are calculated by assigning all patients of the GP practice to the local authority where the GP practice is located.", NA, NA, NA,
    NA, NA, NA, NA,
    NA, "As Census 2021 was during a unique period of rapid change, take care when using this data for planning purposes.", "As Census 2021 was during a unique period of rapid change, take care when using this data for planning purposes.", "As Census 2021 was during a unique period of rapid change, take care when using this data for planning purposes.",
    "As Census 2021 was during a unique period of rapid change, take care when using this data for planning purposes.", "As Census 2021 was during a unique period of rapid change, take care when using this data for planning purposes.", NA, NA,
    NA, NA, NA, NA,
    "Current data is experimental and should be treated with care as it may contain errors.", "Public green space access indicator is restricted to built up areas postcodes only.", "Data showing has been aggregated from postcode where data has been dropped when postcode is missing.", NA,
    NA, NA, NA, NA,
    NA, NA, "Ward-level data aggregated from LSOAs. Some LSOA values are suppressed, so ward estimates may be higher than actual estimate.", NA,
    NA, NA, NA, NA,
    NA, NA, "As Census 2021 was during a unique period of rapid change, take care when using this data for planning purposes.", "As Census 2021 was during a unique period of rapid change, take care when using this data for planning purposes.",
    "As Census 2021 was during a unique period of rapid change, take care when using this data for planning purposes.", NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    "Local authority values are calculated by assigning all patients of the GP practice to the local authority where the GP practice is located.", "Local authority values are calculated by assigning all patients of the GP practice to the local authority where the GP practice is located.", "Local authority values are calculated by assigning all patients of the GP practice to the local authority where the GP practice is located.", "Local authority values are calculated by assigning all patients of the GP practice to the local authority where the GP practice is located.",
    "Shown at MSOA level, not aggregated to ward - OHID's small-area rates aren't built to be re-aggregated with a simple population weighting.", "Shown at MSOA level, not aggregated to ward - OHID's small-area rates aren't built to be re-aggregated with a simple population weighting."
  ),
  AdditionalNotes = c(
    NA, NA, NA, "IDACI = Income Deprivation Affecting Children Index. IDAOPI = Income Deprivation Affecting Older People Index.",
    "Due to the impact of COVID-19 on the NCMP data collection, data was not published for the 2018/19 - 20/21 time period.", "Due to the impact of COVID-19 on the NCMP data collection, data was not published for the 2018/19 - 20/21 time period.", "Due to the impact of COVID-19 on the NCMP data collection, data was not published for the 2018/19 - 20/21 time period.", "Due to the impact of COVID-19 on the NCMP data collection, data was not published for the 2018/19 - 20/21 time period.",
    "Data comes from the Quality and Outcomes Framework (QOF) which is a voluntary incentive scheme for GP practices in England, providing rewards for achieving quality indicators.", "People claiming Jobseeker's Allowance plus those who claim Universal Credit who are out of work as a rate of the population aged 16-64, sourced from the mid-year population estimates.", "People aged 16-24 claiming Jobseeker's Allowance plus those who claim Universal Credit who are out of work as a rate of the population aged 16-24, sourced from the mid-year population estimates.", "People aged 50+ claiming Jobseeker's Allowance plus those who claim Universal Credit who are out of work as a rate of the population aged 50+, sourced from the mid-year population estimates.",
    "ESA is a workless benefit payable to people who are out of work and have been assessed as being incapable of work due to illness or disability and who meet the appropriate contribution conditions.", "People on Universal Credit that are not expected to work at present.", NA, NA,
    "All results are based on weighted data. Weighting changes the data to account for differences between all patients at a practice and the sub-set of patients who actually take part in the survey.", "Based on self reported responses to the 2021 Census. An unpaid carer may look after, give help or support to anyone who has long-term physical or mental ill-health conditions, illness or problems related to old age. This does not include any activities as part of paid employment.", "Based on self reported responses to the 2021 Census. An unpaid carer may look after, give help or support to anyone who has long-term physical or mental ill-health conditions, illness or problems related to old age. This does not include any activities as part of paid employment.", "This dataset provides Census 2021 estimates that classify usual residents in England and Wales by the state of their general health.",
    "People who assessed their day-to-day activities as limited by long-term physical or mental health conditions or illnesses are considered disabled.", "People who assessed their day-to-day activities not limited and are therefore not disabled under the Equality Act but have a long-term physical or mental health conditions or illnesses.", NA, NA,
    "Service users able to access a primary school within 15 minutes by public transport or walking. Ward-level aggregation was calculated by applying percentages to denominators to estimate numerators, then aggregating to ward level. Only educational establishments with the following establishment types were included: community school, voluntary aided school, voluntary controlled school, foundation school, further education, academy sponsor led, sixth form centres, academy converter, free schools, free schools 16 to 19, university technical college, studio schools, academy 16 to 19 converter and academy 16 to 19 sponsor led.", "Service users able to access a secondary school within 15 minutes by public transport or walking. Ward-level aggregation was calculated by applying percentages to denominators to estimate numerators, then aggregating to ward level. Only educational establishments with the following establishment types were included: community school, voluntary aided school, voluntary controlled school, foundation school, further education, academy sponsor led, sixth form centres, academy converter, free schools, free schools 16 to 19, university technical college, studio schools, academy 16 to 19 converter and academy 16 to 19 sponsor led.", "Households able to access a GP practice within 15 minutes by public transport or walking. Ward-level aggregation was calculated by applying percentages to denominators to estimate numerators, then aggregating to ward level.", "Households able to access a hospital within 30 minutes by public transport or walking. Ward-level aggregation was calculated by applying percentages to denominators to estimate numerators, then aggregating to ward level.",
    "Data has been recreated using Carbon and Place's total emissions for each LSOA divided by ONS mid-year population estimates. This data is then built up using best-fit lookups to ward and local authority geographies.", NA, "Mean average price of all properties listed with a postcode over the given year. HM Land Registry Price Paid Data where category type is A = Standard Price Paid entry, includes single residential property sold for value only and property type is detached, semi-detached, terraced or flats/maisonettes, excluding other category.", "Owner-occupied accommodation can be owned outright, with a mortgage/loan or part-owned on a shared ownership scheme.",
    "Private rented households, for example, rented through a private landlord or letting agent.", "Social rented housing through a local council or housing association.", "Occupancy rating of -1 or less: implies that a household's accommodation has fewer rooms than required (overcrowded).", "A temporary or mobile structure, such as a caravan, used or available by an individual or household as accommodation.",
    NA, NA, NA, "Rates are created using ONS mid-year population estimates.",
    "Rates are created using ONS mid-year population estimates.", "Children in absolute low-income families (CiALIF) where the \"in-work\" status is defined as a family that have an accumulated period of at least 26 weeks paid employment or self-employment within the 52-week tax year. Rates are created using ONS mid-year population estimates.", "Children in absolute low-income families (CiALIF) where family type defined as a single adult with dependent children. Rates are created using ONS mid-year population estimates.", "Personal Independence Payment (PIP) helps with some of the extra costs caused by long-term ill-health or a disability if you're aged 16 to 64. Rates are created using ONS mid-year population estimates.",
    "Carer's Allowance is a non-contributory benefit for people who look after a severely disabled person for at least 35 hours a week, who are not gainfully employed (i.e. not earning more than £95 per week after certain deductions) and who are not in full-time education.", "Pension Credit is a benefit to help people over State Pension age and in low income with their living costs.", "Those who do not hold any qualifications equivalent to level 1 or entry level, level 2, level 3, level 4 or above, or other recognised qualifications.", "Those who hold a level 2 qualification as their highest qualification.",
    "Those who hold a level 4 or above qualification.", NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    "VI001. Babies who received at least 3 doses of a DTaP vaccine before the age of 8 months.", "VI002. Children who received at least 1 dose of MMR vaccine between the ages of 1 and 1.5 yrs.", "VI003. Children, aged 5, who received a reinforcing dose of DTaP/IPV and at least 2 doses of an MMR vaccine between the ages of 1 and 5 yrs.", "VI004. Patients, aged 80, who received a shingles vaccine between the ages of 70 and 79 yrs.",
    "Admission episodes for alcohol-related conditions, all primary and secondary diagnoses.", "Emergency hospital admissions for hip fracture, aged 65 and over."
  ),
  stringsAsFactors = FALSE
)
indicator_lookup <- merge(indicator_lookup, metadata, by = "ShortIndicatorName", all.x = TRUE)
indicator_lookup$Theme <- factor(indicator_lookup$Theme, levels = levels(indicator_lookup$Theme))
indicator_lookup <- indicator_lookup[order(indicator_lookup$Theme, indicator_lookup$order), ]

# HasTrend: TRUE for the 19 indicators that had a trend plot (RBWM vs this
# ward, over time) in the original PPTX via plot_trend_rag()/plot_trend_quint()
indicator_lookup$HasTrend <- FALSE
indicator_lookup$HasTrend[indicator_lookup$ShortIndicatorName %in% c(
  "Reception: Prevalence of obesity", "Reception: Prevalence of excess weight",
  "Year 6: Prevalence of obesity", "Year 6: Prevalence of excess weight",
  "Unemployment benefit claimants", "Youth unemployment benefit claimants",
  "Unemployment benefit claimants (50+ yrs)", "Employment and Support Allowance",
  "Universal Credit - no work requirement", "Carbon footprint per person",
  "Mean average house price", "Not connected to gas network", "Universal Credit",
  "Children in absolute low-income", "Working families - absolute low-income",
  "Lone parent - absolute low-income", "PIP claimants", "Carers Allowance",
  "People on pension credit (65+ yrs)"
)] <- TRUE
