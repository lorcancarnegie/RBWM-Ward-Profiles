# ---------------------------------------------------------------------------
# Ward lookup for RBWM Ward Profiles Shiny app
# Author: Lorcan Carnegie
# Date created: 2026-10-19
# R version: R 2026.06.0
# ---------------------------------------------------------------------------

# read in lookup file - kept as a short, top-level copy in the deployment
# folder itself (not the original nested Geography/ path), since paths over
# 100 bytes break shinyapps.io's tar bundling on deploy
x <- read.csv("ward_to_lad_lookup.csv")

# rename columns to standardise
colnames(x)[grepl("^WD.*CD$", colnames(x))] <- "AreaCode"
colnames(x)[grepl("^WD.*NM$", colnames(x))] <- "AreaName"
colnames(x)[grepl("^LAD.*CD$", colnames(x))] <- "LADCode"
colnames(x)[grepl("^LAD.*NM$", colnames(x))] <- "LADName"

# only keep columns we need
x <- x[, c("AreaCode", "AreaName", "LADCode", "LADName")]

# index for RBWM wards
x <- x[x$LADCode == "E06000040", ]
x <- x[, c("AreaCode", "AreaName")]

wards <- x

# ward names to read into Rmd params
ward_name <- sort(x$AreaName)
