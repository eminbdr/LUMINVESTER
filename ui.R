source("info.R")

source("analysis.R")

ui <-{
  navbarPage(
    "LUMINVESTER",
    tabPanel(lang$an_pg, analysisUi),
    tabPanel(lang$in_pg, infoUI)
  )}