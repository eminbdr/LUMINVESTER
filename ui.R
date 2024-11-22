source("info.R")

source("analysis.R")

ui <-{paste("dsasds")

  navbarPage(
    "LUMINVESTER",
    tabPanel(lang$an_pg, analysisUi),
    tabPanel(lang$in_pg, infoUI)
  )}