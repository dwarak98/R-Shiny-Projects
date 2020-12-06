setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("frontEnd.R")
source("backEnd.R")

shinyApp(ui, server)
