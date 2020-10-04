rm(list = ls())

Sys.setenv(TZ = "Etc/GMT")

source("src/GUI/server.R")
source("src/GUI/ui.R")

app <- shinyApp(ui, server)