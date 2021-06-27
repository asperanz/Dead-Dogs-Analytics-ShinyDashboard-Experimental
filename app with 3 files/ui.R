library(shiny)
# library(plotly) # BE CAREFUL!! load plotly pkg before httr pkg
library(shinydashboard)
library(leaflet)
library(DT)
library(httr)

ui <- dashboardPage(
  dashboardHeader(
    title = "Dead Dogs Analytics"
  ),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(leafletOutput("concerts_map", height = 780)),
      box(DTOutput("tbl"))
      # box(plotOutput("rank", height = 370))
      # box(plotlyOutput("timeseries", height = 370))
    )
  )
  
)

# A little-known feature of Shiny is that the UI can be a function, not just
# objects. You can use this to dynamically render the UI based on the request.
# We're going to pass this uiFunc, not ui, to shinyApp(). If you're using
# ui.R/server.R style files, that's fine too--just make this function the last
# expression in your ui.R file.
uiFunc <- function(req) {
  if (!has_auth_code(parseQueryString(req$QUERY_STRING))) {
    url <- oauth2.0_authorize_url(api, app, scope = scope)
    redirect <- sprintf("location.replace(\"%s\");", url)
    tags$script(HTML(redirect))
  } else {
    ui
  }
}