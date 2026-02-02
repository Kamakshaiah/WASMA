# app.R

source("global.R")

# Load modules
source("modules/web_analytics.R")
source("modules/social_media.R")
source("modules/content_analytics.R")
source("modules/ml_models.R")

ui <- dashboardPage(
  dashboardHeader(title = "Web & Media Analytics (WASMA)"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Website Analytics", tabName = "web", icon = icon("chart-line")),
      menuItem("Social Media Analytics", tabName = "social", icon = icon("hashtag")),
      menuItem("Content Analytics", tabName = "content", icon = icon("file-alt")),
      menuItem("ML & Predictive Analytics", tabName = "ml", icon = icon("robot"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "web", webUI("web1")),
      tabItem(tabName = "social", socialUI("social1")),
      tabItem(tabName = "content", contentUI("content1")),
      tabItem(tabName = "ml", mlUI("ml1"))
    )
  )
)

server <- function(input, output, session) {
  webServer("web1")
  socialServer("social1")
  contentServer("content1")
  mlServer("ml1")
}

shinyApp(ui, server)
