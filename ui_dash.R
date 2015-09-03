library(shinydashboard)
library(ggvis)

shinyUI(
  dashboardPage(
    #skin = "black",
    dashboardHeader(title = "MassReg"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Interaction", tabName = "interaction", icon = icon("area-chart")),
        menuItem("Report", tabName = "report", icon = icon("bar-chart")),
        menuItem("Methods", tabName = "methods", icon = icon("sitemap")),
        menuItem("About", tabName = "about", icon = icon("group")))
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "interaction",
          fluidRow(
            column(
              width = 3,
              selectInput('xcol', 'X Variable', names(ht)),
              selectInput('ycol', 'Y Variable', names(ht),
                          selected = names(ht)[[41]]),
              uiOutput("p_ui"),
              sliderInput("slid", label = "Smoothing",
                          value = 0.5, min = 0.2, max = 1)),
            column(
              width = 9,
              div(style = "height:400px", ggvisOutput("p"))
            )
          )
        ),
        tabItem(
          tabName = "report",
          h2("A")
        ),
        tabItem(
          tabName = "methods",
          h2("ah")
        ),
        tabItem(
          tabName = "about"
        )
      )
    )
  )
)
