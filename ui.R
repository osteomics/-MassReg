library(shiny)
library(ggvis)
library(gplots)
library(PerformanceAnalytics)
source("helpers.R")

shinyUI(
  navbarPage(
    "MassREG",
    tabPanel(
      "Introduction",
      h4("In construction",
         h6("Add abstract or article's introduction here"))
    ),
    navbarMenu(
      "Exploring",
      tabPanel(
        "Scatterplot",
        sidebarLayout(
          sidebarPanel(
            selectInput('xcol', 'X Variable', names(ht)),
            selectInput('ycol', 'Y Variable', names(ht),
                        selected = names(ht)[[41]]),
            uiOutput("p_ui"),
            sliderInput("slid", label = "Smoothing",
                        value = 0.5, min = 0.2, max = 1)),
          mainPanel(
            div(style = "height:400px", ggvisOutput("p"))
          )
        )
      ),
      tabPanel("Heatmap",
               plotOutput("heatmap", height = "600px")
      ),
      tabPanel("Correlations 1",
               fluidRow(
                 column(width = 6,
                        radioButtons(
                          "corrm", "Correlation Method",
                          c("Pearson" = "pearson",
                            "Kendall" = "kendall",
                            "Spearman" = "spearman")
                        )
                 ),
                 column(width = 6,
                        radioButtons(
                          "ul", "Subset data",
                          c("Upper body (left side)" = "1",
                            "Upper body (right side)" = "2",
                            "Lower body (left side)" = "3",
                            "Lower body (right side)" = "4")
                        )
                 ),
                 plotOutput("ulBody")
               )
      ),
      tabPanel("Summary",
               verbatimTextOutput("summary")
      )
    )
  )
)
