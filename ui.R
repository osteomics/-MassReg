library(shiny)
library(ggvis)
library(gplots)
library(PerformanceAnalytics)
library(knitr)
source("helpers.R")


shinyUI(
  fluidPage(
    theme = "bootstrap.css",
    navbarPage(
      "MassREG",
      tabPanel(
        "Home",
        fluidRow(
          tabsetPanel(
            "abstract",
            type = "pills",
            tabPanel("Objectives",
                     p(style = "text-align:justify",
                       "Complete skeletal inventory is not always possible in cases",
                       "involving severe fragmentation. In such cases, skeletal mass",
                       "comparisons with published mass skeletal references may be",
                       "used as an alternative to assess skeletal completeness but",
                       "they are too general for a case by case routine analysis.",
                       "Therefore, linear regression equations that allow estimating",
                       "the total skeleton mass based on the mass of some of its",
                       "isolated bones were created.")
            ),
            tabPanel("Methodology",
                     p(style = "text-align:justify",
                       "Total mass and the mass of the clavicle, humerus, femur,",
                       "patella, carpal, metacarpal, tarsal and metatarsal bones",
                       "were recorded in a sample of 60 rather complete skeletons",
                       "from the 21st century identified skeletal collection housed",
                       "at the University of Coimbra (Ferreira et al., 2014). The",
                       "latter included 32 females and 28 males with ages ranging",
                       "from 31 to 96 years old (mean = 76.4; sd = 14.8). The",
                       "linear regression analyses were carried out with the R software.")
            ),
            tabPanel("Results",
                     p(style = "text-align:justify",
                       "The mass of isolated bones can be used",
                       "to predict total skeleton mass. The femur,",
                       "the humerus and the second metacarpal were",
                       "the best predictors of total skeletal mass",
                       "with root mean squared errors ranging from",
                       "292.9 to 346.1 g.")
            ),
            tabPanel("Discussion",
                     p(style = "text-align:justify",
                       "Although relatively successful, the",
                       "non-normal distribution of the sample",
                       "may have reduced the prediction power",
                       "of the equations. Mass regression may",
                       "be more adequately applied to recent",
                       "forensic remains than to archaeological",
                       "remains because the latter are usually",
                       "more affected by post-depositional mass",
                       "loss. The impact for bioanthropology is",
                       "major since this method may provide",
                       "inferences about the completeness of",
                       "the skeleton, the minimum number of",
                       "individuals and sex.")
            )
          )
        ),
        fluidRow(
          p(strong("Keywords:"), "Biological anthropology; forensic anthropology; skeleton mass; scattered remains; funerary practice")
        )
      ),
      tabPanel(
        "Explore",
        tabsetPanel(
          "exploration",
          type = "pills",
          tabPanel("Heatmap", h4("Data exploration through heatmap"),
                   p(style = "text-align:justify",
                     "All variables are in columns representing type of bone,",
                     "ex. CLV.L = clavicle (left side). All the individuals",
                     " of the sample are represented as rows and organized",
                     "by clusters of mass values. Blank (white) spaces are non",
                     "available values that for some reason were not measured."),
                   plotOutput("heatmap", height = "600px")
          ),
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
          tabPanel("Correlations",
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
          tabPanel(
            "Asymmetry",
            fluidRow(
              column(
                width = 6,
                p(style = "text-align:justify",
                  "As a pre-analysis procedure, significant bilateral asymmetries",
                  "in each bone were investigated. This was carried out to determine",
                  "if both sides of the skeleton had to be treated separately during", 
                  "statistical analysis. If differences from both sides were not",
                  "substantial, then it would be possible to overcome the absence",
                  "of a bone by replacing it by its antimere, if available. Bilateral",
                  "asymmetry was investigated through a Wilcoxon signed ranks test.",
                  "Also, relative directional asymmetry (%DA), which has been often",
                  "used previously (Auerbach and Ruff, 2006), was calculated for",
                  "each individual so that a better notion of asymmetry in a case",
                  "by case basis could be attained."),
                p(style = "text-align:justify",
                  "The formula used was the following:"),
                withMathJax("$$\\%DA=\\frac{M_R-M_L}{\\frac{M_R+M_L}{2}}*100$$"),
                p("where every M is an individual bone mass, while L and R are left and right side, respectively")
              ),
              column(
                width = 4, 
                htmlOutput("tb")
              )
            )
          )
        )
      ),
      tabPanel(
        "Predict",
        tabsetPanel(
          "linreg",
          type = "pills",
          tabPanel(
            "Regression",
            sidebarLayout(
              sidebarPanel(
                selectInput('Xcol', 'X Variable', names(ht)),
                selectInput('Ycol', 'Y Variable', names(ht),
                            selected = names(ht)[[41]]),
                numericInput("num", label = "Your Numeric Input", value = 10),
                hr(),
                p("Current Value:", style = "color:#888888;"), 
                verbatimTextOutput("num")
              ),
              mainPanel(
                tableOutput("lmmod"),
                p("Total skeleton mass, based on your input value is:"),
                tableOutput("pred"),
                plotOutput("diag")
              )
            )
          ),
          tabPanel(
            "Metrics",
            fluidRow(
              column(
                width = 6,
                tableOutput("tab1")
              ),
              column(
                width = 6,
                tableOutput("tab2")
              )
            )
          )
        )
      ),
      tabPanel("About",
               h2("Authors"),
               p("David Gonçalves,", "Maria Alejandra Acosta, Catarina Coelho, João Coelho, Francisco Curate, Maria Teresa Ferreira, Márcia Gouveia, Calil Makhoul, David Navega, Débora Pinto, Inês Santos, Ana Vassalo, Eugénia Cunha."),
               em("Life Sciences Department, University of Coimbra"),
               h2("Data Analysis & App Development"),
               p("João Coelho"),
               p("David Navega"),
               em(a(href = "http://osteomics.com/", target = "_blank", "Osteomics")),
               em("(Instituto Pedro Nunes, Portugal)"),
               h2("References"),
               p(style = "text-align:justify",
                 "Auerbach BM, and Ruff CB. 2006. Limb bone bilateral",
                 "asymmetry: variability and commonality among modern",
                 "humans.", em("Journal of Human Evolution"), "50:203-218."),
               p(style = "text-align:justify",
                 "Ferreira MT, Vicente R, Navega D, Gonçalves D,",
                 "Curate F, and Cunha E. 2014. A new forensic",
                 "collection housed at the University of Coimbra,",
                 "Portugal: The 21st century identified skeletal",
                 "collection.", em("Forensic Science International"),
                 "245:202.e201-202.e205.")
      )
    )
  )
)




