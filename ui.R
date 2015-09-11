library(shiny)
library(ggvis)
library(gplots)
library(PerformanceAnalytics)
source("helpers.R")


shinyUI(
  fluidPage(theme = "bootstrap.css",
            navbarPage(
              "MassREG",
              tabPanel(
                "Home",
                h4("Introduction"),
                p(style = "text-align:justify", "add text later")
              ),
              tabPanel(
                "Methodology",
                h4("Materials and Methods"),
                p(style = "text-align:justify", "The investigation focused on the 21st century collection of identified skeletons (CEI/XXI) housed at the Department of Life Sciences of the University of Coimbra (Ferreira et al., 2014). The fact that these skeletons are from individuals who died very recently is relevant to this research, since it avoided possible major biases in skeletal mass that may be related to post-depositional differential skeletal mass loss."),
                p(style = "text-align:justify", "A sample of 61 skeletons of both sexes was examined. It comprised 33 females with ages ranging from 38 to 96 years old (mean = 79.0; sd = 14.1) and 28 males with ages ranging from 31 to 95 years old (mean = 74.0; sd = 15.5). The sample comprised skeletons that were complete or almost complete. All skeletal elements were ideally present and well preserved but exceptions were taken into consideration for the following: teeth, hyoid, sternum, hand and foot phalanges. Due to the old age of the individuals composing the sample, skeletons were often edentulous or almost edentulous so the presence of teeth had to be overlooked. As for the remaining non-dental elements, they usually represent less than 3-4% of the overall skeletal weight (based on the results of Silva et al., 2009) so their absence expectantly did not interfere much with the results."),
                p(style = "text-align:justify", "To investigate the correlation of the weight of each bone with the overall skeleton weight, attention was given to certain bones that usually preserve better in very fragmented assemblages such as the ones composed of burned remains (Gonçalves, 2011). Others were selected because they are total body weight bearing bones and may thus be more significantly correlated with skeleton mass. The following isolated bones were weighted: the tarsal bones (body weight bearing bones); the patella; the MCs and MTs. The femur and humerus were also weighted, although they do not meet the two above mentioned criteria. This was merely carried out to allow for a comparison between both groups."),
                p(style = "text-align:justify", "Linear regression was carried out by using the R software. Regression analysis did not take age and sex into consideration because we assumed that total skeleton mass was directly reflected by the mass of each isolated bone regardless of those variables. In other words, the premise was that any significant effect that age, sex or any other variables may have in the mass of the skeleton has a similar effect on each of the isolated bones investigated in this paper. Therefore, the mass of isolated bones should always be significantly correlated with the total mass of the skeleton to which they belong to. The advantage of such an approach is that, if the regression equations significantly predict skeleton total mass, they can be applied to all cases involving human remains. This is a major benefit because the age at death and sex of the individuals is often unknown in both archaeological and forensic contexts.")
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
                      p(style = "text-align:justify", " As a pre-analysis procedure, significant bilateral asymmetries in each bone were investigated. This was carried out to determine if both sides of the skeleton had to be treated separately during statistical analysis. If differences from both sides were not substantial, then it would be possible to overcome the absence of a bone by replacing it by its antimere, if available. Bilateral asymmetry was not investigated through inferential statistics such as paired-samples tests or correlation tests because these either look at differences between the means or to the linear dependence of both samples. These tests do not give a real account of bilateral asymmetry in a case by case basis which was more important to assess in this investigation. Therefore, relative directional asymmetry (%DA), which has been often used previously (Steele and Mays, 1995; Mays, 2002; Auerbach and Ruff, 2006), was calculated for each individual instead."),
                      p(style = "text-align:justify", "The formula used was the following:"),
                      withMathJax("$$\\%DA=\\frac{M_R-M_L}{\\frac{M_R+M_L}{2}}*100$$"),
                      p("where every M is an individual bone mass, while L and R are left and right side, respectively")
                    ),
                    column(
                      width = 4, 
                      htmlOutput("tb")
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
              ),
              tabPanel(
                "Predict",
                sidebarLayout(
                  sidebarPanel(
                    selectInput('Xcol', 'X Variable', names(ht)),
                    selectInput('Ycol', 'Y Variable', names(ht),
                                selected = names(ht)[[41]]),
                    numericInput("num", label = "Your Numeric Input", value = 1),
                    hr(),
                    p("Current Value:", style = "color:#888888;"), 
                    verbatimTextOutput("num")
                  ),
                  mainPanel(
                    tableOutput("lmmod"),
                    p("Total skeleton mass, based on your input value is:"),
                    textOutput("pred"),
                    plotOutput("diag")
                  )
                )
              ),
              tabPanel("About",
                       h2("Authors"),
                       p(strong("David Gonçalves,"), "Maria Alejandra Acosta, Catarina Coelho, João Coelho, Francisco Curate, Maria Teresa Ferreira, Márcia Gouveia, Calil Makhoul, David Navega, Débora Pinto, Inês Santos, Ana Vassalo, Eugénia Cunha."),
                       em("Life Sciences Department, University of Coimbra"),
                       h2("Data Analysis & App Development"),
                       p("João Coelho"),
                       p("David Navega"),
                       em(a(href = "http://osteomics.com/", target = "_blank", "Osteomics")),
                       em("(Instituto Pedro Nunes, Portugal)"),
                       h2("References"),
                       p("add articles here.")
              )
            )
  )
)


