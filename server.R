library(shiny)

shinyServer(function(input, output) {
  
  ul.metrics <- readRDS("ulmetrics.RDS")
  ul.regrCoefficients <- readRDS("ulregr.RDS")
  assymTab <- readRDS("assymTab.RDS")
  
  plotData <- reactive({
    df <- ht[, c(input$xcol, input$ycol)]
    df <- df[complete.cases(df),]
    names(df) <- c("x", "y")
    df$id <- 1:nrow(df)
    df
  })
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    paste0(names(x), ": ", format(x), collapse = "<br />")
  }
  
  
  reactive({ plotData() %>%
      ggvis(x = ~x, y = ~y) %>%
      layer_points() %>%
      layer_smooths(span = input$slid, se = TRUE) %>%
      add_tooltip(all_values, "hover") %>%
      add_axis("y", title = " ")  %>%
      add_axis("x", title = " ") %>%
      set_options(width = "auto", height = "auto")
  }) %>% bind_shiny("p", "p_ui")
  
  output$heatmap <- renderPlot({
    ht.mat <- as.matrix(scale(ht))
    rownames(ht.mat) <- dt$ID
    heatmap.2(ht.mat, srtCol = 315, hclustfun = hclust, symm = FALSE,
              Colv = F, adjCol = c(0,1), scale = "column",
              lmat = rbind(c(0,3), c(2,1), c(0,4)), lwid = c(1,3),
              lhei = c(0.1,4,1))
  })
  
  corrM <- reactive(input$corrm)
  corrS <- reactive({
    lista[[as.numeric(input$ul)]]
  })
  
  output$tb <- renderTable(assymTab)
  
  output$ulBody <- renderPlot({
    suppressWarnings(chart.Correlation(R = corrS(), method = corrM()))
  })
  
  modData <- reactive({
    df <- ht[, c(input$Xcol, input$Ycol)]
    df <- df[complete.cases(df),]
    names(df) <- c("x", "y")
    df$id <- 1:nrow(df)
    df
  })
  
  model <- reactive({lm(y ~ x, data = modData())})
  
  output$lmmod <- renderTable({
    summary(model())$coefficients
  })
  
  output$num <- renderPrint({ input$num })
  
  output$pred <- renderPrint({as.numeric(
    predict(model(), data.frame(x = as.numeric(input$num))))
  })
  
  output$diag <- renderPlot({
    par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
    plot(model())
    })
  
  output$tab1 <- renderTable({ul.regrCoefficients})
  output$tab2 <- renderTable({ul.metrics})
  
})

