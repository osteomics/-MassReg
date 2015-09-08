library(shiny)

shinyServer(function(input, output) {
  
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
    a[[as.numeric(input$ul)]]
  })
  
  output$ulBody <- renderPlot({
    suppressWarnings(chart.Correlation(R = corrS(), method = corrM()))
  })
})

