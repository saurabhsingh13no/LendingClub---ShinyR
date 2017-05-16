library(shiny)

ui <-fluidPage(
  navbarPage(img(src="http://www.quantuniversity.com/img/logo5.PNG", height = 50, width = 80),
             tabPanel("Dashboard"),
             tabPanel("Data"),
             tabPanel("Stats"),
             navbarMenu("More",
                        tabPanel("Sub-Component A"),
                        tabPanel("Sub-Component B"))
  ),
  titlePanel("Lending Club Dashboard"),
  
  
  sidebarLayout(
    sidebarPanel(
      selectInput('xcol', 'X Variable', names(loan)),
      selectInput('ycol', 'Y Variable', names(loan),
                  selected=names(loan)[[2]]),
      numericInput('clusters', 'Cluster count', 3,
                   min = 1, max = 9),
      sliderInput(inputId = "bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Clustering", plotOutput("plot2")), 
        tabPanel("Histogram", plotOutput("summary2")), 
        tabPanel("ggplot", plotOutput("ggplot_v2")), 
        tabPanel("Table", tableOutput("table2"))
      )
    )
  )
)


server <- function(input, output) {
  selectedData <- reactive({
    loan[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot2 <- renderPlot({
    library(readr)
    loan <- read_csv("loan3.csv")
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  output$summary2 <- renderPlot({
    library(readr)
    loan <- read_csv("loan3.csv")
    x    <- loan$loan_amnt  # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    title<-"Histogram for Loan Amount"
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$ggplot_v2 <- renderPlot({
    library(readr)
    loan <- read_csv("loan3.csv")
    us <- map_data("state")
    
    arr <- loan %>% 
      add_rownames("region") %>% 
      mutate(region=tolower(loan$addr_state_v2))
    
    gg <- ggplot()
    gg <- gg + geom_map(data=us, map=us,
                        aes(x=long, y=lat, map_id=region),
                        fill="#ffffff", color="#ffffff", size=0.15,
                        ) + ggtitle("Loan distribution across US states")
    gg <- gg + geom_map(data=arr, map=us,
                        aes(fill=loan$loan_amnt, map_id=region),
                        color="#ffffff", size=0.15)
    gg <- gg + scale_fill_continuous(low='thistle2', high='#ffffff', 
                                     guide='colorbar')
    gg <- gg + labs(x=NULL, y=NULL)
    gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
    gg <- gg + theme(panel.border = element_blank())
    gg <- gg + theme(panel.background = element_blank())
    gg <- gg + theme(axis.ticks = element_blank())
    gg <- gg + theme(axis.text = element_blank())
    gg
    
    
  })
  
  output$table2 <-renderTable(head(loan))
  
  
}

shinyApp(ui = ui, server= server)

