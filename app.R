ibrary(shiny) 

library(ggplot2) 



setwd("~/Desktop/test")



EOQ<-read.csv("EOQ.csv", header = TRUE)  



ui <- fluidPage( 
  
  titlePanel("EOQ Visualization"), 
  
  sidebarLayout( 
    
    sidebarPanel( 
      
      sliderInput("demand", "Demand", 
                  
                  min = 10000, max = 80000, value = c(10000, 80000), 
                  
                  step = 1000), 
      
      sliderInput("order_cost", "Order Cost", 
                  
                  min = 0, max = 500, value = c(0, 100), 
                  
                  step = 10), 
      
      sliderInput("holding_cost", "Holding Cost", 
                  
                  min = 0, max = 50, value = c(0, 10), 
                  
                  step = 1) 
      
    ), 
    
    mainPanel( 
      
      plotOutput("eoq_graph") 
      
    ) 
    
  ) 
  
) 



server <- function(input, output) { 
  
  eoq <- function(demand, order_cost, holding_cost) {  
    
    q <- sqrt((2 * demand * order_cost) / holding_cost);  
    
    tc <- sqrt((2 * demand * order_cost * holding_cost));  
    
    return(data.frame(Q = q, Total_Cost = tc))  
    
  } 
  
  
  
  eoq_data <- reactive({  
    
    demand <- seq(input$demand[1], input$demand[2], by = 1000);  
    
    order_cost <- seq(input$order_cost[1], input$order_cost[2], by = 10);  
    
    holding_cost <- seq(input$holding_cost[1], input$holding_cost[2], by = 1);  
    
    eoq_df <- eoq(demand, order_cost, holding_cost);  
    
    return(eoq_df)  
    
  }) 
  
  
  
  output$eoq_graph <- renderPlot({  
    
    ggplot(eoq_data(), aes(x = Q, y = Total_Cost)) +  
      
      geom_line() +  
      
      labs(title = "EOQ Visualization",  
           
           x = "Order Quantity (Q)",  
           
           y = "Total Cost") +  
      
      theme_minimal()  
    
  }) 
  
} 



shinyApp(ui = ui, server = server)
