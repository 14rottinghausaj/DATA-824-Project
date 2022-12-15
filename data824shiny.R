library(shiny)
library(tidyverse)
library(DT)
library(plotly)

teamfilter <- readRDS("C:/Users/14rot/OneDrive/Documents/Masters Program/DATA 824/Final Project/DATA_824_Project/team_filter.RData")
teams <- readRDS("C:/Users/14rot/OneDrive/Documents/Masters Program/DATA 824/Final Project/DATA_824_Project/teams.RData")
dat <- readRDS("C:/Users/14rot/OneDrive/Documents/Masters Program/DATA 824/Final Project/DATA_824_Project/project_data.RData")
payouts <- readRDS("C:/Users/14rot/OneDrive/Documents/Masters Program/DATA 824/Final Project/DATA_824_Project/payout.RData")
teampayouts <- readRDS("C:/Users/14rot/OneDrive/Documents/Masters Program/DATA 824/Final Project/DATA_824_Project/totalpayouts.RData")
overview <- readRDS("C:/Users/14rot/OneDrive/Documents/Masters Program/DATA 824/Final Project/DATA_824_Project/results.RData")


shinyApp(
  ui = fluidPage(

  titlePanel("Big 12 Football Gambling Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      position = 'right',
      selectInput('team', 'Select Team', choices = teams[,1]),
      tabsetPanel(
        tabPanel("Overview", tableOutput("ov")), 
        tabPanel("Payouts", tableOutput("pay")),
        tabPanel("Point Differentials", tableOutput("differentials"))
      ),
      width = 2
    ),
    
    mainPanel(
     tableOutput("gl"),
     plotlyOutput("wgr"),width = 10)
  )),
  
  server = function(input, output, session) {
    
    gamelog <- reactive({
      
      input$team %>% teamfilter(dat)
      
    })
    
    overviewtab <- reactive({
      
      input$team %>% teamfilter(dat) %>% overview() %>% rename("Wager Type" = Wager.Type)
      
    })
    
    paytab <- reactive({
      
      input$team %>% teamfilter(dat) %>% teampayouts() %>% rename("Bet Type" = Bet.Type)
      
      
    })
    
    differ <- reactive({
      
      startdat <- input$team %>% teamfilter(dat)
      actuals <- sum(startdat$teamscore) - sum(startdat$oppscore)
      predict <- sum(-startdat$bookSpread)
      m <- matrix(ncol = 2, nrow = 3, byrow=T,c("Predicted", predict, "Actual", actuals, "Performance", ifelse(predict > actuals, -(predict - actuals), abs(predict - actuals))))
      diff <- data.frame("Type"= m[,1], "Value" = m[,2])
      
    })
    
    
    output$gl <- renderTable({ 
      gamelog()
    }, digits =1)
    
    output$ov <- renderTable({ 
      overviewtab() 
    }, digits =2)
    
    output$pay <- renderTable({ 
      paytab()
    }, digits =2)
    
    output$differentials <- renderTable({ 
      differ() 
    }, digits =0)
    
    output$wgr <- renderPlotly({
      input$team %>% 
        teamfilter(dat) %>% 
        plot_ly() %>%
        add_trace(x = ~oppscore, y = ~teamscore, type ='scatter', mode = 'markers',color = ~ML_outcome,symbol = ~spread_outcome) %>%
        layout(shapes = list(list(
          type = "line", 
          x0 = 0, 
          x1 = ~max(oppscore, teamscore), 
          xref = "x",
          y0 = 0, 
          y1 = ~max(oppscore, teamscore),
          yref = "y",
          line = list(color = "black")
        )))
    })
  },
  
  options = list(height = 300, aoColumn = list(list(swidth = "30px", swidth = "30px", swidth = "30px", swidth = "30px", swidth = "30px", swidth = "30px", swidth = "30px", swidth = "30px", swidth = "30px")))
  
)
