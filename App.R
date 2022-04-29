source('global.R')
library(shiny)
library(googleVis)
library(quantmod)
library(shiny)
library(ggplot2)
library(dplyr)
library(forecast)
library(TTR)
library(data.table)
library(zoo)
library(shinydashboard)
library(DT)
library(textshape)
library(plotly)
library(tidyr)
library(rsconnect)
library(tm)
# Define UI ----
ui <- fluidPage(
  dashboardPage (
    skin = "green",
    dashboardHeader(title = 'Stock Price Predictor', titleWidth = 230),
    dashboardSidebar(
      sidebarUserPanel(h2("S&P500")),
      sidebarMenu(
        menuItem('Data', tabName = 'dt', icon = icon('database')),
        menuItem('TimeSeries', tabName = 'ts', icon = icon('line-chart'))
      ),
      
      selectizeInput('sector', h3('Sectors'), 
                     choices = na.omit(unique(stocks_w_sec$Sector)),
                     selected = 'XLI'),
      selectizeInput('price1', h3('Select Stocks'), 
                     choices = unique(stocks_w_spy$Name),
                     selected = 'MMM')
      ###########
    ),
    dashboardBody(
      tabItems(
        
        tabItem(tabName = 'dt',
                fluidRow(box(DT::dataTableOutput('table'),
                             h3('Time series data of S&P 500 companies'),
                             '*you can type in the stock code in the search box for
                             stock specific data',
                             width = 13, solidHeader = T,
                             status = 'warning'))
        ),
        
        tabItem(tabName = 'ts',
                fluidRow( 
                  box(title = 'ARIMA(p,d,q)', width = 3, 
                      solidHeader = T, status = 'warning',
                      sliderInput('ar', 'Autoregression',
                                  min = 0, max = 3, value = 0),
                      sliderInput('diff', 'Differencing',
                                  min = 0 , max = 3, value = 0),
                      sliderInput('ma', 'Moving-average',
                                  min = 0 , max = 3, value = 0)),
                  tabBox(title = tagList(shiny::icon("line-chart"), "Time Series"), width = 9,
                         tabPanel('Price',
                                  plotOutput('fit_stock')),
                         tabPanel('Correlation',
                                  fluidRow(
                                    box(h4(tags$b('ACF')), solidHeader = T, status = 'success',
                                        'Auto correlation between time series and its own lag',
                                        plotOutput('acf')),
                                    box(h4(tags$b('PACF')), solidHeader = T, status = 'success',
                                        'Conditional auto correlation between time seires and its own lag',
                                        plotOutput('pacf'))
                                  )),
                         
                         tabPanel('Forecast',
                                  fluidRow(
                                    box('Forecast',
                                        plotOutput('fore')),
                                    box('Plot fitted data with price',
                                        plotOutput('f_v_d')),
                                    box('Accuracy',
                                        tableOutput('accu'))
                                  ))) 
                )
        ))
    ))
)

# Define server logic ----
server <- function(input, output, session) {
  output$table = DT::renderDataTable({
    datatable(stocks_w_sec, rownames = F)
  },striped = TRUE, bordered = TRUE)
  
  
  ## data fitting
  fit_data = reactive({
    stocks_w_spy %>% 
      filter(., Name == input$price1) %>% 
      select(., Date, Close) %>%
      na.omit(.) %>% 
      column_to_rownames(.)
  })
  #model fitting
  fit_model = reactive({
    stocks_w_spy %>% 
      filter(., Name == input$price1) %>% 
      select(., Date, Close) %>%
      slice(., 1:200) %>% 
      na.omit(.) %>% 
      column_to_rownames(.) %>% 
      as.ts(.) %>% 
      Arima(., order = c(input$ar, input$diff, input$ma))
  })
  #### price of stocks 
  output$fit_stock = renderPlot({
    if (input$diff == 0) {
      ggplot(data = fit_data(), aes(x = 1:nrow(fit_data()), y = Close)) +
        geom_line() + xlab('Time Period') +
        ggtitle(input$price1) + theme_bw()
    } else {
      ggplot() + geom_line(aes(x = 1:length(diff(ts(fit_data()), differences = input$diff)),
                               y = diff(ts(fit_data()), differences = input$diff))) +
        xlab('Time Period') + ylab('After Differencing') +
        ggtitle(input$price1) + theme_bw()
    }
  })
  
  output$acf = renderPlot({
    if (input$diff == 0) {
      Acf(fit_data()[1:200,], main = '')}
    else {
      Acf(diff(ts(fit_data()), differences = input$diff), main = '')
    }
  })
  
  output$pacf = renderPlot({
    if (input$diff == 0) {
      pacf(fit_data()[1:200,], main = '')}
    else {
      pacf(diff(ts(fit_data()), differences = input$diff), main = '')
    }
  })
  
  #### following is the forecast graph code - 
  output$fore = renderPlot({
    plot(forecast(fit_model(), h = 50))
    lines(fit_data()[1:250,])
    abline(h = forecast(fit_model(), h = 50)[[6]][50,1], lty = 'dashed', col = 'red') +
      text(x = 200,
           y = forecast(fit_model(), h = 50)[[6]][50,1] - 1,
           labels = round(forecast(fit_model(), h = 50)[[6]][50,1], 2), cex = 0.8) +
      text(x = 175,
           y = forecast(fit_model(), h = 50)[[6]][50,1] - 1,
           labels = '80%:', cex = 0.8)
    abline(h = forecast(fit_model(), h = 50)[[6]][50,2], lty = 'dotdash', col = 'blue') +
      text(x = 200,
           y = forecast(fit_model(), h = 50)[[6]][50,2] - 1,
           labels = round(forecast(fit_model(), h = 50)[[6]][50,2], 2), cex = 0.8) +
      text(x = 175,
           y = forecast(fit_model(), h = 50)[[6]][50,2] - 1,
           labels = '95%:', cex = 0.8)
    abline(h = forecast(fit_model(), h = 50)[[5]][50,1], lty = 'dashed', col = 'red') +
      text(x = 200,
           y = forecast(fit_model(), h = 50)[[5]][50,1] + 1,
           labels = round(forecast(fit_model(), h = 50)[[5]][50,1], 2), cex = 0.8) +
      text(x = 175,
           y = forecast(fit_model(), h = 50)[[5]][50,1] + 1,
           labels = '80%:', cex = 0.8)
    abline(h = forecast(fit_model(), h = 50)[[5]][50,2], lty = 'dotdash', col = 'blue') +
      text(x = 200,
           y = forecast(fit_model(), h = 50)[[5]][50,2] + 1,
           labels = round(forecast(fit_model(), h = 50)[[5]][50,2], 2), cex = 0.8) +
      text(x = 175,
           y = forecast(fit_model(), h = 50)[[5]][50,2] + 1,
           labels = '95%:', cex = 0.8)
    
  })
  
  output$accu = renderTable({
    accuracy(forecast(fit_model(), h = 50), fit_data()[201:250,])
  })
  ## forecast output 
  output$f_v_d = renderPlot({
    ggplot() +
      geom_line(aes(x = 1:200, y = fit_data()[1:200,])) +
      geom_line(aes(x = 1:length(fitted(fit_model())),
                    y = fitted(fit_model())), col = 'red',
                inherit.aes = F) +
      ggtitle(input$price1) + xlab('Time Period') + ylab('Stock Price') +
      theme_bw()
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)