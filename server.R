function(input, output, session) {
  

  
  riskfree = reactive({  
    df = as.numeric(input$riskfree) 
    return(df)  
  
  
  })
  
  
  
  
 tickers = reactive({  
   tickers = input$variable
   return(tickers)  
   
   
 })
  
 
   prices = reactive({
    
    #tickers = c("MMM", "ABT", "ABBV", "ABMD", "ACN")
    
     tickers = tickers()
     
    getSymbols(tickers, src = "yahoo"
               , from = "2018-01-01"
               , to = Sys.Date(), auto.assign = T, getSymbols.warning4.0=FALSE)
    
    
    
   
    
    prices <- map(tickers,function(x) dailyReturn(get(x)))
    
    prices <- reduce(prices,merge)
    colnames(prices) <- tickers
    return(prices)
    
   } )
   
   
   download = reactive({
     
     #tickers = c("MMM", "ABT", "ABBV", "ABMD", "ACN")
     
     download = as.data.frame(prices())
     
     download$Date = row.names(download)
     
     return(download)
     
   } )
    
    #####################3 optimization
    
    
   
   output$downloadData2 <- downloadHandler(
     
    
     filename = function() { 'Returns.csv' }, content = function(file) {
       write.csv(download(), file, row.names = F)}
     
   )
   
   
   
   
   

   
   
   
    
    max = reactive({
    
    
    prices = prices()
    gauss.object <- fit.gaussmv(-prices)
    
    
    
    gauss.ptf <- portfolio.optimize(gauss.object,
                                    risk.measure = "sd",
                                    type = "tangency",
                                    risk.free = riskfree(),
                                    level = 0.99,
                                    distr = "loss")
    
    
    
    
    weights = as.data.frame(gauss.ptf$opt.weights)
    
    colnames(weights)[1] = "Weights"
    
    weights$Symbols = tickers()
    
    weights = weights %>% spread(Symbols, Weights)
    
    m1 = gauss.ptf$portfolio.dist
    
    weights$Expected_Return = - m1@mu
    
    max = -m1@mu + 6*m1@sigma
    
    return(max)
    
})
    
    
    min = reactive({
      
      prices = prices()
      
      gauss.object <- fit.gaussmv(-prices)
      
      
      gauss.ptf2 <- portfolio.optimize(gauss.object,
                                       risk.measure = "expected.shortfall",
                                       type = "minimum.risk",
                                       level = 0.99,
                                       distr = "loss")
      
      

      
      m = gauss.ptf2$portfolio.dist
      
      min = - m@mu - 6* m@sigma
      
      return(min)
      
    })
    
    
   
    
    
    
    
    
    
    
    
    weights = reactive({  
      prices = prices()
    
    gauss.object <- fit.gaussmv(-prices)
    
    
    
    gauss.ptf <- portfolio.optimize(gauss.object,
                                    risk.measure = "sd",
                                    type = "tangency",
                                    risk.free = riskfree(),
                                    level = 0.99,
                                    distr = "loss")
    
    
  
    
    weights = as.data.frame(gauss.ptf$opt.weights)
    
    colnames(weights)[1] = "Weights"
    
    weights$Symbols = tickers()
    
    weights = weights %>% spread(Symbols, Weights)
    
    m1 = gauss.ptf$portfolio.dist
    
    weights$Expected_Return = - m1@mu
    
    max = -m1@mu + 6*m1@sigma

    
    weights$Method = "Maximize Sharpe Ratio"
    
    gauss.ptf2 <- portfolio.optimize(gauss.object,
                                     risk.measure = "expected.shortfall",
                                     type = "minimum.risk",
                                     level = 0.99,
                                     distr = "loss")
    
    
    weights2 = as.data.frame(gauss.ptf2$opt.weights)
    
    colnames(weights2)[1] = "Weights"
    
    weights2$Symbols = tickers()
    
    weights2 = weights2 %>% spread(Symbols, Weights)
    
    
    m = gauss.ptf2$portfolio.dist
    
    min = - m1@mu - 6* m1@sigma
    
    weights2$Expected_Return = -m@mu
    
    weights2$Method = "Minimize Shortfall"
    
    weights = rbind(weights2, weights)
    
    row.names(weights) = weights$Method
    
    weights$Method = NULL
    
    
    weights$Expected_Return = percent(weights$Expected_Return)
    
    return(weights)
    
    
  })
  
   
  

  
  #############
  
  output$table1 <- renderFormattable({
    
    
    
    
    formattable(weights(), list(title="Optimal Weights",
                              Expected_Return = color_tile("lightcyan", "cyan")))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    })
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Combine the selected variables into a new data frame
  output$plot1 <- renderPlotly({
    
    require(tickers())
    
 
    ####
    tickers = tickers()
    
    getSymbols(tickers, src = "yahoo"
               , from = "2018-01-01"
               , to = Sys.Date(), auto.assign = T, getSymbols.warning4.0=FALSE)
    
    
    
    prices <- map(tickers,function(x) Ad(get(x)))
    
    #prices <- map(tickers,function(x) dailyReturn(get(x)))
    
    prices <- reduce(prices,merge)
    colnames(prices) <- tickers()
    
    dfprices = as.data.frame(prices)
    
    dfprices$date = row.names(dfprices)
    
    
    
    dfprices = dfprices %>% gather(Company, Prices, -date)
    
    
    
    
    
    
    
    
    fig <- plot_ly(dfprices %>% filter(date>"2019-01-01"), x = ~date, y =~Prices, color=~Company)
    
    fig <- fig %>% layout(plot_bgcolor  = "#ffe6ff",
                          paper_bgcolor = "#ffe6ff",
                          
                          title = "",
                          xaxis = list(title="",
                                       rangeselector = list(
                                         buttons = list(
                                           list(
                                             count = 3,
                                             label = "3 mo",
                                             step = "month",
                                             stepmode = "backward"),
                                           list(
                                             count = 6,
                                             label = "6 mo",
                                             step = "month",
                                             stepmode = "backward"),
                                           list(
                                             count = 1,
                                             label = "1 yr",
                                             step = "year",
                                             stepmode = "backward"),
                                           list(
                                             count = 1,
                                             label = "YTD",
                                             step = "year",
                                             stepmode = "todate"),
                                           list(step = "all")))
                                       
                                       ),
                          
                          yaxis = list(title = ""))
    
    fig
  
      
  })
  
  
  
  
  
  
  
  
  
  
  
  output$table2 <- renderFormattable({
    require(tickers())
    
    tickers = tickers()
    
    getSymbols(tickers, src = "yahoo"
               , from = "2018-01-01"
               , to = Sys.Date(), auto.assign = T, getSymbols.warning4.0=FALSE)
    
  returns <- map(tickers,function(x) dailyReturn(get(x)))
  
  returns <- reduce(returns,merge)
  colnames(returns) <- tickers()
  
  dfreturns= as.data.frame(returns)
  
  dfreturns$Date = row.names(dfreturns)
  
  dfreturns = dfreturns %>% arrange(desc(Date))
  
  row.names(dfreturns) = dfreturns$Date
  
  dfreturns$Date = NULL
  

  
  formattable(dfreturns,  lapply(1:nrow(dfreturns), function(row) {
    area(row, col = 1:ncol(dfreturns)) ~ color_tile("white", "lightgray")
  }))
  
  })
  
  
 
  output$minbox <- renderValueBox({
    
    
   
    
 
    
    
    valueBox(paste0(percent(min() ) ), 
             "Min Return", icon = icon(""), color = "yellow")
  }) 
  
  
  
  output$riskfreebox <- renderValueBox({
    
    

    
    
    valueBox(paste0( percent(riskfree())  ), 
             "Risk-Free Return", icon = icon(""), color = "aqua")
  }) 
  
 
  output$maxbox <- renderValueBox({
    
    
    

    
    
   valueBox(paste0(percent(max())), 
             "Max Return", icon = icon(""), color = 	"blue")
  })  
  
  
  

    
    
    
    
    
    
    
 
    
    
    
    
    
    
    

  
  
  
    
 
  
  
  
}
