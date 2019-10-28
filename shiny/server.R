

function(input, output, session){

  remove_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
  }
    
  industry_react <- reactive({
    yearcoind %>% group_by(industry.y) %>%
      transmute(roe= remove_outliers(roe), roa = remove_outliers(roa),
                book.value.of.equity.per.share = remove_outliers(book.value.of.equity.per.share),
                p.b.ratio = remove_outliers(p.b.ratio), p.e.ratio = remove_outliers(p.e.ratio),
                cumulative.dividends.per.share = remove_outliers(cumulative.dividends.per.share),
                dividend.payout.ratio =remove_outliers(dividend.payout.ratio), 
                long.term.debt.to.equity.ratio = remove_outliers(long.term.debt.to.equity.ratio),
                equity.to.assets.ratio = remove_outliers(equity.to.assets.ratio),
                net.margin = remove_outliers(net.margin), asset.turnover = remove_outliers(asset.turnover),
                free.cash.flow.per.share = remove_outliers(free.cash.flow.per.share), 
                current.ratio = remove_outliers(current.ratio), fcfyield = remove_outliers(fcfyield), 
                ocf_margin = remove_outliers(ocf_margin), fcf_margin = remove_outliers(fcf_margin),
                ev.sales = remove_outliers(ev.sales), ev.ocf = remove_outliers(ev.ocf), revgrowth = remove_outliers(revgrowth))
  })
  
  output$industryboxplot <- renderPlot({
    industry_react() %>%
      filter(industry.y %in% c(input$industry)) %>%
      ggplot(aes(x=industry.y, fill = industry.y)) + 
      geom_boxplot(aes_string(y= input$industrymetric)) +
      xlab("industry") + 
      theme(legend.position = 'none', axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$quarteris <- renderPlotly({
    a<- quarteris %>% filter(ticker == input$qisticker)
    name <- a$company_name[1]
    plot_ly(a, x= ~a$quarter.end, y = ~a[,input$qisitem], type = 'bar') %>% 
      layout(title = name,yaxis = list(title = 'Line Item'), xaxis = list(title = 'Quarter End'))
  })

  output$quarterbs <- renderPlotly({
    a<- quarterbs %>% filter(ticker == input$qbsticker)
    name <- a$company_name[1]
    plot_ly(a, x= ~a$quarter.end, y = ~a[,input$qbsitem], type = 'bar') %>% 
      layout(title = name,yaxis = list(title = 'Line Item'), xaxis = list(title = 'Quarter End'))
  })
  
  output$quartercf <- renderPlotly({
    a<- quartercf %>% filter(ticker == input$qcfticker)
    name <- a$company_name[1]
    plot_ly(a, x= ~a$quarter.end, y = ~a[,input$qcfitem], type = 'bar') %>% 
      layout(title = name,yaxis = list(title = 'Line Item'), xaxis = list(title = 'Quarter End'))
  })
  
  output$quartermetrics <- renderPlotly({
    a<- quartermetrics %>% filter(ticker == input$qmticker)
    name <- a$company_name[1]
    plot_ly(a, x= ~a$quarter.end, y = ~a[,input$qmitem], type = 'scatter', mode = 'lines') %>% 
      layout(title = name,yaxis = list(title = 'Quarterly Metrics'), xaxis = list(title = 'Year'))
  })
  
  
  output$yearis <- renderPlotly({
    a<- yearis %>% filter(ticker == input$aisticker)
    name <- a$company_name[1]
    plot_ly(a, x= ~a$years, y = ~a[,input$aisitem], type = 'bar') %>% 
      layout(title = name,yaxis = list(title = 'Line Item'), xaxis = list(title = 'Quarter End'))
  })
  
  output$yearbs <- renderPlotly({
    a<- yearbs %>% filter(ticker == input$absticker)
    name <- a$company_name[1]
    plot_ly(a, x= ~a$years, y = ~a[,input$absitem], type = 'bar') %>% 
      layout(title = name,yaxis = list(title = 'Line Item'), xaxis = list(title = 'Quarter End'))
  })
  
  output$yearcf <- renderPlotly({
    a<- yearcf %>% filter(ticker == input$acfticker)
    name <- a$company_name[1]
    plot_ly(a, x= ~a$years, y = ~a[,input$acfitem], type = 'bar') %>% 
      layout(title = name,yaxis = list(title = 'Line Item'), xaxis = list(title = 'Quarter End'))
  })
  
  output$yearmetrics <- renderPlotly({
    a<- yearmetrics %>% filter(ticker == input$amticker)
    name <- a$company_name[1]
    plot_ly(a, x= ~a$years, y = ~a[,input$amitem], type = 'scatter', mode = 'lines') %>% 
      layout(title = name,yaxis = list(title = 'Annual Metrics'), xaxis = list(title = 'Year'))
  })
  
  output$price <- renderDygraph({
    a<- pricedf[pricedf$ticker == input$ticker,]$price
    b<- pricedf[pricedf$ticker == input$ticker,]$quarter.end
    c<- as.xts(a,order.by = b)
    d<- pricedf[pricedf$ticker == input$ticker,]$company_name[1]
    colnames(c) <- c('Price')
    dygraph(c, main = d) %>% dyRangeSelector(height = 30) %>% dyAxis('y',label = 'Price ($)')
  })
  
  output$description <- renderText({
    pricedf[pricedf$ticker == input$ticker,]$description[1]
  })
  
  output$industry <- renderText({
    paste("Industries: ",pricedf[pricedf$ticker == input$ticker,]$industry[1])
  })
  
  output$acquired <- renderText({
    ifelse(pricedf[pricedf$ticker == input$ticker,]$acquired[1] == T & 
             pricedf[pricedf$ticker == input$ticker,]$bankruptcy[1] == T,
           'Aquired, Bankruptcy',ifelse(pricedf[pricedf$ticker == input$ticker,]$acquired[1] == T,
           'Acquired',ifelse(pricedf[pricedf$ticker == input$ticker,]$bankruptcy[1] == T,
           'Bankruptcy','')))
  })

  output$cumprice <- renderDygraph({
    a<- cumdf[cumdf$ticker == input$cumticker,]$cum.price.change
    b<- cumdf[cumdf$ticker == input$cumticker,]$years
    c<- as.xts(a,order.by = b)
    d<- cumdf[cumdf$ticker == input$cumticker,]$company_name[1]
    colnames(c) <- c('Price')
    dygraph(c, main = d) %>% dyRangeSelector(height = 30) %>% dyAxis('y',label = 'Price ($)')
  })

}