library(shinydashboard)
library(dplyr)
library(dygraphs)
library(xts)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(plotly)


#original datasets
yearco <- read.csv('year_cofiles.csv', stringsAsFactors = F, header = T)
quarterco <- read.csv('quarter_cofiles.csv', stringsAsFactors = F, header = T)
yearcoind <- read.csv('year_cofiles_ind.csv', stringsAsFactors = F, header = T)

#price page
pricedf<- quarterco[!is.na(quarterco$price),] %>% 
  select(quarter.end, price, company, ticker, description, industry, acquired, bankruptcy, company_name)

pricedf$quarter.end <- ymd(pricedf$quarter.end)


#cumulative return 
cumdf <- yearco[!is.na(yearco$cum.price.change),] %>%
  select(years, price, cum.price.change, company, ticker, company_name, description, industry)

cumdf$years <- as.Date(ISOdate(cumdf$years,12,31))

#quarterly statements
quarterbs <- quarterco[c(2,4,6:14,43,52,53)]
quarteris <- quarterco[c(2,15:20,43,52,53)]
quartercf <- quarterco[c(2,21:26,43,52,53)]
quartermetrics <- quarterco[c(2,30:42,43,47,48,49,52,53)]

#year statements
yearbs <- yearco[c(1:12,53,54)]
yearis <- yearco[c(1,2,53,54,13:18)]
yearcf <- yearco[c(1,2,53,54,19:24)]
yearmetrics <- yearco[c(1,2,53,54,28:40,43:45,48:50)]


