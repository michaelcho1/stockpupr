library(splitstackshape) #text to columns
library(stringr) #strings
library(bpa) #get pattern
library(ggplot2)
library(dplyr)
library(lubridate)


#RBIND COMPANY FILES INTO 1 DATAFRAME
setwd('C:/Users/mjdch/Desktop/stockpup')
files = list.files('C:/Users/mjdch/Desktop/stockpup/company',pattern = '*.csv')
cofiles = do.call(rbind, lapply(files, function(x)read.csv(paste0('./company/',x), header=T,stringsAsFactors = F)))
length(unique(cofiles$ticker))

#RENAME COLUMNS 
names(cofiles) <- tolower(names(cofiles))
cofiles <- rename(cofiles,goodwill.intangibles = goodwill...intangibles)

#REPLACE NONE WITH NA
convert_na <- function(str){
  return(ifelse(str=='None',NA,str))
}

cofiles[]<- lapply(cofiles,convert_na)

apply(cofiles,2,function(x) which(x %in% c('None','N','none')))

##CLEAN DATE COLUMN
cofiles$quarter.end %>% get_pattern %>% table %>% as.data.frame

get_date <- function(date_str){
  if(grepl("^\\d{1,2}\\b/\\d{1,2}\\b/\\d{4}\\b", date_str[1]) ){
    return(mdy(date_str))
  } else if (grepl("^\\d{4}\\b-\\d{1,2}\\b-\\d{1,2}\\b", date_str[1]) ){
    return(ymd(date_str))
  }
}

cofiles$quarter.end <- get_date(cofiles$quarter.end)

cofiles$quarter.end[1]
year(cofiles$quarter.end[1])
month(cofiles$quarter.end[1])
which(is.na(get_date(cofiles$quarter.end)))
cofiles[which(is.na(get_date(cofiles$quarter.end))),43]

#CHARACTER COLUMNS TO NUMERIC
apply(cofiles,2,class)
colnames(cofiles[,lapply(cofiles,class)=='character'])
which(sapply(cofiles,is.character))
colnums <- c(6,7,8,9,10,15,16,17,18,19,21,22,23,24,25,27,28,29,30,31,32,33,34,36,37,38,39,40,41,42)
cofiles[,colnums] <- sapply(cofiles[colnums], as.numeric)
sapply(cofiles,class)  

#CREATE COLUMN YEARS FOR GROUPBY LATER
colnames(cofiles)

years <- function(x){
  if(month(x) %in% c(3,4,5,6,7,8,9,10,11,12)){
    return(year(x))
  } else if(month(x) %in% c(1,2)){
    return(year(x)-1)
  }
}

#for companies that have five quarters in a year (BBY and CMVT)
years_five <- function(x){
  if(month(x) %in% c(2,3,4,5,6,7,8,9,10,11,12)){
    return(year(x))
  } else if(month(x) %in% c(1)){
    return(year(x)-1)
  }
}

#fix BBY years, for CMVT just use regular year function
years_bby <- function(x){
  if(month(x) %in% c(4,5,6,7,8,9,10,11,12)){
    return(year(x))
  } else if(month(x) %in% c(1,2,3)){
    return(year(x)-1)
  }
}

table(sapply(cofiles$quarter.end[cofiles$ticker =='BBY'], years_bby))


#MUTATE YEARS, MARKETCAP, ENTERPRISEVALUE AND FINISH QUARTER_COFILES
a<- cofiles %>% mutate(years = ifelse(ticker %in% c('BBY'),sapply(quarter.end,years_bby),
                                      ifelse(ticker %in% c('CMVT'), sapply(quarter.end,year),
                                             ifelse(ticker %in% c('BBBY','ADBE','BSC','FDO','JEF','LEH','MON','MS','MU','NSM','RAD'),
                                                    sapply(quarter.end,years_five),sapply(quarter.end,years)))),
                       marketcap = price * shares.split.adjusted, 
                       enterprise.value = marketcap + long.term.debt+non.controlling.interest+preferred.equity- cash.at.end.of.period,
                       fcfyield = free.cash.flow.per.share/price,
                       ocf_margin = cash.from.operating.activities/revenue,
                       fcf_margin = (free.cash.flow.per.share * shares.split.adjusted)/revenue
                       ) 
colnames(a)
quarter_cofiles <- a
view(quarter_cofiles)
write.csv(quarter_cofiles, 'quarter_cofiles.csv',row.names = F)
###################################################################################################
#CREATE YEAR_COFILES FOR YEARLY DATA
cumpfun<- function(x){
  x[!is.na(x)] <- cumprod(x[!is.na(x)]+1)
  x[1] = 1
  return(x)
}

year_cofiles <- quarter_cofiles %>% 
  group_by(ticker,years) %>% 
  mutate(n=n()) %>% 
  filter(n==4) %>% 
  summarise(shares.split.adjusted = mean(shares.split.adjusted),
            assets = mean(assets), current.assets = mean(current.assets),
            liabilities = mean(liabilities), current.liabilities = mean(current.liabilities),
            shareholders.equity = mean(shareholders.equity), non.controlling.interest = mean(non.controlling.interest),
            preferred.equity = mean(preferred.equity), goodwill.intangibles = mean(goodwill.intangibles),
            long.term.debt = mean(long.term.debt),
            revenue = sum(revenue), earnings = sum(earnings),
            earnings.available.for.common.stockholders = sum(earnings.available.for.common.stockholders),
            eps.basic = sum(eps.basic), eps.diluted = sum(eps.diluted),
            dividend.per.share = sum(dividend.per.share),
            cash.from.operating.activities = sum(cash.from.operating.activities),
            cash.from.investing.activities = sum(cash.from.investing.activities),
            cash.from.financing.activities = sum(cash.from.financing.activities),
            cash.change.during.period = sum(cash.change.during.period),
            cash.at.end.of.period = mean(cash.at.end.of.period),
            capital.expenditures = sum(capital.expenditures),
            price = mean(price), price.high = mean(price.high), price.low = mean(price.low),
            roe = mean(roe), roa = mean(roa), book.value.of.equity.per.share = mean(book.value.of.equity.per.share),
            p.b.ratio = mean(p.b.ratio), p.e.ratio = mean(p.e.ratio), cumulative.dividends.per.share = first(cumulative.dividends.per.share),
            dividend.payout.ratio = mean(dividend.payout.ratio), long.term.debt.to.equity.ratio = mean(long.term.debt.to.equity.ratio),
            equity.to.assets.ratio = mean(equity.to.assets.ratio), net.margin = mean(net.margin), asset.turnover = mean(asset.turnover),
            free.cash.flow.per.share = mean(free.cash.flow.per.share), current.ratio  = mean(current.ratio),
            marketcap = mean(marketcap), enterprise.value = mean(enterprise.value), fcfyield = mean(fcfyield),
            ocf_margin = mean(ocf_margin), fcf_margin = mean(fcf_margin))%>%
  mutate(price.change = ifelse((years-lag(years,1))==1, ((price - lag(price,1))/lag(price,1)), NA),
         cum.price.change = cumpfun(price.change),
         ev.sales = enterprise.value/revenue,
         ev.ocf = enterprise.value/cash.from.operating.activities,
         revgrowth = ifelse((years-lag(years,1))==1, ((revenue - lag(revenue,1))/lag(revenue,1)), NA))

View(year_cofiles)

#####################################

#BIND INDUSTRY AND DESCRIPTION DATA
industry_data = read.csv("industries.csv",header = TRUE, stringsAsFactors = F)

year_cofiles <- left_join(year_cofiles, industry_data, by = 'ticker')
write.csv(year_cofiles, 'year_cofiles.csv',row.names = F)

quarter_cofiles <- left_join(quarter_cofiles, industry_data, by = 'ticker')
write.csv(quarter_cofiles, 'quarter_cofiles.csv',row.names = F)

view(year_cofiles)
view(quarter_cofiles)

#create a master with all industries and all cofiles in one dataframe to assess industry data. groupby industry
industries <- read.csv('tidy_industries.csv', stringsAsFactors = F, header = T)
industry_1 <- industries %>% filter(industry_n == 'industry_1')
industry_2 <- industries %>% filter(industry_n == 'industry_2')
industry_3 <- industries %>% filter(industry_n == 'industry_3')
industry_4 <- industries %>% filter(industry_n == 'industry_4')
industry_5 <- industries %>% filter(industry_n == 'industry_5')
industry_6 <- industries %>% filter(industry_n == 'industry_6')

co_1 <- left_join(year_cofiles, industry_1, by = 'ticker')
co_2 <- left_join(year_cofiles, industry_2, by = 'ticker')
co_3 <- left_join(year_cofiles, industry_3, by = 'ticker')
co_4 <- left_join(year_cofiles, industry_4, by = 'ticker')
co_5 <- left_join(year_cofiles, industry_5, by = 'ticker')
co_6 <- left_join(year_cofiles, industry_6, by = 'ticker')

final <- rbind(co_1,co_2,co_3,co_4,co_5,co_6)
colnames(final)
final <- final %>% select(-acquired.y, -bankruptcy.y, -company.y, -company_name.y, -description.y, -industry.x)
final <- final[!is.na(final$industry.y),]
write.csv(final, 'year_cofiles_ind.csv',row.names = F)
#FINAL DATASET CREATED



