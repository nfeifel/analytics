### load environemnt

if(!require(jsonlite)){
  install.packages("jsonlite")
  library(jsonlite)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
} 

if(!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)
} 

if(!require(scales)){
  install.packages("scales")
  library(scales)
} 

if(!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
}

if(!require(rvest)){
  install.packages("rvest")
  library(rvest)
}

if(!require(quantmod)){
  install.packages("quantmod")
  library(quantmod)
}

if(!require(xt)){
  install.packages("xt")
  library(xt)
}

if(!require(tibble)){
  install.packages("tibble")
  library(tibble)
}

# define date ranges

start_date <- as.Date("2010-07-17") # date Bitcoin netwrk launched
end_date <- Sys.Date() # current date

start_date_unix <- as.numeric(as.POSIXct(start_date))
end_date_unix <- as.numeric(as.POSIXct(end_date))

### get data from coinmetrics.io (now requires authetication, use coindesk API below for instant pull)

# btc_data <- fromJSON(paste0("https://coinmetrics.io/api/v1/get_asset_data_for_time_range/btc/price(usd)/",start_date_unix,"/",end_date_unix))[['result']] %>%
#  as.data.frame()
# btc_data$V1 <- as.POSIXct(btc_data$V1,origin = '1970-01-01 00:00.00 UTC')
# names(btc_data) <- c('date','price (usd)')

### get data from coindesk api (doesn't require autheticantion, which coinmetrics now does)

coindesk_pull <- fromJSON(paste0("https://api.coindesk.com/v1/bpi/historical/close.json?start=",start_date,"&end=",end_date))
n <- as.vector(names(coindesk_pull["bpi"][[1]]))
d <- unlist(coindesk_pull["bpi"][[1]])
attr(d,'names') <- NULL

btc_data <- data.frame('date' = n,'price' = d)
btc_data$date <- as.Date(btc_data$date)

### create feature for price line

max_date <- max(btc_data$date)

curent_btc_price <- btc_data %>%
  filter(as.Date(date) == max_date) %>%
  select(price) %>%
  as.numeric()

curent_btc_price_rounded_50 <- (round(curent_btc_price / 50) * 50)
curent_btc_price_rounded_50_str <- (round(curent_btc_price / 50) * 50) %>% as.character()
curent_btc_price_rounded_50_str <- paste0('$',curent_btc_price_rounded_50_str)

btc_data <- mutate(btc_data, Category = ifelse(price >= curent_btc_price_rounded_50,paste('Over',curent_btc_price_rounded_50_str),paste('Under',curent_btc_price_rounded_50_str)))

### first line chart

ggplot(data=btc_data, aes(x=date, y=price,color = Category,group=1)) +
  geom_line() +
  scale_colour_manual(values=c("tomato","limegreen")) +
  labs(title = "BTC/USD Daily Opening Price (Log)") +
  ylab("Price (USD)") +
  xlab("Date") +
  scale_y_continuous(labels=dollar_format(prefix="$"),trans='log10') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 18),
        axis.title.x = element_text(face = 'bold',size = 14),
        axis.title.y = element_text(face = 'bold',size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text( size=14, face="bold"),
        legend.text = element_text( size=12, face="bold"))

### pie chart

x <- table(btc_data$Category) %>% as.data.frame()
names(x) <- c('Category','Frequency')
x <- mutate(x, Percent = Frequency/sum(x$Frequency))

ggplot(x, aes(x="", y=Frequency, fill=Category)) +
  geom_bar(width = 0.5, size = 0.5, stat = "identity", color = "black") + 
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(format(Frequency,big.mark = ','),"\n days\n (",round(Percent*100), "%)")), position = position_stack(vjust = 0.5), size = 4, fontface = 'bold') + 
  labs(title = paste("# of Days BTC Opened Above Vs. Below", curent_btc_price_rounded_50_str,"\n Since",start_date)) +
         scale_fill_manual(values = c("tomato","limegreen")) +
         theme(axis.text = element_blank(),
               axis.ticks = element_blank(),
               panel.grid  = element_blank(),
               legend.position="right",
               legend.title = element_text( size=14, face="bold"),
               legend.text = element_text( size=12, face="bold"),
               plot.title = element_text(hjust = 0.5, face = 'bold', size = 18),
               axis.title = element_blank())
       
       
### dollar cost averaging
       
btc_data_2 <- btc_data %>% filter(date >= '2017-12-17') %>% arrange(date)
nums <- seq(1,10000,7) # makes the dataset weekly. use seq(1,520,30) for monthly buy-ins
btc_data_2 <- na.omit(btc_data_2[nums,])
       
btc_data_2$total_invested <- 0
btc_data_2$btc_bought <- 0
btc_data_2$cumulative_market_value <- 0
btc_data_2$net_position <- 0
       
investment_variable <- 0

for (i in 1:nrow(btc_data_2)) {
 
 investment_variable <- investment_variable + 10 # specify investment amount
 
 btc_data_2$total_invested[i] <- investment_variable
 
 btc_data_2$btc_bought[i] <- 10 / btc_data_2$price[i]
 
 btc_data_2$cumulative_market_value[i] <- sum(btc_data_2$btc_bought) * btc_data_2$price[i]
 
 btc_data_2$net_position[i] <- btc_data_2$cumulative_market_value[i] - btc_data_2$total_invested[i]
 
}

btc_data_2 <- mutate(btc_data_2, ROI = ifelse(net_position > 0,'Positive return','Negative return'))

### dollar cost averaging chart NOT in article

ggplot(btc_data_2,aes(x=date, y=net_position, color = ROI, group =1)) +
  geom_line()+
  labs(title =  "Cumulative return on Investment of a Weekly $10 Investment in BTC\n Since BTC's Peak on 2017-12-27",caption = "(Based on data from coinmetrics.io)",color='Line Meaning') +
  scale_color_manual(values=c("tomato","limegreen")) +
  scale_y_continuous(labels=dollar_format(prefix="$")) +
  ylab("Value (USD)") +
  xlab("Date") +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 14),
        axis.title.x = element_text(face = 'bold',size = 12),
        axis.title.y = element_text(face = 'bold',size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text( size=12, face="bold"),
        legend.text = element_text( size=10, face="bold"))

### dollar cost averaging chart in article

btc_data_2 <- btc_data_2 %>%
 select(date,total_invested,cumulative_market_value) %>%
 gather(type,value,-date) %>%
 arrange(date) 

ggplot(btc_data_2,aes(x=date, y=value,color = type)) +
  geom_line()+
  labs(title =  "Cumulative performance of a Weekly $10 Investment in BTC\n Since BTC's Peak on 2017-12-27",caption = "(Based on data from coinmetrics.io)",color='Line Meaning') +
  scale_color_manual(labels = c("Market value of investments", "Total amount invested"), values = c("limegreen", "black")) +
  scale_y_continuous(labels=dollar_format(prefix="$")) +
  ylab("Value (USD)") +
  xlab("Date") +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 14),
        axis.title.x = element_text(face = 'bold',size = 14),
        axis.title.y = element_text(face = 'bold',size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text( size=14, face="bold"),
        legend.text = element_text( size=12, face="bold"))

### btc vs s&p 500

btc_data_3 <- btc_data

btc_data_3$percent_change <- 0

### get dily btc change

for (i in 2:nrow(btc_data_3)) {
 
 btc_data_3$percent_change[i] <- (btc_data_3$price[i]-btc_data_3$price[i-1])/btc_data_3$price[i-1]
 
}

btc_data_3 <- btc_data_3 %>% arrange(date)

### the below calculates btc investment growth, is not used in article

btc_data_3$current_value <- c(100,rep(0,nrow(btc_data_3)-1))

for (i in 2:nrow(btc_data_3)) {
 
 btc_data_3$current_value[i] <-as.numeric(btc_data_3$current_value[i-1]*(1+btc_data_3$percent_change[i]))
 
}

###

btc_data_3$date <- btc_data_3$date - days(1) # convert from open price to close price
btc_data_3$date <- btc_data_3$date %>% as.Date()

btc_data_3 <- btc_data_3 %>% filter(date <= '2019-05-20') %>% select(-current_value)

### get SPY data via quantmod

getSymbols("SPY",from="2013-04-26",to="2019-05-21")

stock_returns <- SPY %>% as.data.frame() %>% rownames_to_column('date') # convert index to columns

stock_returns$date <- as.Date(stock_returns$date)

stock_returns$sp_change <- 0

### get daily SPY chnage

for (i in 2:nrow(stock_returns)) {
 
 stock_returns$sp_change[i] <- (stock_returns$SPY.Close[i]-stock_returns$SPY.Close[i-1])/stock_returns$SPY.Close[i-1]
 
}

stock_returns <- stock_returns %>% filter(date != '2013-04-26')

### merge btc and SPY datasets

merged_data <- left_join(btc_data_3,stock_returns, by = 'date') %>% filter(date != '2013-04-27',date != '2013-04-28')

merged_data$sp_change[is.na(merged_data$sp_change)] <- 0 # turns NAs from stock market weekends into 0s

### get annualized returns

portfolio <- data.frame(date = c(merged_data$date[1]-1,merged_data$date), 
                       btc_change = c(0,merged_data$percent_change),
                       s_p_500_change = c(0,merged_data$sp_change))

start_date_portfolio <- as.Date('2014-01-01') # designate investment starting point
portfolio <- portfolio %>% filter(date >= start_date_portfolio)
sp500_allocation <- .8 # designate SPY allocation
btc_allocation <- 0.2 # designate btc allocation

portfolio$current_value = c(100,rep(0,nrow(portfolio)-1)) # sets up initial $100 investment

for (i in 2:nrow(portfolio)) {
 
 portfolio$current_value[i] <- as.numeric(portfolio$current_value[i-1] * (((1+portfolio$btc_change[i])*btc_allocation) + ((1+portfolio$s_p_500_change[i])*sp500_allocation)))
 
}

val <- tail(portfolio,1)['current_value'] %>% as.numeric()
total_return <- ((val-100)/100)
annualized <- total_return/(nrow(portfolio)/365)

### get annualized returns when btc goes to $0

portfolio <- data.frame(date = c(merged_data$date[1]-1,merged_data$date), 
                       btc_change = c(0,-1,rep(0,nrow(merged_data)-1)),
                       s_p_500_change = c(0,merged_data$sp_change))

start_date_portfolio <- as.Date('2014-01-01') # designate investment starting point
portfolio <- portfolio %>% filter(date >= start_date_portfolio)
sp500_allocation <- 1 # designate SPY allocation
btc_allocation <- 0 # designate btc allocation

portfolio$current_value = c(100,rep(0,nrow(portfolio)-1)) # sets up initial $100 investment
portfolio$btc_change <- c(0,-1,rep(0,nrow(portfolio)-2)) # makes btc go to $0

for (i in 2:nrow(portfolio)) {
 
 portfolio$current_value[i] <- as.numeric(portfolio$current_value[i-1] * (((1+portfolio$btc_change[i])*btc_allocation) + ((1+portfolio$s_p_500_change[i])*sp500_allocation)))
 
}

val <- tail(portfolio,1)['current_value'] %>% as.numeric()
total_return <- ((val-100)/100)
annualized <- total_return/(nrow(portfolio)/365)

