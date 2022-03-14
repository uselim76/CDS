### Part I - Calculation  of Equity Volatility

# options(warn=-1)

# Necessary libraries for code
library(readxl)
library(dplyr)
library(xts)
library(magrittr)
library(lubridate)
library(tidyverse)

# Set working directory
setwd("C:/Users/selim.unal/Documents/Private/OzU/Project")

## Load the stock quotes and their respective number of shares
BIST30=as.data.frame(read_excel("./Quotes/BIST30v2.xlsx"))

## Load the risk free rates, name the columns and convert to xts object
Interest=as.data.frame(read_excel("./Quotes/Interest_Rate.xlsx"))
colnames(Interest)=c("date","rfrate")
Interest$rfrate=Interest$rfrate/100
Interest$date=as.Date(Interest$date,format=c("%d-%m-%Y"))
Interest=as.xts(Interest$rfrate,order.by=Interest$date)

## Get the stock tickers and create empty data frame for data
tickers=BIST30$Stock
all_data=data.frame()


## Make all calculations for each stock ticker in a loop 
for (z in 1:24){
  ## First Part - Calculate Stock Returns
  stock=tickers[z]
  stock=as.data.frame(read_csv(paste0("./Quotes/",stock,"_TR.txt")))[,c(1,5)]
  returns=diff(log(stock[,2]))
  stock=stock[-1,]
  stock[,1]=as.Date(stock[,1])
  dates=stock[,1]
  stock=as.xts(stock[,2],order.by=dates)
  returns=as.xts(returns,order.by=dates)
  stock=merge(stock,returns)
  


  ## Clacullate annualized stock volatility on a 250 day rolling window
  window=250
  length=dim(stock)[1]
  sigmas=returns
  sigmas[,1]=0
  stock=merge(stock,sigmas)
  for(y in 1:(length-window)){
    start=y
    end=start+window-1
    temp=stock[start:end,2]
    mu=mean(temp)
    sigma=sd(temp)
    hist_vol=sqrt(window) * sigma
    stock[end+1,3]=hist_vol
    } 
  colnames(stock)=c("Close","Return","Volatility")

  ### Second Part - Financial Debt Calculation
  financials=as.data.frame(read_excel(paste0("./New_Financials/",tickers[z],".xlsx")))
  dates=financials[financials[,1]=="End of Period",][1,]
  dates=as.character(dates[,2:21])
  dates=as.Date(dates,format="%d.%m.%Y")
  short_term_loans=financials[financials[,1]=="ST Financial Debt",][1,]
  short_term_loans=as.xts(as.numeric(short_term_loans[,2:21]),order.by=dates)
  long_term_loans=financials[financials[,1]=="LT Financial Debt",][1,]
  long_term_loans=as.xts(as.numeric(long_term_loans[,2:21]),order.by=dates)
  debt=short_term_loans+  long_term_loans * 0.5

  data=merge(stock,debt)
  check=which(is.na(data$Close))
  data$debt[(check+1)]=data$debt[check]
  data=data[!is.na(data$Close),]
  data=data[!is.na(data$debt),]
  data$mcap=data$Close*BIST30[z,4]/1000000
  data=merge(data,Interest, join="left")
  colnames(data)[6]="rfrate"

  data$timespan=1






### Part III - Define KMV Function to calculate asset volatility from equity volatility 
## and to calculate asset value from market capitalization and asset volatility
new_data=data.frame(date=index(data), coredata(data))
temp=new_data[,c(1,6,4,5,7,8)]
colnames(temp)=c("Date","market_capitalization","annual_volatility","defaultable_debt",
                 "risk_free_rate","time_span")
index=1
kmvfunc=function(Z)
{ 
  A0=Z[1]
  sigA=Z[2]
  y=index
  
  D=temp$defaultable_debt[y]
  r=temp$risk_free_rate[y]
  T=temp$time_span[y]
  
  d1=(log(A0/D)+(r+0.5*sigA^2)*T)/(sigA*sqrt(T))
  d2=d1-sigA*sqrt(T)
  
  
  E0=A0*pnorm(d1)-D*exp(-r*T)*pnorm(d2)
  sigE=pnorm(d1)*sigA*A0/E0
  
  error=(E0/temp$market_capitalization[y]-1)^2 + (sigE/temp$annual_volatility[y]-1)^2
  
  return(error)
}


## Create an initialization data frame for each date
x0=cbind(temp$defaultable_debt+temp$market_capitalization,
     temp$annual_volatility*temp$market_capitalization/(temp$defaultable_debt+temp$market_capitalization))

asset_value=c()
asset_volatility=c()
## Calculate asset value and volatility  for each quarter from 2016-4 to 2021-3 
## by optimizing kmv function
for( x in 1:20){
  index=x
  optimized_values=optim(x0[index,],kmvfunc)
  print(c(x,optimized_values$par))
  asset_value=c(asset_value,optimized_values$par[1])
  asset_volatility=c(asset_volatility,optimized_values$par[2])
}

data$asset_value=asset_value
data$asset_volatility=asset_volatility




### Part IV - Calculate metrics for each stock in each reporting quarter
data$d1=(log(data$asset_value/data$debt)+(data$rfrate+0.5*data$asset_volatility^2)*data$timespan)/(data$asset_volatility*sqrt(data$timespan))
data$d1
data$d2=data$d1- data$asset_volatility*sqrt(data$timespan)
data$d2

data$Put=data$debt*exp(-data$rfrate*data$timespan)*pnorm(-data$d2) - data$asset_value*pnorm(-data$d1)
data$Put
data$market_value_of_debt=data$debt*exp(-data$rfrate*data$timespan)-data$Put
data$market_value_of_debt


### Part V - Calculate default probability, loss given default and yield spread
data$probability_of_default=pnorm(-data$d2)  
data$probability_of_default

data$present_value_of_debt=data$debt*exp(-data$rfrate*data$timespan)
data$present_value_of_debt

data$expected_loss=data$present_value_of_debt-data$market_value_of_debt
data$expected_loss

data$loss_given_default=data$expected_loss / data$probability_of_default
data$loss_given_default

data$yield_to_market=(1/data$timespan)*log(data$debt/data$market_value_of_debt)
data$spread=data$yield_to_market-data$rfrate
data$spread

check=data.frame(date=index(data), coredata(data))
check$ticker=tickers[z]

## Add data to all_data database for each loop
all_data=rbind(all_data, check)
}

## After loop ends, write data frame to an excel file.
library(writexl)
write_xlsx(all_data,"data_new.xlsx")