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

# Load stock, sector information and indicators
cf = read.csv('data/con_f.csv', stringsAsFactors = F)
con_cf = read.csv('data/_con.csv', stringsAsFactors = F)
stocks = read.csv('data/all_stocks_1yr.csv', stringsAsFactors = F)

spy_temp = read.csv('data/SPY.csv', stringsAsFactors = F)
spy = spy_temp %>%
  select(., -(Adj.Close))
spy$Name = rep('SPY', nrow(spy))

sector_data = read.csv('data/sector_data.csv')

# Make stocks with Sector
temp = con_cf %>% 
  mutate(., name = gsub('Industrials', 'XLI', x = Sector, fixed = T)) %>%
  mutate(., name = gsub('Health Care', 'XLV', x = name, fixed = T)) %>%
  mutate(., name = gsub('Information Technology', 'XLK', x = name, fixed = T)) %>%
  mutate(., name = gsub('Consumer Staples', 'XLP', x = name, fixed = T)) %>%
  mutate(., name = gsub('Energy', 'XLE', x = name, fixed = T)) %>%
  mutate(., name = gsub('Financials', 'XLF', x = name, fixed = T)) %>%
  mutate(., name = gsub('Materials', 'XLB', x = name, fixed = T)) %>%
  mutate(., name = gsub('Real Estate', 'XLRE', x = name, fixed = T)) %>%
  mutate(., name = gsub('Utilities', 'XLU', x = name, fixed = T)) %>%
  mutate(., name = gsub('Consumer Discretionary', 'XLY', x = name, fixed = T)) %>%
  mutate(., name = gsub('Telecommunications Services', 'VOX', x = name, fixed = T))
temp = temp %>% 
  select(., Name = Symbol, Sector = name)
stocks_w_sec = stocks %>% 
  left_join(., temp, by = "Name")
stocks_w_spy = stocks_w_sec %>% 
  select(., -c(Sector)) %>% 
  rbind(., spy)
write.csv(stocks_w_sec, file = 'data/stocks_w_sec.csv',row.names = F)