#https://www.kaggle.com/NUFORC/ufo-sightings

library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

library(tidyverse)
library(lubridate)
library(tsibble)
library(feasts)
library(fable)
library(dplyr)
library(data.table)

ufo <- read.csv("scrubbed.csv")
timey <- strptime(ufo$datetime,format="%m/%d/%Y")
ufo$datetime <- as.Date(timey,format="%Y-%m-%d")

plot(ufo$country)
plot(ufo$state)

summary(ufo$country)
sort(summary(ufo$state))
summary(ufo$shape)

as_tsibble(ufo, key = c(state, country, shape), index = datetime)

ufo <- data.table(ufo)
ufoyear <- ufo[, .N, by=year(datetime)]
ufoyear <- as_tsibble(ufoyear, key = N, index = year)

ggplot(filter_index(ufoyear, "1945" ~ "2013")) +
  geom_bar(aes(year, N), 
           position = "dodge", stat = "summary", fun.y = "mean") +
  labs(title="Yearly UFO sightings 1945 - 2013",
       x="Year",y="UFO sightings")

ufomonth <- ufo[, .N, by=.(year(datetime), month(datetime))]
#ufomonth$month <- month.abb[ufomonth$month]
ufomonth$time <- paste(ufomonth$year, ufomonth$month, sep = "-")
ufotime = subset(ufomonth, select = c(time, N))
ufotime$time <- as.Date(as.numeric(as.character(ufotime$time)), origin="1906-11")

ufotime <- as_tsibble(ufotime, key = N, index = time)

ufotime %>% autoplot(N) +
  labs(title = "Mystery time series")

