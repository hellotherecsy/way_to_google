## package import 
library(shiny)
library(shinydashboard)
library(ggplot2)
library(stringr)
library(leaflet)
library(dplyr)
library(plotly)
library(gridExtra)
library(data.table)
library(tm)
library(wordcloud)
library(memoise)
library(ggwordcloud)
library(SnowballC)
library(rquery)
library(imager)
source('http://www.sthda.com/upload/rquery_wordcloud.r')

## Data import  
jobs <- read.csv("./data/google_jobs.csv", header = TRUE)
geo_info <- read.csv("./data/geo_info.csv", header = TRUE)
continent_info <- read.csv("./data/continent_info.csv", header = TRUE)

names(geo_info) <- c('X','city', 'lon', 'lat')
geo_info <- subset(geo_info, select = -c(X))
geo_info <- unique(geo_info)

jobs_map <- merge(jobs, geo_info, by= 'city',  all = TRUE) 
jobs_map <- jobs_map %>% group_by(., city,lon,lat,positions) %>% count()
names(jobs_map) <- c( 'city', 'lon', 'lat','positions','count')

jobs_map_sum <- jobs_map %>% group_by(., city,lon,lat ) %>% 
  summarise(., sum_of_jobs = sum(count))
