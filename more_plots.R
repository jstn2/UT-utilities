# .......................................
# 11/19
# Plots of data at different scales
# Visualize random forest results
# .......................................

rm(list=ls())

library(rpart)
library(lubridate)
library(ggplot2)
library(plyr)
library(leaps)
library(stringr)
library(randomForest)
library(dygraphs)
library(data.table)
library(plotly)
library(dplyr)


# Set these values if you want to upload to your plotly account
Sys.setenv("plotly_username"=" ")
Sys.setenv("plotly_api_key"=" ")

# set to TRUE to export static images of plots 
save_img <- TRUE
#save_img <- FALSE

# set to TRUE to show plots in RStudio plots or viewer window
viewer <- TRUE
#viewer <- FALSE

# set to TRUE to upload and host plots on plotly account.
# This requires username and api_key to be set in your system environment.
#upload <- TRUE
upload <- FALSE


# Coerce control variables to logical
if (class(save_img) != "logical") save_img <- FALSE
if (class(viewer) != "logical")   viewer <- FALSE
if (class(upload) != "logical")   upload <- FALSE

library(tidyverse)
getCurrentFileLocation <-  function(){
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file)==0)
  {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

setwd(getCurrentFileLocation())

getwd()
# *^ check to make sure it is the correct path (where the script is stored)

data_dir <- paste0(getwd(),"/data")

load(file.path(data_dir, "hourly_CEHW.RData"))

# Location where exported figures will be saved (create folder if it doesn't exist)
fig_dir <- paste0(getwd(),"/fig")
if(!dir.exists(fig_dir)){dir.create(fig_dir)}


# Adjust sizing of the pdf output. The default layout is 7.35"x5.32" so 
# text may appear too small.
fig_scale <- 0.5

# Size for svg graphics
fig_width=417
fig_height=300



# Point to the functions that are stored in a separate script and 
# bring the functions into the workspace
src_dir <- getwd()
source(file=file.path(src_dir,'plot_functions.R'))


# More cleaning for the plots
extra_clean <- function(df){
  df[830,2:4] <- NA
  df[848:849,2:4] <- NA
  df[2018,2:4] <- NA
  df
}
# ECJc <- extra_clean(ECJc)
# GDCc <- extra_clean(GDCc)
# JCDc <- extra_clean(JCDc)
# dailyECJ <- make_daily(ECJc)
# dailyJCD <- make_daily(JCDc)
# dailyGDC <- make_daily(GDCc)
# dailyECJt <- make_daily(ECJtot)
# dailyJCDt <- make_daily(JCDtot)
# dailyGDCt <- make_daily(GDCtot)

daily_c <- list()
daily_t <- list()

for (i in 1:length(dfs_clean)){
  dfs_clean[[i]] <- extra_clean(dfs_clean[[i]])
  daily_c[[i]] <- make_daily(dfs_clean[[i]])
  daily_t[[i]] <- make_daily(dfs_tot[[i]])
}

# Make plots of whole year of daily data, one building per plot with all
# utilities. 
for (i in 1:length(daily_t)){
  #make_plot(daily_t[[i]], bldg_names[i])
}


# Combine all buildings into a single table for each parameter
rearrange <- function(datalist){
  for (i in 1:length(datalist)){
    if (i==1){
      colname <- bldg_names[i]
      cool <- data.table(DateTime=datalist[[i]]$DateTime, datalist[[i]]$Cool)
      names(cool)[i+1] <- colname
      
      elec <- data.table(DateTime=datalist[[i]]$DateTime, datalist[[i]]$Elec)
      names(elec)[i+1] <- colname
      heat <- data.table(DateTime=datalist[[i]]$DateTime, datalist[[i]]$Heat)
      names(heat)[i+1] <- colname
      water <- data.table(DateTime=datalist[[i]]$DateTime, datalist[[i]]$Water)
      names(water)[i+1] <- colname
    } else {
      colname <- bldg_names[i]
      cooltemp <- data.table(DateTime=datalist[[i]]$DateTime, datalist[[i]]$Cool)
      cool <- merge(x = cool, y = cooltemp, by = "DateTime", all = TRUE)
      names(cool)[i+1] <- colname
      
      electemp <- data.table(DateTime=datalist[[i]]$DateTime, datalist[[i]]$Elec)
      elec <- merge(x = elec, y = electemp, by = "DateTime", all = TRUE)
      names(elec)[i+1] <- colname
      
      heattemp <- data.table(DateTime=datalist[[i]]$DateTime, datalist[[i]]$Heat)
      heat <- merge(x = heat, y = heattemp, by = "DateTime", all = TRUE)
      names(heat)[i+1] <- colname
      
      watertemp <- data.table(DateTime=datalist[[i]]$DateTime, datalist[[i]]$Water)
      water <- merge(x = water, y = watertemp, by = "DateTime", all = TRUE)
      names(water)[i+1] <- colname
    }
  }
  result <- list(cool=cool, elec=elec, heat=heat, water=water)
  return(result)
}

hourly_all <- rearrange(dfs_clean)
#daily_all <- rearrange(daily_t)
daily_c_all <- rearrange(daily_c)



# Plot daily data in each building by parameter
make_plot2(daily_c_all[["cool"]], "Chilled Water", "[Ton-Hrs]")
make_plot2(daily_c_all[["elec"]], "Electricity", "[kWh]")
make_plot2(daily_c_all[["heat"]], "Steam", "[Lb]")
make_plot2(daily_c_all[["water"]], "Water", "[Gal]")

# Plot daily intensity (per sqft) sorted by parameter
# Vector of gross square footage in each building
bldg_area <- data.table(building=bldg_names, sqft=c(239644.0, 239778.0, 745413.0))

adjust = 1
make_plot3(daily_c_all[["cool"]], "Chilled Water", "(Ton-Hrs/sq.ft.)",bldg_area[[2]])
make_plot3(daily_c_all[["elec"]], "Electricity", "(kWh/sq.ft.)",bldg_area[[2]])
make_plot3(daily_c_all[["heat"]], "Steam", "(Lb/sq.ft.)",bldg_area[[2]])
make_plot3(daily_c_all[["water"]], "Water", "(Gal/sq.ft.)",bldg_area[[2]])

# Hourly intensity for the whole year
make_plot3(hourly_all[["cool"]], "Cool_hourly_persqft", "(Ton-Hrs/sq.ft.)",bldg_area[[2]], 0.5)
make_plot3(hourly_all[["elec"]], "Elec_hourly_persqft", "(kWh/sq.ft.)",bldg_area[[2]], 0.5)
make_plot3(hourly_all[["heat"]], "Heat_hourly_persqft", "(Lb/sq.ft.)",bldg_area[[2]], 0.5)
make_plot3(hourly_all[["water"]], "Water_hourly_persqft", "(Gal/sq.ft.)",bldg_area[[2]], 0.5)



# /////////////////////////////////////////////////////
# Make plot of total annual building usage for the whole time period
# /////////////////////////////////////////////////////

# This function divides by the length of the time series (end date - start date)
# so it is assuming that there are no significant gaps in the data
ann_tots <- ann_sum_all(dfs_tot)

# vector of building areas for normalization
sqft <- bldg_area[[2]]

plot_tot_usage(ann_tots, sqft) 


