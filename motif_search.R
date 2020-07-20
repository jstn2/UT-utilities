#######################################################################################################
#
# motif_search.R
# Written by Connor Chewning
# 07/06/2020
#
# This code takes an input time series of building data and allows the user to select a reference time 
#   series (also refered to as a motif) based on a start date and duration of the pattern. 
#   A seperate vector may also be inputed as the motif.
#
# MASS_v2, standard euclidean distance, and symbolic approximation search functions are then used to 
# find similar patterns to the motif given an error threshold set by the user.
#
#######################################################################################################


################################## import packages and set up script ##################################
library(tidyverse)
library(orca)
library(ggplot2)
library(plotly)
library(plyr)
library(dplyr)
library(tsmp)    # for mass search
library(TSclust) # for sax search
# Make this file's location the working directory
# Finds the current file's location
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

# Point to the folder where data, tables and figures are stored
data_dir <- paste0(getwd(),"/data")
tab_dir <- paste0(getwd(),"/table")
fig_dir <- paste0(getwd(),"/fig")

# if folder for tables and figures does not exist, then create them
if(!dir.exists(tab_dir)){dir.create(tab_dir)}
if(!dir.exists(fig_dir)){dir.create(fig_dir)}

# Import Clean Building Data
load(file.path(data_dir, "hourly_CEHW.RData"))


######################################## Set Parameters ###############################################

# set to TRUE to export static images of plots 
save_img <- TRUE
save_img <- FALSE

# set to TRUE to show plots in RStudio plots or viewer window
viewer <- TRUE
#viewer <- FALSE

# set to TRUE to upload and host plots on plotly account.
# This requires username and api_key to be set in your system environment.
upload <- TRUE
upload <- FALSE

# Set these values to match your plotly account if you want to host the plots
# on the plotly website
Sys.setenv("plotly_username"= " ")
Sys.setenv("plotly_api_key"= " ")

# set to TRUE to export html files of plotly plots 
save_html <- TRUE
#save_html <- FALSE

# Set to true to select motif from time series
# Set to false to set motif from loaded .csv file
select_motif <- TRUE
#select_motif <- FALSE

# If selecting motif from data set, define the start date and duration of motif in days
# the time provided must be present within the time series. You can check this within the 
# dfs_clean variable
motif_start <- "2018-08-01 12:00:00" # date yyyy-mm-dd hh:mm:ss
motif_duration <- 5 # days

# If loading motif add file name below and ensure that it is in the data folder
# This file must be a .csv with one column of values and a header
motif_file_name <- "pattern.csv"

# set the size of the window for the SAX search
# this is how many equal sized frames that the series will be reduced to along the x-axis
window_size <- motif_duration*24

# set the size of the alphabet for the SAX search
# This is the amount of symbols used to represent the values of the series along the y-axis.
# By default this is equal to the length of the motif. Change to appropriat evalue if loading motif from .csv
alphabet_size <- 10

# set how many standard deviations to remove from matric profile in search (see approx. line 317 in perforSearch function)
standard_dev <- 1

# set error thresholds for searches. These are normalized so values must be between 0 and 1.
#          <- c(MASS_v2, ed_std, ed_SAX)
thresholds <- c(0.026,   0.15,   0.024) 

# Set name of Building (Three letter building ref according to imported data)
building_name <- "ECJ"

# Set name of utility (EX: "Elec", "Cool", "Heat", "Water" ~ according to imported data)
utility <- "Elec"

# Set the units of the utility
units <- "KwH"

# Coerce control variables to logical
if (class(save_img) != "logical")       save_img <- FALSE
if (class(viewer) != "logical")         viewer <- FALSE
if (class(upload) != "logical")         upload <- FALSE
if (class(save_html) != "logical")      save_html <- FALSE
if (class(select_motif) != "logical")   select_motif <- TRUE


############################## Functions in order of reference ################################

# Sets the input time series based on user preference
setTimeSeries <- function(utility, building, building_names, dfs){
  
  # mark where in the data frame the building is based on the name list
  position <- match(building, building_names)
  
  # grab the times series
  TimeSeries <- dfs[[position]][[utility]]
  
  # garb dates from time series
  DateTime <- dfs[[position]][["DateTime"]]
  
  return(data.frame(DateTime, TimeSeries))
}

# Sets the motif based on user preference
setMotif <- function(ts, select, file_name, start, duration){
  
  start <- match(as.Date(start), as.Date(ts$DateTime))
  
  ts <- ts$TimeSeries
  
  motif <- c()
  
  if(select){
    # then create the vector based on start and length from the time series
    
    # check to make sure that it is shorter than the time series
    if( duration*24 > length(ts) ){
      print("Duration of motif is too long.")
    }
    else{
      motif <- ts[start:(start+(duration*24))]
    }
  }
  else{ # create the motif from the vector
    
    # read csv file
    vector <- read_csv(file.path(data_dir, file_name))[[1]]

    # check to make sure that it is shorter than the time series
    if( length(vector) > length(ts) ){
      print("Duration of imported motif is too long.")
    }
    else{
      motif <- vector
    }
  }
  return(motif)
}

# Pattern detection with standard euclidean distance
ed_std <- function(x,sig_stream){

  #standartize the time series
  #x <- (x - mean(x))/sd(x) # comment out if don't want to standartize
  #sig_stream <- (sig_stream - mean(x))/sd(x) # comment out if don't want to standartize
  #--------------------------------------------------------------
    
  # length of sub-series in time steps
  sig_len <- length(sig_stream)
  
  # vector to store the ED
  ED <- numeric(length(x) - sig_len + 1)             
  
  # get Euclidean distance between the time series and selected signature
  for(i in 1:(length(x) - sig_len + 1)){
  
    # get substream of length sig_len
    sub_stream <- x[i : (i + sig_len - 1)]
    
    # get distance
    #diff <- as.numeric(sub_stream - sig_stream)
    #diff <- cbind(sub_stream, sig_stream)
    ED[i] <- sqrt(sum((sub_stream-sig_stream)^2))
  }
  return(ED)
}

# Pattern detection with SAX (symbolic aggregate approximation) uses diss.MINDIST.SAX()
ed_sax <- function(x, y, w, a){
  # Parameters: 
  # x <- main data stream
  # y <- pattern to be compared (must be shorter in length than x)
  # w <- the amount of equal sized frames that the series will be reduced to during SAX calculation
  # a <- size of alphabet, The size of the alphabet, the amount of symbols used to represent the values of the series.
  
  # make empty vector to hold the distance
  ed <- integer(length(x)-length(y))
  
  # loop through x (main data stream)
  continue <- TRUE
  ind <- 1
  while (continue){
    
    # select substream
    sub_stream <- x[ind:(ind+length(y)-1)]
    
    # Compute SAX and place in ed vector
    ed[ind] <- diss.MINDIST.SAX(sub_stream, y, w, alpha = a)
    
    # check to see index plus pattern (y) length exceeds main data stream (x) length
    if( (length(x[1:ind]) + length(y)) > length(x) ){
      continue <- FALSE
    }
    ind <- ind + 1
  }
  ed
}

# implement search algorithms to find errors across time series
performSearch <- function(df, y, w, a, s){
  #s is how many std to remove from MP
  
  span <- length(y)
  
  # divide up data frame into time series and date
  date <- df$DateTime
  x <- df$TimeSeries
  
  # remove NA values
  x <- ifelse(is.na(x),0,x)
  y <- ifelse(is.na(y),0,y)
  
  # run each search type and divide by sqrt(n)
  err_v2 <- dist_profile(x, y, method = "v2")[[1]] / sqrt(span)
  err_ed <- ed_std(x,y) / sqrt(span)
  err_sax <- ed_sax(x, y, w, a) / sqrt(span)
  
  # Remove complex numbers from errors (takes just the real parts)
  err_v2 <- Re(err_v2)
  
  # combine all error into a single data frame
  # err_all <- matrix( c(err_v2, err_abs, err_sax), nrow=length(err_v2), ncol=3)
  err_all <- data.frame(date[1:length(err_v2)], x[1:length(err_v2)], err_v2, err_ed, err_sax)
  
  # make list of names for reference when plotting and checking
  colnames(err_all)<- c("DateTime","TimeSeries","err_v2", "err_ed","err_sax")
  
  # loop through matrix profiles and process for export
  for (j in c(3:5) ){
    
    # remove outliers
    err_all[ (err_all[,j] > (median(err_all[,j], na.rm = TRUE)) + s*sd(err_all[,j], na.rm = TRUE)) , j] <- NA
    
    # normalize all matrix profiles
    err_all[,j] <- err_all[,j] / max(err_all[,j], na.rm = TRUE)
  }
  return(err_all)
}

# plots the distance profiles
plotDistanceProfile <- function(df){
  
  fig <- plot_ly(df, x = ~DateTime, y = ~err_v2, name = 'MASS', type = 'scatter', mode = 'markers',marker = list(size = 4))%>% 
    add_trace(y = ~err_ed, name = 'Euclidean Distance', mode = 'markers')%>%
    add_trace(y = ~err_sax, name = 'Symbolic Approximation', mode = 'markers')%>% 
    layout(title = paste0("Distance Profiles for ",building_name," ",utility),
             xaxis = list(title = "Date"),
             yaxis = list (title = "Normalized Distance"))
  return(fig)
}

# save, view, or plot images depending on arguments. 
# Pass in variable from beginning of script to maintain settings throughout
processImage <- function(fig, name, save_img, viewer, upload, save_html){
  
  if(save_img){
    wd <- getwd()
    setwd(fig_dir)
    orca(fig, paste0(name,".pdf"))
    setwd(wd)
  }
  
  if(viewer){print(fig)}
  
  if(upload){
    api_create(fig, filename = name)
  }
  
  if(save_html){
    wd <- getwd()
    setwd(fig_dir)
    htmlwidgets::saveWidget(as_widget(fig), paste0(name,".html"))
    setwd(wd)
  }
}

# Identify patterns
identifyPatterns <- function(df, thresh){
   
  match_indexes <- list()
  for (j in c(1:3) ){
    # make a list of vectors that holds the indexes of all 
    match_indexes[[j]] <- which(df[,j+2] < thresh[j])
  }
  return(match_indexes)
}

# Plot the time series with highlighted portions indicating where the patterns found are
plotSeriesAndPatterns <- function(df, matches){
  
  # remove outliers for better visualization (comment out to use raw cleaned data from data_cleaner.R)
  s=3 #standard deviations to remove
  df$TimeSeries[which(df$TimeSeries > (median(df$TimeSeries, na.rm = TRUE)) + s*sd(df$TimeSeries, na.rm = TRUE))] <- NA
  
  # list of search names for plot
  search_list <- c("Mass","Euclidean Distance","Symbolic Approximation")
  
  #make a list to hold the plots for each search function
  fig_list <- list()
  
  # loop through for each search
  for (j in 1:3){
    
    # make list for shapes (highlighted sections of the chart)
    shape_list <- list()
    
    # check and see if there are any matches for this search, if not then leave shape_list empty
    if( is.na(matches[[j]][1]) == FALSE ){
    
      # allocate shapes to list
      for (i in 1:length(matches[[j]])){
        
        strt_indx = matches[[j]][i]
        
        if((matches[[j]][i]+motif_duration) < length(df$DateTime)) {end_indx = matches[[j]][i]+motif_duration}
        
        else{end_indx = length(df$DateTime)}
        
        shape_list[[i]] <- list(type = "rect", fillcolor = "darkseagreen", line = list(color = "darkseagreen"), opacity = 0.3,
                                x0 = df$DateTime[strt_indx], 
                                x1 = df$DateTime[end_indx], 
                                xref = "x",
                                y0 = 0, 
                                y1 = max(df$TimeSeries, na.rm = TRUE), 
                                yref = "y")
        
      }
    }
    
    # decide if to show y axis or not in plots based on which turn it is
    if(j == 2){axis_label <- paste0(utility, " in ",units)}
    else{axis_label <- ""}
    
    # make individual figures
    fig <- plot_ly(df, x = ~DateTime, y = ~TimeSeries, name = paste0(search_list[j]), type = 'scatter', mode = 'lines')%>% 
      layout(title = paste0("Time Series for ",building_name," ",utility, " with Matches Marked"),
             xaxis = list(title = "Date"),
             yaxis = list (title = axis_label),
             shapes = shape_list)
    
    
    # add figures to the list
    fig_list[[j]] <- fig
    
  }
  
  # make subplot with all three figures
  fig <- subplot(fig_list,nrows=3,shareX=TRUE, titleY=TRUE)
  
  return(fig)
}

# Plot all the patterns in an overlay with motif plotted last and on top
plotPatterns <- function(df, matches, motif){
  
  # list of search names for plot
  search_list <- c("Mass","Euclidean Distance","Symbolic Approximation")
  
  # make fig_list
  fig_list <- list()
  
  # loop through for each search
  for (j in 1:3){
    
    # create data frame containing the patterns found
    patterns <- matrix(, nrow = motif_duration, ncol = length(matches[[j]]))
    
    # check to see if the patterns are null and skip if so
    if( is.na(matches[[j]][1]) == FALSE ){

      # go through and grab each pattern and add to data frame
      for (i in 1:length(matches[[j]])){
        
        # determine start and end points of the patterns
        strt_indx = matches[[j]][i]
        if((matches[[j]][i]+motif_duration)<length(df$DateTime)){end_indx = matches[[j]][i]+motif_duration-1}
        else{end_indx = length(df$DateTime)}
        
        # add to df
        patterns[1:(end_indx-strt_indx+1),i] <- df$TimeSeries[strt_indx:end_indx]
        
      }
    
    
      #turn patterns into a dataframe
      patterns <- as.data.frame(patterns)
      
      # add a count variable to data frame for plotr
      patterns['x'] <- c(1:length(patterns[,1]))
      
      # decide if to show legend or not in plots based on which turn it is
      if(j == 1){legend <- TRUE}
      else{legend <- FALSE}
      
      # decide if to show y axis or not in plots based on which turn it is
      if(j == 2){axis_label <- paste0(utility, " in ",units)}
      else{axis_label <- ""}
      
      # initialize figure
      y_data <- paste0(df$DateTime[matches[[j]][1]])
      fig1 <- plot_ly(patterns, x = ~x/24, y = ~V1, type = 'scatter', name = "Matches", mode = 'lines',line = list(color = "darkseagreen", width = 1), showlegend = legend)
      
      if (length(matches[[j]]) > 1){
        # loop through to add traces to plot
        for (i in 2:(length(matches[[j]])-1)){
        
          y_data <- paste0(df$DateTime[matches[[j]][i]])
          fig1 <-  fig1 %>% add_trace(fig1, y = patterns[,i], name = y_data, line = list(color = "darkseagreen", width = 1), showlegend = FALSE)
          
        }
      }
      
      # add true motif
      fig1 <-  fig1 %>% add_trace(fig1, y = motif, name = "Motif", line = list(color = "cornflowerblue", width = 5), showlegend = legend)
      
      fig1 <- fig1 %>% layout(title = paste0("Matched Patterns for ",building_name," ",utility),
                              xaxis = list(title = "Duration in Days"),
                              yaxis = list(title = axis_label))
      fig1 <- fig1 %>% add_annotations(
                                      text = search_list[j],
                                      x = 0.5,
                                      y = 1.05,
                                      yref = "paper",
                                      xref = "paper",
                                      xanchor = "middle",
                                      yanchor = "top",
                                      showarrow = FALSE,
                                      font = list(size = 10)
      )
      
    }
    else{ # there are no matches so just plot the pattern
      
      #turn patterns into a dataframe
      patterns <- data.frame(x = c(1:length(motif)), motif = motif)
      
      fig1 <- plot_ly(patterns, x = ~x/24, y = ~motif, type = 'scatter', name = "Motif", mode = 'lines',line = list(color = "cornflowerblue", width = 5)) %>% 
      layout(title = paste0("Matched Patterns for ",building_name," ",utility),
                 xaxis = list(title = "Duration in Days"),
                 yaxis = list(title = paste0(utility, " in ",units)),
                 showlegend = FALSE)
      
    }
      
    fig_list[[j]] <- fig1
     
  }
  
  # make subplot with all three figures
  fig <- subplot(fig_list,nrows=3,shareX=TRUE, titleY=TRUE)
  
  return(fig)
  
}


###################################### Main Code  ##########################################

# Set the time series based on building and utility info from cleaned data
time_series <- setTimeSeries(utility, building_name, bldg_names, dfs_clean)

# Set the motif based on the user preferences of loading or selectiong motif from time series
motif <- setMotif(time_series, select_motif, motif_file_name, motif_start, motif_duration)
motif_duration <- length(motif)

# Perform Searches and Retrive errors
errors <- performSearch(time_series, motif, window_size, alphabet_size, standard_dev)

# plot distance proflies
distance_plot <- plotDistanceProfile(errors)
processImage(distance_plot, paste0("distance_plot_",building_name,"_",utility), save_img, viewer, upload, save_html)

# Use thresholds to identify similar patterns
matches <- identifyPatterns(errors, thresholds)

# Plot patterns and provide information on errors
time_series_plot <- plotSeriesAndPatterns(errors, matches)
processImage(time_series_plot, paste0("time_series_plot_",building_name,"_",utility), save_img, viewer, upload, save_html)

# make plots of patterns overlaid
pattern_plot <- plotPatterns(errors, matches, motif)
processImage(pattern_plot, paste0("pattern_plot_",building_name,"_",utility), save_img, viewer, upload, save_html)


