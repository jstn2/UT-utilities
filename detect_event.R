# .............................................................
# UT Water and Energy smart meter hourly data analysis
# .............................................................
# Version 2
# .............................................................
# Written by Justin DuRant
# 2020
# Mark and add events in data
# try to detect events 
# make confusion matrix 
# Calculate matthew correlation coef
# Make ROC curves
# Visualize events and detection alarms
# .............................................................

rm(list=ls())

# Set these values to match your plotly account if you want to host the plots
# on the plotly website
Sys.setenv("plotly_username"= " ")
Sys.setenv("plotly_api_key"= " ")

# # If you see "Error: The orca command-line utility is required
# # for this functionality." and you already followed installation instructions, uncomment below.
# # Replace the path (C:\\...) with the appropriate path for your machine. It might be similar to
# # this example or different
# Sys.setenv("PATH" = paste(Sys.getenv("PATH"), "C:\\Users\\ (usrname) \\AppData\\Local\\Programs\\orca", sep = .Platform$path.sep))

# When orca fails to connect to ... port because connection refused, run this 
# command to kickstart it:  orca_serve()
# This error often happens if it is opening for the first time from deep within 
# a function because it doesn't wait long enough for initialization


# Suppress automatic opening of plot in web browser when running api_create()
options(browser = 'false')

library(tidyverse)
library(data.table)
library(mltools) # for MCC
library(caret)   # for confusionMatrix
library(plotly)
library(htmlwidgets)      # For saving interactive plots as html
library(randomForest)
library(manipulateWidget) # For making panels of interactive plots. Can't save with orca.
library(raster)  # rowSums
library(orca)
library(zoo)     # for rollapply
library(TSclust) # for SAX function
library(tsmp)    # MASS function (fast similarity search)


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

# Point to the folder where data is stored
data_dir <- paste0(getwd(),"/data")

load(file.path(data_dir, "hourly_CEHW.RData"))


#load("models.RData")
# save some time by only loading rfmods
load(file.path(data_dir, "rf_mods701.RData"))
rf_mods <- rf_mods701

# set to TRUE to export static images of plots 
save_img <- TRUE
save_img <- FALSE

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

# directory to save tables and figures
tab_dir <- paste0(getwd(),"/table")
fig_dir <- paste0(getwd(),"/fig")
if(!dir.exists(tab_dir)){dir.create(tab_dir)}
if(!dir.exists(fig_dir)){dir.create(fig_dir)}

# Adjust sizing of the pdf output. default layout is 7.35"x5.32" so 
# text may appear too small.
fig_scale <- 0.5

# Adjust sizing of svg output
fig_width=417
fig_height=300

# Point to the functions that are stored in a separate script
src_dir <- getwd()
source(file=file.path(src_dir,'detect_event_function.R'))
source(file=file.path(src_dir,'plot_functions.R'))



# /////////////////////////////////////////////////////
# Add fake events
# /////////////////////////////////////////////////////

# Make "event" variables in the df and mark technical errors based on 
# the earlier data cleaning 

for (i in 1:length(dfs_tot)){
  dfs_tot[[i]] <- mark_events(dfs_tot[[i]], dfs_clean[[i]])
}

### See if error exceeds threshold ###
# k is number of standard errors for different error types 
# threshold = median(error[(i-len_hrs):i]) + k*std_error(error[(i-len_hrs):i])
# k <- c(elec-val, cool-val, heat-val, water-val)

#k <<- c(15,15,13,13)
k <<- c(0,0,0,0)
k_abs <- k
k_sq  <- k
k_sca <- k
k_ed  <- k

# length of window for threshold calculations
#len_days <<- 20
len_days <<- 35
len_hrs <- 24*len_days 
#len_hrs <- 12

# length (hours) for moving average of point errors
avg_hrs <- 2
# length of window for euclidean distance calculation
ED_hrs <- 4

# duration of synthetic events to inject
min_eventdays <- 7
max_eventdays <- 14

min_eventlen <<- 24*min_eventdays
max_eventlen <<- 24*max_eventdays

# Percentile of the time series to use for average event size
perc <<- 0.95

# The number of days to wait before triggering an "alarm" state from the
# synthetic events. This variable will be used in the PRC curves and other eval
# as the "true event" state for comparison with the detected events. For
# example, if action_len is set to one day, then this variable will be zero for
# the first day of the event and one for the rest of the event. If this length
# is longer than min_eventdays, the fuction that adds synthetic events will
# break.
action_len <<- 5


# /////////////////////////////////////////////////////////////////////////////
# Add synthetic events to the data one utility at a time, make rf predictions,
# detect events for that one parameter, save important stuff and loop through
# other utilities
# /////////////////////////////////////////////////////////////////////////////


switches <<- c("elec","cool","heat","water")
num_events <<- 9 # How many events to add

# all_event function is in detect_event_function.R

df_preds <- list()
for (i in 1:length(dfs_tot)){
  df_preds[[i]] <- all_event(dfs_tot[[i]], rf_mods[[i]])
}

# Look at plot of events and data/prediction
for (i in 1:length(df_preds)){
  plot_events_rf_3(df_preds[[i]], bldg_names[i])
}
 

# require certain frequency of events (time_frac) over a longer period (action_len)
# and from a certain amount of error models (mod_frac) before raising an alarm

mod_frac <- .7
time_frac <- .95
action_len <- 5   # days


df_check <- list()
for (i in 1:length(df_preds)){
  df_check[[i]] <- post_process_events(df_preds[[i]], action_len, time_frac, mod_frac)
}


# Show the true state, individual error flags, and final alarm from post-processing
for (i in 1:length(df_check)){
  action_heatmap(df_check[[i]], bldg_names[i])
}


# integrated detection barplot (Intermediate step, marginally useful)
plot_combined(df_check[[1]], bldg_names[1])


## Plot the results (events, error, threshold) without post-processing ###
for (i in 1:length(df_check)){
  plot_events_rf_2(df_check[[i]], bldg_names[i])
}

## ////////////////////////////////////////////////////////////
# Calculate MCC, TPR and FPR for all models, export tables
## ////////////////////////////////////////////////////////////

conf_process <- function(x,y){
  temp <- mcc_and_conf(x,y) # Get confusion matrix and MCC
  MCC <- temp[[1]]
  FPR <- temp[[2]][2]/sum(temp[[2]][c(1,2)]) # FP/FP+TN = FP/N
  TPR <- temp[[2]][4]/sum(temp[[2]][3:4])    # TP/TP+FN = TP/P
  PPV <- temp[[2]][4]/sum(temp[[2]][c(2,4)]) # TP/(TP+FP) = TP/(positive calls)
  res <- list(MCC,FPR,TPR, PPV)
  return(res)
}

res_extract <- function(data, stat_list, name){
  dt <- data.table(mcc = sapply(stat_list, "[[", 1), fpr = sapply(stat_list, "[[", 2), 
                   tpr = sapply(stat_list, "[[", 3), ppv = sapply(stat_list, "[[", 4))
  new_names <- c(paste0(name,"_mcc"),paste0(name,"_fpr"),paste0(name,"_tpr"), paste0(name,"_ppv"))
  names(dt) <- new_names
  
  data <- cbind(data,dt)
  return(data)
}

# save MCC, FPR, TPR, PPV for each utility in each building
eval_tables <- function(df){
  C_act <- conf_process(df$C_action_pred,df$C_action) # get MCC, FPR, TPR, PPV
  E_act <- conf_process(df$E_action_pred,df$E_action)
  H_act <- conf_process(df$H_action_pred,df$H_action)
  W_act <- conf_process(df$W_action_pred,df$W_action)
  
  # C_act <- conf_process(df$C_action_pred,df$C_event) # get MCC, FPR, TPR, PPV
  # E_act <- conf_process(df$E_action_pred,df$E_event)
  # H_act <- conf_process(df$H_action_pred,df$H_event)
  # W_act <- conf_process(df$W_action_pred,df$W_event)
  
  utils <- c("Cool","Elec","Steam","Water")
  result <- data.table(utility=utils, se_mult=k )
  
  sort_vals <- function(list){
    MCC <- numeric()
    FPR <- numeric()
    TPR <- numeric()
    PPV <- numeric()
    for (i in 1:length(list)){
      MCC[i] <- list[[i]][[1]]
      FPR[i] <- list[[i]][[2]]
      TPR[i] <- list[[i]][[3]]
      PPV[i] <- list[[i]][[4]]
    } 
    return(list(MCC,TPR,FPR,PPV))
  }
  
  all_action <- list(C_act, E_act, H_act, W_act)
  sorted_vals <- sort_vals(all_action)
  
  result[, `:=` (MCC=sorted_vals[[1]], TPR=sorted_vals[[2]], 
                 FPR=sorted_vals[[3]], PPV=sorted_vals[[4]]) ] 
  
  return(result)
}

# ecj_result <- eval_tables_2(ECJcheck3)
# gdc_result <- eval_tables_2(GDCcheck3)
# jcd_result <- eval_tables_2(JCDcheck3)

all_tabs <- list()
for (i in 1:length(df_check)){
  all_tabs[[i]] <- eval_tables(df_check[[i]], bldg_names[i])
}


wd <- getwd()
setwd(fig_dir)
file_name = "eval_tables.csv"
sink(file_name)
for (j in 1:length(all_tabs)){
  cat(bldg_names[j], sep="\n", append=TRUE)
  write.csv(all_tabs[[j]]) 
}
sink()
setwd(wd)






