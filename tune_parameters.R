# ..........................................................................
# Make precision-recall curves for tuning the event detection parameters
#
# Justin DuRant
# ..........................................................................

library(plyr)
library(leaps)
library(stringr)
library(randomForest)
library(xts)
library(dplyr)
library(magrittr)
library(knitr)
library(data.table)
library(BBmisc)
library(plotly)
library(mltools) # for MCC
library(caret)   # for confusionMatrix
library(manipulateWidget) # For making panels of interactive plots. Can't save with orca.
library(raster)  # rowSums
library(orca)
library(zoo)     # for rollapply
library(beepr)   # for alert when a task is finished use beep()


# set to TRUE to export pdfs  or svgs of plots 
save_img <- TRUE
#save_img <- FALSE

# set to TRUE to show plots in RStudio plots or viewer window
viewer <- TRUE
viewer <- FALSE

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
load(file.path(data_dir, "rf_mods701.RData"))
rf_mods <- rf_mods701


# directory to save tables and figures
tab_dir <- paste0(getwd(),"/table")
fig_dir <- paste0(getwd(),"/fig/prc")
if(!dir.exists(tab_dir)){dir.create(tab_dir)}
if(!dir.exists(fig_dir)){dir.create(fig_dir)}

# Adjust sizing of svg output
fig_width=417
fig_height=300


# Point to the functions that are stored in a separate script
src_dir <- getwd()
source(file=file.path(src_dir,'detect_event_function.R'))
source(file=file.path(src_dir,'plot_functions.R'))

# ///////////////////////////////////////////////
# roc and prec-rec curves
# ///////////////////////////////////////////////


# Make "event" variables in the df and mark technical errors
for (i in 1:length(dfs_tot)){
  dfs_tot[[i]] <- mark_events(dfs_tot[[i]], dfs_clean[[i]])
}

# Add synthetic events to the data one utility at a time, make rf predictions, 
# detect events for that one parameter, save important stuff and loop through other utilities

switches <<- c("elec","cool","heat","water")

# How many events to add
num_events <<- 9 

# Percentile of the time series to use for average event size
perc <<- 0.95

# duration of synthetic events to inject
min_eventdays <- 7
max_eventdays <- 14

min_eventlen <<- 24*min_eventdays
max_eventlen <<- 24*max_eventdays

# length of window for threshold calculation
len_days <<- 30
len_hrs <- 24*len_days 
# length (hours) for moving average of point errors
avg_hrs <- 2 
# length of window for euclidean distance calculation
ED_hrs <- 4 

# The number of days to wait before triggering an "alarm" state from the
# synthetic events. This variable will be used in the PRC curves and other eval
# as the "true event" state for comparison with the detected events. For
# example, if action_len is set to one day, then this variable will be zero for
# the first day of the event and one for the rest of the event. If this length
# is longer than min_eventdays, the fuction that adds synthetic events will
# break.
action_len <<- 3

# Event flag post-processing:
# Require certain frequency of events (time_frac) over a longer period (action_len)
# and from a certain amount of error models (mod_frac) before raising an alarm
mod_frac <- .7
time_frac <- .95
#action_len <- 3

# Loop over these threshold values for the curves
#k_vals <- seq(-10,50,5)
k_vals <- seq(-5,45,10)
#k_vals <- seq(10,20,5)



# this function returns a List of 4 dataframes with results for cool,
# elec, heat, water. The time it takes depends on number of kvals and avgwindows
many_precrec <- function(df, mods, k_vals, avg_windows){
  all_precrec <- list()
  result_list <- list()
  
  for (j in 1:length(avg_windows)){
    avg_hrs <<- avg_windows[j]
    ED_hrs <<- avg_windows[j]
    result_list[[j]] <- prec_rec(df, mods, k_vals)
  }
  
  for (g in 1:length(avg_windows)){
    temp <- result_list[[g]]
    
    c <- temp[,grep("Cool.", names(temp)), with=FALSE]
    names(c) <- gsub("Cool.Cool","Cool",names(c))
    c$avg_window <- rep(avg_windows[g], nrow(c))
    
    e <- temp[,grep("Elec.", names(temp)), with=FALSE]
    names(e) <- gsub("Elec.Elec","Elec",names(e))
    e$avg_window <- avg_windows[g]
    
    h <- temp[,grep("Heat.", names(temp)), with=FALSE]
    names(h) <- gsub("Heat.Steam","Heat",names(h))
    h$avg_window <- avg_windows[g]
    
    w <- temp[,grep("Water.", names(temp)), with=FALSE]
    names(w) <- gsub("Water.Water","Water",names(w))
    w$avg_window <- avg_windows[g]
    
    cehw <- list(c,e,h,w)
    
    all_precrec[[g]] <- cehw
  }
  
  cool_res <- gatherlist(all_precrec, 1)
  elec_res <- gatherlist(all_precrec, 2)
  heat_res <- gatherlist(all_precrec, 3)
  water_res <- gatherlist(all_precrec, 4)
  
  results <- list(cool_res, elec_res, heat_res, water_res)
}


#avg_windows <- seq(2, 24, 2)
avg_windows <- c(2, 5)

avg_window_prc <- list()
for (i in 1:length(dfs_tot)){
  avg_window_prc[[i]] <- many_precrec(dfs_tot[[i]], rf_mods[[i]], k_vals, avg_windows)
}


# Make plot for one bldg, one utility with all the values of modified parameter
plot_prec_avgwindow <- function(df, xname, yname, splitname, bldg, util, type){
  
  quo_x <- enquo(xname)
  quo_y <- enquo(yname)
  quo_split <- enquo(splitname)
  
  splitvar <- as.factor(df$avg_window)
  
  # get length of threshold
  colname <- names(df)[grep("thresh", names(df))]
  thresh_hrs <- df[, ..colname][1]
  
  legendtitle <- list(yref='paper',xref="paper",y=1.05,x=1.15, 
                      text="error avg len (hrs)",showarrow=F)
  
  if (type=="prc"){
    p1 <- plot_ly(data = df, x=quo_x, y=quo_y, split = quo_split, 
                  name=splitvar, mode="lines", type="scatter" ) %>%
      layout(yaxis=list(title='Precision', range=c(0,1)),
             xaxis = list(title='Recall', range=c(0,1)),
             annotations=legendtitle )
    filename <-  paste0("prec-rec-changeAvg-short-",bldg,"-",util,"-threshold-", (thresh_hrs)/24, "-days.pdf")
  } else {
    p1 <- plot_ly(data = df, x=quo_x, y=quo_y, split = quo_split, 
                  name=splitvar, mode="lines", type="scatter" ) %>%
      layout(yaxis=list(title='True Positive Rate', range=c(0,1)),
             xaxis = list(title='False Positive Rate', range=c(0,1)),
             annotations=legendtitle )
    filename <-  paste0("ROC-changeAvg-short-",bldg,"-",util,"-threshold-", (thresh_hrs)/24, "-days.pdf")
  }
  
  
  
  # p1 <- df %>%
  #   group_by(avg_window) %>% 
  #   plot_ly(x=~Cool.Cool_tpr, y=~Cool.Cool_ppv, name="Cool", mode="lines", 
  #               type="scatter") %>%
  
  if (save_img){
    wd <- getwd()
    setwd(fig_dir) # Change directory for saving figures
    #if (!exists("adjust")) { adjust = 1 }
    adjust = .8
    orca(p1, filename ) #, width=fig_width*adjust, height=fig_height*adjust)
    
    setwd(wd) # Change back to original working directory
  }
  
  if (viewer) {print(p1)} 
  #return(p1)
}

# send the different utilities to the plot function one at a time
plot_allprecrec <- function(eval_list, bldg){
  plot_prec_avgwindow(eval_list[[1]], Cool_tpr, Cool_ppv, avg_window, bldg, "Cool", "prc")
  plot_prec_avgwindow(eval_list[[2]], Elec_tpr, Elec_ppv, avg_window, bldg, "Elec", "prc")
  plot_prec_avgwindow(eval_list[[3]], Heat_tpr, Heat_ppv, avg_window, bldg, "Heat", "prc")
  plot_prec_avgwindow(eval_list[[4]], Water_tpr, Water_ppv, avg_window, bldg, "Water", "prc")
}

for (i in 1:length(dfs_tot)){
  plot_allprecrec(avg_window_prc[[i]], bldg_names[[i]])
}

beep()

# ROC curve is not that useful unless the number in each class is roughly equal
plot_allROC <- function(eval_list, bldg){
  plot_prec_avgwindow(eval_list[[1]], Cool_fpr, Cool_tpr, avg_window, bldg, "Cool", "ROC")
  plot_prec_avgwindow(eval_list[[2]], Elec_fpr, Elec_tpr, avg_window, bldg, "Elec", "ROC")
  plot_prec_avgwindow(eval_list[[3]], Heat_fpr, Heat_tpr, avg_window, bldg, "Heat", "ROC")
  plot_prec_avgwindow(eval_list[[4]], Water_fpr, Water_tpr, avg_window, bldg, "Water", "ROC")
  
}

for (i in 1:length(dfs_tot)){
  plot_allROC(avg_window_prc[[i]], bldg_names[[i]])
}


# ////////////////////////////////////////////////
# loop threshold length 1 to 40 days
# ////////////////////////////////////////////////

avg_hrs <<- 2
ED_hrs <<- 4

k_vals <- seq(-5,40,5)

many_precrec2 <- function(df, mods, k_vals, threshes){
  all_precrec <- list()
  result_list <- list()
  
  for (j in 1:length(threshes)){
    len_days <<- threshes[j]
    len_hrs <<- 24*len_days # length of window for threshold calculation
    result_list[[j]] <- prec_rec(df, mods, k_vals)
  }
  
  # reorganize the results into nicer tables instead of list
  for (g in 1:length(threshes)){
    temp <- result_list[[g]]
    
    c <- temp[,grep("Cool.", names(temp)), with=FALSE]
    names(c) <- gsub("Cool.Cool","Cool",names(c))
    c$avg_window <- rep(avg_hrs, nrow(c))
    
    e <- temp[,grep("Elec.", names(temp)), with=FALSE]
    names(e) <- gsub("Elec.Elec","Elec",names(e))
    e$avg_window <- rep(avg_hrs, nrow(e))
    
    h <- temp[,grep("Heat.", names(temp)), with=FALSE]
    names(h) <- gsub("Heat.Steam","Heat",names(h))
    h$avg_window <- rep(avg_hrs, nrow(h))
    
    w <- temp[,grep("Water.", names(temp)), with=FALSE]
    names(w) <- gsub("Water.Water","Water",names(w))
    w$avg_window <- rep(avg_hrs, nrow(w))
    
    cehw <- list(c,e,h,w)
    
    all_precrec[[g]] <- cehw
  }
  
  cool_res <- gatherlist(all_precrec, 1)
  elec_res <- gatherlist(all_precrec, 2)
  heat_res <- gatherlist(all_precrec, 3)
  water_res <- gatherlist(all_precrec, 4)
  
  results <- list(cool_res, elec_res, heat_res, water_res)
}

#threshes <- c(15,20,25,30,35,40)
threshes <- c(20, 35)

#initialize
thresh_prc <- list()
for (i in 1:length(dfs_tot)){
  thresh_prc[[i]] <- many_precrec2(dfs_tot[[i]], rf_mods[[i]], k_vals, threshes)
}


plot_prec_thresh <- function(df, xname, yname, splitname, bldg, util, type){
  
  quo_x <- enquo(xname)
  quo_y <- enquo(yname)
  quo_split <- enquo(splitname)
  
  # moving average and ED window length
  avghrs <- df$avg_window[1]
  
  # get length of threshold
  colname <- names(df)[grep("thresh", names(df))]
  thresh_hrs <- df[, ..colname]
  thresh_day <- thresh_hrs/24
  
  # se multiplier for hovertext
  colname2 <- names(df)[grep("se_mult", names(df))]
  se_mult <- df[, ..colname2]
  
  splitvar <- as.factor(thresh_day[[1]])
  
  legendtitle <- list(yref='paper',xref="paper",y=1.1, x=1.2,    # y=1.05,x=1.15, 
                      text="threshold window \n(days)",showarrow=F)
  
  if (type=="prc"){
    p1 <- plot_ly(data = df, x=quo_x, y=quo_y, split = quo_split, 
                  name=splitvar, mode="lines", type="scatter", 
                  text = paste(as.matrix(se_mult)),
                  hoverinfo='text') %>%
      layout(yaxis=list(title='Precision', range=c(0,1)),
             xaxis = list(title='Recall', range=c(0,1)),
             annotations=legendtitle  )
    filename <-  paste0("prec-rec-changeThresh4-",util,"-",bldg,"-avgHrs-", avghrs, ".svg")
  } else {
    p1 <- plot_ly(data = df, x=quo_x, y=quo_y, split = quo_split, 
                  name=splitvar, mode="lines", type="scatter" ) %>%
      layout(yaxis=list(title='True Positive Rate', range=c(0,1)),
             xaxis = list(title='False Positive Rate', range=c(0,1)),
             annotations=legendtitle )
    filename <-  paste0("ROC-changeThresh-",bldg,"-",util,"-avgHrs-", avghrs, ".pdf")
  }
  
  
  if (save_img){
    wd <- getwd()
    setwd(fig_dir) # Change directory for saving figures
    #if (!exists("adjust")) { adjust = 1 }
    adjust = 1.2
    orca(p1, filename , width=fig_width*adjust, height=fig_height*adjust)
    
    setwd(wd) # Change back to original working directory
  }
  
  if (viewer){print(p1) }
  #return(p1)
}

#   Changing threshold
plot_allprecrec2 <- function(eval_list, bldg){
  plot_prec_thresh(eval_list[[1]], Cool_tpr, Cool_ppv, Cool.thresh_len, bldg, "Cool", "prc")
  plot_prec_thresh(eval_list[[2]], Elec_tpr, Elec_ppv, Elec.thresh_len, bldg, "Elec", "prc")
  plot_prec_thresh(eval_list[[3]], Heat_tpr, Heat_ppv, Heat.thresh_len, bldg, "Heat", "prc")
  plot_prec_thresh(eval_list[[4]], Water_tpr, Water_ppv, Water.thresh_len, bldg, "Water", "prc")
  
}

for (i in 1:length(dfs_tot)){
  plot_allprecrec2(thresh_prc[[i]], bldg_names[[i]])
}


beep(2)



# ////////////////////////////////////////////////
# loop alarm length 3 to 7 days
# ////////////////////////////////////////////////

avg_hrs <<- 2
ED_hrs <<- 4

k_vals <- seq(-5,40,5)

len_days <<- 30
len_hrs <<- 24*len_days # length of window for threshold calculation

mod_frac <- .7
time_frac <- .95
#action_len <- 3

many_precrec3 <- function(df, mods, k_vals, action_lens){
  all_precrec <- list()
  result_list <- list()
  
  for (j in 1:length(action_lens)){
    action_len <<- action_lens[j]
    
    result_list[[j]] <- prec_rec(df, mods, k_vals)
  }
  
  # reorganize the results into nicer tables instead of list
  for (g in 1:length(action_lens)){
    temp <- result_list[[g]]
    
    c <- temp[,grep("Cool.", names(temp)), with=FALSE]
    names(c) <- gsub("Cool.Cool","Cool",names(c))
    c$avg_window <- rep(avg_hrs, nrow(c))
    c$actionlen <- rep(action_lens[g], nrow(c))
    
    e <- temp[,grep("Elec.", names(temp)), with=FALSE]
    names(e) <- gsub("Elec.Elec","Elec",names(e))
    e$avg_window <- rep(avg_hrs, nrow(e))
    e$actionlen <- rep(action_lens[g], nrow(e))
    
    h <- temp[,grep("Heat.", names(temp)), with=FALSE]
    names(h) <- gsub("Heat.Steam","Heat",names(h))
    h$avg_window <- rep(avg_hrs, nrow(h))
    h$actionlen <- rep(action_lens[g], nrow(h))
    
    w <- temp[,grep("Water.", names(temp)), with=FALSE]
    names(w) <- gsub("Water.Water","Water",names(w))
    w$avg_window <- rep(avg_hrs, nrow(w))
    w$actionlen <- rep(action_lens[g], nrow(w))
    
    cehw <- list(c,e,h,w)
    
    all_precrec[[g]] <- cehw
  }
  
  cool_res <- gatherlist(all_precrec, 1)
  elec_res <- gatherlist(all_precrec, 2)
  heat_res <- gatherlist(all_precrec, 3)
  water_res <- gatherlist(all_precrec, 4)
  
  results <- list(cool_res, elec_res, heat_res, water_res)
}

action_lens <- c(3,4,5,6,7)
#action_lens <- c(3,7)

action_prc <- list()
for (i in 1:length(dfs_tot)){
  action_prc[[i]] <- many_precrec3(dfs_tot[[i]], rf_mods[[i]], k_vals, action_lens)
}


plot_prec_actionlen <- function(df, xname, yname, splitname, bldg, util, type){
  
  quo_x <- enquo(xname)
  quo_y <- enquo(yname)
  quo_split <- enquo(splitname)
  
  # moving average and ED window length
  avghrs <- df$avg_window[1]
  
  # get length of window for alarm trigger (from signals)
  colname <- names(df)[grep("action", names(df))]
  action_len <- df[, ..colname]
  
  
  # se multiplier for hovertext
  colname2 <- names(df)[grep("se_mult", names(df))]
  se_mult <- df[, ..colname2]
  
  splitvar <- as.factor(action_len[[1]])
  
  legendtitle <- list(yref='paper',xref="paper",y=1.05,x=1.15, 
                      text="action length (days)",showarrow=F)
  
  if (type=="prc"){
    p1 <- plot_ly(data = df, x=quo_x, y=quo_y, split = quo_split, 
                  name=splitvar, mode="lines", type="scatter", hoverinfo = 'se_mult') %>%
      layout(yaxis=list(title='Precision', range=c(0,1)),
             xaxis = list(title='Recall', range=c(0,1)),
             annotations=legendtitle  )
    filename <-  paste0("prec-rec-changeActionLen-",util,"-",bldg,"-avgHrs-", avghrs, ".pdf")
  } else {
    p1 <- plot_ly(data = df, x=quo_x, y=quo_y, split = quo_split, 
                  name=splitvar, mode="lines", type="scatter" ) %>%
      layout(yaxis=list(title='True Positive Rate', range=c(0,1)),
             xaxis = list(title='False Positive Rate', range=c(0,1)),
             annotations=legendtitle )
    filename <-  paste0("ROC-changeActionLen-",bldg,"-",util,"-avgHrs-", avghrs, ".pdf")
  }
  
  
  if (save_img){
    wd <- getwd()
    setwd(fig_dir) # Change directory for saving figures
    #if (!exists("adjust")) { adjust = 1 }
    adjust = .8
    orca(p1, filename ) #, width=fig_width*adjust, height=fig_height*adjust)
    
    setwd(wd) # Change back to original working directory
  }
  
  if (viewer) {print(p1)} 
  #return(p1)
}

plot_allprecrec3 <- function(eval_list, bldg){
  plot_prec_actionlen(eval_list[[1]], Cool_tpr, Cool_ppv, actionlen, bldg, "Cool", "prc")
  plot_prec_actionlen(eval_list[[2]], Elec_tpr, Elec_ppv, actionlen, bldg, "Elec", "prc")
  plot_prec_actionlen(eval_list[[3]], Heat_tpr, Heat_ppv, actionlen, bldg, "Heat", "prc")
  plot_prec_actionlen(eval_list[[4]], Water_tpr, Water_ppv, actionlen, bldg, "Water", "prc")
  
}

for (i in 1:length(dfs_tot)){
  plot_allprecrec3(action_prc[[i]], bldg_names[[i]])
}

beep(2)


# ////////////////////////////////////////////////
# loop time fraction for alarm 0.2 to 0.99
# ////////////////////////////////////////////////

mod_frac <- .7
#time_frac <- .9
action_len <- 3

many_precrec4 <- function(df, mods, k_vals, time_fracs){
  all_precrec <- list()
  result_list <- list()
  
  for (j in 1:length(time_fracs)){
    time_frac <<- time_fracs[j]
    
    result_list[[j]] <- prec_rec(df, mods, k_vals)
  }
  
  # reorganize the results into nicer tables instead of list
  for (g in 1:length(time_fracs)){
    temp <- result_list[[g]]
    
    c <- temp[,grep("Cool.", names(temp)), with=FALSE]
    names(c) <- gsub("Cool.Cool","Cool",names(c))
    c$avg_window <- rep(avg_hrs, nrow(c))
    c$time_frac <- rep(time_fracs[g], nrow(c))
    
    e <- temp[,grep("Elec.", names(temp)), with=FALSE]
    names(e) <- gsub("Elec.Elec","Elec",names(e))
    e$avg_window <- rep(avg_hrs, nrow(e))
    e$time_frac <- rep(time_fracs[g], nrow(e))
    
    h <- temp[,grep("Heat.", names(temp)), with=FALSE]
    names(h) <- gsub("Heat.Steam","Heat",names(h))
    h$avg_window <- rep(avg_hrs, nrow(h))
    h$time_frac <- rep(time_fracs[g], nrow(h))
    
    w <- temp[,grep("Water.", names(temp)), with=FALSE]
    names(w) <- gsub("Water.Water","Water",names(w))
    w$avg_window <- rep(avg_hrs, nrow(w))
    w$time_frac <- rep(time_fracs[g], nrow(w))
    
    cehw <- list(c,e,h,w)
    
    all_precrec[[g]] <- cehw
  }
  
  cool_res <- gatherlist(all_precrec, 1)
  elec_res <- gatherlist(all_precrec, 2)
  heat_res <- gatherlist(all_precrec, 3)
  water_res <- gatherlist(all_precrec, 4)
  
  results <- list(cool_res, elec_res, heat_res, water_res)
}

#time_fracs <- c(0.5, 0.9, 0.95, .99)
time_fracs <- c(0.8, 0.95)

alarm_prc <- list()
for (i in 1:length(dfs_tot)){
  alarm_prc[[i]] <- many_precrec4(dfs_tot[[i]], rf_mods[[i]], k_vals, time_fracs)
}


plot_prec_alarmfrac <- function(df, xname, yname, splitname, bldg, util, type){
  
  quo_x <- enquo(xname)
  quo_y <- enquo(yname)
  quo_split <- enquo(splitname)
  
  # moving average and ED window length
  avghrs <- df$avg_window[1]
  
  # get fraction of signals for alarm (phi)
  colname <- names(df)[grep("frac", names(df))]
  frac <- df[, ..colname]
  
  
  # se multiplier for hovertext
  colname2 <- names(df)[grep("se_mult", names(df))]
  se_mult <- df[, ..colname2]
  
  splitvar <- as.factor(frac[[1]])
  
  legendtitle <- list(yref='paper',xref="paper",y=1.05,x=1.15, 
                      text="alarm fraction",showarrow=F)
  
  if (type=="prc"){
    p1 <- plot_ly(data = df, x=quo_x, y=quo_y, split = quo_split, 
                  name=splitvar, mode="lines", type="scatter",
                  text = paste(as.matrix(se_mult)),
                  hoverinfo='text') %>%
      layout(yaxis=list(title='Precision', range=c(0,1)),
             xaxis = list(title='Recall', range=c(0,1)),
             annotations=legendtitle  )
    filename <-  paste0("prec-rec-changeAlarmFrac-",util,"-",bldg,"-avgHrs-", avghrs, ".pdf")
  } else {
    p1 <- plot_ly(data = df, x=quo_x, y=quo_y, split = quo_split, 
                  name=splitvar, mode="lines", type="scatter" ) %>%
      layout(yaxis=list(title='True Positive Rate', range=c(0,1)),
             xaxis = list(title='False Positive Rate', range=c(0,1)),
             annotations=legendtitle )
    filename <-  paste0("ROC-changeAlarmFrac-",util,"-",bldg,"-avgHrs-", avghrs, ".pdf")
  }
  
  
  if (save_img){
    wd <- getwd()
    setwd(fig_dir) # Change directory for saving figures
    #if (!exists("adjust")) { adjust = 1 }
    adjust = .8
    orca(p1, filename ) #, width=fig_width*adjust, height=fig_height*adjust)
    
    setwd(wd) # Change back to original working directory
  }
  
  if (viewer) {print(p1)} 
  #return(p1)
}

plot_allprecrec4 <- function(eval_list, bldg){
  plot_prec_alarmfrac(eval_list[[1]], Cool_tpr, Cool_ppv, time_frac, bldg, "Cool", "prc")
  plot_prec_alarmfrac(eval_list[[2]], Elec_tpr, Elec_ppv, time_frac, bldg, "Elec", "prc")
  plot_prec_alarmfrac(eval_list[[3]], Heat_tpr, Heat_ppv, time_frac, bldg, "Heat", "prc")
  plot_prec_alarmfrac(eval_list[[4]], Water_tpr, Water_ppv, time_frac, bldg, "Water", "prc")
  
}


for (i in 1:length(dfs_tot)){
  plot_allprecrec4(alarm_prc[[i]], bldg_names[[i]])
}

beep()

