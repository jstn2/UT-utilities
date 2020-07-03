# ...............................................................
# data cleaning script for UT Utility usage analysis
# Spring 2020
# Justin DuRant
# ...............................................................
# What this script does:
# 1. Take in csv file with cooling, elec, and heat hourly data
# 2. Make sure datetime is in proper format
# 3. Censor outliers from technical/transmission errors
# 4. Save .RData files for other scripts to use
# ...............................................................

rm(list=ls())


library(lubridate)
library(magrittr)
library(ggplot2)
library(plotly)
library(plyr)
library(dplyr)
library(leaps)
library(stringr)
library(data.table)
library(htmlwidgets)


# Set the working directory to the folder where this script is stored
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


# Get a list of file names to take as inputs. 
# Set the target directory and the function will get a list of all the csv in
# that folder. Change "/data" to match the name of the folder if necessary
data_dir <- paste0(getwd(),"/data")
input <- dir(data_dir, pattern="\\.csv$") # "input" is a vector of file names


# Bring the csv files into the workspace as data tables. Also make a new string
# without the file extension for naming the objects in R workspace
name_list <- gsub("\\.csv$","", input) 
for (i in 1:length(name_list)) {
  assign(name_list[i], fread(file.path(data_dir, input[i])))
}

bldg_names <- c("ECJ", "GDC","JCD","BUI")
dfs_tot <- list(ECJ, GDC, JCD, BUI)

# //////////////////////////////////////////
# import and process the temperature data 
# //////////////////////////////////////////

# the weather data from a single station downloaded from
# ftp://ftp.ncdc.noaa.gov/pub/data/uscrn/products/hourly02 comes in .txt files
# with one file for each year. This code section combines the files for the
# desired period and extracts the hourly temperature
txt_input <- dir(data_dir, pattern="\\Austin_33_NW.txt$")
weather_data <- list()
for (i in 1:length(txt_input)){
  weather_data[[i]] <- read.table(file.path(data_dir, txt_input[i]), header=TRUE)
}
weather <- do.call("rbind", weather_data)

# note: SUR_TEMP_TYPE indicates scale (deg C). 
temp <- weather %>% dplyr::select("LST_DATE","LST_TIME","T_HR_AVG") 


#Clean up NA  in temperature data (stored as -9999)
for (i in 1:nrow(temp)) {
  if(temp$T_HR_AVG[i] < -1000) {
    # Print the NA row num, and the values from prev and next day
    cat('\nrow', i, temp$T_HR_AVG[i], temp$T_HR_AVG[i-24], temp$T_HR_AVG[i+24])
    #replace with average of yesterday and tomorrow
    temp$T_HR_NEW[i] <- mean(c(temp$T_HR_AVG[i-24], temp$T_HR_AVG[i+24]))
  }
  else {
    temp$T_HR_NEW[i] <- temp$T_HR_AVG[i]
  }
}

# convert to Fahrenheit
temp$TEMP_F <- (temp$T_HR_NEW*(9/5)+32)

# Pad the hour column with leading zeroes so lubridate can parse the time
temp$LST_TIME <- formatC(temp$LST_TIME, width = 4, format = "d", flag = "0")
# put together date and time into a single column
temp <- temp %>% mutate(dt_int = str_c(LST_DATE,LST_TIME,sep = " "))
# Use lubridate function to convert it to datetime variable
temp$dt <- ymd_hm(temp$dt_int)

# Get rid of intermediate columns and rename
TEMP <- temp %>% dplyr::select("dt", "T_HR_NEW", "TEMP_F")
names(TEMP) <- c("DateTime", "TEMP_C", "TEMP_F")

# Check that the values look good (No NA values of -9999)
# plot(temp$dt, temp$TEMP_F)

# Save the temperature data to RData file
save(TEMP, file=file.path(data_dir,"temp.RData"))

# You can comment the above section and switch to this after the temperature
# data were processed and saved the first time (if desired)
#load(file.path(data_dir, "temp.RData")

# ////////////////////////////////////
# Clean and reorganize the water data
# ////////////////////////////////////

# Update the code here if the water data has a different name than water
# water <- water-file-name

# Change these two rows to avoid NA from DST start using as_datetime()
#water$TS[18349] <- '2018-03-11 03:00:00'
#water$TS[53293] <- '2019-03-10 03:00:00'
water$TS <- as_datetime(water$TS) #Convert from character

# Default tz from the as_datetime() function is UTC, so update it to central
# time. This keeps same the same numerical "time" values, not the same moment as
# measured in the original timezone.
tz(water$TS) <- "America/Chicago"

#cols <- 2:4
# Shift the water back by 15 minutes (one time step), and cut out RLM data
#wat_shift <-  water[, (cols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]
#wat_shift <- cbind(water[,1], water[, shift(.SD, 1, NA, "lead", TRUE), .SDcols=2:4])                  #original line
wat_shift <- cbind(water[,1], water[, shift(.SD, 1, NA, "lead", TRUE), .SDcols=2:length(water)])

# Check for NA in water data and investigate
which(is.na(wat_shift[,2]))

# The last row is NA because we shifted back by one time step, let's remove it
wat_shift <- na.omit(wat_shift)
wat_shift$TS <- as_datetime(wat_shift$TS) # It was coerced into character by data.table

# Check for technical errors in 15minute data
# check_data takes in vectorized date (x) (just to print it) and 15-min data
# values (y). This function first deaccumulates the cumulative data to get 15
# minute incremental usage. Next, the function finds each run of zeros, and then
# if the run of zeros is length >= 2 and if the value of the spike after the run
# of zeros is greater than the median + 3sd of abs(y[i+1]-y[i]), then the whole
# run of zeros along with the preceeding and following 15min points is reset to
# NA

check_water <- function(x,y,switch=1) {
  x <- as.character(x) # Make the datetime into char so it is easier to print the dates
  # Check for zeros in raw data and print associated datetime
  index <- which(y==0)
  if (length(index) >0) {
    #cat("\n\nZero values in raw cumulative data:")
    for (i in 1:length(index)) {
      #cat("\nZero value at ", x[index[i]])
    }
  }
  # Make vector of 15 minute usage (deaccumulate)
  y_s <- shift(y, n=1, fill=0, "lag")
  # Take the difference at each time step, but at midnight that is large neg number, 
  # so instead use just the midnight value
  y_incr <- mapply(function(x,y) min(abs(x-y),x), y, y_s)
  # Check for zero values in the deaccumulated data
  # rle is great
  rle_incr <- rle(y_incr)
  # Get start and end of all runs, length of run, and data value,
  # then only take the zero values (includes single zeros)
  end = cumsum(rle_incr$lengths)
  start = head(c(1, (lag(end, k=1)+1)), -1)
  length = rle_incr[[1]]
  value = rle_incr[[2]]
  zero_runs <- data.table(start, end, length, value)[which(value==0 & length > 1),] # Select the zero runs that are longer than one
  if (nrow(zero_runs)>0) {
    for (i in 1:nrow(zero_runs)){
      #cat("\nRun of zeros, length=",zero_runs$length[i],"\nfrom",x[zero_runs$start[i]]," to ",x[zero_runs$end[i]])
      #cat("\nTwo points before zeros start:",y_incr[zero_runs$start[i]-2],y_incr[zero_runs$start[i]-1])
      #cat("\n15min after the zeros has magnitude: ", y_incr[zero_runs$end[i]+1], y_incr[zero_runs$end[i]+2], "\n")
      }
  }
  # index_zero <- which(y_incr==0) #Not very useful because so many zeros
  # if (length(index_zero>0)) {
  #   cat("\n\nZero values in deaccumualted data:")
  #   cat("\n-------------------------------------")
  #   for (i in 1:length(index_zero)) cat("\nZero usage at ", x[index_zero[i]])
  # }
  # Get a value for median difference to identify big spikes
  y_i_sh <- shift(y_incr, n=1, fill=0, "lag") # Shift the deaccumulated data (look backwards)
  # Get a cutoff for the largest differences from one hour to another to look for unusual spikes (drops)
  big_diff <- (median(abs(y_incr-y_i_sh)) + 3*sd(abs(y_incr-y_i_sh)))
  #cat("\nBig difference cutoff (med+3sd) is: ",big_diff,'\n')
  index_spike <- which(y_i_sh==0 & abs(y_incr-y_i_sh)>=big_diff) # The spots where there is a spike after zeros
  print(index_spike)
  if (switch==1){
    if (length(index_spike) >0) {
      for (i in 1:length(index_spike)) {
        temp <- zero_runs[which(zero_runs$end == index_spike[i]-1),] # Select a matching row from the runs of zeros
        if (nrow(temp)>0){
          y_incr[(temp$start-1):(temp$end+1)] <- NA # Cut out values corresponding to technical error
          cat("\nData to NA from ",x[temp$start-1],"to ",x[temp$end+1])
        }
      }
    }
  }
  
  return(y_incr)
}
# make a new dataframe to store the cleaned and deaccumulated 15minute data
wat_clean <- wat_shift
names(wat_clean) <- c("DateTime",bldg_names)
wat_uc <- wat_shift
names(wat_uc) <- c("DateTime",bldg_names)

for (i in 1:length(bldg_names)){
  # send each column (one bldg) to the water cleaning function one at a time 
  wat_clean[[bldg_names[i]]] <- check_water(wat_shift[[1]],wat_shift[[(i+1)]])
  # For unclean data, we use the same function to convert it to incremental
  # instead of accumulated but don't delete any data, so the switch == 0.
  wat_uc[[bldg_names[i]]] <- check_water(wat_shift[[1]],wat_shift[[(i+1)]], 0)
}

# Check the datetime and convert if needed
#tz(wat_clean$DateTime) 
tz(wat_clean$DateTime) <- "America/Chicago"
tz(wat_uc$DateTime) <- "America/Chicago"

# /////////////////////////////////////////////////////
# turn 15-minute data to hourly
# /////////////////////////////////////////////////////

# add up the 15-minute values to make hourly time series
hourly <- function(df) {
  temp_ls <- list()
  for (j in 2:ncol(df)) {
    temp_ls[[j-1]] = aggregate(df[,..j], by=list(hour=hour(df$DateTime),
                                                 date=date(df$DateTime)), sum)
  }
  dt <- data.table(DateTime = make_datetime(year = year(temp_ls[[2]]$date), 
                                            month = month(temp_ls[[2]]$date), 
                                            day = day(temp_ls[[2]]$date), 
                                            hour = temp_ls[[2]]$hour, 
                                            min = 0L,sec = 0, 
                                            tz = "America/Chicago"))
  #return(cbind(dt, temp_ls[[1]][3],temp_ls[[2]][3],temp_ls[[3]][3],temp_ls[[4]][3]))
  # Select the third element of each element of this list, which is the data
  data <- cbind(dt, sapply(temp_ls, "[[", 3)) 
  names(data) <- names(df)
  return(data)
}

wat_hour <- hourly(wat_clean)
wat_hour_uc <- hourly(wat_uc)

# /////////////////////////////////////////////////////
# Visualize 15 minute data, compare clean and unclean
# /////////////////////////////////////////////////////

a <- list(
  title = "15 min Water Use (Gal)",
  showticklabels = TRUE
)
p1 <- plot_ly(wat_clean, x = ~DateTime, y = ~eval(as.name(bldg_names[1])), type="scatter", mode="lines", name=bldg_names[1],
             line = list(width = 0.5))
  for (i in 2:length(bldg_names)) {
    p1 <- p1 %>% add_trace(p1, y = ~eval(as.name(bldg_names[i])), name = bldg_names[i])
  }
  p1 <- p1 %>% layout(yaxis=a)
# htmlwidgets::saveWidget(as_widget(p1), "water_15min.html")
#p1
p2 <- plot_ly(wat_uc, x = ~DateTime, y = ~eval(as.name(bldg_names[1])), type="scatter", mode="lines", name=paste(bldg_names[1],"uc"),
              line = list(width = 0.5))
  for (i in 2:length(bldg_names)) {
    p2 <- p2 %>% add_trace(p2, y = ~eval(as.name(bldg_names[i])), name = paste(bldg_names[i],"uc"))
  }
  p2 <- p2 %>% layout(yaxis=a)
# htmlwidgets::saveWidget(as_widget(p2), "water_raw_15min.html")
#p2
p = list(p1,p2)
p_tot <- subplot(p,nrows=2,shareX=TRUE, titleY=TRUE)
p_tot


p3 <- plot_ly(wat_hour, x = ~DateTime, y = ~eval(as.name(bldg_names[1])), type="scatter", mode="lines", name=bldg_names[1],
              line = list(width = 0.5))
  for (i in 2:length(bldg_names)) {
    p3 <- p3 %>% add_trace(p3, y = ~eval(as.name(bldg_names[i])), name = bldg_names[i])
  }
  p3 <- p3 %>% layout(p3, yaxis=list(title="Hourly Water Use (Gal)"), colorway=c('#00ae48','#167cb9','#ff0606','#8e44ad'))
#htmlwidgets::saveWidget(as_widget(p3), "water_hour.html")
#p3

p4 <- plot_ly(wat_hour_uc, x = ~DateTime, y = ~eval(as.name(bldg_names[1])), type="scatter", mode="lines", name=paste(bldg_names[1],"uc"),
              line = list(width = 0.5))
  for (i in 2:length(bldg_names)) {
    p4 <- p4 %>% add_trace(p4, y = ~eval(as.name(bldg_names[i])), name = paste(bldg_names[i],"uc"))
  }
  p4 <- p4 %>% layout(yaxis=list(title="Hourly Water Use (raw) (Gal)"), colorway = c('#00ae48','#167cb9','#ff0606','#8e44ad'))
#htmlwidgets::saveWidget(as_widget(p4), "water_uc_hour.html")
#p4
p = list(p3,p4)
p_tot_h <- subplot(p,nrows=2,shareX=TRUE, titleY=TRUE)
p_tot_h

# /////////////////////////////////////////////////////
# End Water Data Cleaning
# Start other data cleaning
# /////////////////////////////////////////////////////

# "warning message: one failed to parse" - row 5301 which is 2019-03-10 2:00
# (right at spring forward) lubridate considers that time non-existent, so we
# change it to 3:00 am 
fix_date <- function(df){
  # as_datetime will give a warning and make one row NA at DST start
  df$DateTime <- as_datetime(df$DateTime, tz = "America/Chicago")
  dst_index <- which(is.na(df$DateTime))
  print(dst_index)
  if (length(dst_index) > 0){
    print("Check inserted date in function fix_date() to make sure it is correct")
  } else (print("No NA values from DST start"))
  # Here we insert the correct time where there was NA 
  df$DateTime[which(is.na(df$DateTime))] <- as_datetime('2019-03-10 03:00:00', tz = "America/Chicago")
  df
}

for (i in 1:length(dfs_tot)){
  dfs_tot[[i]] <- fix_date(dfs_tot[[i]])
}

# combine the water with the other variables
for (i in 1:length(dfs_tot)){
  water_subset <- select(wat_hour, c("DateTime", bldg_names[i]))
  dfs_tot[[i]] <- left_join(dfs_tot[[i]], water_subset, by="DateTime")
  names(dfs_tot[[i]])[5] <- "Water"
}

# /////////////////////////////////////////////////////
# Add temperature data to everything
# /////////////////////////////////////////////////////

tz(TEMP$DateTime) <- "America/Chicago" # This does not modify the times, just corrects the timezone variable so we don't get a shift

add_temp <- function(df) {
  temp_F <- subset(TEMP, DateTime %in% df$DateTime)[[3]]
  df$temp_F <- temp_F
  df
}

for (i in 1:length(dfs_tot)){
  dfs_tot[[i]] <- add_temp(dfs_tot[[i]])
}

# /////////////////////////////////////////////////////
# Take in a data table for single building with C,E,H,W hourly data, 
# find runs of zeros in the first three parameters, cut them out if 
# they meet the conditions, print remarks about the data.
# /////////////////////////////////////////////////////

clean_data <- function(df) {
  x <- as.character(df[[1]]) 
  data <- list()
  zero_runs <- list()
  for (i in 2:ncol(df)){
    data[[i-1]] <- df[[i]]
    rle_temp <- rle(df[[i]])
    end = cumsum(rle_temp$lengths)
    start = head(c(1, (lag(end, k=1)+1)), -1)
    length = rle_temp[[1]]
    value = rle_temp[[2]]
    zero_runs[[i-1]] <- data.table(start, end, length, value)[which(value==0 & length > 1),] 
  }
  
  # Count how many times each unique run of zeros appears in different data streams
  zeros_all <- zero_runs[[1]]
  for (i in 2:length(zero_runs)){
    zeros_all <- rbind(zeros_all,zero_runs[[i]])
  }
  zero_count <- count(zeros_all)
  
  # Make an array to keep track of data removal
  num <- nrow(df)
  removal_record <- data.table(DT = df[[1]], Elec = numeric(num), Cool = numeric(num), Heat = numeric(num))
  
  # Get a value for median difference to identify big spikes
  big_diff <- numeric(length(data))
  index_spike <- list()
  for (k in 1:length(data)){
    temp <- data[[k]]
    temp_sh <- shift(temp, n=1, fill=0, "lag") # Shift the deaccumulated data (look backwards)
    # Get a cutoff for the largest differences from one hour to another to look for unusual spikes (drops)
    big_diff <- (median(abs(temp-temp_sh), na.rm=TRUE) + 3*sd(abs(temp-temp_sh), na.rm=TRUE))
    # Save indices of each spike after a run of zeros
    index_spike[[k]] <- which(temp_sh==0 & abs(temp-temp_sh)>=big_diff)
    cat('\n',k)
    cat('\nSpike cutoff value is: ', big_diff)
    cat('\nSpikes at: ', index_spike[[k]])
  }
  
  for (N in 1:(length(zero_runs)-1)){
    if (length(index_spike[[N]]) > 0) {
      for (i in 1:length(index_spike[[N]])) {
        cat('\nStart of parameter', N)
        temp <- zero_runs[[N]][which(zero_runs[[N]]$end == index_spike[[N]][i]-1),] # Select a matching row for that run of zeros
        if (nrow(temp)>0){
          df[(temp$start-1):(temp$end+1), N+1] <- NA # Cut out values corresponding to technical error
          cat("\nData to NA from ",x[temp$start-1],"to ",x[temp$end+1])
          removal_record[(zero_runs[[N]]$start[i]-1):zero_runs[[N]]$end[i]+1, N+1] <- 1
        }
      }
    }
  }
  # Add something to check if the zero run occurs in multiple streams using the zero_count variable?
  
  #obj <- list(df,removal_record)
  #obj
  return(df)
  # for (N in 1:length(zero_runs)){
  #   if (nrow(zero_runs[[N]])>0) {
  #     cat("\nStart of Parameter ", N)
  #     cat("\n-----------------------")
  #     for (i in 1:nrow(zero_runs[[N]])){
  #       cat("\nRun of zeros, length=",zero_runs[[N]]$length[i],"\nfrom",x[zero_runs[[N]]$start[i]]," to ",x[zero_runs[[N]]$end[i]])
  #       cat("\nTwo points before zeros start:",data[[N]][zero_runs[[N]]$start[i]-2],data[[N]][zero_runs[[N]]$start[i]-1])
  #       cat("\n15min after the zeros has magnitude: ", data[[N]][zero_runs[[N]]$end[i]+1], data[[N]][zero_runs[[N]]$end[i]+2], "\n")
  #       removal_record[(zero_runs[[N]]$start[i]-1):zero_runs[[N]]$end[i]+1, N+1] <- 1
  #     }
  #   }
  # }

}


# Make a new list for the cleaned data 
dfs_clean <- dfs_tot

for (i in 1:length(dfs_tot)){
  dfs_clean[[i]] <- clean_data(dfs_tot[[i]])
}

# /////////////////////////////////////////////////////
# Additional Cleaning based on visual inspection
# /////////////////////////////////////////////////////

extra_clean <- function(df){
  df$Elec[c(830,848:849,1631:1635,2018,5214)] <- NA
  df$Cool[c(830,848:849,1631:1635,2018,5214)] <- NA
  df$Heat[c(830,848:849,1631:1635,2018,5214)] <- NA
  return(df)
}

# Uncomment the following lines to call the extra_clean function
# for (i in 1:length(dfs_tot)){
#   dfs_clean[[i]] <- extra_clean(dfs_clean[[i]])
# }




# For this plot, need the second output of the function e.g. clean_data(ECJtot)[[2]]
# p_ecjrec <- plot_ly(ECJrec, x = ~DT, y = ~Elec, type="scatter", mode="lines", name="Elec",
#               line = list(width = 2)) %>%
#   add_trace(y = ~Cool, name = 'Cool') %>%
#   add_trace(y = ~Heat, name = 'Heat') %>%
#   add_trace(y = ~Water, name = 'Water')

make_plot <- function(df, df_uc, name){
  p_df <- plot_ly(df, x = ~DateTime, y = ~Elec, type="scatter", mode="lines", name="Elec",
                   line = list(width = 0.5)) %>%
    add_trace(y = ~Cool, name = 'Cool') %>%
    add_trace(y = ~Heat, name = 'Heat') %>%
    add_trace(y = ~Water, name = 'Water') %>%
    layout(title = name, yaxis=list(title="Hourly Usage, clean"))
  p_df_uc <- plot_ly(df_uc, x = ~DateTime, y = ~Elec, type="scatter", mode="lines", name="Elec uc",
                  line = list(width = 0.5)) %>%
    add_trace(y = ~Cool, name = 'Cool uc') %>%
    add_trace(y = ~Heat, name = 'Heat uc') %>%
    add_trace(y = ~Water, name = 'Water uc') %>%
    layout(title = name, yaxis=list(title="Hourly Usage, uncleaned"))
  
  p = list(p_df,p_df_uc)
  p_tot <- subplot(p,nrows=2,shareX=TRUE, titleY=TRUE)
  htmlwidgets::saveWidget(as_widget(p_tot), paste(name, ".html", sep=""))
  p_tot
}

for (i in 1:length(dfs_tot)){
  make_plot(dfs_clean[[i]], dfs_tot[[i]], bldg_names[i])
}



# /////////////////////////////////////////////////////
# Add extra columns for the models 
# /////////////////////////////////////////////////////

addTime <- function(df) {
  temp <- df
  temp$year <- year(df$DateTime)
  temp$month <- month(df$DateTime)
  temp$wday <- wday(update(df$DateTime, week_start = getOption("lubridate.week.start", 1)))
  temp$hour <- hour(df$DateTime)
  temp$Schoolday <- ifelse(temp$wday >= 1 & temp$wday <= 5 & temp$DateTime > "2018-08-02" & temp$DateTime < "2019-05-10", 1, 0)
  temp$Schoolday[temp$DateTime > "2018-12-20" & temp$DateTime < "2019-01-22"] <- 0

  # Make it a factor instead of numeric
  temp$Schoolday <- as.factor(temp$Schoolday)
  #temp <- left_join(temp,days, by="DateTime")
  temp
}

prevHr <- function(df) {
  temp <- df
  temp$Elec_HrLag <- NA
  temp$Cool_HrLag <- NA
  temp$Heat_HrLag <- NA
  temp$Water_HrLag <- NA
  for (i in 2:(nrow(df))) {
    temp$Elec_HrLag[i] <- df[i-1,2]
    temp$Cool_HrLag[i] <- df[i-1,3]
    temp$Heat_HrLag[i] <- df[i-1,4]
    temp$Water_HrLag[i] <- df[i-1,5]
  }
  temp
}

prevDay <- function(df) {
  temp <- df
  temp$Elec_DayLag <- NA
  temp$Cool_DayLag <- NA
  temp$Heat_DayLag <- NA
  temp$Water_DayLag <- NA
  for (i in 25:(nrow(df))) {
    temp$Elec_DayLag[i] <- df[i-24,2]
    temp$Cool_DayLag[i] <- df[i-24,3]
    temp$Heat_DayLag[i] <- df[i-24,4]
    temp$Water_DayLag[i] <- df[i-24,5]
  }
  temp
}

add_all_cols <- function(df){
  df <- addTime(df)
  df <- prevHr(df)
  df <- prevDay(df)
  df
}


for (i in 1:length(dfs_clean)){
  dfs_clean[[i]] <- add_all_cols(dfs_clean[[i]])
  dfs_tot[[i]] <- add_all_cols(dfs_tot[[i]])
}



## /////////////////////////////////////////////////////
## put original hourly water data (not already cleaned) into "tot" data frames
## /////////////////////////////////////////////////////

# which(is.na(ECJtot$DateTime))

other_water <- function(df, bldg){
  dftemp <- df[, !names(df) %in% "Water"]
  wattemp <- as.data.frame(wat_hour_uc)
  wattemp <- wattemp[, names(wattemp) %in% c("DateTime", bldg)]
  dfnew <- merge(x=dftemp, y=wattemp, 
                 by="DateTime", all.x=TRUE)
  names(dfnew)[length(names(dfnew))] <- "Water" 
  dfnew
}

for (i in 1:length(dfs_tot)){
  dfs_tot[[i]] <- other_water(dfs_tot[[i]], bldg_names[i])
}


## /////////////////////////////////////////////////////
## Save the data for other scripts to use it
## /////////////////////////////////////////////////////



save(dfs_clean, dfs_tot, bldg_names, file=file.path(data_dir,"hourly_CEHW.RData"))







