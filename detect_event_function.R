#..//// //// ///// //// /// /// //// ///// //// /// /// //// ///// //// /// ////
#.|||| |||| ||||| |||| ||| ||| |||| ||||| |||| ||| ||| |||| ||||| |||| ||| ||||
#.\\\\ \\\\ \\\\\ \\\\ \\\ \\\ \\\\ \\\\\ \\\\ \\\ \\\ \\\\ \\\\\ \\\\ \\\ \\\\
#.|||| |||| ||||| |||| ||| ||| |||| ||||| |||| ||| ||| |||| ||||| |||| ||| ||||
#//// //// ///// //// /// /// //// ///// //// /// /// //// ///// //// /// ////

# Functions for detect_event.R script
# Justin DuRant
# Feb 2020

# Note: In RStudio use Alt+o to collapse all functions or Alt+Shift+o to expand all

library(zoo) # for rollapply
library(data.table) 

# f///////
# f//
# f//
# f//////     xx x/    n| |||
# f//          xx      n|   ||
# f//        x/ xx     n|   ||
# f//      x/    xx    n|   ||


# Pass regular df and cleaned df to this function and it will mark 
# events in unclean data that were removed earlier
mark_events <- function(df, df_c) {
  length = nrow(df)
  #if(!("E_event" %in% colnames(df))) {
  # Add new binary variables to mark event (default to zero)
  df$E_event <- integer(length)
  df$C_event <- integer(length)
  df$H_event <- integer(length)
  df$W_event <- integer(length)
  # Set event variable to 1 if the clean data is NA
  # Mark other outliers by referencing index
  extras <- c(830,848:849,2018,5214,7370:7371)
  df$E_event[which(is.na(df_c$Elec))] <- 1
  df$E_event[extras] <- 1
  df$C_event[which(is.na(df_c$Cool))] <- 1
  df$C_event[extras] <- 1
  df$H_event[which(is.na(df_c$Heat))] <- 1
  df$W_event[which(is.na(df_c$Water))] <- 1
  #extra_water <- c()
  #df$W_event[extra_water] <- 1
  return(df)
  #} else {
  #cat("No need to run the function again")
  #}
}


# Input the quantile and the vector of data, get an event vector 
# to superimpose on the data
event_shapr <- function(y, q, len, ramp_min, ramp_max){
  
  # calculate the magnitude of sd using quantile
  size <- quantile(y, q, na.rm=TRUE)
  stdev <- sd(y, na.rm=TRUE)
  mag_1 <- abs(rnorm(1,size,stdev)) 
  # do = TRUE
  # while (do){
  #   mag_1 <- abs(rnorm(1,0,size)) # one sample from norm with mean 0
  #   #print(mag_1)
  #   if (mag_1 > size) do = FALSE # Previously 0.5*size
  # }

  # Sample length of the ramp up (hours)
  ramp <- sample(seq(ramp_min, ramp_max), 1)
  # Make event vector with linear increase then constant
  if (ramp>0) {
    ramp_vect <- seq(0, mag_1, (mag_1/(ramp-1))) # linear increase with num steps = ramp
    flat_len <- len-ramp
    mag_vect <- c(ramp_vect, rep(mag_1,flat_len))
  } else  mag_vect <- rep(mag_1,len)
  
  mag_vect
}

# Get a table of the runs of zero in the Event variable (e.g. y = df$E_event)
run_fun <- function(y) {
  rle_event <- rle(head(y, -24)) # Cut out last 24 hours because of day lag
  end = cumsum(rle_event$lengths)
  start = c(1, tail((lag(end, k=1)+1), -1))
  length = rle_event[[1]]
  value = rle_event[[2]]
  event_table <- data.table(start, end, length, value)#  [which(value==0)]
  event_table
}

# Mark all the times where it has been an event for long enough time 
mark_action <- function(event_vect, action_len){
  n <- length(event_vect)
  newvect <- integer(n)
  # len is number of hours before alarm
  len <- action_len*24
  for (i in (len+1):n){
    if (sum(event_vect[(i-len):i]) >= len ){
      newvect[i] <- 1
    }
  }
  return(newvect)
}

# This function creates longer, non-overlapping events.
# set min and max duration in hours
add_events <- function(df, num_events, switch, min_len, max_len, perc){
  
  # percentiles for event magnitude sd
  qe <- perc
  qc <- perc
  qh <- perc
  qw <- perc
  
  # qe <- 0.90
  # qc <- 0.90
  # qh <- 0.90
  # qw <- 0.90
  
  
  # Set parameters for event shape: initial ramp up length (hours)
  ramp_min <- 0 
  ramp_max <- min_len # Not necessary, but ensures the ramp is not longer than event
  
  # Make the samples reproducible
  set.seed(7321)
  
  # Get length of every event
  lengths <- sample(seq(min_len,max_len), num_events, replace = TRUE)
  

  
  gap <- len_days # No events in the first period of data before threshold has values
  
  ## Electricity
  if (switch=="elec"){
    # Make the samples reproducible
    #set.seed(321)
    
    for (i in seq(num_events)){
      event_table <- run_fun(df$E_event) 
      
      # Length of event i (hours). reuse for all parameters
      len <- lengths[i]
      
      # Sample from periods of no event that are longer than length, plus buffer
      good_rows <- which((event_table$length > (len+4)) & (event_table$value == 0) & (event_table$start >= 24*gap))
      # store length of each non-event space to use for weighted sampling so they 
      # are more evenly distributed
      w8s <- event_table$length[good_rows] 
      
      if (length(good_rows) > 0) {
        if (length(good_rows) == 1) {
          # If it is a single number, sample makes a sequence 1 to the value, 
          # but you just want to use that row
          rownum <- good_rows
        } else { rownum <- sample(good_rows, 1, prob=w8s) }
        
        # In that window of no event, sample a starting point not too close to the end
        loc <- event_table$start[rownum] + sample(seq(1, (event_table$length[rownum] - len)), 1) 
        
        # Sample magnitude for elec
        mag_e <- event_shapr(df$Elec, qe, len, ramp_min, ramp_max)
        
        # print(len)
        # print(loc)
        
        # Add the event to the data 
        df$Elec[loc:(loc+len-1)] <- df$Elec[loc:(loc+len-1)] + mag_e
        df$Elec_HrLag[(loc+1):(loc+len)] <- df$Elec_HrLag[(loc+1):(loc+len)] + mag_e
        df$Elec_DayLag[(loc+24):(loc+len+23)] <- df$Elec_DayLag[(loc+24):(loc+len+23)] + mag_e
        df$E_event[loc:(loc+len-1)] <- df$E_event[loc:(loc+len-1)] +1  ##I did this instead of assigning 1 to make sure I wasn't getting overlap
      } else { cat(paste0('\nElec skipped event ',i,' length ',len)) }
    }
    df$E_action <- mark_action(df$E_event, action_len)
  }
  
  ## Chilled Water
  if (switch=="cool"){
    # Make the samples reproducible
    #set.seed(123)
    
    for (i in seq(num_events)){
      event_table <- run_fun(df$C_event)
      len <- lengths[i]
      # Sample from periods of no event that are longer than length
      good_rows <- which((event_table$length > (len+4)) & (event_table$value == 0) & (event_table$start >= 24*gap))
      w8s <- event_table$length[good_rows]
      
      if (length(good_rows) > 0) {
        if (length(good_rows) == 1) {
          rownum <- good_rows
        } else { rownum <- sample(good_rows, 1, prob=w8s) }
        
        #print(rownum)
        # In that window of no event, sample a starting point not too close to the end
        loc <- event_table$start[rownum] + sample(seq(1, (event_table$length[rownum] - len)), 1) 
        
        # Sample magnitude for Cool
        mag_c <- event_shapr(df$Cool, qc, len, ramp_min, ramp_max)
        
        # Add the event to the data
        df$Cool[loc:(loc+len-1)] <- df$Cool[loc:(loc+len-1)] + mag_c
        df$Cool_HrLag[(loc+1):(loc+len)] <- df$Cool_HrLag[(loc+1):(loc+len)] + mag_c
        df$Cool_DayLag[(loc+24):(loc+len+23)] <- df$Cool_DayLag[(loc+24):(loc+len+23)] + mag_c
        df$C_event[loc:(loc+len-1)] <- df$C_event[loc:(loc+len-1)] + 1
      } else { cat(paste0('\nCool skipped event ',i,' length ',len)) }
    }
    df$C_action <- mark_action(df$C_event, action_len)
  }
  
  ## Steam
  
  if (switch=="heat"){
    # Make the samples reproducible
    #set.seed(657)
    
    for (i in seq(num_events)){
      event_table <- run_fun(df$H_event)
      len <- lengths[i]
      # Sample from periods of no event that are longer than length
      good_rows <- which((event_table$length > (len+4)) & (event_table$value == 0) & (event_table$start >= 24*gap))
      w8s <- event_table$length[good_rows] 
      if (length(good_rows) > 0) {
        if (length(good_rows) == 1) {
          rownum <- good_rows
        } else { rownum <- sample(good_rows, 1, prob=w8s) }
        
        # In that window of no event, sample a starting point not too close to the end
        loc <- event_table$start[rownum] + sample(seq(1, (event_table$length[rownum] - len)), 1) 
        
        # Sample magnitude for Heat
        mag_h <- event_shapr(df$Heat, qh, len, ramp_min, ramp_max)
        #print(loc)
        #print(event_table$start[rownum])
        #print(event_table$length[rownum])
        #print(len)
        # Add the event to the data
        df$Heat[loc:(loc+len-1)] <- df$Heat[loc:(loc+len-1)] + mag_h
        df$Heat_HrLag[(loc+1):(loc+len)] <- df$Heat_HrLag[(loc+1):(loc+len)] + mag_h
        df$Heat_DayLag[(loc+24):(loc+len+23)] <- df$Heat_DayLag[(loc+24):(loc+len+23)] + mag_h
        df$H_event[loc:(loc+len-1)] <- df$H_event[loc:(loc+len-1)] + 1
      } else { cat(paste0('\nSteam skipped event ',i,' length ',len)) }
    }
    df$H_action <- mark_action(df$H_event, action_len)    
  }
  
  ## Water
  if (switch=="water"){
    # Make the samples reproducible
    #set.seed(789)
    
    for (i in seq(num_events)){
      event_table <- run_fun(df$W_event)
      len <- lengths[i]
      # Sample from periods of no event that are longer than length
      good_rows <- which((event_table$length > (len+4)) & (event_table$value == 0) & (event_table$start >= 24*gap))
      w8s <- event_table$length[good_rows] 
      if (length(good_rows) > 0) {
        if (length(good_rows) == 1) {
          rownum <- good_rows
        } else { rownum <- sample(good_rows, 1) }
        
        # In that window of no event, sample a starting point not too close to the end
        loc <- event_table$start[rownum] + sample(seq(1,(event_table$length[rownum] - len)), 1) 
        # Sample magnitude for Water
        mag_w <- event_shapr(df$Water, qw, len, ramp_min, ramp_max)
        
        df$Water[loc:(loc+len-1)] <- df$Water[loc:(loc+len-1)] + mag_w
        df$Water_HrLag[(loc+1):(loc+len)] <- df$Water_HrLag[(loc+1):(loc+len)] + mag_w
        df$Water_DayLag[(loc+24):(loc+len+23)] <- df$Water_DayLag[(loc+24):(loc+len+23)] + mag_w
        df$W_event[loc:(loc+len-1)] <- df$W_event[loc:(loc+len-1)] +1
      } else { cat(paste0('\nWater skipped event',i,'length',len)) } 
    }
    df$W_action <- mark_action(df$W_event, action_len)
  }
  
  
  return(df)
}

#takes in dataframe and returns four "x" matrices for model fitting, one for each of CEHW
subsetter <- function(df){
  y_c <- as.matrix(df["Cool"])
  y_e <- as.matrix(df["Elec"])
  y_h <- as.matrix(df["Heat"])
  y_w <- as.matrix(df["Water"])
  remove <- c("DateTime","year","month") # vector of columns you DON'T want
  # subset
  x_c <- as.matrix(df[, setdiff(names(df), c(remove, "Cool"))])
  x_e <- as.matrix(df[, setdiff(names(df), c(remove, "Elec"))])
  x_h <- as.matrix(df[, setdiff(names(df), c(remove, "Heat"))])
  x_w <- as.matrix(df[, setdiff(names(df), c(remove, "Water"))])
  newdata <- list(y_c,y_e,y_h,y_w,x_c,x_e,x_h,x_w)
  return(newdata)
}

# mods is list containing fitted models, data is df of unclean data with events marked
make_prediction <- function(mods, df, switch){
  y_hat = list()
  variables <- c("DateTime", "Elec", "Cool", "Heat" ,"Water",    
                "temp_F" ,    "year"  ,   "month" , "wday"  ,   "hour",      
                "Schoolday" ,   "Elec_HrLag" ,  "Cool_HrLag" ,  "Heat_HrLag" ,  "Water_HrLag",
                "Elec_DayLag",  "Cool_DayLag" , "Heat_DayLag" , "Water_DayLag" )
  data <- df
  data1 <-na.omit(data, target.colnames=variables) # Only omit NA if it is in the variables for the model
  y = list(data1$Cool, data1$Elec, data1$Heat, data1$Water)
  num_types <- length(mods)/4         # Number of different model types being tested
  x <- rep(subsetter(data1)[5:8], num_types)
  # for (i in 1:length(mods)){
  #   if (class(mods[[i]])[1]=="glm"){
  #     #print("doing lm")
  #     y_hat[[i]] <- predict(mods[[i]], newdata=data)
  #   } else {
  #     #print("not lm")
  #     y_hat[[i]] <- predict(mods[[i]], newdata=x[[i]])
  #   }
  # }
  # y_hat 1:4 is rf
  if (switch=="cool"){
    #data$cool_rf <- y_hat[[1]]
    data1$cool_rf <- predict(mods[[1]], newdata=x[[1]])
  }
  if (switch=="elec"){
    #data$elec_rf <- y_hat[[2]]
    data1$elec_rf <- predict(mods[[2]], newdata=x[[2]])
  }
  if (switch=="heat"){
    #data$heat_rf <- y_hat[[3]]
    data1$heat_rf <- predict(mods[[3]], newdata=x[[3]])
  }
  if (switch=="water"){
    #data$water_rf <- y_hat[[4]]
    data1$water_rf <- predict(mods[[4]], newdata=x[[4]])
  }
  # y_hat 5:8 is lm
  # data$cool_lm <- y_hat[[5]]
  # data$elec_lm <- y_hat[[6]]
  # data$heat_lm <- y_hat[[7]]
  # data$water_lm <- y_hat[[8]]
  #No more cubist
  
  return(data1)
}

se <- function(x) {
  val <- sd(x)/(sqrt(length(x)))
  val
}

# Calculate euclidian distance value scaled to sqrt(length)
ED <- function(x,y){
  z <- rbind(x,y)
  norm2 <- dist(z, method="euclidean") # Recycling will occur unannounced if lengths differ
  ed_val <- norm2/(sqrt(ncol(z))) # Normalize the 2nd norm by sqrt(length) to reduce bias
  ed_val
}

# Input two-column dataframe instead of two vectors
EDroll <- function(x){
  z <- t(x)
  norm2 <- dist(z, method="euclidean") # Recycling will occur unannounced if lengths differ
  ed_val <- norm2[1]/(sqrt(ncol(z))) # Normalize the 2nd norm by sqrt(length) to reduce bias
  ed_val
}

# Identify spikes in the data by comparing to previous data point
spikes <- function(y){
  big_diff <- numeric(length(y))
  index_spike <- list()
  temp <- y
  temp_sh <- data.table::shift(temp, n=1, fill=0, "lag") # Shift the deaccumulated data (look backwards)
  # Get a cutoff for the largest differences from one hour to another to look for unusual spikes (drops)
  big_diff <- (median(abs(temp-temp_sh), na.rm=TRUE) + 3*sd(abs(temp-temp_sh), na.rm=TRUE))
  # Save indices of each spike after a run of zeros
  index_spike <- which(abs(temp-temp_sh) >= big_diff)
}


##////////////////////////////////////////////////////////////
# Dynamic threshold functions
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# Call the med_se function below with frollapply to loop over data
# and calculate the thresholds v fast
med_se <- function(y,k) {
  val <- median(y)+(k*se(y))
  val
}

abs_err_fun <- function(df, num, se_num, avg_hrs, switch) {
  # absolute error values (moving average)
  # Calculate threshold values using moving window with size=num. apply the function
  # med_se and pass additional parameter k to that function
  if (switch=="elec"){
    df$elec_rf_err_abs <- frollmean(abs(df$Elec - df$elec_rf), avg_hrs)
    df$max_e_abs <- frollapply(df$elec_rf_err_abs, num, med_se, k=se_num[1])
  }
  if (switch=="cool"){
    df$cool_rf_err_abs <- frollmean(abs(df$Cool - df$cool_rf), avg_hrs)
    df$max_c_abs <- frollapply(df$cool_rf_err_abs, num, med_se, k=se_num[2])
  }
  if (switch=="heat"){
    df$heat_rf_err_abs <- frollmean(abs(df$Heat - df$heat_rf), avg_hrs)
    df$max_h_abs <- frollapply(df$heat_rf_err_abs, num, med_se, k=se_num[3])
  }
  if (switch=="water"){
    df$water_rf_err_abs <- frollmean(abs(df$Water - df$water_rf), avg_hrs)
    df$max_w_abs <- frollapply(df$water_rf_err_abs, num, med_se, k=se_num[4])
  }

  df
}

# Squared Error
sq_err_fun <- function(df, num, se_num, avg_hrs, switch){
  # Calculate threshold values using moving window with size=num. 
  se_num <- se_num 
  if (switch=="elec"){
    df$elec_rf_err_sq <- frollmean((df$Elec - df$elec_rf)^2, avg_hrs)
    df$max_e_sq <- frollapply(df$elec_rf_err_sq, num, med_se, k=se_num[1])
  }
  if (switch=="cool"){
    df$cool_rf_err_sq <- frollmean((df$Cool - df$cool_rf)^2, avg_hrs)
    df$max_c_sq <- frollapply(df$cool_rf_err_sq, num, med_se, k=se_num[2])
  }
  if (switch=="heat"){
    df$heat_rf_err_sq <- frollmean((df$Heat - df$heat_rf)^2, avg_hrs)
    df$max_h_sq <- frollapply(df$heat_rf_err_sq, num, med_se, k=se_num[3])
  }
  if (switch=="water"){
    df$water_rf_err_sq <- frollmean((df$Water - df$water_rf)^2, avg_hrs)
    df$max_w_sq <- frollapply(df$water_rf_err_sq, num, med_se, k=se_num[4])
  }

  df
}

# Scaled Error
sca_err_fun <- function(df, num, se_num, avg_hrs, switch) {
  # Calculate threshold values using moving window with size=num
  se_num <- se_num 
  if (switch=="elec"){
    df$elec_rf_err_sca <- frollmean((abs(df$Elec - df$elec_rf))/(pmax(1,df$Elec)), avg_hrs)
    df$max_e_sca <- frollapply(df$elec_rf_err_sca, num, med_se, k=se_num[1])
  }
  if (switch=="cool"){
    df$cool_rf_err_sca <- frollmean((abs(df$Cool - df$cool_rf))/(pmax(1,df$Cool)), avg_hrs)
    df$max_c_sca <- frollapply(df$cool_rf_err_sca, num, med_se, k=se_num[2])
  }
  if (switch=="heat"){
    df$heat_rf_err_sca <- frollmean((abs(df$Heat - df$heat_rf))/(pmax(1,df$Heat)), avg_hrs)
    df$max_h_sca <- frollapply(df$heat_rf_err_sca, num, med_se, k=se_num[3])
  }
  if (switch=="water"){
    df$water_rf_err_sca <- frollmean((abs(df$Water - df$water_rf))/(pmax(1,df$Water)), avg_hrs)
    df$max_w_sca <- frollapply(df$water_rf_err_sca, num, med_se, k=se_num[4])
  }
  
  df
}

# Euclidean distance
ed_err_fun <- function(df, num, se_num, ED_hrs, switch) {
  # Get all the values for the euclidean distance using window_length = ED_hrs
  # rollapply (package: zoo) has default align="center", change to "right" for look back
  # Get threshold values using the med+se function with window length = num 
  if (switch=="elec"){
    df$elec_rf_err_ed <- rollapply(df[, c("Elec","elec_rf")], ED_hrs, EDroll, 
                                   by.column=FALSE, fill=NA, align="right")
    df$max_e_ed <- frollapply(df$elec_rf_err_ed, num, med_se, k=se_num[1])
  }
  if (switch=="cool"){
    df$cool_rf_err_ed <- rollapply(df[, c("Cool","cool_rf")], ED_hrs, EDroll, 
                                   by.column=FALSE, fill=NA, align="right")
    df$max_c_ed <- frollapply(df$cool_rf_err_ed, num, med_se, k=se_num[2])
  }
  if (switch=="heat"){
    df$heat_rf_err_ed <- rollapply(df[, c("Heat","heat_rf")], ED_hrs, EDroll, 
                                   by.column=FALSE, fill=NA, align="right")
    df$max_h_ed <- frollapply(df$heat_rf_err_ed, num, med_se, k=se_num[3])
  }
  if (switch=="water"){
    df$water_rf_err_ed <- rollapply(df[, c("Water","water_rf")], ED_hrs, EDroll, 
                                    by.column=FALSE, fill=NA, align="right")
    df$max_w_ed <- frollapply(df$water_rf_err_ed, num, med_se, k=se_num[4])
  }
  
  df
}

# Currently not used  
# num is length of pattern to use for matrix profile, threshold is cutoff of 
# normalized distances for it to be a "match", se_num is multiplier for event threshold
MASS_fun <- function(df, num, se_num, threshold) {
  # Look back over certain time period (hours=num) and calculate the matrix profile.
  # Normalize all the distances and count the matches that are found (where distance 
  # is below a threshold like 0.025). Also calculate a dynamic threshold based on the 
  # number of matches to identify event if number of matches drops below the threshold
  
  match_counter <- function(pattern, dat, threshold){
    #num <- length(pattern)
    #metric <- dat
    # remove na
    dat <- ifelse(is.na(dat),0,dat)
    pattern <- ifelse(is.na(pattern),0,pattern)
    err_v2 <- dist_profile(dat, pattern, method = "v2")[[1]] / sqrt(num)
    # remove outliers
    #s = 1         #how many std to remove above
    err_v2[which(err_v2 > (median(err_v2, na.rm = TRUE)) + sd(err_v2, na.rm = TRUE))] <- NA
    # normalize
    err_v2 <- err_v2 / max(err_v2, na.rm=TRUE)
    # find matches
    matches <- length(which(err_v2 < threshold))
    matches
  }
  
  df$elec_matches <- frollapply(df$Elec, num, match_counter, dat=df$Elec, threshold=threshold)
  df$cool_matches <- frollapply(df$Cool, num, match_counter, dat=df$Cool, threshold=threshold)
  df$heat_matches <- frollapply(df$Heat, num, match_counter, dat=df$Heat, threshold=threshold)
  df$water_matches <- frollapply(df$Water, num, match_counter, dat=df$Water, threshold=threshold)

  # only Calculate the distance at certain intervals of by_num hours
  by_num <- 1
  # df$elec_matches <- rollapply(df$Elec, width=num, FUN=match_counter, by=by_num, align="right",
  #                              fill=NA, dat=df$Elec, threshold=threshold)
  # df$cool_matches <- rollapply(df$Cool, width=num, FUN=match_counter, by=by_num, align="right",
  #                              fill=NA, dat=df$Cool, threshold=threshold)
  # df$heat_matches <- rollapply(df$Heat, width=num, FUN=match_counter, by=by_num, align="right",
  #                              fill=NA, dat=df$Heat, threshold=threshold)
  # df$water_matches <- rollapply(df$Water, width=num, FUN=match_counter, by=by_num, align="right",
  #                               fill=NA, dat=df$Water, threshold=threshold)
  # 
  # Get threshold values using the med+se function with window length = num. 
  # negative one for subtract
  df$max_e_MASS <- pmax(1, frollapply(df$elec_matches, by_num*num, med_se, k=-1*se_num[1]))
  df$max_c_MASS <- pmax(1, frollapply(df$cool_matches, by_num*num, med_se, k=-1*se_num[2]))
  df$max_h_MASS <- pmax(1, frollapply(df$heat_matches, by_num*num, med_se, k=-1*se_num[3]))
  df$max_w_MASS <- pmax(1, frollapply(df$water_matches, by_num*num, med_se, k=-1*se_num[4]))
  
  df
}

#  ///////////////////////////     \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# ||||||||||||||||||||||||||| main ||||||||||||||||||||||||||||||||||
# \\\\\\\\\\\\\\\\\\\\\\\\\\\     //////////////////////////////////

# This is the main function for detecting events
guess_events <- function(df, len_hrs, k_abs, k_sq, k_sca, k_ed, avg_hrs, ED_hrs, switch, thresh=0.01){
  # err_type can be "absolute", "squared", "scaled", or "ED" (euclidean distance)
  # len_hrs is How many hours to look back threshold calculation
  # k_* is How many std errs (+median) for threshold 
  # q is quantile of the error values to use for threshold (need large "days")
  # ED_hrs is num of hours for euclidean distance
  
  # threshold for MASS
  # threshold <- thresh
  
  # Length of moving window for threshold calc
  num <- len_hrs
  #cat(paste("Euclidean distance interval length:",ED_hrs,"hours"))
  length <- nrow(df)
  # Initialize variables
  if(!("rf_E_event_abs" %in% colnames(df))) {
    df$rf_E_event_abs <- integer(length)
    df$rf_C_event_abs <- integer(length)
    df$rf_H_event_abs <- integer(length)
    df$rf_W_event_abs <- integer(length)
    df$rf_E_event_sq <- integer(length)
    df$rf_C_event_sq <- integer(length)
    df$rf_H_event_sq <- integer(length)
    df$rf_W_event_sq <- integer(length)
    df$rf_E_event_sca <- integer(length)
    df$rf_C_event_sca <- integer(length)
    df$rf_H_event_sca <- integer(length)
    df$rf_W_event_sca <- integer(length)
    df$rf_E_event_ed <- integer(length)
    df$rf_C_event_ed <- integer(length)
    df$rf_H_event_ed <- integer(length)
    df$rf_W_event_ed <- integer(length)
    df$rf_E_event_rule <- integer(length)
    df$rf_C_event_rule <- integer(length)
    df$rf_H_event_rule <- integer(length)
    df$rf_W_event_rule <- integer(length)
  }

  # df$rf_E_event_MASS <- integer(length)
  # df$rf_C_event_MASS <- integer(length)
  # df$rf_H_event_MASS <- integer(length)
  # df$rf_W_event_MASS <- integer(length)
  
  # absolute error values 
  df <- abs_err_fun(df, num, k_abs, avg_hrs, switch)
  
  # Squared Error
  df <- sq_err_fun(df, num, k_sq, avg_hrs, switch)
  
  # Scaled Error (by value of observation)
  df <- sca_err_fun(df, num, k_sca, avg_hrs, switch)
  
  # Euclidean distance (div by sqrt(length))
  df <- ed_err_fun(df, num, k_ed, ED_hrs, switch)
  
  # Count number of matches using matrix profile 
  #df <- MASS_fun(df, num, k_ed, threshold)
  
  # assign '1' to event variable if event is detected
  if (switch=="elec"){
    df$rf_E_event_abs[which(df$elec_rf_err_abs >= df$max_e_abs)] <- 1
    df$rf_E_event_sq[which(df$elec_rf_err_sq >= df$max_e_sq)] <- 1
    df$rf_E_event_sca[which(df$elec_rf_err_sca >= df$max_e_sca)] <- 1
    df$rf_E_event_ed[which(df$elec_rf_err_ed >= df$max_e_ed)] <- 1
    # Check for zero or negative and mark as event, other rules too in spike function
    df$rf_E_event_rule[which(df$Elec <= 0)] <- 1
    df$rf_E_event_rule[spikes(df$Elec)] <- 1
  }
  
  if (switch=="cool"){
    df$rf_C_event_abs[which(df$cool_rf_err_abs >= df$max_c_abs)] <- 1
    df$rf_C_event_sq[which(df$cool_rf_err_sq >= df$max_c_sq)] <- 1
    df$rf_C_event_sca[which(df$cool_rf_err_sca >= df$max_c_sca)] <- 1
    df$rf_C_event_ed[which(df$cool_rf_err_ed >= df$max_c_ed)] <- 1
    df$rf_C_event_rule[which(df$Cool <= 0)] <- 1
    df$rf_C_event_rule[spikes(df$Cool)] <- 1
  }
  
  if (switch=="heat"){
    df$rf_H_event_abs[which(df$heat_rf_err_abs >= df$max_h_abs)] <- 1
    df$rf_H_event_sq[which(df$heat_rf_err_sq >= df$max_h_sq)] <- 1
    df$rf_H_event_sca[which(df$heat_rf_err_sca >= df$max_h_sca)] <- 1
    df$rf_H_event_ed[which(df$heat_rf_err_ed >= df$max_h_ed)] <- 1
    df$rf_H_event_rule[which(df$Heat <= 0)] <- 1
    df$rf_H_event_rule[spikes(df$Heat)] <- 1
  }
  
  if (switch=="water"){
    df$rf_W_event_abs[which(df$water_rf_err_abs >= df$max_w_abs)] <- 1
    df$rf_W_event_sq[which(df$water_rf_err_sq >= df$max_w_sq)] <- 1
    df$rf_W_event_sca[which(df$water_rf_err_sca >= df$max_w_sca)] <- 1
    df$rf_W_event_ed[which(df$water_rf_err_ed >= df$max_w_ed)] <- 1
    df$rf_W_event_rule[which(df$Water <= 0)] <- 1
    ## Water spikes marks too many normal things
    # df$rf_W_event_rule[spikes(df$Water)] <- 1
  }
  
  
  ## For matrix profile, mark as event if LESS than threshold
  # df$rf_E_event_MASS[which(df$elec_matches <= df$max_e_MASS)] <- 1
  # df$rf_C_event_MASS[which(df$cool_matches <= df$max_c_MASS)] <- 1
  # df$rf_H_event_MASS[which(df$heat_matches <= df$max_h_MASS)] <- 1
  # df$rf_W_event_MASS[which(df$water_matches <= df$max_w_MASS)] <- 1

  return(df)
}

post_process_events <- function(df, action_len, time_frac, mod_frac){
  all_e <- df[,which(grepl("E_event_", names(df)))]
  num_mods <- ncol(all_e) # Assuming utilities are all the same
  all_c <- df[,which(grepl("C_event_", names(df)))]
  all_h <- df[,which(grepl("H_event_", names(df)))]
  all_w <- df[,which(grepl("W_event_", names(df)))]
  
  df$E_combined <- rowSums(all_e)
  df$C_combined <- rowSums(all_c)
  df$H_combined <- rowSums(all_h)
  df$W_combined <- rowSums(all_w)
  
  df$E_action_pred <- 0
  df$C_action_pred <- 0
  df$H_action_pred <- 0
  df$W_action_pred <- 0
  
  len <- action_len*24
  val <- time_frac*len
  for (i in (len+1):nrow(df)){
    #print(sum(df$E_combined[(i-len):i] >= mod_frac))
    if ((sum(df$E_combined[(i-len):i] >= mod_frac)) >= val){
      df$E_action_pred[i] <- 1
    }
    if (sum(df$C_combined[(i-len):i] >= mod_frac) >= val){
      df$C_action_pred[i] <- 1
    }
    if (sum(df$H_combined[(i-len):i] >= mod_frac) >= val){
      df$H_action_pred[i] <- 1
    }
    if (sum(df$W_combined[(i-len):i] >= mod_frac) >= val){
      df$W_action_pred[i] <- 1
    }
  }
  df
}

mcc_and_conf <- function(pred,act){
  val <- mcc(pred, act)
  conf_mat <- confusionMatrix(as.factor(pred), as.factor(act), positive='1')[['table']]
  result = list("MCC"=val, "CF" = conf_mat)
  return(result)
}


all_event <- function(df, models){
  j = 1
  for (switch in switches){
    # add events to one utility at a time
    df_fake <- add_events(df, num_events, switch, min_eventlen, max_eventlen, perc)
    # Make prediction on that utility while all other utilities have normal data
    # with no events
    df_preds <- make_prediction(models, df_fake, switch)
    
    # Save results in a new dataframe only on the first switch 
    if (j==1){
      dfsave <- guess_events(df_preds, len_hrs, k_abs, k_sq, k_sca, k_ed, avg_hrs, ED_hrs, switch)
      
      j = j+1
    }
    
    # save the important stuff so it is not overwritten in the next step using
    # the same dataframe
    if (switch=="elec"){
      dfsave$Elec <- df_preds$Elec
      dfsave$elec_rf <- df_preds$elec_rf
      dfsave$E_event <- df_preds$E_event
      dfsave$E_action <- df_preds$E_action
    }
    if (switch=="cool"){
      dfsave$Cool <- df_preds$Cool
      dfsave$cool_rf <- df_preds$cool_rf
      dfsave$C_event <- df_preds$C_event
      dfsave$C_action <- df_preds$C_action
    }
    if (switch=="heat"){
      dfsave$Heat <- df_preds$Heat
      dfsave$heat_rf <- df_preds$heat_rf
      dfsave$H_event <- df_preds$H_event
      dfsave$H_action <- df_preds$H_action
    }
    if (switch=="water"){
      dfsave$Water <- df_preds$Water
      dfsave$water_rf <- df_preds$water_rf
      dfsave$W_event <- df_preds$W_event
      dfsave$W_action <- df_preds$W_action
    }
    
    if (j!=1) {
      dfsave <- guess_events(dfsave, len_hrs, k_abs, k_sq, k_sca, k_ed, avg_hrs, ED_hrs, switch)
      
    }
    
  }
  return(dfsave)
}


# not used
plot_conf_mat <- function(confmat,title){
  TrueClass <- factor(c(0, 0, 1, 1))
  PredClass <- factor(c(0, 1, 0, 1))
  Y      <- c(confmat[1], confmat[2], confmat[3], confmat[4])
  df <- data.frame(TClass, PClass, Y)
  p <- ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
    geom_tile(aes(fill = Y), colour = "white") +
    geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
    scale_fill_gradient(low = "gray90", high = "green2") +
    theme_bw() + theme(legend.position = "none") + ggtitle(title)
  print(p)
}

check_events <- function(df,bldg) {
  rfe <- mcc_and_conf(df$rf_E_event,df$E_event)
  rfc <- mcc_and_conf(df$rf_C_event,df$C_event)
  rfh <- mcc_and_conf(df$rf_H_event,df$H_event)
  rfw <- mcc_and_conf(df$rf_W_event,df$W_event)
  mcc_rf <- data.table(parameter=c('Cool','Elec','Heat','Water'), rf_mcc=c(rfc[[1]],rfe[[1]],rfh[[1]],rfw[[1]]))
  plot_conf_mat(rfe[[2]],paste(bldg,"Elec, err type:",err_type,", thrshd stdevs:",sds,", num days:",len_days))
  plot_conf_mat(rfc[[2]],paste(bldg,"Cool, err type:",err_type,", thrshd stdevs:",sds,", num days:",len_days))
  plot_conf_mat(rfh[[2]],paste(bldg,"Heat, err type:",err_type,", thrshd stdevs:",sds,", num days:",len_days))
  plot_conf_mat(rfw[[2]],paste(bldg,"Water, err type:",err_type,", thrshd stdevs:",sds,", num days:",len_days))
  mcc_rf
}

# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# Precision recall curves
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# Turn confusion matrix into all the values we want for plotting
conf_process <- function(x,y){
  temp <- mcc_and_conf(x,y) # Get confusion matrix and MCC
  MCC <- temp[[1]]
  FPR <- temp[[2]][2]/sum(temp[[2]][c(1,2)]) # FP/FP+TN = FP/N
  TPR <- temp[[2]][4]/sum(temp[[2]][3:4])    # TP/TP+FN = TP/P
  PPV <- temp[[2]][4]/sum(temp[[2]][c(2,4)]) # TP/(TP+FP) = TP/(positive calls)
  res <- list(MCC,FPR,TPR, PPV)
  return(res)
}

# Take the FPR, PPV etc from list and rearrange into nice table
res_extract <- function(data, stat_list, name){
  dt <- data.table(mcc = sapply(stat_list, "[[", 1), fpr = sapply(stat_list, "[[", 2), 
                   tpr = sapply(stat_list, "[[", 3), ppv = sapply(stat_list, "[[", 4))
  new_names <- c(paste0(name,"_mcc"),paste0(name,"_fpr"),paste0(name,"_tpr"), paste0(name,"_ppv"))
  names(dt) <- new_names
  
  data <- cbind(data,dt)
  return(data)
}

# Input df like ECJtot, modslist like rf_mods[[1]], kvals like seq(0,10,2).
# Output of this function also used for ROC curve.
prec_rec <- function(df_in, mod_list, k_vals){
  C_res = list()
  E_res = list()
  H_res = list()
  W_res = list()
  
  for (i in 1:length(k_vals)){
    k <- rep(k_vals[i],4)
    k_abs <<- k
    k_sq  <<- k
    k_sca <<- k
    k_ed  <<- k
    
    # Run the event detection for that value of k
    # This is the slow step, so it runs and returns the data to be
    # processed by other functions
    
    df1 <- all_event(df_in, mod_list)
    df <- post_process_events(df1, action_len, time_frac, mod_frac)
    
    ## Store the values of mcc, tpr, fpr
    C_res[[i]] <- conf_process(df$C_action_pred,df$C_action) # get MCC, FPR, TPR, PPV
    E_res[[i]] <- conf_process(df$E_action_pred,df$E_action)
    H_res[[i]] <- conf_process(df$H_action_pred,df$H_action)
    W_res[[i]] <- conf_process(df$W_action_pred,df$W_action)
    
    ## Optional version with "event" instead of "action" (more total positive hours)
    # C_res[[i]] <- conf_process(df$C_action_pred,df$C_event)
    # E_res[[i]] <- conf_process(df$E_action_pred,df$E_event)
    # H_res[[i]] <- conf_process(df$H_action_pred,df$H_event)
    # W_res[[i]] <- conf_process(df$W_action_pred,df$W_event)
  }
  k_vals <- k_vals
  result_c <- data.table(se_mult=k_vals, thresh_len=rep(len_hrs,length(k_vals)))
  result_e <- data.table(se_mult=k_vals, thresh_len=rep(len_hrs,length(k_vals)))
  result_h <- data.table(se_mult=k_vals, thresh_len=rep(len_hrs,length(k_vals)))
  result_w <- data.table(se_mult=k_vals, thresh_len=rep(len_hrs,length(k_vals)))
  
  result_c <- res_extract(result_c, C_res, "Cool")
  result_e <- res_extract(result_e, E_res, "Elec") #[, c("se_mult","thresh_len"):=NULL] 
  result_h <- res_extract(result_h, H_res, "Steam") #[, c("se_mult","thresh_len"):=NULL] 
  result_w <- res_extract(result_w, W_res, "Water") #[, c("se_mult","thresh_len"):=NULL] 
  
  result <- list(Cool=result_c, Elec=result_e, Heat=result_h, Water=result_w)
  
  result_df <- do.call(cbind, result)
  return(result_df)
}

# For processing multiple model evals to make precision recall curves
gatherlist <- function(list, subindex){
  dftot <- data.frame()
  for (i in 1:length(list)){
    df <- list[[i]][[subindex]]
    dftot <- rbind(dftot, df)
  }
  return(dftot)
}

#  plot tpr vs. ppv
plot_prec <- function(df, bldg){
  p1 <- plot_ly(df, x=~Cool.Cool_tpr, y=~Cool.Cool_ppv, name="Cool", mode="lines", type="scatter") %>%
    add_trace(x=~Elec.Elec_tpr, y=~Elec.Elec_ppv, name="Elec", mode="lines") %>%
    add_trace(x=~Heat.Steam_tpr, y=~Heat.Steam_ppv, name="Steam", mode="lines") %>%
    add_trace(x=~Water.Water_tpr, y=~Water.Water_ppv, name="Water", mode="lines") %>%
    add_annotations(x=-0.17, y=1, xref = "paper", yref = "y", text = "(b)",
                    xanchor = 'right', showarrow = F,font=list(size=14)  ) %>% 
    #title=paste(bldg, "Precision/Recall curve, threshold", (df$Cool.thresh_len[1])/24, "days"),
    layout(yaxis=list(title='Precision', range=c(0,1)),
           xaxis = list(title='Recall', range=c(0,1)))
  if (save_img){
    wd <- getwd()
    setwd(fig_dir) # Change directory for saving figures
    #if (!exists("adjust")) { adjust = 1 }
    adjust = .8
    orca(p1, paste0(bldg,"-prec-rec-example-threshold-", (df$Cool.thresh_len[1])/24, "-days.svg"), width=fig_width*adjust, height=fig_height*adjust)
    
    setwd(wd) # Change back to original working directory
  }
  
  print(p1) 
  #return(p1)
}

# plot x=fpr, y= tpr for long events (use prec rec function above for model performance)
plot_roc_long <- function(df, bldg){
  p1 <- plot_ly(df, x=~Cool.Cool_fpr, y=~Cool.Cool_tpr, name="Cool", mode="lines", type="scatter") %>%
    add_trace(x=~Elec.Elec_fpr, y=~Elec.Elec_tpr, name="Elec", mode="lines") %>%
    add_trace(x=~Heat.Steam_fpr, y=~Heat.Steam_tpr, name="Steam", mode="lines") %>%
    add_trace(x=~Water.Water_fpr, y=~Water.Water_tpr, name="Water", mode="lines") %>%
    #title=paste(bldg, "ROC curve, threshold", (df$Cool.thresh_len[1])/24, "days"),
    add_annotations(x=-0.17, y=1, xref = "paper", yref = "y", text = "(a)",
                    xanchor = 'right', showarrow = F, font=list(size=14) ) %>% 
    layout(yaxis=list(title='True Positive Rate', range=c(0,1)),
           xaxis = list(title='False Positive Rate', range=c(0,1)))
  
  if (save_img){
    wd <- getwd()
    setwd(fig_dir) # Change directory for saving figures
    
    if (!exists("adjust")) { adjust = 1 }
    adjust = .8
    orca(p1, paste0(bldg,"-ROC-example-threshold-", (df$Cool.thresh_len[1])/24, "-days.svg"), width=fig_width*adjust, height=fig_height*adjust)
    
    setwd(wd) # Change back to original working directory
  }
  if (viewer){
    print(p1)
  }
  
  #return(p1)
}



# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# Pattern detection with SAX (symbolic aggregate approximation)
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# Function: ed_sax(x, y, w, a)
# Written by Connor Chewning
# This function takes two time series, (x, y), y is assumed to be shorter in
# length than x, and computes the minimum SAX distance profile using the
# diss.MINDIST.SAX() function from the TSclust package.

# Parameters: x <- main data stream
# y <- pattern to be compared (must be shorter in length than x)
# w <- the amount of equal sized frames that the series will be reduced to 
#      During SAX calculation
# a <- size of alphabet

library(TSclust)

ed_sax <- function(x, y, w, a){
  
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
