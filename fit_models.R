# ..........................................
# Fit models and evaluate for 3 buildings
# ..........................................
# Linear regression, cubist, randomforest
# Summary plots of random forest variable importance
# Justin DuRant
# revised May 2020

# Note:
# This script uses the orca utility to save pdfs of some plots. The orca program
# needs to be installed on your computer first. Find the instructions at
# https://github.com/plotly/orca

rm(list=ls())

library(rpart)
library(Cubist)
library(lubridate)
library(plyr)
library(leaps)
library(stringr)
library(randomForest)
library(xts)
library(magrittr)
library(knitr)
library(data.table)
library(BBmisc)
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


data_dir <- paste0(getwd(),"/data")

load(file.path(data_dir,"hourly_CEHW.RData"))

# directory to save tables and figures
tab_dir <- paste0(getwd(),"/table")
fig_dir <- paste0(getwd(),"/fig")

if(!dir.exists(tab_dir)){ dir.create(tab_dir) }
if(!dir.exists(fig_dir)){ dir.create(fig_dir) }


train_test <- function(df) {
  ## 75% of the sample 
  smp_size <- floor(0.75 * nrow(df))
  
  ## set the seed to make partition reproducible
  set.seed(123)
  ## Vector of row index to use in training. Default is no replacement.
  train_ind <- sample(seq_len(nrow(df)), size = smp_size)
  
  train <- df[train_ind, ]
  test <- df[-train_ind, ]
  return(list(train, test))
}

#takes in dataframe and returns four "x" matrices for model fitting, one for each CEHW
subsetter <- function(df){
  df <- df[complete.cases(df),] # Get rid of any rows with NA values
  # y_c <- as.matrix(df["Cool"])
  # y_e <- as.matrix(df["Elec"])
  # y_h <- as.matrix(df["Heat"])
  # y_w <- as.matrix(df["Water"])
  y_c <- df["Cool"]
  y_e <- df["Elec"]
  y_h <- df["Heat"]
  y_w <- df["Water"]
  remove <- c("DateTime","year","month") # vector of columns you DON'T want
  # subset
  # x_c <- as.matrix(df[, setdiff(names(df), c(remove, "Cool"))])
  # x_e <- as.matrix(df[, setdiff(names(df), c(remove, "Elec"))])
  # x_h <- as.matrix(df[, setdiff(names(df), c(remove, "Heat"))])
  # x_w <- as.matrix(df[, setdiff(names(df), c(remove, "Water"))])
  x_c <- df[, setdiff(names(df), c(remove, "Cool"))]
  x_e <- df[, setdiff(names(df), c(remove, "Elec"))]
  x_h <- df[, setdiff(names(df), c(remove, "Heat"))]
  x_w <- df[, setdiff(names(df), c(remove, "Water"))]
  newdata <- list(y_c,y_e,y_h,y_w,x_c,x_e,x_h,x_w)
  return(newdata)
}

# Process regsubsets() enumeration of linear regression options
print_best <- function(regsub_obj, var_name){
  t <- summary(regsub_obj, all.best=TRUE)
  t2 <- cbind(t$which,t$bic)
  #t2[order(ncol(t2))]
  t3 <- t2[order(t2[,ncol(t2)]),]
  z <- data.frame(names(which(t3[1,]==1))) #Names of the variables for best model
  names(z) <- var_name
  return(z)
}


# Determine which variables are good for lm
lm_var_test <- function(df,bldg){
  #subsets <- subsetter(na.omit(data))
  #y_ls <- subsets[1:4]
  #x_ls <- subsets[5:8]
  regsub_cool <- regsubsets(Cool ~ Elec+Heat+Water+wday+hour+Schoolday+Elec_HrLag+Cool_HrLag+Heat_HrLag+
                              Water_HrLag+Water_DayLag+Elec_DayLag+Cool_DayLag+Heat_DayLag+temp_F, nvmax=16, nbest=4,
                            data=as.data.frame(df), method = "exhaustive")
  cool <- print_best(regsub_cool, "cool_vars")
  #print(summary(regsub_cool, matrix=FALSE))
  #print(coef(regsub_cool, id=1))
  #plot(regsub_cool, main=paste(bldg,"regsubsets Cool"))
  #plot(regsub_cool, main=paste(bldg,"regsubsets Cool", scale="Cp"))

  regsub_elec <- regsubsets(Elec ~ Cool+Heat+Water+wday+hour+Schoolday+Elec_HrLag+Cool_HrLag+Heat_HrLag+
                              Water_HrLag+Water_DayLag+Elec_DayLag+Cool_DayLag+Heat_DayLag+temp_F, nvmax=16, nbest=4,
                            data=as.data.frame(df))
  elec <- print_best(regsub_elec, "elec_vars")
  #plot(regsub_elec, main=paste(bldg,"regsubsets Elec"))
  #plot(regsub_elec, main=paste(bldg,"regsubsets Elec", scale="Cp"))

  regsub_heat <- regsubsets(Heat ~ Cool+Elec+Water+wday+hour+Schoolday+Elec_HrLag+Cool_HrLag+Heat_HrLag+
                              Water_HrLag+Water_DayLag+Elec_DayLag+Cool_DayLag+Heat_DayLag+temp_F, nvmax=16, nbest=4,
                            data=as.data.frame(df))
  heat <- print_best(regsub_heat, "heat_vars")
  #plot(regsub_heat, main=paste(bldg,"regsubsets Heat"))
  #plot(regsub_heat, main=paste(bldg,"regsubsets Heat", scale="Cp"))

  regsub_water <- regsubsets(Water ~ Cool+Elec+Heat+wday+hour+Schoolday+Elec_HrLag+Cool_HrLag+Heat_HrLag+
                              Water_HrLag+Water_DayLag+Elec_DayLag+Cool_DayLag+Heat_DayLag+temp_F, nvmax=16, nbest=4,
                            data=as.data.frame(df))
  water <- print_best(regsub_water, "water_vars")
  #plot(regsub_water, main=paste(bldg,"regsubsets Water"))
  #plot(regsub_water, main=paste(bldg,"regsubsets Water"), scale="Cp")
  
  all <- list(cool, elec, heat, water)

  max_vars = 15
  for (i in 1:length(all)){
    q <- nrow(all[[i]])
    if (q > max_vars) {max_vars <- q}
  }
  
  p = list()
  for (i in 1:length(all)){
    p[i] <- lapply(all[[i]], "length<-", max_vars)
  }
  
  alldf <- data.frame(p[[1]], p[[2]], p[[3]], p[[4]])
  alldf <- setNames(alldf, c("cool", "elec", "heat", "water"))
  alldf
}

lmvars <- list()

for (i in 1:length(dfs_clean)){
  lmvars[[i]] <- lm_var_test(dfs_clean[[i]], bldg_names[i])
}

names(lmvars) <- bldg_names

cool_vars <- sapply(lmvars, "[[", 1)
elec_vars <- sapply(lmvars, "[[", 2)
heat_vars <- sapply(lmvars, "[[", 3)
water_vars <- sapply(lmvars, "[[", 4)


# Display variables for cool prediction, select ones that appear a majority of the time
print(cool_vars)
# at least 2/3: Elec, heat, hour, Elec_HrLag, Cool_HrLag, Heat_HrLag, Cool_DayLag, Elec_DayLag, temp_F

# elec
print(elec_vars)
# Cool, Heat, Water, wday, hour, Schoolday, Elec_HrLag, Cool_HrLag, Heat_HrLag, Water_HrLag, Elec_DayLag, Heat_DayLag, temp_F

# heat
print(heat_vars)
# Cool, Elec, Water, hour, Elec_HrLag, Cool_HrLag, Heat_HrLag, Cool_DayLag, Heat_DayLag, temp_F

# water
print(water_vars)
# Elec, hour, Heat, Elec_HrLag, Heat_HrLag, Water_HrLag, Water_DayLag


# Number of trees for random forest model. Default is 500, increasing can improve 
# accuracy but requires more computation time. 
treenum <- 701
# take out Cubist for now. lm formulas based on regsubsets results
fit_mods <- function(train_data) {
  subsets <- subsetter(train_data)
  y_ls <- subsets[1:4]
  x_ls <- subsets[5:8]
  cat('\n|--------------|\n')
  # The extra [,1] in the response vector is to extract the vector from the data.frame, which is acting like a list
  rf_cool <- randomForest(x=x_ls[[1]], y=y_ls[[1]][,1], ntree=treenum, importance=TRUE,
                          nPerm = 1)
  cat('--')
  rf_elec <- randomForest(x=x_ls[[2]], y=y_ls[[2]][,1], ntree=treenum, importance=TRUE,
                          nPerm = 1)
  cat('--')
  rf_heat <- randomForest(x=x_ls[[3]], y=y_ls[[3]][,1], ntree=treenum, importance=TRUE,
                          nPerm = 1)
  cat('--')
  rf_water <- randomForest(x=x_ls[[4]], y=y_ls[[4]][,1], ntree=treenum, importance=TRUE,
                          nPerm = 1)
  cat('--')
  cb_cool <- cubist(x_ls[[1]], y_ls[[1]][,1], committees = 4, control = cubistControl(),
                      weights = NULL)
  cat('-')
  cb_elec <- cubist(x_ls[[2]], y_ls[[2]][,1], committees = 4, control = cubistControl(),
                    weights = NULL)
  cat('-')
  cb_heat <- cubist(x_ls[[3]], y_ls[[3]][,1], committees = 4, control = cubistControl(),
                    weights = NULL)
  cat('-')
  cb_water <- cubist(x_ls[[4]], y_ls[[4]][,1], committees = 4, control = cubistControl(),
                    weights = NULL)
  cat('-')
  lm_cool <- glm(Cool ~ Elec+Heat+hour+Elec_HrLag+Cool_HrLag+Heat_HrLag+Cool_DayLag+Elec_DayLag+temp_F,
                 data=as.data.frame(train_data))
  cat('-')
                 # old model: Elec+Schoolday+Elec_HrLag+Cool_HrLag+Water_DayLag+Cool_DayLag,
  lm_elec <- glm(Elec ~ Cool+Heat+Water+wday+hour+Schoolday+Elec_HrLag+Cool_HrLag+Heat_HrLag+Water_HrLag+Elec_DayLag+Heat_DayLag+temp_F,
                 data=as.data.frame(train_data)) 
  cat('-')
                 # old model: Cool+Elec_HrLag+Cool_HrLag+Heat_HrLag+Elec_DayLag+temp_F,
  lm_heat <- glm(Heat ~ Cool+Elec+Water+hour+Elec_HrLag+Cool_HrLag+Heat_HrLag+Cool_DayLag+Heat_DayLag+temp_F,
                 data=as.data.frame(train_data))
  cat('-')
                 # old model: Heat_HrLag+Elec_DayLag+Heat_DayLag+temp_F
  lm_water <- glm(Water ~ Elec+hour+Heat+ Elec_HrLag+Heat_HrLag+Water_HrLag+Water_DayLag,
                  data=as.data.frame(train_data)) 
  cat('-\n')
                # old model: hour+Water_HrLag+Water_DayLag+Elec+Elec_DayLag
  models <- list(rf_cool,rf_elec,rf_heat,rf_water, cb_cool,cb_elec,cb_heat,cb_water, lm_cool,lm_elec,lm_heat,lm_water)
  #models <- list(rf_cool,rf_elec,rf_heat,rf_water, lm_cool,lm_elec,lm_heat,lm_water)
  names(models) <- c("rf_cool","rf_elec","rf_heat","rf_water","cb_cool","cb_elec","cb_heat","cb_water","lm_cool","lm_elec","lm_heat","lm_water")
  #names(models) <- c("rf_cool","rf_elec","rf_heat","rf_water","lm_cool","lm_elec","lm_heat","lm_water")
  return(models)
}

rsq <- function(x, y){
  if (length(x)==length(y)){
    cor(x, y, use = "complete.obs")^2
  } else {
    print("Length error")
    return(0)
    }
}


RMSE <- function(obs, pred) {
  temp <- cbind(obs,pred)
  temp <- temp[complete.cases(temp), ] #Get rid of rows with NA in Pred or Obs
  result <- sqrt(mean((temp[,1] - temp[,2])^2))
  #print(sprintf('RMSE is %.3f', result))
  return(result)
}

# mods is list containing fitted models, data is list containing train df and test df, building is text building name
test_rf <- function(mods,data,building){
  y_hat = list()
  y_hat_train = list()
  train_data <- na.omit(data[[1]])
  test_data <- na.omit(data[[2]])
  y_train = list(train_data$Cool, train_data$Elec, train_data$Heat, train_data$Water)
  num_types <- length(mods)/4         # Number of different model types being tested
  y_train <- rep(y_train, num_types)
  y_test = list(test_data$Cool, test_data$Elec, test_data$Heat, test_data$Water)
  y_test <- rep(y_test, num_types)
  xtrain <- rep(subsetter(na.omit(train_data))[5:8], num_types)
  xtest <- rep(subsetter(na.omit(test_data))[5:8], num_types)
  for (i in 1:length(mods)){
    if (class(mods[[i]])[1]=="glm"){
      #print("lm")
      y_hat_train[[i]] <- predict(mods[[i]], newdata=train_data)
      y_hat[[i]] <- predict(mods[[i]], newdata=test_data)
    } else if (class(mods[[i]])[1]=="cubist"){
      #print("cubist")
      y_hat_train[[i]] <- predict(mods[[i]], newdata=xtrain[[i]], neighbors=2)
      y_hat[[i]] <- predict(mods[[i]], newdata=xtest[[i]], neighbors=2)
    } else {
      #print("rf")
      y_hat_train[[i]] <- predict(mods[[i]], newdata=xtrain[[i]])
      y_hat[[i]] <- predict(mods[[i]], newdata=xtest[[i]])
      varImpPlot(mods[[i]], main=paste(building,names(mods)[i]))
    }
    #varImpPlot(mods[[i]], main=paste(building,names(mods)[i]))
  }
  num <- length(mods)
  rsqval <- data.table(model = names(mods), trainR2 = numeric(num), testR2 = numeric(num))
  rmseval <- data.table(model = names(mods), trainRMSE = numeric(num), testRMSE = numeric(num))
  #print("starting eval")
  for (i in 1:length(y_hat)){
    rsqval[i,2] <- rsq(y_train[[i]],y_hat_train[[i]])
    rmseval[i,2] <- RMSE(y_train[[i]],y_hat_train[[i]])
    rsqval[i,3] <- rsq(y_test[[i]],y_hat[[i]])
    rmseval[i,3] <- RMSE(y_test[[i]],y_hat[[i]])
    #plot(y_test[[i]],y_hat[[i]], main=names(mods)[i])
  }
  # print(rsqval)
  # print(rmseval)
  output <- list(rsq=rsqval,rmse=rmseval)
  return(output)
}


# ///////////////////////////////////////////////////////
# Fit the models to the data and evaluate
# ///////////////////////////////////////////////////////

all_mods <- list()

for (i in 1:length(dfs_clean)) {
  
  # Split up the data, 70% for training. Returns list.
  train_test_list <- train_test(dfs_clean[[i]])
  
  # Some settings for the cubist model
  cubistControl(unbiased = FALSE, rules = NA, extrapolation = 100,
                seed = 321, label = "outcome")
  
  # Extract training data from list and send to model fitting function
  mods_list <- fit_mods(na.omit(train_test_list[[1]]))
  
  # Save the model objects in the workspace (optional)
  #assign(paste0(bldg_names[i],"_mods"), mods_list)
  
  # Save the models into a single list
  all_mods[[i]] <- mods_list
  names(all_mods)[i] <- paste0(bldg_names[i],"_mods")
}



save(all_mods, file=file.path(data_dir,"models_with_cubist.RData"))


#print linear model equations
# for (j in 1:3){
#   print(names(all_mods)[[j]])
#   for (i in 9:12){
#     print(names(all_mods[[j]])[[i]])
#     print(all_mods[[j]][[i]][["coefficients"]])
#   }
# }


# Save just the random forest models in a different file so it doesn't take so
# long to load
rf_mods701 <- list(ECJ_mods=all_mods[[1]][1:4], 
                   GDC_mods=all_mods[[2]][1:4], 
                   JCD_mods=all_mods[[3]][1:4])
save(rf_mods701,  file=file.path(data_dir,"rf_mods701.RData"))
#load("rf_mods701.RData")

# pdf(file="r2_and_RMSE.pdf")
# for (i in 1:length(all_mods)){
#   df <- dfs_clean[[i]]
#   train_test_list <- train_test(df)
#   eval_output <- test_rf(all_mods[[i]][c(1:4,9:12)],train_test_list,names(dfs_clean)[i]) #Function gives r2 table and rmse table
#   rmse_t <- t(eval_output[[2]][,2:3])
#   r2_t <- t(eval_output[[1]][,2:3])
#   colnames(r2_t) <- as.list(eval_output[[1]]$model)
#   colnames(rmse_t) <- as.list(eval_output[[2]]$model)
#   barplot(as.matrix(r2_t), beside=TRUE, main = paste(names(dfs_clean)[i],"R^2 values (cleaned data)"),
#           las=2, legend=TRUE, xlim = c(0, 50))
#   barplot(as.matrix(rmse_t), beside=TRUE, main = paste(names(dfs_clean)[i],"RMSE values (cleaned data)"),
#           las=2, legend=TRUE, xlim = c(0, 50))
# }
# dev.off()


# Write r2 and rmse values to csv file
wd <- getwd()
setwd(tab_dir)
file_name = "all_mods_eval_tables_rf.csv"
sink(file_name)
for (i in 1:length(all_mods)){
  df <- dfs_clean[[i]]
  train_test_list <- train_test(df)
  eval_output <- test_rf(all_mods[[i]],train_test_list,names(dfs_clean)[i]) #Function gives r2 table and rmse table
  rmse_t <- t(eval_output[[2]][,2:3])
  r2_t <- t(eval_output[[1]][,2:3])
  colnames(r2_t) <- as.list(eval_output[[1]]$model)
  colnames(rmse_t) <- as.list(eval_output[[2]]$model)
  
  cat(names(all_mods)[i], sep="\n", append=TRUE)
  # print(eval_output[[1]])
  # print(eval_output[[2]])
  write.csv(eval_output[[1]])
  write.csv(eval_output[[2]])
  
  # for (i in 1:length(tab)){
  #   cat(params[i], sep="\n", append=TRUE)
  #   write.csv(tab[[i]]) 
  # }
}
sink()
setwd(wd)

# pdf(file="rf_varImp.pdf", width=7, height=5)
# for (i in 1:length(all_mods)){
#   df <- dfs_clean[[i]]
#   train_test_list <- train_test(df)
#   eval_output <- test_rf(all_mods[[i]][c(1:4,9:12)],train_test_list,names(dfs_clean)[i]) #Function gives r2 table and rmse table
# }
# dev.off()



# ///////////////////////////////////////////////////////
# Combine variable importance ranking across models 
# ///////////////////////////////////////////////////////

# return data table with %IncMSE and ranking of variables
extract_imp <- function(mod) {
  t <- mod[["importance"]]
  sorted <- t[order(-t[,1]),]
  ranked <- cbind(sorted[,1],seq(nrow(t), 1, -1))
  colnames(ranked) <- c("%IncMSE","rank")
  t2 <- data.table(ranked, keep.rownames=TRUE)
  return(t2)
}

importance_summ <- function(models){
  cool <- extract_imp(models[[1]])
  elec <- extract_imp(models[[2]])
  heat <- extract_imp(models[[3]])
  water <- extract_imp(models[[4]])
  var_tabs <- list(cool=cool, elec=elec, heat=heat, water=water)
  return(var_tabs)
}



# Add up numerical ranking of each parameter
combine_par <- function(tables){
  comb1 <- join(tables[[1]], tables[[2]], by="rn")
  comb2 <- join(comb1, tables[[3]], by="rn")
  comb2$sum <- comb2[,3]+comb2[,5]+comb2[,7]
  comb2
}

# Make the plots
plot_imp <- function(dt, nametext){
  dt$var_name <- factor(dt$rn, levels = dt$rn)
  p <- plot_ly(dt, x = ~var_name, y = ~sum, type = 'bar', name = 'var-rank') %>%
    layout(title=nametext, yaxis = list(title = '%IncMSE variable ranking'), showlegend=FALSE)
  if(viewer) {print(p)}
  if(save_img){
    wd <- getwd()
    setwd(fig_dir)
    orca(p, file= paste0(nametext,".pdf"))
    setwd(wd)
  }
}

# combine all the variable rankings for each parameter, send to plot function
combine_imp <- function(var_tabs){
  cool_list <- sapply(var_tabs, "[", 1)
  cool <- combine_par(cool_list)
  plot_imp(cool[order(-cool$sum),],"variable ranking Chilled Water" )
  
  elec_list <- sapply(var_tabs, "[", 2)
  elec <- combine_par(elec_list)
  plot_imp(elec[order(-elec$sum),],"variable ranking Electricity" )
  
  heat_list <- sapply(var_tabs, "[", 3)
  heat <- combine_par(heat_list)
  plot_imp(heat[order(-heat$sum),],"variable ranking Steam")
  
  water_list <- sapply(var_tabs, "[", 4)
  water <- combine_par(water_list)
  plot_imp(water[order(-water$sum),],"variable ranking Water")
}

plot_all_varImp <- function(mod_list){
  var_tabs <- list()
  for(i in 1:length(mod_list)){
    var_tabs[[i]] <- importance_summ(mod_list[[i]])
  }
  combine_imp(var_tabs)
}

plot_all_varImp(rf_mods701)



