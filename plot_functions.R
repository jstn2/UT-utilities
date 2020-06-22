# 04/2020
# Functions for creating plots. 
# Collected from other scripts

# Note: In RStudio use Alt+o to collapse all functions or Alt+Shift+o to expand all

library(rpart)
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
library(leaps)
library(stringr)
library(dygraphs)
library(data.table)
library(plotly)


# Suppress automatic opening of plot in browser when running api_create()
options(browser = 'false')

#.................................
#..p\\\\\...l\....................
#..p\...\\..l\..............t\....
#..p\...\\..l\..............t\....
#..p\\\\\...l\....o\\\...t\\\\\\..
#..p\.......l\...o...\\.....t\....
#..p\.......l\...o ..\\.....t\....
#..p\.......l\....o\\\......t\\...
#..-----------------------------..

# Aggregate hourly data to daily.
# This needs lubridate package, "date()" is something else in base R
make_daily <- function(df){
  df$date <- date(df$DateTime)
  daily_c <- aggregate(df$Cool, by=list(df$date), FUN=sum, na.rm=TRUE, na.action=NULL)
  daily_e <- aggregate(df$Elec, by=list(df$date), FUN=sum, na.rm=TRUE, na.action=NULL)
  daily_h <- aggregate(df$Heat, by=list(df$date), FUN=sum, na.rm=TRUE, na.action=NULL)
  daily_w <- aggregate(df$Water, by=list(df$date), FUN=sum, na.rm=TRUE, na.action=NULL)
  newdf <- data.table(DateTime = daily_c[,1], Cool = daily_c[,2], Elec = daily_e[,2], Heat = daily_h[,2], 
                      Water = daily_w[,2] )
  newdf
}


# Make plots of whole year of daily data
make_plot <- function(df, bldg){
  p_df <- plot_ly(df, x = ~DateTime, y = ~Elec, 
                  type="scatter", mode="lines", name="Elec",
                  line = list(width = 1)) %>%
    add_trace(y = ~Cool, name = 'Cool') %>%
    add_trace(y = ~Heat, name = 'Heat') %>%
    add_trace(y = ~Water, name = 'Water') %>%
    layout(title = bldg, yaxis=list(title="Daily Usage"))
  if(viewer){print(p_df)}
  if(save_img){
    wd <- getwd()
    setwd(fig_dir)
    orca(p_df, paste0(bldg,"-daily.pdf"))
    setwd(wd)
  }
  if(upload){
    api_create(p_df, filename = paste0(name,"-daily"))
  }
  #htmlwidgets::saveWidget(as_widget(p_df), paste(bldg, ".html", sep=""))
}

# Plot daily data in all building by parameter
make_plot2 <- function(df, name, units){
  p_df <- plot_ly(df,  type="scatter", mode="lines",
                  line = list(width = 2)) 
    for (i in 1:length(bldg_names)){
      p_df <- add_trace(p_df, x=df$DateTime, y=df[[bldg_names[i]]], name=bldg_names[i]) 
    }
    p_df <-  layout(p_df, title = name, yaxis=list(title=paste("Daily Usage", units)))
  if(viewer){print(p_df)}
  if(save_img){
    wd <- getwd()
    setwd(fig_dir)
    orca(p_df, paste0(name,"-daily.pdf"))
    setwd(wd)
  }
  if(upload){
    api_create(p_df, filename = paste0(name,"-daily"))
  }
  #htmlwidgets::saveWidget(as_widget(p_df), paste(name, ".html", sep=""))
}


# Plot daily or hourly intensity (per sqft) sorted by parameter
make_plot3 <- function(df, name, units, sqft, linewide=2){
  p_df <- plot_ly(df, type="scatter", mode="lines",
                  line = list(width = linewide))
    for (i in 1:length(bldg_names)){
      p_df <- add_trace(p_df, x=df$DateTime, y=df[[bldg_names[i]]]/sqft[i], name=bldg_names[i]) 
    }
    
    # add_trace(y = ~ETC/sqft[2], name = 'ETC') %>%
    # add_trace(y = ~GDC/sqft[3], name = 'GDC') %>%
    # add_trace(y = ~JCD/sqft[4], name = 'JCD') %>%
    p_df <- layout(p_df, title = name, 
                   yaxis=list(title=paste("Daily Usage Intensity", units), 
                              rangemode="tozero"))
  if(viewer){print(p_df)}
  if(save_img){
    wd <- getwd()
    setwd(fig_dir)
    orca(p_df, paste0(name,"-daily-intensity.pdf"))
    setwd(wd)
  }
  if(upload){
    api_create(p_df, filename = paste0(name,"-daily-intensity"))
  }
  #htmlwidgets::saveWidget(as_widget(p_df), paste(name, ".html", sep=""))
  
}

# ////////////////////////////
# Total usage plots 
# ////////////////////////////

# Add up the data for the entire time period 
ann_sum <- function(df) {
  newdf <- data.table(parameter=c('Cool (Ton-Hr)','Elec (kWh)','Steam (lb)','Water (Gal)'),
                      ann_tot=double(4))
  # Get the length of the total time series by subtracting end time - start time
  years <- lubridate::time_length(difftime(df$DateTime[nrow(df)], 
                                           df$DateTime[1]), "years")
  newdf$ann_tot <- c(sum(na.omit(df$Cool))/years, 
                     sum(na.omit(df$Elec))/years, 
                     sum(na.omit(df$Heat))/years, 
                     sum(na.omit(df$Water))/years)
  newdf
}

# loop over all the data in the list
ann_sum_all <- function(dflist){
  for (i in 1:length(dflist)){
    if(i==1){
      ann_tots <- ann_sum(dflist[[i]])
      names(ann_tots)[2] <- bldg_names[i]
    }else{
      ann_tot_temp <- ann_sum(dflist[[i]])
      ann_tots <- merge(x=ann_tots, y=ann_tot_temp, by="parameter", all=TRUE)
      names(ann_tots)[i+1] <- bldg_names[i]
    }
  }
  return(ann_tots)
}

plot_tot_usage <- function(df, sqft){
  for (i in 1:length(bldg_names)){
    if(i==1){
      # avoid warning about discrete and non-discrete data on the same axis
      p1 <- plot_ly(df, x=~parameter, y=df[[bldg_names[i]]], name=bldg_names[i], type = 'bar')
      p2 <- plot_ly(df, x=~parameter, y=df[[bldg_names[i]]]/sqft[i], name=bldg_names[i], type = 'bar')
    } else{
      p1 <- add_trace(p1, x = ann_tots$parameter, y=ann_tots[[bldg_names[i]]], name=bldg_names[i])
      p2 <- add_trace(p2, x = ann_tots$parameter, y=ann_tots[[bldg_names[i]]]/sqft[i], name=bldg_names[i])
    }
  }
  p1 <- layout(p1, yaxis = list(title = 'Total Usage'), barmode = 'group')
  p2 <- layout(p2, yaxis = list(title = 'Normalized Usage'), barmode = 'group')
  
  if(viewer){
    print(p1)
    print(p2)
  }
  if(save_img){
    wd <- getwd()
    setwd(fig_dir)
    orca(p1, "tot-usage.pdf")
    orca(p2, "tot-usage-normalized.pdf")
    setwd(wd)
  }
  if(upload){
    api_create(p1, filename = "tot-usage")
    api_create(p2, filename = "tot-usage-normalized")
  }
}

# ////////////////////////////////////////////////////////////
#  Plot combinations of data and prediction, thresholds and errors, 
#  event (0 or 1) and event prediction (0 or 1)
# ////////////////////////////////////////////////////////////

# Show multiple error types
# Set up to show one week of data
plot_events_rf <- function(df,bldg, wknum) {
  ### Cool ###
  # plot cooling data and predicted vals
  p1 <- plot_ly(df, x=~DateTime, y=~Cool, name="Cool", type='scatter', mode='lines') %>%
    add_trace(y= ~cool_rf, name="cool_rf", line=list(width=1)) %>%
    layout(title=paste(bldg,'Cooling - random forest'),
           yaxis=list(title='Ton-Hrs'))
  # Plot actual error and error threshold (squared, scaled, Eucl Dist)
  p11 <- plot_ly(df, x=~DateTime, y=~cool_rf_err_abs, name="Cool rf abs err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_c_abs, name="Abolute error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='|pred-obs|'))
  p2 <- plot_ly(df, x=~DateTime, y=~cool_rf_err_sq, name="Cool rf sqr err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_c_sq, name="Sqr error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title=("error<sup>2</sup>")))
  p3 <- plot_ly(df, x=~DateTime, y=~cool_rf_err_sca, name="Cool rf scaled err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_c_sca, name="Scaled error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='error/obs'))
  p4 <- plot_ly(df, x=~DateTime, y=~cool_rf_err_ed, name="Cool rf Eucl Dist", type='scatter', mode='lines') %>%
    add_trace(y= ~max_c_ed, name="Eucl dist thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='Eucl Dist'))
  
  # p5 <- plot_ly(df, x=~DateTime, y=~C_event, name="Marked Event", type='bar') %>%
  #   layout(yaxis=list(title='Real Event'), barmode='stack') 
  # p9 <- plot_ly(df, x=~DateTime, y=~rf_C_event_abs, name="abs_err Event", type='bar') %>%
  #   layout(yaxis=list(title='abs err'), barmode='stack') 
  # p6 <- plot_ly(df, x=~DateTime, y=~rf_C_event_sq, name="squared_err Event", type='bar') %>%
  #   layout(yaxis=list(title='sq err'), barmode='stack') 
  # p7 <- plot_ly(df, x=~DateTime, y=~rf_C_event_sca, name="scaled_err Event", type='bar') %>%
  #   layout(yaxis=list(title='sca err'), barmode='stack')
  # p8 <- plot_ly(df, x=~DateTime, y=~rf_C_event_ed, name="Eucl_dist Event", type='bar') %>%
  #   layout(yaxis=list(title='Eucl dist'), barmode='stack') 
  # # Plot the rest of the event detectors
  # p10 <- plot_ly(df, x=~DateTime, y=~rf_C_event_rule, name="rule_based Event", type='bar') %>%
  #   layout(yaxis=list(title='Rule based'), barmode='stack') 
  
  #p = list(p1,p11,p2,p3,p4,p5,p9,p6,p7,p8) #,p10) # Left off rule-based
  p = list(p1,p11,p2,p3,p4) # Model, prediction and error measures
  p_tot_c <- subplot(p,nrows=length(p),shareX=TRUE, titleY=TRUE)
  #saveWidget(p_tot, paste0(bldg,'_rf_cool.html'), selfcontained = F, libdir = "lib")
  
  ### Elec ###
  p1 <- plot_ly(df, x=~DateTime, y=~Elec, name="Elec", type='scatter', mode='lines') %>%
    add_trace(y= ~elec_rf, name="elec_rf", line=list(width=1)) %>%
    layout(title=paste(bldg,'Elec - random forest'),
           yaxis=list(title='kWh'))
  # Plot error and error threshold (squared, scaled, Eucl Dist)
  p11 <- plot_ly(df, x=~DateTime, y=~elec_rf_err_abs, name="Elec rf abs err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_e_abs, name="Abolute error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='|pred-obs|'))
  p2 <- plot_ly(df, x=~DateTime, y=~elec_rf_err_sq, name="Elec rf sqr err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_e_sq, name="Sqr error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title=("error<sup>2</sup>")))
  p3 <- plot_ly(df, x=~DateTime, y=~elec_rf_err_sca, name="Elec rf scaled err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_e_sca, name="Scaled error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='error/obs'))
  p4 <- plot_ly(df, x=~DateTime, y=~elec_rf_err_ed, name="Elec rf Eucl Dist", type='scatter', mode='lines') %>%
    add_trace(y= ~max_e_ed, name="Eucl dist thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='Eucl Dist'))
  
  # p5 <- plot_ly(df, x=~DateTime, y=~E_event, name="Marked Event", type='bar') %>%
  #   layout(yaxis=list(title='Real Event'), barmode='stack') 
  # p6 <- plot_ly(df, x=~DateTime, y=~rf_E_event_sq, name="squared_err Event", type='bar') %>%
  #   layout(yaxis=list(title='sq err'), barmode='stack') 
  # p7 <- plot_ly(df, x=~DateTime, y=~rf_E_event_sca, name="scaled_err Event", type='bar') %>%
  #   layout(yaxis=list(title='sca err'), barmode='stack')
  # p8 <- plot_ly(df, x=~DateTime, y=~rf_E_event_ed, name="Eucl_dist Event", type='bar') %>%
  #   layout(yaxis=list(title='Eucl dist'), barmode='stack') 
  # # Plot the rest of the event detectors
  # p9 <- plot_ly(df, x=~DateTime, y=~rf_E_event_abs, name="abs_err Event", type='bar') %>%
  #   layout(yaxis=list(title='abs err'), barmode='stack') 
  # p10 <- plot_ly(df, x=~DateTime, y=~rf_E_event_rule, name="rule_based Event", type='bar') %>%
  #   layout(yaxis=list(title='Rule based'), barmode='stack') 
  # 
  # p = list(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
  p = list(p1,p11,p2,p3,p4)
  p_tot_e <- subplot(p,nrows=length(p),shareX=TRUE, titleY=TRUE)
  # #saveWidget(p_tot, paste0(bldg,'_rf_elec.html'), selfcontained = F, libdir = "lib")
  
  ### Steam ###
  p1 <- plot_ly(df, x=~DateTime, y=~Heat, name="Steam", type='scatter', mode='lines') %>%
    add_trace(y= ~heat_rf, name="steam_rf", line=list(width=1)) %>%
    layout(title=paste(bldg,'Steam - random forest'),
           yaxis=list(title='lb'))
  # Plot error and error threshold (squared, scaled, Eucl Dist)
  p11 <- plot_ly(df, x=~DateTime, y=~heat_rf_err_abs, name="Steam rf abs err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_h_abs, name="Abolute error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='|pred-obs|'))
  p2 <- plot_ly(df, x=~DateTime, y=~heat_rf_err_sq, name="Steam rf sqr err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_h_sq, name="Sqr error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title=("error<sup>2</sup>")))
  p3 <- plot_ly(df, x=~DateTime, y=~heat_rf_err_sca, name="Steam rf scaled err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_h_sca, name="Scaled error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='error/obs'))
  p4 <- plot_ly(df, x=~DateTime, y=~heat_rf_err_ed, name="Steam rf Eucl Dist", type='scatter', mode='lines') %>%
    add_trace(y= ~max_h_ed, name="Eucl dist thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='Eucl Dist'))
  
  p = list(p1,p11,p2,p3,p4)
  p_tot_h <- subplot(p,nrows=length(p),shareX=TRUE, titleY=TRUE)
  
  ### Water ###
  p1 <- plot_ly(df, x=~DateTime, y=~Water, name="Water", type='scatter', mode='lines') %>%
    add_trace(y= ~water_rf, name="water_rf", line=list(width=1)) %>%
    layout(title=paste(bldg,'Water - random forest'),
           yaxis=list(title='Gal'))
  # Plot error and error threshold (squared, scaled, Eucl Dist)
  p11 <- plot_ly(df, x=~DateTime, y=~water_rf_err_abs, name="Water rf abs err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_w_abs, name="Abolute error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='|pred-obs|'))
  p2 <- plot_ly(df, x=~DateTime, y=~water_rf_err_sq, name="Water rf sqr err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_w_sq, name="Sqr error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title=("error<sup>2</sup>")))
  p3 <- plot_ly(df, x=~DateTime, y=~water_rf_err_sca, name="Water rf scaled err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_w_sca, name="Scaled error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='error/obs'))
  p4 <- plot_ly(df, x=~DateTime, y=~water_rf_err_ed, name="Water rf Eucl Dist", type='scatter', mode='lines') %>%
    add_trace(y= ~max_w_ed, name="Eucl dist thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='Eucl Dist'))
  # 
  # p5 <- plot_ly(df, x=~DateTime, y=~W_event, name="Marked Event", type='bar') %>%
  #   layout(yaxis=list(title='Real Event'), barmode='stack') 
  # p6 <- plot_ly(df, x=~DateTime, y=~rf_W_event_sq, name="squared_err Event", type='bar') %>%
  #   layout(yaxis=list(title='sq err'), barmode='stack') 
  # p7 <- plot_ly(df, x=~DateTime, y=~rf_W_event_sca, name="scaled_err Event", type='bar') %>%
  #   layout(yaxis=list(title='sca err'), barmode='stack')
  # p8 <- plot_ly(df, x=~DateTime, y=~rf_W_event_ed, name="Eucl_dist Event", type='bar') %>%
  #   layout(yaxis=list(title='Eucl dist'), barmode='stack') 
  # # Plot the rest of the event detectors
  # p9 <- plot_ly(df, x=~DateTime, y=~rf_W_event_abs, name="abs_err Event", type='bar') %>%
  #   layout(yaxis=list(title='abs err'), barmode='stack') 
  # p10 <- plot_ly(df, x=~DateTime, y=~rf_W_event_rule, name="rule_based Event", type='bar') %>%
  #   layout(yaxis=list(title='Rule based'), barmode='stack') 
  # 
  # p = list(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
  p = list(p1,p11,p2,p3,p4)
  p_tot_w <- subplot(p,nrows=length(p),shareX=TRUE, titleY=TRUE)
  
  if (viewer) {
    print(p_tot_c)
    print(p_tot_e)
    print(p_tot_h)
    print(p_tot_w)
  }
  # all_plots=list(p_tot_c,p_tot_e,p_tot_w)
  # 
  #plot_total <- combineWidgets(list=all_plots, nrow=NULL, ncol=1, height=1000*length(all_plots))
  #plot_total
  #saveWidget(plot_total, paste0(bldg,'_rf_events_2.html'))
  
  if (save_img){
    wd <- getwd()
    setwd(fig_dir) # Change directory for saving figures
    
    server <- orca_serve()
    Sys.sleep(8)
    server$export(p_tot_c,paste0(bldg,"-week-",wknum,"-cool-pred-errs.pdf"), scale=fig_scale)
    server$export(p_tot_e,paste0(bldg,"-week-",wknum,"-elec-pred-errs.pdf"), scale=fig_scale)
    server$export(p_tot_h,paste0(bldg,"-week-",wknum,"-heat-pred-errs.pdf"), scale=fig_scale)
    server$export(p_tot_w,paste0(bldg,"-week-",wknum,"-water-pred-errs.pdf"), scale=fig_scale)
    server$close()
    
    setwd(wd) # Change back to original working directory
  }
  
}

# Just data, prediction and injected event (1 or 0)
plot_events_rf_3 <- function(df,bldg) {
  ## Cool ##
  # plot cooling data and predicted vals
  p1 <- plot_ly(df, x=~DateTime, y=~Cool, name="Cool", type='scatter', mode='lines') %>%
    add_trace(y= ~cool_rf, name="cool rf", line=list(width=1)) %>%
    layout(title=paste(bldg,'Chilled Water Events'),
           yaxis=list(title='Ton-Hrs'))
  p8 <- plot_ly(df, x=~DateTime, y=~C_event, name="Marked Event", type='bar') %>%
    layout(yaxis=list(title='Event'), barmode='stack')
  p9 <- plot_ly(df, x=~DateTime, y=~C_action, name="action_var", type='bar') %>%
    layout(yaxis=list(title='5day'), barmode='stack')
  
  #p = list(p1,p2,p5,p6,p7)
  p=list(p1,p8)
  p_tot_c <- subplot(p,nrows=length(p) ,shareX=TRUE, titleY=TRUE, heights=c(0.7, 0.3))
                    
  #saveWidget(p_tot, paste0(bldg,'_rf_cool.html'), selfcontained = F, libdir = "lib")

  ## Elec ##
  p1 <- plot_ly(df, x=~DateTime, y=~Elec, name="Elec", type='scatter', mode='lines') %>%
    add_trace(y= ~elec_rf, name="elec_rf", line=list(width=1)) %>%
    layout(title=paste(bldg,'Electricity Events'),
           yaxis=list(title='kWh'))
  p8 <- plot_ly(df, x=~DateTime, y=~E_event, name="Marked Event", type='bar') %>%
    layout(yaxis=list(title='Event'), barmode='stack')
  
 #p = list(p1,p2,p5,p6,p7)
  p = list(p1,p8)
  p_tot_e <- subplot(p,nrows=length(p), shareX=TRUE, titleY=TRUE, heights=c(0.7, 0.3))

  # #saveWidget(p_tot, paste0(bldg,'_rf_elec.html'), selfcontained = F, libdir = "lib")
  
  ##Heat ##
  p1 <- plot_ly(df, x=~DateTime, y=~Heat, name="Heat", type='scatter', mode='lines') %>%
    add_trace(y= ~heat_rf, name="heat_rf", line=list(width=1)) %>%
    layout(title=paste(bldg,'Steam Events'),
           yaxis=list(title='lbs'))
  p8 <- plot_ly(df, x=~DateTime, y=~H_event, name="Marked Event", type='bar') %>%
    layout(yaxis=list(title='Event'), barmode='stack')
  
 
  #p = list(p1,p2,p5,p6,p7)
  p = list(p1,p8)
  p_tot_h <- subplot(p,nrows=length(p), shareX=TRUE, titleY=TRUE, heights=c(0.7, 0.3))

  ### Water ###
  p1 <- plot_ly(df, x=~DateTime, y=~Water, name="Water", type='scatter', mode='lines') %>%
    add_trace(y= ~water_rf, name="water_rf", line=list(width=1)) %>%
    layout(title=paste(bldg,'Water Events'),
           yaxis=list(title='kWh'))
  p8 <- plot_ly(df, x=~DateTime, y=~W_event, name="Marked Event", type='bar') %>%
    layout(yaxis=list(title='Event'), barmode='stack')
  
  
  #p = list(p1,p2,p5,p6,p7)
  p = list(p1,p8)
  p_tot_w <- subplot(p,nrows=length(p),shareX=TRUE, titleY=TRUE, heights=c(0.7, 0.3))
  
  if(viewer) {
    print(p_tot_c)
    print(p_tot_e)
    print(p_tot_h)
    print(p_tot_w)
  }
  
  if(save_img){
    wd <- getwd()
    setwd(fig_dir)
    orca(p_tot_c, paste0(bldg,"-cool-pred-err-thresh.pdf"))
    orca(p_tot_e, paste0(bldg,"-elec-pred-err-thresh.pdf"))
    orca(p_tot_h, paste0(bldg,"-heat-pred-err-thresh.pdf"))
    orca(p_tot_w, paste0(bldg,"-water-pred-err-thresh.pdf"))
    setwd(wd)
  }
   
  #all_plots=list(p_tot_c,p_tot_e,p_tot_h,p_tot_w)
  #plot_total <- combineWidgets(list=all_plots, nrow=NULL, ncol=1, height=200*length(all_plots))
  #plot_total
  #saveWidget(plot_total, paste0(bldg,'_rf_events_all.html'))
}

# data, preds, all err and thresh 
plot_events_rf_2 <- function(df,bldg) {
  ### Cool ###
  # plot cooling data and predicted vals
  p1 <- plot_ly(df, x=~DateTime, y=~Cool, name="Cool", type='scatter', mode='lines') %>%
    add_trace(y= ~cool_rf, name="cool rf", line=list(width=1)) %>%
    layout(title=paste(bldg,'Chilled Water Events'),
           yaxis=list(title='Ton-Hrs'))
  p8 <- plot_ly(df, x=~DateTime, y=~C_event, name="Marked Event", type='bar') %>%
    layout(yaxis=list(title='Real Event'), barmode='stack')
  # 
  # Plot actual error and error threshold (squared, scaled, Eucl Dist)
  p2 <- plot_ly(df, x=~DateTime, y=~cool_rf_err_abs, name="Cool rf abs err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_c_abs, name="Abolute error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='|pred-obs|'))
  p3 <- plot_ly(df, x=~DateTime, y=~cool_rf_err_sq, name="Cool rf sqr err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_c_sq, name="Sqr error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title=("error<sup>2</sup>")))
  p4 <- plot_ly(df, x=~DateTime, y=~cool_rf_err_sca, name="Cool rf scaled err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_c_sca, name="Scaled error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='error/obs'))
  p5 <- plot_ly(df, x=~DateTime, y=~cool_rf_err_ed, name="Cool rf Eucl Dist", type='scatter', mode='lines') %>%
    add_trace(y= ~max_c_ed, name="Eucl dist thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='Eucl Dist'))
  # p6 <- plot_ly(df, x=~DateTime, y=~cool_matches, name="Cool mp matches", type='scatter', mode='lines') %>%
  #   add_trace(y= ~max_c_MASS, name="MASS thresh.", line=list(width=1)) %>%
  #   layout(yaxis=list(title='Match count'))
  # p7 <- plot_ly(df, x=~DateTime, y=~rf_C_event_MASS, name="MASS Event", type='bar') %>%
  #   layout(yaxis=list(title='MASS event'), barmode='stack')
  
  p = list(p1,p2,p3,p4,p5)
  #p=list(p1,p8)
  p_tot_c <- subplot(p,nrows=length(p),shareX=TRUE, titleY=TRUE)
  #saveWidget(p_tot, paste0(bldg,'_rf_cool.html'), selfcontained = F, libdir = "lib")
  
  
  ### Elec ###
  p1 <- plot_ly(df, x=~DateTime, y=~Elec, name="Elec", type='scatter', mode='lines') %>%
    add_trace(y= ~elec_rf, name="elec_rf", line=list(width=1)) %>%
    layout(title=paste(bldg,'Electricity Events'),
           yaxis=list(title='kWh'))
  p8 <- plot_ly(df, x=~DateTime, y=~E_event, name="Marked Event", type='bar') %>%
    layout(yaxis=list(title='Real Event'), barmode='stack')
  # 
  # Plot error and error threshold (squared, scaled, Eucl Dist)
  p2 <- plot_ly(df, x=~DateTime, y=~elec_rf_err_abs, name="Elec rf abs err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_e_abs, name="Abolute error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='|pred-obs|'))
  p3 <- plot_ly(df, x=~DateTime, y=~elec_rf_err_sq, name="Elec rf sqr err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_e_sq, name="Sqr error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title=("error<sup>2</sup>")))
  p4 <- plot_ly(df, x=~DateTime, y=~elec_rf_err_sca, name="Elec rf scaled err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_e_sca, name="Scaled error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='error/obs'))
  p5 <- plot_ly(df, x=~DateTime, y=~elec_rf_err_ed, name="Elec rf Eucl Dist", type='scatter', mode='lines') %>%
    add_trace(y= ~max_e_ed, name="Eucl dist thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='Eucl Dist'))
  # p6 <- plot_ly(df, x=~DateTime, y=~elec_matches, name="Elec mp matches", type='scatter', mode='lines') %>%
  #   add_trace(y= ~max_e_MASS, name="MASS thresh.", line=list(width=1)) %>%
  #   layout(yaxis=list(title='Match count'))
  # p7 <- plot_ly(df, x=~DateTime, y=~rf_E_event_MASS, name="MASS Event", type='bar') %>%
  #   layout(yaxis=list(title='MASS event'), barmode='stack')
  
  p = list(p1,p2,p3,p4,p5)
  #p = list(p1,p8)
  p_tot_e <- subplot(p,nrows=length(p), shareX=TRUE, titleY=TRUE)
  
  # #saveWidget(p_tot, paste0(bldg,'_rf_elec.html'), selfcontained = F, libdir = "lib")
  
  ### Heat ###
  p1 <- plot_ly(df, x=~DateTime, y=~Heat, name="Heat", type='scatter', mode='lines') %>%
    add_trace(y= ~heat_rf, name="heat_rf", line=list(width=1)) %>%
    layout(title=paste(bldg,'Steam Events'),
           yaxis=list(title='lbs'))
  p8 <- plot_ly(df, x=~DateTime, y=~H_event, name="Marked Event", type='bar') %>%
    layout(yaxis=list(title='Real Event'), barmode='stack')
  # 
  # Plot error and error threshold (squared, scaled, Eucl Dist)
  p2 <- plot_ly(df, x=~DateTime, y=~heat_rf_err_abs, name="Heat rf abs err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_h_abs, name="Abolute error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='|pred-obs|'))
  p3 <- plot_ly(df, x=~DateTime, y=~heat_rf_err_sq, name="Heat rf sqr err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_h_sq, name="Sqr error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title=("error<sup>2</sup>")))
  p4 <- plot_ly(df, x=~DateTime, y=~heat_rf_err_sca, name="Heat rf scaled err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_h_sca, name="Scaled error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='error/obs'))
  p5 <- plot_ly(df, x=~DateTime, y=~heat_rf_err_ed, name="Heat rf Eucl Dist", type='scatter', mode='lines') %>%
    add_trace(y= ~max_h_ed, name="Eucl dist thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='Eucl Dist'))
  # p6 <- plot_ly(df, x=~DateTime, y=~heat_matches, name="Heat mp matches", type='scatter', mode='lines') %>%
  #   add_trace(y= ~max_h_MASS, name="MASS thresh.", line=list(width=1)) %>%
  #   layout(yaxis=list(title='Match count'))
  # p7 <- plot_ly(df, x=~DateTime, y=~rf_H_event_MASS, name="MASS Event", type='bar') %>%
  #   layout(yaxis=list(title='MASS event'), barmode='stack')
  
  p = list(p1,p2,p3,p4,p5)
  #p = list(p1,p8)
  p_tot_h <- subplot(p,nrows=length(p), shareX=TRUE, titleY=TRUE)
  
  
  ### Water ###
  p1 <- plot_ly(df, x=~DateTime, y=~Water, name="Water", type='scatter', mode='lines') %>%
    add_trace(y= ~water_rf, name="water_rf", line=list(width=1)) %>%
    layout(title=paste(bldg,'Water Events'),
           yaxis=list(title='kWh'))
  p8 <- plot_ly(df, x=~DateTime, y=~W_event, name="Marked Event", type='bar') %>%
    layout(yaxis=list(title='Real Event'), barmode='stack')
  # 
  # Plot error and error threshold (squared, scaled, Eucl Dist)
  p2 <- plot_ly(df, x=~DateTime, y=~water_rf_err_abs, name="Water rf abs err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_w_abs, name="Abolute error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='|pred-obs|'))
  p3 <- plot_ly(df, x=~DateTime, y=~water_rf_err_sq, name="Water rf sqr err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_w_sq, name="Sqr error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title=("error<sup>2</sup>")))
  p4 <- plot_ly(df, x=~DateTime, y=~water_rf_err_sca, name="Water rf scaled err", type='scatter', mode='lines') %>%
    add_trace(y= ~max_w_sca, name="Scaled error thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='error/obs'))
  p5 <- plot_ly(df, x=~DateTime, y=~water_rf_err_ed, name="Water rf Eucl Dist", type='scatter', mode='lines') %>%
    add_trace(y= ~max_w_ed, name="Eucl dist thresh.", line=list(width=1)) %>%
    layout(yaxis=list(title='Eucl Dist'))
  # p6 <- plot_ly(df, x=~DateTime, y=~water_matches, name="Water mp matches", type='scatter', mode='lines') %>%
  #   add_trace(y= ~max_w_MASS, name="MASS thresh.", line=list(width=1)) %>%
  #   layout(yaxis=list(title='Match count'))
  # p7 <- plot_ly(df, x=~DateTime, y=~rf_W_event_MASS, name="MASS Event", type='bar') %>%
  #   layout(yaxis=list(title='MASS event'), barmode='stack')
  
  p = list(p1,p2,p3,p4,p5)
  #p = list(p1,p8)
  p_tot_w <- subplot(p,nrows=length(p),shareX=TRUE, titleY=TRUE)
  
  if(viewer) {
    print(p_tot_c)
    print(p_tot_e)
    print(p_tot_h)
    print(p_tot_w)
  }
  
  if(save_img) {
    wd <- getwd()
    setwd(fig_dir)
    orca(p_tot_c, paste0(bldg,"-cool-pred-err-thresh.pdf"))
    orca(p_tot_e, paste0(bldg,"-elec-pred-err-thresh.pdf"))
    orca(p_tot_h, paste0(bldg,"-heat-pred-err-thresh.pdf"))
    orca(p_tot_w, paste0(bldg,"-water-pred-err-thresh.pdf"))
    setwd(wd)
  }
  
  if (upload){
    api_create(p_tot_c, filename = paste0(bldg,"-pred-err-thresh-cool"))
    api_create(p_tot_e, filename = paste0(bldg,"-pred-err-thresh-elec"))
    api_create(p_tot_h, filename = paste0(bldg,"-pred-err-thresh-heat"))
    api_create(p_tot_w, filename = paste0(bldg,"-pred-err-thresh-water"))
  }
  
  #all_plots=list(p_tot_c,p_tot_e,p_tot_h,p_tot_w)
  #plot_total <- combineWidgets(list=all_plots, nrow=NULL, ncol=1, height=600*length(all_plots))
  #plot_total
  #saveWidget(plot_total, paste0(bldg,'_rf_events2.html'))
}


# ////////////////////////////////////////////////////////////
#  plot heatmaps for events
# ////////////////////////////////////////////////////////////

heatplot_old <- function(dates, mat1, y_var, nametext, color2){
  p1 <- plot_ly(x = dates, y = y_var,
                z = mat1[1,], colors= c("gray95", color2), type = "heatmap", showscale=FALSE)# %>%
  layout(title=nametext, yaxis=list(autorange="reversed"))
  p1
}

heatplot <- function(dates, mat1, y_var, nametext, color2, color1="gray85"){
  p1 <- plot_ly(x = dates, y = y_var[1],
                z = t(mat1[1,]), colors= c(color1, color2), type = "heatmap", showscale=FALSE)# %>%
  #layout(title=nametext, yaxis=list(autorange="reversed"))
  p2 <- plot_ly(x = dates, y = y_var[2:(length(y_var)-1)],
                z = mat1[2:(nrow(mat1)-1),], colors= c("gray95", color2), type="heatmap", showscale=FALSE) #%>%
    #layout(title=nametext, yaxis=list(autorange="reversed"))
  p3 <- plot_ly(x = dates, y = y_var[(length(y_var))],
                z = t(mat1[(nrow(mat1)),]), colors= c("gray95", "chartreuse"), type="heatmap", showscale=FALSE) %>%
    layout(title=nametext, yaxis=list(autorange="reversed"))
  
  #p1
  #p2
  plots <- list(p1,p2,p3)
  frac <- 1/(length(y_var))
  pf <- subplot(plots, nrows=3, shareX=TRUE, heights=c(frac,(1-(2*frac)), frac))
  pf
}

# Does not include post-processing
event_heatmap <- function(df,bldg){
  dates <- df$DateTime
  # Elec
  mat_re <- matrix(df$E_event, nrow = 1, byrow=TRUE)
  mat_ab <- matrix(df$rf_E_event_abs, nrow = 1, byrow=TRUE)
  mat_sq <- matrix(df$rf_E_event_sq, nrow = 1, byrow=TRUE)
  mat_sc <- matrix(df$rf_E_event_sca, nrow = 1, byrow=TRUE)
  mat_ed <- matrix(df$rf_E_event_ed, nrow = 1, byrow=TRUE)
  mat_ru <- matrix(df$rf_E_event_rule, nrow = 1, byrow=TRUE)
  type <- c("Synthetic event","Absolute","Squared","Scaled","Eucl dist", "Rule based")
  mat <- rbind(mat_re,mat_ab,mat_sq,mat_sc,mat_ed, mat_ru)
  pe <- heatplot(dates, mat, type, "Electricity", "darkgreen")
  
  # Chilled water
  mat_re <- matrix(df$C_event, nrow = 1, byrow=TRUE)
  mat_ab <- matrix(df$rf_C_event_abs, nrow = 1, byrow=TRUE)
  mat_sq <- matrix(df$rf_C_event_sq, nrow = 1, byrow=TRUE)
  mat_sc <- matrix(df$rf_C_event_sca, nrow = 1, byrow=TRUE)
  mat_ed <- matrix(df$rf_C_event_ed, nrow = 1, byrow=TRUE)
  mat_ru <- matrix(df$rf_C_event_rule, nrow = 1, byrow=TRUE)
  type <- c("Synthetic event","Absolute","Squared","Scaled","Eucl dist", "Rule based")
  mat <- rbind(mat_re,mat_ab,mat_sq,mat_sc,mat_ed, mat_ru)
  pc <- heatplot(dates, mat, type, "Chilled Water", "blue")
  
  # Heat
  mat_re <- matrix(df$H_event, nrow = 1, byrow=TRUE)
  mat_ab <- matrix(df$rf_H_event_abs, nrow = 1, byrow=TRUE)
  mat_sq <- matrix(df$rf_H_event_sq, nrow = 1, byrow=TRUE)
  mat_sc <- matrix(df$rf_H_event_sca, nrow = 1, byrow=TRUE)
  mat_ed <- matrix(df$rf_H_event_ed, nrow = 1, byrow=TRUE)
  mat_ru <- matrix(df$rf_H_event_rule, nrow = 1, byrow=TRUE)
  type <- c("Synthetic event","Absolute","Squared","Scaled","Eucl dist", "Rule based") 
  mat <- rbind(mat_re,mat_ab,mat_sq,mat_sc,mat_ed, mat_ru)
  ph <- heatplot(dates, mat, type, "Steam", "red")
  
  # Water
  # Chilled water
  mat_re <- matrix(df$W_event, nrow = 1, byrow=TRUE)
  mat_ab <- matrix(df$rf_W_event_abs, nrow = 1, byrow=TRUE)
  mat_sq <- matrix(df$rf_W_event_sq, nrow = 1, byrow=TRUE)
  mat_sc <- matrix(df$rf_W_event_sca, nrow = 1, byrow=TRUE)
  mat_ed <- matrix(df$rf_W_event_ed, nrow = 1, byrow=TRUE)
  mat_ru <- matrix(df$rf_W_event_rule, nrow = 1, byrow=TRUE)
  type <- c("Synthetic event","Absolute","Squared","Scaled","Eucl dist", "Rule based")
  mat <- rbind(mat_re,mat_ab,mat_sq,mat_sc,mat_ed, mat_ru)
  pw <- heatplot(dates, mat, type, "Water", "darkslategrey")
  
  
  if (viewer) {
    print(pc)
    print(pe)
    print(ph)
    print(pw)
  }
  
  if (save_img){
    wd <- getwd()
    setwd(fig_dir) # Change directory for saving figures
    if (!exists("adjust")) { adjust = 1 }
    orca(p_df, paste0(name,"-ann-daily-norm.svg"), width=fig_width*adjust, height=fig_height*adjust)
    
    server <- orca_serve()
    Sys.sleep(8)
    server$export(pc,paste0(bldg,"-cool-event-heatmap.svg"), width=fig_width*adjust, height=fig_height*adjust)
    server$export(pe,paste0(bldg,"-elec-event-heatmap.svg"), width=fig_width*adjust, height=fig_height*adjust)
    server$export(ph,paste0(bldg,"-heat-event-heatmap.svg"), width=fig_width*adjust, height=fig_height*adjust)
    server$export(pw,paste0(bldg,"-water-event-heatmap.svg"), width=fig_width*adjust, height=fig_height*adjust)
    server$close()
    
    setwd(wd) # Change back to original working directory
  }
  
  if (upload){
    api_create(pc, filename = paste0(bldg,"-event-heatmap-cool"))
    api_create(pe, filename = paste0(bldg,"-event-heatmap-elec"))
    api_create(ph, filename = paste0(bldg,"-event-heatmap-heat"))
    api_create(pw, filename = paste0(bldg,"-event-heatmap-water"))
  }
  
  #plots = list(pc,pe,ph,pw)
  #p_tot <- combineWidgets(list=plots, nrow=2, width=800, height=550)
  #p_tot <- subplot(plots, nrows=2, shareX=TRUE, titleY=TRUE, shareY=FALSE)
  #p_tot

}

action_heatmap <- function(df,bldg){
  dates <- df$DateTime
  
  # Elec
  mat_re <- matrix(df$E_event, nrow = 1, byrow=TRUE)
  #mat_action <- matrix(df$E_action, nrow = 1, byrow=TRUE)
  mat_ab <- matrix(df$rf_E_event_abs, nrow = 1, byrow=TRUE)
  mat_sq <- matrix(df$rf_E_event_sq, nrow = 1, byrow=TRUE)
  mat_sc <- matrix(df$rf_E_event_sca, nrow = 1, byrow=TRUE)
  mat_ed <- matrix(df$rf_E_event_ed, nrow = 1, byrow=TRUE)
  mat_actionpred <- matrix(df$E_action_pred, nrow = 1, byrow=TRUE)
  
  type <- c("True State","Absolute","Squared","Scaled","Eucl dist","Alarm")
  mat <- rbind(mat_re,mat_ab,mat_sq,mat_sc,mat_ed,mat_actionpred)
  pe <- heatplot(dates, mat, type, paste(bldg, "Electricity"), "darkgreen")
  
  
  # Chilled water
  mat_re <- matrix(df$C_event, nrow = 1, byrow=TRUE)
  #mat_action <- matrix(df$C_action, nrow = 1, byrow=TRUE)
  mat_ab <- matrix(df$rf_C_event_abs, nrow = 1, byrow=TRUE)
  mat_sq <- matrix(df$rf_C_event_sq, nrow = 1, byrow=TRUE)
  mat_sc <- matrix(df$rf_C_event_sca, nrow = 1, byrow=TRUE)
  mat_ed <- matrix(df$rf_C_event_ed, nrow = 1, byrow=TRUE)
  mat_actionpred <- matrix(df$C_action_pred, nrow = 1, byrow=TRUE)
  
  type <- c("True State","Absolute","Squared","Scaled","Eucl dist","Alarm")
  mat <- rbind(mat_re,mat_ab,mat_sq,mat_sc,mat_ed,mat_actionpred)
  pc <- heatplot(dates, mat, type, paste(bldg, "Chilled Water"), "blue")
  
  
  # Heat
  mat_re <- matrix(df$H_event, nrow = 1, byrow=TRUE)
  #mat_action <- matrix(df$H_action, nrow = 1, byrow=TRUE)
  mat_ab <- matrix(df$rf_H_event_abs, nrow = 1, byrow=TRUE)
  mat_sq <- matrix(df$rf_H_event_sq, nrow = 1, byrow=TRUE)
  mat_sc <- matrix(df$rf_H_event_sca, nrow = 1, byrow=TRUE)
  mat_ed <- matrix(df$rf_H_event_ed, nrow = 1, byrow=TRUE)
  mat_actionpred <- matrix(df$H_action_pred, nrow = 1, byrow=TRUE)
  
  type <- c("True State","Absolute","Squared","Scaled","Eucl dist","Alarm")
  mat <- rbind(mat_re,mat_ab,mat_sq,mat_sc,mat_ed,mat_actionpred)
  ph <- heatplot(dates, mat, type, paste(bldg, "Steam"), "red")
  
  
  # Water
  mat_re <- matrix(df$W_event, nrow = 1, byrow=TRUE)
  #mat_action <- matrix(df$W_action, nrow = 1, byrow=TRUE)
  mat_ab <- matrix(df$rf_W_event_abs, nrow = 1, byrow=TRUE)
  mat_sq <- matrix(df$rf_W_event_sq, nrow = 1, byrow=TRUE)
  mat_sc <- matrix(df$rf_W_event_sca, nrow = 1, byrow=TRUE)
  mat_ed <- matrix(df$rf_W_event_ed, nrow = 1, byrow=TRUE)
  mat_actionpred <- matrix(df$W_action_pred, nrow = 1, byrow=TRUE)
  
  type <- c("True State","Absolute","Squared","Scaled","Eucl dist","Alarm")
  mat <- rbind(mat_re,mat_ab,mat_sq,mat_sc,mat_ed,mat_actionpred)
  pw <- heatplot(dates, mat, type, paste(bldg, "Water"), "darkslategrey")
  
  
  if (viewer) {
    print(pc)
    print(pe)
    print(ph)
    print(pw)
  }
  
  if (save_img){
    wd <- getwd()
    setwd(fig_dir) # Change directory for saving figures
    if (!exists("adjust")) { adjust = 1 }
    
    server <- orca_serve()
    #orca can take some time to connect so this forces it to pause. Increase the
    #time (in seconds) if you get Error in curl::curl_fetch_memory(url, handle =
    #handle) : Failed to connect to 127.0.0.1 port 5151: Connection refused
    Sys.sleep(8) 
    server$export(pc,paste0(bldg,"-cool-action-heatmap.svg"), width=fig_width*adjust, height=fig_height*adjust)
    server$export(pe,paste0(bldg,"-elec-action-heatmap.svg"), width=fig_width*adjust, height=fig_height*adjust)
    server$export(ph,paste0(bldg,"-heat-action-heatmap.svg"), width=fig_width*adjust, height=fig_height*adjust)
    server$export(pw,paste0(bldg,"-water-action-heatmap.svg"), width=fig_width*adjust, height=fig_height*adjust)
    server$close()
    
    setwd(wd) # Change back to original working directory
  }
  
  if (upload){
    api_create(pc, filename = paste0(bldg,"-action-heatmap-cool"))
    api_create(pe, filename = paste0(bldg,"-action-heatmap-elec"))
    api_create(ph, filename = paste0(bldg,"-action-heatmap-heat"))
    api_create(pw, filename = paste0(bldg,"-action-heatmap-water"))
  }
  
  #plots = list(pc,pe,ph,pw)
  #p_tot <- combineWidgets(list=plots, nrow=2, width=800, height=550)
  #p_tot <- subplot(plots, nrows=2, shareX=TRUE, titleY=TRUE, shareY=FALSE)
  #p_tot
  #return(p_tot)
  
}

# ////////////////////////////////////////////////////
#  Bar plot of integrated detection (sum of models)
# ////////////////////////////////////////////////////

combine_events <- function(df){
  df2 <- df[,c("DateTime", "C_event", "E_event", "H_event", "W_event")]
  df2$C_event_all <- unname(apply(df[,grep("C_event_", names(df))], 1, sum)) # sum the rows. apply(df,2,sum) would be cols
  df2$E_event_all <- unname(apply(df[,grep("E_event_", names(df))], 1, sum)) 
  df2$H_event_all <- unname(apply(df[,grep("H_event_", names(df))], 1, sum)) 
  df2$W_event_all <- unname(apply(df[,grep("W_event_", names(df))], 1, sum)) 
  df2
}

# generate "shapes" for shading on the plot to show events
make_shapes <- function(df, y_var, event){
  j=1
  k=1
  x_start <- df$DateTime[1]
  x_stops <- df$DateTime[1]
  for (i in 2:nrow(df)){
    if (event[i] == 1 & event[i-1]==0){
      x_start[j] <- df$DateTime[i-1]
      j=j+1
    }
    if (event[i] == 0 & event[i-1]==1){
      x_stops[k] <- df$DateTime[i+1]
      k=k+1
    }
  }
  shape=list(type='rect', line = list(color = 'rgba(0,0,0,0)'), 
             fillcolor="rgba(190,10,0,0.4)", xref='x', yref='y')
  #shape_offset = 0.5
  shapes <- list()
  for (i in seq_along(x_start)){
    shape[["x0"]] <- x_start[i]
    shape[["x1"]] <- x_stops[i]
    shape[["y0"]] <- min(c(0,(1.05*min(y_var))))
    shape[["y1"]] <- 1.05*max(y_var)
    shapes <- c(shapes, list(shape))
  }
  return(shapes)
}

plot_combined <- function(df, bldg){
  t <- combine_events(df)
  # plot number of alarms raised vs time with shading for real (injected) events
  
  ### Cool ###
  p1 <- plot_ly(t, x=~DateTime, y=~C_event_all, type='bar', name="Cool") %>% 
    layout(title=paste(bldg,"Chilled Water Integrated Detection"),yaxis=list(title='Number of Outlier Indicators'))
  p1 <- layout(p1, shapes=make_shapes(t,t$C_event_all, t$C_event), xaxis = list(showgrid=FALSE)) 
  #p1
  ### Elec ###
  p2 <- plot_ly(t, x=~DateTime, y=~E_event_all, type='bar', name="Elec") %>% 
    layout(title=paste(bldg,"Electricity Integrated Detection"),yaxis=list(title='Number of Outlier Indicators'))
  p2 <- layout(p2, shapes=make_shapes(t,t$E_event_all, t$E_event), xaxis = list(showgrid=FALSE)) 
  ### Steam ###
  p3 <- plot_ly(t, x=~DateTime, y=~H_event_all, type='bar', name="Steam") %>% 
    layout(title=paste(bldg,"Steam Integrated Detection"),yaxis=list(title='Number of Outlier Indicators'))
  p3 <- layout(p3, shapes=make_shapes(t,t$H_event_all, t$H_event), xaxis = list(showgrid=FALSE)) 
  ### Water ###
  p4 <- plot_ly(t, x=~DateTime, y=~W_event_all, type='bar', name="Water") %>% 
    layout(title=paste(bldg,"Water Integrated Detection"),yaxis=list(title='Number of Outlier Indicators'))
  p4 <- layout(p4, shapes=make_shapes(t,t$W_event_all, t$W_event), xaxis = list(showgrid=FALSE)) 
  
  #plots = list(p1,p2,p3,p4)
  #p_tot <- subplot(plots,nrows=2,shareX=TRUE, titleY=TRUE, shareY=TRUE)
  #p_tot
  if (save_img){
    wd <- getwd()
    setwd(fig_dir) # Change directory for saving figures
    if (!exists("adjust")) { adjust = 1 }
    
    orca(p1, paste0(bldg,"-combine-detect-cool.pdf"))
    orca(p2, paste0(bldg,"-combine-detect-elec.pdf"))
    orca(p3, paste0(bldg,"-combine-detect-heat.pdf"))
    orca(p4, paste0(bldg,"-combine-detect-water.pdf"))
    
    setwd(wd)
  }
  if(viewer){
    print(p1)
    print(p2)
    print(p3)
    print(p4)
  }
  
  if (upload){
    api_create(p1, filename = paste0(bldg,"-combine-detect-cool"))
    api_create(p2, filename = paste0(bldg,"-combine-detect-elec"))
    api_create(p3, filename = paste0(bldg,"-combine-detect-heat"))
    api_create(p4, filename = paste0(bldg,"-combine-detect-water"))
  }
  
}
