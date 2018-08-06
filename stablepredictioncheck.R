###########to find the best resolution till which the 5-min relationship holds. 
#######In other words, what is the lowest I can go and still achieve the same results as 5 minutes? 

library(data.table)
library(zoo)
library(plotly)
library(lfstat)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(chron)
library(scales)
library(gridExtra)
library(grid)
library(openair)
library(dplyr)


ggplotRegression <- function (fit) {
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red", fullrange = F) + theme_bw()+
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 4),
                       "Intercept =",signif(fit$coef[[1]],4),
                       " Slope =",signif(fit$coef[[2]], 4),
                       " P =",signif(summary(fit)$coef[2,4], 4)))
}



############## FUNCTION DEFINTION
analyze.data <- function(x) {
  houseid <- unique(x$houseID)
  x$DateTime <- as.POSIXct(as.character(x$DateTime), 
                           format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  x$numDatetime <- as.numeric(x$DateTime)
  x$Date <- as.POSIXct(format(x$DateTime, format = "%Y-%m-%d"),
                       format = "%Y-%m-%d", tz = "GMT")
  
  x$time <- format(x$DateTime, format = "%H:%M:%S",  tz = "GMT")
  x$time1 <- as.POSIXct(x$time, format = "%H:%M:%S",  tz = "GMT")
  x$month <- month(x$DateTime)
  x$year <-year(x$DateTime)
  x$hour <- as.numeric(format(x$DateTime, format = "%H", tz = "GMT"))
  
  x <- x[order(x$numDatetime),]
  

  x$deltaT <- x$Ibutton_LR - x$ews_T
  x$rollmean.delT <- x$deltaT
  x$rollmean.LR <- x$Ibutton_LR
  x$rollmean.ews_T <- x$ews_T
  x$fluestep <- c(NA,diff(x$Ibutton_W))
  
  ## 6 am to 6 am as one day definition. (this is not a very good one)
  indexes <- which(x$hour == 6)
  x <- x[min(indexes):max(indexes),]
  no.days <- round(nrow(x)/48)
  lastdays <- nrow(x) - no.days*48
  
  x$no.days <- c(rep(1:no.days, each = 48),rep(no.days,lastdays))
  x$weekoftheyear <- week(as.Date(x$DateTime))
  weekday.no <-as.numeric(strftime(x$DateTime, format = "%u"))
  x$weekday.no<- ifelse(x$hour<6,weekday.no - 1,weekday.no)
  
  x$indexcol <- seq.int(nrow(x))
  
  ## cooling period calculations:
  
  ### column required for later purposes (linear model calculation)
  
  offset <- as.numeric(with(x[which(x$Ibutton_W <=15),],
                            quantile((Ibutton_LR - Ibutton_W), 0.75)))
  
  ## subtract the offset to get adjusted LR temp.
  x$LR_adj <- x$Ibutton_LR - offset
  
  ## calculate indoor delta T with the adjusted living room temperature value
  x$DTi <- ifelse(x$Ibutton_W != 14,x$Ibutton_W - x$LR_adj, NA)
  summary(x$DTi)
  
  ## change in the (flue - LR_adj) at every step
  x$step_DTi <- c(NA, diff(x$DTi))
  x$logDTi <- log(x$DTi,10)
  
  ## replace all meaningless values:
  x$logDTi <- ifelse(x$logDTi == Inf |x$logDTi == -Inf |is.na(x$logDTi), NA,x$logDTi)
  
  
  ######calculate the slope, intercept and correlation value######
  ## this is to avoid mixing the 'merge' and losing the data beforehand.
  dummydata <- x
  
  results.z <- data.frame(indexcol = NA, AdjR =NA,
                          Intercept = NA,Slope = NA)
  
  ## calculate the slope, intercept and correlation value
  for(i in 1:(length(dummydata$indexcol) - 13)){
    sub.dummy <- dummydata[i:(i+11),]
    sub.dummy$x <- 1:length(sub.dummy$indexcol)
    sub.dummy$x <- (sub.dummy$x*5)/60   ## converting to an hourly variable
    print(i)
    
    if(!(is.na(sub.dummy$logDTi[1]))){
      mod <- lm(formula = logDTi ~ x, data = sub.dummy) # corelation equation (log T response to time)
      #variable names
      var.names <- c("Intercept","Slope")
      vec <- cbind.data.frame(sub.dummy$indexcol[1], 
                              summary(mod)$adj.r.squared, 
                              as.numeric(summary(mod)$coef[, 1][1]),
                              as.numeric(summary(mod)$coef[, 1][2]))
      
      names(vec) <- names(results.z)
      print(vec)
      results.z <- rbind.data.frame(results.z, vec)
    } else {
      print("first row - NA")
    }
  } 
  results.z <- results.z[-1,]
  
  ## something wrong happens when you perform the below step: more rows than in the actual dataset??
  results.table <- merge(dummydata, results.z, by = "indexcol", all = T)
  
  ########fit definition ##########
  ## look at the good fit for decaying curves.
  results.table$goodfit <- ifelse((results.table$step_DTi<0.1 & 
                                     results.table$AdjR>0.95 & results.table$DTi > 3),1,0)
  
  ## reclassify the count so it shows a count only when it is cooling.
  r <- rle(results.table$goodfit)
  sequence <- unlist(sapply(r$lengths, seq))
  results.table$tdecay <- sequence
  results.table$tdecay[results.table$goodfit <= 0] <- 0
  results.table$tdecay[is.na(results.table$goodfit)] <- NA
  
  r$values[r$lengths <= 12] <- 0
  results.table$invrle <- inverse.rle(r)
  r.new <- rle(results.table$invrle)
  seq2 <- unlist(sapply(r.new$lengths, seq))
  results.table$tdecay.corr <- seq2
  results.table$tdecay.corr[results.table$invrle <= 0] <- 0
  results.table$tdecay.corr[is.na(results.table$invrle)] <- NA
  
  
  
  ## offsetting by 3. 
  results.table$tdecay_lagged <- results.table$tdecay.corr - 3
  
  ### temperature before the start of the decay.
  results.table$T0 <- NA
  
  for( i in 1:length(results.table$indexcol)){
    
    if (is.na(results.table$tdecay_lagged[i])) {
      results.table$T0[i] <- NA
    } else if(results.table$tdecay_lagged[i] > 0) {
      results.table$T0[i] <- results.table$DTi[1+i-results.table$tdecay_lagged[i]]
    } else {
      results.table$T0[i] <- 0
      
    }
  }
  
  ## normalised temperature:
  results.table$norm_DTi <- results.table$DTi/results.table$T0
  results.table$log_normDTi <- log(results.table$norm_DTi, 10)
  results.table$tdecayperhour <- results.table$tdecay_lagged*5/60
  
  results.table <-  do.call(data.frame,lapply(results.table, 
                                              function(x) replace(x, is.infinite(x),NA)))
  
  ######define activezone######
  
  ## define activezone as starting at 30 degree cross over and ending at the start of cooling zone.
  
  results.table$activezone <- ifelse(results.table$Ibutton_W>30 & 
                                       results.table$tdecay.corr == 0,30,0)
  
  ## picking out start times
  results.table$activezonediff <-c(NA,diff(results.table$activezone))
  
  ## get rid of negative values for start point 'easy' definition:
  results.table$activezonediff <-ifelse(results.table$activezonediff==30,1,0)
  
  # #count number of heating records per day
  
  events.flue <-rle(results.table$activezone)
  sequence <- unlist(sapply(events.flue$lengths, seq))
  results.table$count.event <- sequence
  results.table$count.event[results.table$activezone <= 0] <- 0
  
  results.table$total.activemin <- results.table$count.event*5
  
  results.table$countonplot <- ifelse(results.table$activezone == 0,0,
                                      results.table$Ibutton_W)
  results.table$countonplot.cool <- ifelse(results.table$tdecay_lagged>= 0,
                                           results.table$Ibutton_W,0)
  
  ## order the timeseries
  results.table <- results.table[order(results.table$numDatetime),]
  
  ## minor NA adjustments:
  
  results.table$goodfit <- ifelse(is.na(results.table$goodfit) &
                                    results.table$activezone == 0, 0, results.table$goodfit)
  dailyheatrecord <- data.frame()
  dailyheatrecord <-aggregate(results.table$activezone, by=list(results.table$no.days), sum)
  dailyheatrecord$x <- (dailyheatrecord$x/30)*5
  dailyheatrecord$xhour <- dailyheatrecord$x/60
  colnames(dailyheatrecord) <- c("no.days", "heatingminutesperday","HourperDay.Use")
  results.table <- merge(results.table, dailyheatrecord, by = "no.days", all = T)
  
  results.table$HeatingDay <- ifelse(results.table$HourperDay.Use>0,"Heating Day", "Non-Heating Day")
  results.table$HeatingDay <- ifelse(is.na(results.table$HeatingDay), "No Data Record",results.table$HeatingDay)
  
  ## on a day, classify active heating, cooling or no activity zone
  results.table$HeatingPhase <- ifelse(results.table$tdecay_lagged > 1, "Cooling Phase",
                                       ifelse(results.table$activezone == 0 & 
                                                results.table$goodfit != 1,
                                              "No Activity Phase","Active Heating Phase"))
  
  results.table$HeatingPhase <- ifelse(is.na(results.table$HeatingPhase), 
                                       "No Data Record",results.table$HeatingPhase)
  
  results.table$daytime <- ifelse(results.table$hour>=18| results.table$hour <6, "night","day")
  ews.temp <- aggregate(results.table$ews_T, 
                        by = list(results.table$no.days,results.table$daytime), 
                        mean, na.rm = T)
  colnames(ews.temp) <- c("no.days","daytime","meanT")
  ews.temp <- ews.temp[which(ews.temp$daytime == "night"),]
  
  idx <- c(5, diff(results.table$DateTime))
  i2 <- c(1,which(idx != 5), nrow(results.table)+1)
  results.table$grp.linebreak <- rep(1:length(diff(i2)), diff(i2))
  
  
  ##create a date indicator key for 'night of' column:
  
  nightof.Dates <- results.table$Date[which(results.table$hour == 12)]
  nightof.no.days <- results.table$no.days[which(results.table$hour == 12)]
  nightof.table <- cbind.data.frame(Date = unique(nightof.Dates), 
                                    no.days = unique(nightof.no.days))
  dailyheatrecord_ews <- data.frame()
  dailyheatrecord_ews <- merge(ews.temp,dailyheatrecord, by = "no.days", all = T)
  dailyheatrecord_ews <-merge(dailyheatrecord_ews,nightof.table, by = "no.days", all = T)
  dailyheatrecord_ews$weekday <- wday(dailyheatrecord_ews$Date, label = T)
  dailyheatrecord_ews$weekoftheyear <- isoweek(dailyheatrecord_ews$Date)
  
  tmp <- dailyheatrecord_ews$HourperDay.Use
  
  dailyheatrecord_ews$Totalactivehours <- round(tmp,2)
  
  dailyheatrecord_ews$houseID <- rep(houseid, nrow(dailyheatrecord_ews))
  
  p1 <- ggplot(results.table) +
    geom_line(aes(DateTime,Ibutton_W, color = "Flue Temperature", 
                  group = grp.linebreak)) +
    scale_x_datetime(date_breaks = "14 days", date_labels = "%d-%m") +
    xlab("Date and Time") +ylab("Temperature (deg C)") +
    theme_bw() + 
    theme(legend.title = element_blank(),
          panel.border = element_rect(fill=NA, colour = "black", size=1),
          plot.title = element_text(size = 20,hjust = 0.5),
          axis.title.y =element_text(color = "black", size = 18),
          axis.text.y = element_text(color = "black", size = 16),
          axis.text.x = element_text(size = 12),
          axis.title.x=element_text(size = 14))
  
  print(p1)
  print("Plot 1 done")
  #########plot 2: Flue Temperature with active heating/cooling classification############
  
  p2 <- ggplot(results.table) +
    geom_line(aes(DateTime,Ibutton_W, color = "Active heating zone",
                  group = grp.linebreak)) +
    geom_line(aes(DateTime, countonplot.cool, color = "Cooling Zone",
                  group = grp.linebreak)) +
    scale_x_datetime(date_breaks = "14 days", date_labels = "%d-%m") +
    ggtitle(" Fire-Lighting events - 2017") +
    xlab("DateTime") +
    ylab("Temperature (deg C)") +
    theme_bw() + 
    theme(legend.title = element_blank(), 
          legend.position = "bottom",
          panel.border = element_rect(fill=NA, colour = "black", size=1),
          axis.text.x = element_text(size = 8))
  
  print(p2)
  print("Plot 2 done")
  
  
  #######plot3: tiled plot for heating phases########
  
  labelling.table <-  cbind.data.frame(Date = unique(results.table$Date), 
                                       no.days = 1:length( unique(results.table$Date)))
  
  labelling.table <- labelling.table[1:no.days,]
  ind <- seq(1, nrow(labelling.table), by=4)
  labelling.table <- labelling.table[ind,]
  try3 <- results.table[order(results.table$ews_T),]
  try3$Date.factor <- as.factor(as.character(try3$Date))
  try3$Date.factor <- reorder(try3$Date.factor,rev(try3$DateTime))
  p3 <- ggplot(data = try3) +
    geom_tile(aes(time1,no.days,fill = factor(try3$HeatingPhase))) + xlab("Time") +ylab("Date") +
    ggtitle(" Phases Each day - 2017") +
    scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M",
                     limits = c(as.POSIXct("00:00:00", format = "%H:%M:%S", tz = "GMT"),
                                as.POSIXct("23:55:00", format = "%H:%M:%S", tz = "GMT"))) +
    theme_bw()+ scale_y_reverse(labels = labelling.table$Date, 
                                breaks = labelling.table$no.days) +
    theme(legend.title = element_blank(), legend.position = "bottom",
          panel.border = element_rect(fill=NA, colour = "black", size=1),
          axis.text.y = element_text(size = 10))
  
  print(p3)
  print("Plot 3 done")
  
  
  ##### plot 4: start times ######### 
  
  p4 <- ggplot(data = try3[which(try3$activezonediff == 1),]) +
    geom_point(aes(time1,no.days)) + xlab("Time") +ylab("Date") +
    ggtitle(" IButton start time plot - 2017") +
    scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M",
                     limits = c(as.POSIXct("00:00:00", format = "%H:%M:%S", tz = "GMT"),
                                as.POSIXct("23:55:00", format = "%H:%M:%S", tz = "GMT"))) +
    theme_bw()+ scale_y_reverse(labels = labelling.table$Date, 
                                breaks = labelling.table$no.days) +
    theme(legend.title = element_blank(), legend.position = "bottom",
          panel.border = element_rect(fill=NA, colour = "black", size=1),
          axis.text.y = element_text(size = 10))
  
  print(p4)
  print("Plot 4 done")
  
  
  ##### plot 5:Number of startups diurnal #########
  
  try2 <- results.table[which(results.table$activezonediff==1),]
  p5 <- ggplot(try2) +
    geom_histogram(aes(hour),stat="count", fill = "seagreen3") +
    scale_x_continuous(limits = c(0,23), breaks = sort(unique(results.table$hour)),
                       labels = c("00:00","01:00","02:00","03:00","04:00",
                                  "05:00","06:00","07:00","08:00","09:00",
                                  "10:00","11:00","12:00","13:00","14:00",
                                  "15:00","16:00","17:00","18:00","19:00",
                                  "20:00","21:00","22:00","23:00"), 
                       name = "Hour of the day") +
    ggtitle("Number of Startups observed  - 2017") +
    ylab("Number of start ups") +
    theme_bw()+
    theme(text = element_text(size=12))
  
  print(p5)
  print("Plot 5 done")
  
  ## plot 6: calendar plot #####
  
  dailyheatrecord_ews$date <- dailyheatrecord_ews$Date +43200 ##Converting to UTC for plotting ONLY!
  
  colorscale <- c( "grey","#FEE6CE","#FDD0A2","#FDAE6B","#FD8D3C","#F16913","#D94801")
  
  calendarPlot(dailyheatrecord_ews, pollutant = "Totalactivehours",year = 2017,
               cols = colorscale, par.settings=list(fontsize=list(text=22), 
                                                    axis.line = list(col = "black")),
               limits = c(0, 24), labels = c(0,1,3,6,9,12,15,18,21),
               breaks = c(0,1,3,6,9,12,15,18,21,24), wshift = 2,
               main = " Hours of Woodburner Use in 2017")
  
  print("Plot 6 done")
  
  
  ### plot 7:Living room diurnal#########
  p7 <- ggplot(data = results.table) + theme_bw() +
    geom_smooth(aes(time1,rollmean.LR, color = HeatingPhase), se = FALSE)+
    scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M",
                     limits = c(as.POSIXct("00:00:00", format = "%H:%M:%S", tz = "GMT"),
                                as.POSIXct("23:55:00", format = "%H:%M:%S", tz = "GMT")),
                     name = "Hour of the day") +
    ylab("Living Room Temperature (degree C)") +
    ggtitle("Diurnal living room temperature variation in each phase")
  print(p7)
  print("Plot 7 done")
  
  ### plot 8: Outdoor diurnal#########
  p8 <- ggplot(data = results.table) + theme_bw() +
    geom_smooth(aes(time1,rollmean.ews_T, color = HeatingPhase), se = FALSE)+
    scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M",
                     limits = c(as.POSIXct("00:00:00", format = "%H:%M:%S", tz = "GMT"),
                                as.POSIXct("23:55:00", format = "%H:%M:%S", tz = "GMT")),
                     name = "Hour of the day") +
    ylab("Outdoor Temperature (degree C)") +
    ggtitle("Diurnal outdoor temperature variation in each phase")
  print(p8)
  print("Plot 8 done")
  
  ### plot 9: Indoor-Outdoor diurnal#########
  p9 <- ggplot(data = results.table) + theme_bw() +
    geom_smooth(aes(time1,rollmean.delT, color = HeatingPhase), se = FALSE) +
    scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M",
                     limits = c(as.POSIXct("00:00:00", format = "%H:%M:%S", tz = "GMT"),
                                as.POSIXct("23:55:00", format = "%H:%M:%S", tz = "GMT")),
                     name = "Hour of the day") +
    ylab("I/O Temperature Difference (degree C)") +
    ggtitle("Diurnal I/O temperature variation in each phase")
  print(p9)
  print("Plot 9 done")
  
  
  
  ## plot 10: Daily Heating and Outside Temp#####
  orange.text <- element_text(color = "darkorange2", size = 16)
  p10 <- ggplot(data = dailyheatrecord_ews) +
    geom_col(aes(x=Date, y=Totalactivehours),fill = "steel blue") +
    geom_point(aes(x=Date, y=meanT, color = "meanT"),size=2, color = "darkorange2") +
    geom_path(aes(x=Date, y=meanT, color = "meanT"), size=1, color = "darkorange2") +
    scale_x_datetime(date_breaks = "14 days", date_labels = "%d-%b") +
    labs(title = "Total hours per day woodburner use - 2017", xlab = "Date") +
    scale_y_continuous(breaks = c(0,3,6,9,12,15,18,21,24),labels = c(0,3,6,9,12,15,18,21,24),
                       name = expression("Total hours per day use"),limits = c(0,25),
                       sec.axis = sec_axis(~ . *1,
                                           name = "Evening Temperatures (Outdoors - deg C)")) + 
    theme_bw() +
    theme(legend.title = element_blank(),
          panel.border = element_rect(fill=NA, colour = "black", size=1),
          plot.title = element_text(size = 20,hjust = 0.5),
          axis.text.y.right = orange.text, axis.title.y.right = orange.text,
          axis.title.y =element_text(color = "steelblue", size = 18),
          axis.text.y = element_text(color = "steelblue", size = 16),
          axis.text.x = element_text(size = 12),axis.title.x=element_text(size = 14))
  print(p10)
  print("Plot 10 done")
  
  
  ## plot 11: Regression for all the data########
  p11 <- ggplotRegression(lm(log_normDTi ~ tdecayperhour, data = results.table))
  p11 <- p11 + xlab("Number of hours since the living room started cooling down") +
    ylab("log(norm(DTi)") +
    theme(axis.title= element_text(size = 12))
  print(p11)
  print("Plot 11 done")
  
}



path <- "Q:/AQData/Data/CONA/2017/WORKING/Ibuttons/"
setwd(path)

load("masterFile.RData")
f1$DateTime <- as.POSIXct(as.character(f1$DateTime), format = "%Y-%m-%d %H:%M:%S", tz = "GMT")


House2.10min <- House2 %>%
  group_by(DateTime = cut(DateTime, breaks="10 min")) %>%
  summarize(Ibutton_LR= mean(Ibutton_LR),
            Ibutton_W = mean(Ibutton_W),
            ews_T = mean(ews_T), 
            depNo = round(mean(depNo)),
            houseID = 2)

analyze.data(House2.10min)







