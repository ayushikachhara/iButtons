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
######## FUNCTION DEFINITION:

## to filter and extract required info from each file
round_minute<-function(x,precision){
  
  m<- minute(x)+second(x)/60
  m.round<- round(m/precision)*precision
  minute(x)<-m.round
  second(x)<-0
  return (x)
}


##calculating 'modes'
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



## axis reversal:

c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
  a <- as.trans(a)
  b <- as.trans(b)
  
  name <- paste(a$name, b$name, sep = "-")
  
  trans <- function(x) a$trans(b$trans(x))
  inv <- function(x) b$inverse(a$inverse(x))
  
  trans_new(name, trans, inv, breaks, format)
}

filter_func_pastdata<-function(house.file, house.id){
  
  ##import data
  fileName <- read.csv(house.file, stringsAsFactors = FALSE)
  fileName$DateTime <- as.POSIXct(fileName$DateTime, format = "%d/%m/%Y %H:%M")
  colnames(fileName) <- c("DateTime","Ibutton_LR","Ibutton_W","Ibutton_Kitchen","ews_T","HouseAddress")
  
  fileName <- fileName[order(fileName$DateTime),]
  fileName$datetime_round.x <- round_minute(fileName$DateTime,5)
  fileName$numDatetime <- as.numeric(fileName$datetime_round.x)
  
  ##time based formatting
  ## ALL CALCULATIONS DONE ON THE BASIS OF DATETIME_ROUND.X
  fileName$Date <- as.POSIXct(format(fileName$datetime_round.x, format = "%Y/%m/%d"),
                              format = "%Y/%m/%d", tz = Sys.timezone())
  fileName$time <- format(fileName$datetime_round.x, format = "%H:%M:%S", tz = Sys.timezone())
  
  fileName$time1 <- as.POSIXct(fileName$time, format = "%H:%M:%S", tz = Sys.timezone())
  fileName$hour <- as.numeric(format(fileName$datetime_round.x, format = "%H", tz = Sys.timezone()))
  indexes <- which(fileName$hour == 6)
  fileName <- fileName[min(indexes):max(indexes),]
  no.days <- round(nrow(fileName)/288)
  lastdays <- nrow(fileName) - no.days*288
  
  fileName$no.days <- c(rep(1:no.days, each = 288),rep(no.days,lastdays))
  fileName$weekoftheyear <- week(as.Date(fileName$datetime_round.x))
  index <- unique(fileName$weekoftheyear)
  
  
  ######################################
  ## temp calc
  fileName$deltaT <- fileName$Ibutton_LR - fileName$ews_T
  fileName$rollmean.delT <- rollmean(fileName$deltaT,7,align = "right", fill = NA) 
  fileName$rollmean.LR <- rollmean(fileName$Ibutton_LR,7,align = "right", fill = NA) 
  fileName$ews_T <- fill_na(fileName$ews_T)
  fileName$rollmean.ews_T <- rollmean(fileName$ews_T,7,align = "right", fill = NA) 
  fileName$fluestep <- c(NA,diff(fileName$Ibutton_W))
  fileName$fluestep_next <- c(tail(fileName$fluestep, -1),NA)
  
  threshold.step.fixed <- as.numeric(quantile(fileName$fluestep,0.96,na.rm = T))
  
  fileName$ifheating <- ifelse((fileName$fluestep>= threshold.step.fixed&
                                    fileName$fluestep_next>= fileName$fluestep &
                                    fileName$Ibutton_W<=30),40,0)
  
  fileName$activezone <- ifelse(fileName$Ibutton_W>=35,35,0)
  fileName$activezonediff <-c(NA,diff(fileName$activezone))
  fileName$activezonediff <-ifelse(fileName$activezonediff==35,1,0)
  
  ######################################
  #count number of heating records per day
  events.flue <-rle(fileName$activezone)
  
  sequence <- unlist(sapply(events.flue$lengths, seq))
  countzero <- sequence
  countzero[fileName$activezonediff <=0] <- 0
  fileName$count.event <- sequence
  fileName$count.event[fileName$activezone <= 0] <- 0
  fileName$total.activemin <- fileName$count.event*5
  fileName$countonplot <- ifelse(fileName$count.event>0,30,0)
  ######################################
 
  return(fileName)
}

#########################
### Static variables
path <-"Q:/AQData/Data/CONA/2017/WORKING/Ibuttons/2016/"
setwd(path)

list_files <- list.files(path, "*.csv")


for(i in 1:length(list_files)) {
  
  house.id <- strsplit(list_files[1],".csv")[[1]]
  house.id <- strsplit(house.id,"house")[[1]][2]
  final.file <- filter_func_pastdata(list_files[1],house.id)

  ######division of a day from 6am to 6am analysis.
  
  heatingday <- aggregate(final.file$activezonediff, by=list(final.file$no.days), sum)
  colnames(heatingday) <- c("no.days","HeatingDay")
  #heatingday$Date <- as.POSIXct(format(heatingday$Date, format = "%Y-%m-%d"))
  final.file <- merge(final.file, heatingday, by = "no.days", all = T)
  final.file$HeatingDay <- ifelse(final.file$HeatingDay != 0,1,0)
  
  
  ##heating versus non-heating day comparison of LR temperatures.
  hourlymean <- aggregate(final.file$Ibutton_LR, by=list(final.file$hour,final.file$HeatingDay), mean)
  colnames(hourlymean) <- c("Hour", "HeatingDay","Ibutton_LR")
  
  
  ##total heating minutes per day.
  dailyheatrecord <-aggregate(final.file$countonplot, by=list(final.file$no.days), sum)
  dailyheatrecord$x <- (dailyheatrecord$x/30)*5
  dailyheatrecord$xhour <- dailyheatrecord$x/60
  colnames(dailyheatrecord) <- c("no.days", "heatingminutesperday","HourperDay.Use")
  final.file <- merge(final.file, dailyheatrecord, by = "no.days", all = T)
  
  final.file$daytime <- ifelse(final.file$hour>=18, "night","day")
  ecan.temp <- aggregate(final.file$ews_T, by=list(final.file$Date,final.file$daytime), mean, na.rm = T)
  colnames(ecan.temp) <- c("Date","daytime","meanT")
  ecan.temp <- ecan.temp[which(ecan.temp$daytime == "night"),]
  
  dailyheatrecord_ecan <- cbind(ecan.temp,dailyheatrecord)
  dailyheatrecord_ecan$weekday <- wday(dailyheatrecord_ecan$Date, label = T)
  dailyheatrecord_ecan$weekoftheyear <- isoweek(dailyheatrecord_ecan$Date)
  index <- unique(dailyheatrecord_ecan$weekoftheyear)
  
  dailyheatrecord_ecan$weeknum <- ifelse(dailyheatrecord_ecan$weekoftheyear == index[1], 1, 
                                         ifelse(dailyheatrecord_ecan$weekoftheyear == index[2],2,
                                                ifelse(dailyheatrecord_ecan$weekoftheyear == index[3],3,4)))
  
  
  
  tmp <- dailyheatrecord_ecan$HourperDay.Use
  dailyheatrecord_ecan$HourperDay.Use <- ifelse(tmp == 0, "0", dailyheatrecord_ecan$HourperDay.Use)
  dailyheatrecord_ecan$HourperDay.Use <- ifelse(tmp > 0 & tmp <= 3, "1-3", dailyheatrecord_ecan$HourperDay.Use)
  dailyheatrecord_ecan$HourperDay.Use <- ifelse(tmp > 3 & tmp <= 6, "3-6", dailyheatrecord_ecan$HourperDay.Use)
  dailyheatrecord_ecan$HourperDay.Use <- ifelse(tmp > 6 & tmp <= 9, "6-9", dailyheatrecord_ecan$HourperDay.Use)
  dailyheatrecord_ecan$HourperDay.Use <- ifelse(tmp > 9 & tmp <= 12, "9-12", dailyheatrecord_ecan$HourperDay.Use)
  dailyheatrecord_ecan$HourperDay.Use <- ifelse(tmp > 12 & tmp <= 15, "12-15", dailyheatrecord_ecan$HourperDay.Use)
  dailyheatrecord_ecan$HourperDay.Use <- ifelse(tmp > 15 & tmp <= 18," 15-18", dailyheatrecord_ecan$HourperDay.Use)
  dailyheatrecord_ecan$HourperDay.Use <- ifelse(tmp > 18, "Above 18", dailyheatrecord_ecan$HourperDay.Use)
  dailyheatrecord_ecan$HourperDay.Use <- factor(dailyheatrecord_ecan$HourperDay.Use)
  dailyheatrecord_ecan$Totalactivehours <- round(tmp,2)
  levels(dailyheatrecord_ecan$HourperDay.Use) <- c("0","1-3","3-6","6-9","9-12","12-15","15-18","Above 18")
  dailyheatrecord_ecan <- dailyheatrecord_ecan %>% select(Date,weekday,weeknum,meanT,
                                                          heatingminutesperday,HourperDay.Use,
                                                          Totalactivehours)
  
  
  ######################## 
  ## PLOTS:
  
  
  # ### plotting bit
  # 
  # PDFPath = paste0("Q:/AQData/Data/CONA/2017/WORKING/Ibuttons/","2016House_",house.id,"v2.pdf")
  # pdf(file=PDFPath, paper = "USr", width = 15)
  # 
  p1 <- ggplot(final.file) +
    geom_line(aes(datetime_round.x,Ibutton_W, color = "Flue Temperature")) +
    geom_line(aes(datetime_round.x, Ibutton_LR,color = "Living room Temperature")) +
    geom_line(aes(datetime_round.x, countonplot, color = "Active heating zone")) +
    geom_hline(yintercept = 30)+
    scale_x_datetime(date_breaks = "1 day", date_labels = "%d-%m")+
    ggtitle("Fire-Lighting events 2016") +xlab("DateTime") +ylab("Temperature (deg C)") +
    theme_bw() + theme(legend.title = element_blank(), legend.position = "bottom",
                       panel.border = element_rect(fill=NA, colour = "black", size=1), text = element_text(size=15))
  print(p1)

  
  
  ## plot2
  #### axis reversal empty list"
  rev_date <- c_trans("reverse", "time")
  p2 <- ggplot(data = final.file[which(final.file$activezonediff == 1),]) +
    geom_point(aes(time1,Date, color = "Start times")) + xlab("Time") +ylab("Date") +
    ggtitle("IButton start time plot  2016") +
    scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M",
                     limits = c(as.POSIXct("00:00:00", format = "%H:%M:%S"),
                                as.POSIXct("23:55:00", format = "%H:%M:%S")), expand = c(0,0))+
    theme_bw()+ scale_y_continuous(trans = rev_date)+
    theme(legend.title = element_blank(), legend.position = "bottom",
          panel.border = element_rect(fill=NA, colour = "black", size=1))

  print(p2)
  
  
  
  ## plot 3
  p3 <- ggplot(final.file[which(final.file$activezonediff ==1),]) +
    geom_histogram(aes(factor(hour)),stat = "count",fill = "steelblue") + 
    ggtitle("Startups observed  - 2016") +
    scale_x_discrete(limits= c(0:24),breaks = c(2,4,6,8,10,12,14,16,18,20,22,24), 
                     name = "Hour of the day", expand = c(0,0), labels = c("02:00","04:00","06:00","08:00","10:00","12:00",
                                                                           "14:00","16:00","18:00","20:00","22:00","24:00")) + 
    scale_y_continuous(labels = c(1,2,3,4,5,6,7,8),limits = c(0,8), breaks = c(1,2,3,4,5,6,7,8),
                       name = "Number of light ups observed")+
    theme_bw() +
    theme(panel.border = element_rect(fill=NA, colour = "black", size=1),
          plot.title = element_text(hjust = 0.5),text = element_text(size=20))
  
  print(p3)
  
  ## plot 4
  p4 <- ggplot(data = dailyheatrecord_ecan, aes(weekday,weeknum)) +
    geom_tile(aes(fill =HourperDay.Use), color = "black",size=1)+
    scale_x_discrete(limits=c("Mon", "Tues","Wed", "Thurs", "Fri","Sat","Sun"))+
    geom_text(aes(label=round(dailyheatrecord_ecan$Totalactivehours), size = 5))+
    scale_fill_manual(values = brewer.pal(6,"PuBu"),breaks = levels(dailyheatrecord_ecan$HourperDay.Use),
                        name = "total active hours per day")+
    labs(title = "Total active heating: comparison by week - 2016",
         y = "Week Numbers(Round1)") + scale_y_reverse(labels = c(1,2,3),breaks = c(1,2,3))+
    coord_equal() +theme_classic() +theme(panel.border = element_rect(fill=NA, colour = "black", size=1),
                                          plot.title = element_text(hjust = 0.5),text = element_text(size=15))

  print(p4)
  
  dailyheatrecord_ecan$date <- dailyheatrecord_ecan$Date+43200  ##Converting to UTC for plotting ONLY!
  dailyheatrecord_ecan$HourperDay.Use <- factor(dailyheatrecord_ecan$HourperDay.Use, 
                                                levels = order(dailyheatrecord_ecan$Totalactivehours))
  colorscale <- c( "grey","#FEE6CE","#FDD0A2","#FDAE6B","#FD8D3C","#F16913","#D94801")
  
  
  x <- calendarPlot(dailyheatrecord_ecan, pollutant = "Totalactivehours",year = 2016,
                    cols = colorscale, par.settings=list(fontsize=list(text=22), axis.line = list(col = "black")),
                    key.position = "left", 
                    limits = c(0, 24), labels = c(0,3,6,9,12,15,18,21,24), 
                    breaks = c(0,3,6,9,12,15,18,21,24,30), wshift = 2,
                    main = " ")
  x$plot$x.scales$labels <- c("Sat","Sun","Mon","Tues","Wed","Thurs","Fri")
  x$plot$legend$right$args$key$height <- 3
  
  plot(x)
  
  # plot 5
  p4 <- ggplot(data = final.file[which(!is.na(final.file$HeatingDay)),]) +
    geom_smooth(aes(x = hour,y = rollmean.LR,  fill = factor(HeatingDay))) +
    scale_x_discrete(limits=c(0:23)) +
    labs(title = "Heating vs NonHeating Day: Living Room Temperature Comparison  2016", xlab = "Hour of the day",
         ylab = "Living Room temperature (deg C)") +
    theme(legend.title = element_blank(), legend.position = "bottom",
          panel.border = element_rect(fill=NA, colour = "black", size=1),
          plot.title = element_text(hjust = 0.5),text = element_text(size=15)) +theme_bw()

  print(p4)
  # 
  p5 <- ggplot(data = final.file[which(!is.na(final.file$HeatingDay)),]) +
    geom_smooth(aes(x = hour,y = rollmean.LR,  fill = factor(HeatingDay)), method = "loess") +
    scale_x_discrete(limits=c(0:23)) +
    labs(title = "Heating vs NonHeating Day: Living Room Temperature Comparison 2016\n (loess)", xlab = "Hour of the day",
         ylab = "Living Room temperature (deg C)") +
    theme(legend.title = element_blank(), legend.position = "bottom",
          panel.border = element_rect(fill=NA, colour = "black", size=1)) +theme_bw()

  print(p5)
  # 
  # # plot 5
  p6 <- ggplot(data = final.file[which(!is.na(final.file$HeatingDay)),]) +
    geom_smooth(aes(x = hour,y = deltaT,  fill = factor(HeatingDay)), method = "loess") +
    ggtitle("Heating vs NonHeating Day: I/O Temperature Difference  2016(loess)") +ylab("I/O temperature difference (deg C)") +
    theme(panel.border = element_rect(fill=NA, colour = "black", size=1)) +theme_bw()

  print(p6)

  # # plot 5
  p7 <- ggplot(data = final.file[which(!is.na(final.file$HeatingDay)),]) +
    geom_smooth(aes(x = hour,y = deltaT,  fill = factor(HeatingDay))) +
    ggtitle("Heating vs NonHeating Day: I/O Temperature Difference  2016 (gam)") +ylab("I/O temperature difference (deg C)") +
    theme(panel.border = element_rect(fill=NA, colour = "black", size=1)) +theme_bw()

  print(p7)
  
  p10 <- ggplot(data = final.file[which(!is.na(final.file$HeatingDay)),]) +
    geom_smooth(aes(x = hour,y = rollmean.ews_T,  fill = factor(HeatingDay))) +
    scale_x_discrete(limits=c(0:23)) +
    labs(title = "Outside Temperature Comparison  2016", xlab = "Hour of the day")+
    theme(legend.title = element_blank(), legend.position = "bottom",
          panel.border = element_rect(fill=NA, colour = "black", size=1),
          plot.title = element_text(hjust = 0.5),text = element_text(size=15)) +theme_bw()
  
  print(p10)
  
  
  orange.text <- element_text(color = "darkorange2", size = 16)
  p8 <- ggplot(data = dailyheatrecord_ecan) +
    geom_col(aes(x=Date, y=Totalactivehours),fill = "steelblue") +
    geom_point(aes(x=Date, y=meanT, color = "meanT"),size=2, color = "darkorange2")+
    geom_path(aes(x=Date, y=meanT, color = "meanT"), size=1, color = "darkorange2")+
    scale_x_datetime(date_breaks = "2 days", date_labels = "%d-%b")+
    labs(title = "Total hours per day woodburner use - 2016", xlab = "Date") +
    scale_y_continuous(breaks = c(0,3,6,9,12,15,18,21,24),labels = c(0,3,6,9,12,15,18,21,24),
                       name = expression("Total hours per day use"),limits = c(0,25),
                       sec.axis = sec_axis(~ . *1,
                                           name = "Evening Temperatures (Outdoors - deg C)"))+ theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          panel.border = element_rect(fill=NA, colour = "black", size=1),
          plot.title = element_text(size = 20,hjust = 0.5),
          axis.text.y.right = orange.text, axis.title.y.right = orange.text,
          axis.title.y =element_text(color = "steelblue", size = 18), 
          axis.text.y = element_text(color = "steelblue", size = 16),
          axis.text.x = element_text(size = 12),axis.title.x=element_text(size = 14))
  print(p8)
  
  # grid.newpage()
  # grid.table(dailyheatrecord_ecan)
  modal.hour <- getmode(final.file$hour[which(final.file$activezonediff ==1)])
  
  #dev.off() 
  
  final.file <- final.file %>% select(datetime_round.x, no.days, daytime,
                                      time1,hour,weekoftheyear,weeknum,
                                      Ibutton_LR,Ibutton_W,ews_T,deltaT,
                                      rollmean.LR,rollmean.delT,fluestep,
                                      activezone,activezonediff,count.event,
                                      total.activemin,HeatingDay,heatingminutesperday,
                                      HourperDay.Use)
  
  #write.csv(final.file, paste0("Q:/AQData/Data/CONA/2017/Raw Data/Ibuttons/","/processed_data/house",house.id, "_2016.csv"))
  #write.csv(dailyheatrecord_ecan,
           # paste0("Q:/AQData/Data/CONA/2017/Raw Data/Ibuttons/","/processed_data/DAILY/houseDAILY",house.id, "_2016.csv"))
  
  ######################################
  
  print(i)
  print(modal.hour)
}
