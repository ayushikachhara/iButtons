library(data.table)
library(zoo)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(chron)
library(scales)
library(gridExtra)
library(grid)
library(openair)
library(leaflet)
library(maps)
library(mapproj)
library(maptools)
library(RColorBrewer)
library(shiny)
library(shinyjs)
library(shinythemes)
library(rgdal)
library(sp)
library(raster)
library(mapview)
library(automap)
library(dygraphs)  ## new addition on 09/05/2017
library(xts)
library(plotly)
library(lubridate)
library(dplyr)



### grid plots for all houses: 

path <- "Q:/AQData/Data/CONA/2017/WORKING/Ibuttons/all_rounds29112017/"

setwd(path)


listoffiles <- list.files(path)


# ##combine all:
all.houses <- do.call(rbind, lapply(listoffiles, read.csv))



all.houses$DateTime <- as.POSIXct(as.character(all.houses$DateTime),
                                  format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
all.houses$Date <- as.POSIXct(as.character(all.houses$Date),
                              format = "%Y-%m-%d", tz = Sys.timezone())

all.houses$time <- format(all.houses$DateTime, format = "%H:%M:%S", tz = "GMT")
all.houses$time1 <- as.POSIXct(all.houses$time, format = "%H:%M:%S", tz = "GMT")

list.perhouse <- split(all.houses, all.houses$houseID)


labelling.table <-  cbind.data.frame(Date = unique(all.houses$Date), 
                                     no.days = 1:length( unique(all.houses$Date)))

labelling.table <- labelling.table[which(!is.na(labelling.table$Date)),]
labelling.table <- labelling.table[order(labelling.table$Date),]
labelling.table$no.days <- 1:nrow(labelling.table)
labelling.table <- labelling.table[1:max(all.houses$no.days),]
ind <- seq(1, nrow(labelling.table), by=8)
labelling.table <- labelling.table[ind,]

########## StartUp Points: #######

plot.function1 <- function(house.file){
  
  labelling.table <-  cbind.data.frame(Date = unique(house.file$Date),
                                       no.days = 1:length( unique(house.file$Date)))

  labelling.table <- labelling.table[1:max(house.file$no.days),]
  ind <- seq(1, nrow(labelling.table), by=6)
  labelling.table <- labelling.table[ind,]
  
  p1 <- ggplot(data = house.file[which(house.file$activezonediff == 1),]) +
    geom_point(aes(time1,no.days), shape = 24, fill = "red2") + xlab("Time") +ylab("Date") +
    ggtitle(paste("HOUSE NUMBER:",house.file$houseID)) +
    scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M",
                     limits = c(as.POSIXct("00:00:00", format = "%H:%M:%S",tz = "GMT"),
                                as.POSIXct("23:55:00", format = "%H:%M:%S",tz = "GMT"))) +
    theme_bw()+ 
    scale_y_reverse(limits = c(max(labelling.table$no.days), min(labelling.table$no.days)),
                    labels = labelling.table$Date, 
                    breaks = labelling.table$no.days) +
    theme(legend.title = element_blank(), legend.position = "bottom",
          panel.border = element_rect(fill=NA, colour = "black", size=1),
          axis.text.y = element_text(size = 10),
          panel.background = element_rect(fill = "springgreen4",
                                          colour = "springgreen4"))
  
  return(p1)
}

plot.list1 <- lapply(list.perhouse, plot.function1)

all.plot1 <-ggplot(data = all.houses[which(all.houses$activezonediff == 1),]) +
  geom_point(aes(time1,no.days), shape = 23, fill = "red", size = 3, color = "blue") + 
  xlab("Hour of the Day") +ylab("Date") +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M",
                   limits = c(as.POSIXct("00:00:00", format = "%H:%M:%S",tz = "GMT"),
                              as.POSIXct("23:55:00", format = "%H:%M:%S",tz = "GMT"))) +
  
  theme_classic()+ 
  scale_y_reverse(limits = c(max(labelling.table$no.days), 
                             min(labelling.table$no.days)),
                  labels = labelling.table$Date, 
                  breaks = labelling.table$no.days) +
  theme(legend.title = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size=0.2),
        axis.text.y = element_text(size = 16),axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),axis.title.x = element_text(size = 16),
        panel.background = element_rect(fill ="green3"))


all.plot1

grid.arrange(plot.list1[[1]],plot.list1[[2]], 
             plot.list1[[3]],plot.list1[[4]],
             plot.list1[[5]],plot.list1[[6]],
             plot.list1[[7]], all.plot1, 
             ncol = 3)


plot.list1[[6]]
###### StartUp tiles:


plot.function2 <- function(house.file){
  labelling.table <-  cbind.data.frame(Date = unique(house.file$Date),
                                       no.days = 1:length( unique(house.file$Date)))
  
  labelling.table <- labelling.table[1:max(house.file$no.days),]
  ind <- seq(1, nrow(labelling.table), by=6)
  labelling.table <- labelling.table[ind,]
  
  p2 <- ggplot(data = house.file) +
    geom_tile(aes(time1,no.days,fill = factor(house.file$HeatingPhase))) + 
    xlab("Time") +ylab("Date") +
    ggtitle(paste("HOUSE NUMBER: ",house.file$houseID)) +
    scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M",
                     limits = c(as.POSIXct("00:00:00", format = "%H:%M:%S",tz = "GMT"),
                                as.POSIXct("23:55:00", format = "%H:%M:%S",tz = "GMT"))) +
    theme_bw()+ 
    scale_y_reverse(limits = c(max(labelling.table$no.days), min(labelling.table$no.days)),
                    labels = labelling.table$Date, 
                    breaks = labelling.table$no.days) +
    theme(legend.title = element_blank(), legend.position = "bottom",
          panel.border = element_rect(fill=NA, colour = "black", size=1),
          axis.text.y = element_text(size = 10))
  return (p2)
}

plot.list2 <- lapply(list.perhouse, plot.function2)
plot.list2[[6]]

all.plot2 <- ggplot(data = all.houses) +
  geom_tile(aes(time1,no.days,fill = HeatingPhase)) + 
  xlab("Time") +ylab("Date") +
  ggtitle("ALL HOUSES") +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M",
                   limits = c(as.POSIXct("00:00:00", format = "%H:%M:%S",tz = "GMT"),
                              as.POSIXct("23:55:00", format = "%H:%M:%S",tz = "GMT"))) +
  theme_bw()+ 
  scale_y_reverse(limits = c(max(labelling.table$no.days), min(labelling.table$no.days)),
                  labels = labelling.table$Date, 
                  breaks = labelling.table$no.days) +
  theme(legend.title = element_blank(), legend.position = "bottom",
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        axis.text.y = element_text(size = 10))

grid.arrange(plot.list2[[1]],plot.list2[[2]], 
             plot.list2[[3]],plot.list2[[4]],plot.list2[[6]],
             plot.list2[[7]], all.plot2, 
             ncol = 3)


plot.function3 <- function(house.file){
  try2 <- house.file[which(house.file$activezonediff==1),]
  p3 <- ggplot(try2) +
    geom_histogram(aes(hour),stat="count", fill = "seagreen3") + theme_bw()+
    scale_x_continuous(limits = c(0,23), breaks = sort(unique(house.file$hour)),
                       labels = c("00:00","01:00","02:00","03:00","04:00",
                                  "05:00","06:00","07:00","08:00","09:00",
                                  "10:00","11:00","12:00","13:00","14:00",
                                  "15:00","16:00","17:00","18:00","19:00",
                                  "20:00","21:00","22:00","23:00"), 
                       name = "Hour of the day") +
    scale_y_continuous(limits = c(0,120),name = "Number of Startups")+
    ggtitle(paste("HOUSE NUMBER:", try2$houseID[1])) +
    theme(text = element_text(size=12))
}

plot.list3 <- lapply(list.perhouse, plot.function3)

#####startup hour###########
startup.hour <- all.houses[which(all.houses$activezonediff == 1),]
labels.hour <- unique(startup.hour$hour)

labels.hour <- paste0(labels.hour, ":00")
#### colour vector:#######
cols <- rep(c("green3", "red2"),12)
startup.hour <- all.houses[which(all.houses$activezonediff == 1),]
all.plot3 <- ggplot(data = startup.hour, aes(x = as.numeric(hour))) +
  geom_bar(aes(y = (..count..)), fill = cols, color = rev(cols), width = 1) +
  theme_classic() + scale_y_continuous(breaks = c(0,30,60,90,120),name = "Number of Fire light-ups")+
  scale_x_continuous(labels = labels.hour,
                     breaks =  unique(startup.hour$hour), 
                     name = "Hour of the Day") +
  theme(panel.border = element_rect(fill=NA, colour = "black", size=1),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.background = element_rect(fill = "azure1"))

all.plot3


grid.arrange(plot.list3[[1]],plot.list3[[2]], 
             plot.list3[[3]],plot.list3[[4]],plot.list3[[6]],
             plot.list3[[7]], all.plot3, 
             ncol = 2)



########### how many houses are burning in the given hour? ####

hourly.count <- all.houses%>% 
  group_by(houseID,Date, hour, HeatingPhase) %>%
  summarize(count = n())
#library(dplyr)

hourly.count1 <- hourly.count[hourly.count$HeatingPhase == "Active Heating Phase",]
hourly.count1 <- hourly.count1[1:4463,]

hourly.count1 <- hourly.count1 %>%
  group_by(Date, hour) %>%
  summarise(count = n())


all.Dates <- cbind.data.frame(Date = unique(hourly.count1$Date))
all.Dates$no.days <- 1:nrow(all.Dates)
all.Dates <- all.Dates[order(all.Dates$Date),]
all.Dates$no.days <- 1:nrow(all.Dates)

hourly.count1 <- merge(hourly.count1, all.Dates, by = "Date", all = T)

labelling.table <-  cbind.data.frame(Date = unique(hourly.count1$Date), 
                                     no.days = 1:length( unique(hourly.count1$Date)))

labelling.table <- labelling.table[which(!is.na(labelling.table$Date)),]
labelling.table <- labelling.table[order(labelling.table$Date),]
labelling.table$no.days <- 1:nrow(labelling.table)
labelling.table <- labelling.table[1:max(hourly.count1$no.days),]
ind <- seq(1, nrow(labelling.table), by=8)
labelling.table <- labelling.table[ind,]


all.plot4 <- ggplot(data = hourly.count1) +
  geom_tile(aes(no.days,hour,fill = count)) + 
  scale_fill_continuous(low = "green3", high = "red2", name = "Number of houses")+
  xlab("Time") +ylab("Date") +
  ggtitle("ALL HOUSES") + theme_classic()+
  scale_x_continuous(labels = labels.hour,
                     breaks =  unique(startup.hour$hour), 
                     name = "Hour of the Day") +
  scale_y_reverse(limits = c(max(labelling.table$no.days), 
                             min(labelling.table$no.days)),
                  labels = labelling.table$Date, 
                  breaks = labelling.table$no.days) +
  theme(text = element_text(size=12),
        panel.background = element_rect(fill = "azure1"))
all.plot4


###################################
hourly.count2 <- all.houses%>% 
  group_by(houseID,hour, HeatingPhase) %>%
  summarize(count = n())
hourly.count2 <- hourly.count2[which(!is.na(hourly.count2$houseID)),]
hourly.count2 <- hourly.count2[which(hourly.count2$HeatingPhase != "No Data Record"),]

try3 <- hourly.count2[which(hourly.count2$HeatingPhase == "Active Heating Phase"),]

ggplot(try3) +
  geom_bar(aes(x = hour, y = count, fill = factor(houseID)),
           stat = "identity", width = 0.8) + theme_classic() +
  ylab("")+
  scale_fill_manual(values = c("red2","gold","green3","dodgerblue3",
                               "yellow","purple","indianred2"), 
                    name = "",
                    labels = c("House 1","House 2", "House 5",
                               "House 6", "House 14", "House 20",
                               "House 22"))+
  scale_x_continuous(labels = labels.hour,
                     breaks =  unique(startup.hour$hour), 
                     name = "Hour of the Day")+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), 
        legend.position = c(0.25, 0.85),
        legend.background = element_rect(fill = "lightskyblue"),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.direction = "horizontal", 
        panel.border = element_rect(fill=NA, colour = "black", size=0.2),
        axis.text.x = element_text(size = 16,angle = 45, vjust=0.5),axis.title.x = element_text(size = 16),
        panel.background = element_rect(fill = "lightskyblue", color = "black"))
  

hourly.count2$count <- ifelse(hourly.count2$HeatingPhase == "Cooling Phase", 
                              -hourly.count2$count, hourly.count2$count)

hourly.count2 <- hourly.count2[which(hourly.count2$HeatingPhase != "No Activity Phase"),]

all.plot5 <- ggplot(hourly.count2) +
  geom_bar(aes(x = hour, y = count, fill = factor(HeatingPhase)),
           stat = "identity") + theme_classic() +
  scale_fill_manual(values = c("red2","green3"), name = "Activity Routine") +
  ylab("Total records per phase")+
  scale_x_continuous(labels = labels.hour,
                    breaks =  unique(startup.hour$hour), 
                    name = "Hour of the Day")+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), panel.background = element_rect(fill = "azure1"))


###################################Daily###################
path2 <- "Q:/AQData/Data/CONA/2017/WORKING/Ibuttons/DAILY/daily_round6"
setwd(path2)
dailyheatrecords <- list.files(path = path2, pattern = ".csv")

daily.all <- do.call(rbind, lapply(dailyheatrecords, read.csv))

daily.all$Date <- as.POSIXct(as.character(daily.all$Date),
                              format = "%Y-%m-%d", tz = Sys.timezone())


daily.summary1 <- aggregate(daily.all$Totalactivehours, by = list(daily.all$Date), 
                           FUN = function(x)mean(x, na.rm = T))

colnames(daily.summary1) <- c("Date", "Totalactivehours")

daily.summary2 <- aggregate(daily.all$meanT, by = list(daily.all$Date), 
                            FUN = function(x)mean(x, na.rm = T))

colnames(daily.summary2) <- c("Date", "meanT")

daily.summary.all <- merge(daily.summary1,daily.summary2, by = "Date", all =T)

daily.summary3 <- daily.all%>% 
  group_by(Date) %>%
  summarize(count = n())


list.perhouse2 <- split(daily.all, daily.all$houseID)


## plot 10: Daily Heating and Outside Temp#####
orange.text <- element_text(color = "blue", size = 16)
p10 <- ggplot(data = daily.summary.all) +
  geom_col(aes(x=Date, y=Totalactivehours),fill = "red",color = "red3" ,size = 1) +
  geom_smooth(aes(x=Date, y=meanT, color = "meanT"), size=1, color = "blue", method = "lm", formula = y~I(0+x)) +
  geom_point(aes(x=Date, y=meanT, color = "meanT"),size=2, color = "blue", shape = 23, fill = "blue") +
  scale_x_datetime(date_breaks = "14 days", date_labels = "%d-%b") +
  labs(xlab = "Date") +
  scale_y_continuous(breaks = c(0,3,6,9,12,15,18,21,24),labels = c(0,3,6,9,12,15,18,21,24),
                     name = expression("Total hours per day use"),limits = c(0,25),
                     sec.axis = sec_axis(~ . *1,
                                         name = "Evening Temperatures (Outdoors - deg C)")) + 
  theme_classic() +
  theme(legend.title = element_blank(),
        panel.background = element_rect("gold"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        axis.text.y.right = orange.text, axis.title.y.right = orange.text,
        axis.title.y =element_text(color = "red", size = 16),
        axis.text.y = element_text(color = "red", size = 16),
        axis.text.x = element_text(size = 16),
        axis.title.x=element_text(size = 16))

print(p10)

#########################
grid.arrange(all.plot1, all.plot4, all.plot5, p10,all.plot3, ncol = 2)
