############ the script needs to be changed ONLY for round no at one location : 
###this is done because we had a rotating style deployment and so needed to avoid repetition for the previous rounds. 
## the two lines that need changes are: line 95

## NOTE H14_HRV HAS BEEN FILTERED OUT DUE TO DIFFERENT NAMES GIVEN TO THE SENSORS.

library(data.table)
library(zoo)
library(plotly)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(chron)
library(scales)
library(gridExtra)
library(grid)
library(openair)
library(lfstat)  ## for fill_na function

#################FUNCTION DEFINITIONS#################


## round to the nearest '5th'precision-based' minute.
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

## to filter and extract required info from each file

filter_func<-function(house.file, house.id, deploymentno){
  
  ##import data
  #house.file <- filenames[2]
  
  fileName <- read.csv(house.file, skip = 19, stringsAsFactors = FALSE)
  fileName$Date.Time <- dmy_hms(fileName$Date.Time)
  fileName <- fileName[order(fileName$Date.Time),]
  
  ##extract instrument info from master key
  current.serialno <- as.character(strsplit(house.file, "_")[[1]][1])
  datemin <- dmy_hm(master_key$install_start_datetime[which(master_key$serialno == current.serialno)])
  datemax <- dmy_hm(master_key$install_end_datetime[which(master_key$serialno == current.serialno)])
  ibutton.type <- master_key$instrument_type[which(master_key$serialno == current.serialno)]
  
  ## run the precision fucntion to get the nearest date
  fileName$datetime_round <- round_minute(fileName$Date.Time, 5)
  fileName$numDatetime <- as.numeric(fileName$datetime_round)
  
  ## subset for only the 'install' times
  fileName <- fileName %>% select(datetime_round, numDatetime, Value) %>%
    filter(datetime_round >= datemin &  datetime_round <= datemax)
  
  ##change colnames:
  colnames(fileName) <- c("datetime_round","numDatetime",ibutton.type)
  
  ##add reference columns:
  fileName$houseID <- rep(house.id, length(fileName$numDatetime))
  fileName$deploymentno <- rep(deploymentno, length(fileName$numDatetime))
  return(fileName)

 
}

#################### static variables and files#####################

path <- "Q:/AQData/Data/CONA/2017/WORKING/Ibuttons/Data"
setwd(path)
list_files <- list.files(path, "*.csv")

roundno <- 6

master_key <- read.csv("Q:/AQData/Data/CONA/2017/Instrument_register2017.csv", stringsAsFactors = FALSE)

## subset only for available data
master_key <- master_key %>% filter(ifdownloaded == 1 & deploymentno == roundno)

## run the precision function on each file
houseIDs <- unique(master_key$houseID)


## implement this if a house needs to be missed. Needed for Round 4 as below:
#houseIDs <- c(1,2,6,14,20,22)
##read ews_data:
ews_data <- read.csv("Q:/AQData/Data/CONA/2017/WORKING/Ibuttons/Data/ews_ecan/rangiora_ews20112017.csv",
                     skip = 8, stringsAsFactors = FALSE)
ews_data$Date.NZST. <- dmy_hm(ews_data$Date.NZST.)
ews_data$numDatetime <- as.numeric(ews_data$Date.NZST.)
ews_data <- ews_data %>% select(Date.NZST.,numDatetime, MnTemp.C.)
colnames(ews_data) <- c("Date.Time","numDatetime","ews_T")




################# Per house analysis ########################

for(i in 1:length(houseIDs)) {

  ## running the function over the files
  filenames <- master_key$filename[which(master_key$houseID == houseIDs[i] &
                                           master_key$instrument_type != "Ibutton_LR_HRV")]
  
  
  
  file1 <- filter_func(filenames[1],houseIDs[i],roundno)
  file2 <- filter_func(filenames[2],houseIDs[i],roundno)
 
  final.file <- merge(file1,file2, by = "numDatetime")  ## NOT ADDING THE NON-MATCHING DATES IN THE ANALYSIS.
  
  ##add ews_data
  final.file <- merge(final.file, ews_data, by = "numDatetime", all.x = T)
  
  
  final.file <- final.file %>% select(numDatetime, DateTime = datetime_round.x,
                                      depNo = deploymentno.x,houseID = houseID.x,
                                      Ibutton_LR,Ibutton_W, ews_T)
  
  ############time based formatting##########################
  
  ## ALL CALCULATIONS DONE ON THE BASIS OF DateTime
  final.file$Date <- as.POSIXct(format(final.file$DateTime, format = "%Y/%m/%d"),format = "%Y/%m/%d")
  final.file$time <- format(final.file$DateTime, format = "%H:%M:%S")
  final.file$time1 <- as.POSIXct(final.file$time, format = "%H:%M:%S")
  final.file$hour <- as.numeric(format(final.file$DateTime, format = "%H"))
  
  ## making a 6 am to 6 am day.
  indexes <- which(final.file$hour == 6)
  final.file <- final.file[min(indexes):max(indexes),]
  no.days <- round(nrow(final.file)/288)
  lastdays <- nrow(final.file) - no.days*288
  
  final.file$no.days <- c(rep(1:no.days, each = 288),rep(no.days+1,lastdays))
  final.file$weekoftheyear <- week(as.Date(final.file$DateTime))
  index <- unique(final.file$weekoftheyear)
  
  # final.file$weeknum <- ifelse(final.file$weekoftheyear == index[1], 1, 
  #                              ifelse(final.file$weekoftheyear == index[2],2,))
  weekday.no <-as.numeric(strftime(final.file$Date, format = "%u"))
  final.file$weekday.no<- ifelse(final.file$hour<6,weekday.no - 1,weekday.no)
  
  #################temperature calculations#####################
  final.file$ews_T <- fill_na(final.file$ews_T)
  
  final.file$deltaT <- final.file$Ibutton_LR - final.file$ews_T
  final.file$rollmean.delT <- rollmean(final.file$deltaT,7,align = "right", fill = NA) 
  final.file$rollmean.LR <- rollmean(final.file$Ibutton_LR,7,align = "right", fill = NA) 
  final.file$rollmean.ews_T <- rollmean(final.file$ews_T,7,align = "right", fill = NA) 
  final.file$fluestep <- c(NA,diff(final.file$Ibutton_W))
  

  ############outputs################
  
  write.csv(final.file, paste0("Q:/AQData/Data/CONA/2017/WORKING/Ibuttons/Per_Round_data/",
                               "Round",roundno,"House", houseIDs[i], "_2017v2.csv"))
  print(paste("House no:",houseIDs[i]))
  
}

