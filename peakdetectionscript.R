#########################################################################

library(zoo)
library(dynlm)
library(forecast)
library(TTR)
library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)
library(nlme)
library(quantmod)
library(plotly)

path <- "Q:/AQData/Data/CONA/2017/WORKING/Ibuttons/allrounds/"
setwd(path)

## Create Some Dummy Data
list_files <- list.files(path = path, "*.csv")
x <- read.csv(list_files[6])

x$datetime_round.x <- ymd_hms(x$datetime_round.x)
x$year <-year(x$datetime_round.x)
x$time <- format(x$datetime_round.x, format = "%H:%M:%S")
x$time1 <- as.POSIXct(x$time, format = "%H:%M:%S")
x$numdatetime <- as.numeric(x$datetime_round.x)
x <- x[order(x$datetime_round.x),]
offset <- as.numeric(with(x[which(x$Ibutton_W <=15),],
                          quantile((Ibutton_LR - Ibutton_W), 0.75)))

#offset <- 3.5
## subtract the offset to get adjusted LR temp.
x$LR_adj <- x$Ibutton_LR - offset

## calculate indoor delta T with the adjusted living room temperature value
x$DTi <- ifelse(x$Ibutton_W != 14,x$Ibutton_W - x$LR_adj, NA)
summary(x$DTi)

x$step_DTi <- c(NA, diff(x$DTi))
x$logDTi <- log(x$DTi,10)

## replace all meaningless values:
x$logDTi <- ifelse(x$logDTi == Inf |x$logDTi == -Inf |is.na(x$logDTi), NA,x$logDTi)
#######################################


dummydata <- x
#######################################################################

results.z <- data.frame(X.1 = NA, AdjR =NA,
                        Intercept = NA,Slope = NA)

## calculate the slope, intercept and correlation value
for(i in 1:(length(dummydata$X) - 13)){
  sub.dummy <- dummydata[i:(i+11),]
  sub.dummy$x <- 1:length(sub.dummy$X.1)
  sub.dummy$x <- (sub.dummy$x*5)/60   ## converting to an hourly variable
  print(i)
  
  if(!(is.na(sub.dummy$logDTi[1]))){
    mod <- lm(formula = logDTi ~ x, data = sub.dummy) # corelation equation (log T response to time)
    #variable names
    var.names <- c("Intercept","Slope")
    vec <- cbind.data.frame(sub.dummy$X.1[1], 
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
results.table <- merge(x, results.z, by = "X.1", all = T)

# results.table$datetime <- x$datetime_round.x[1:length(results.table$AdjR)]
# results.table$Ibutton_W <- x$Ibutton_W[1:length(results.table$AdjR)]
# results.table$flue_steps <- c(NA, diff(results.table$Ibutton_W))

## look at the good fit for decaying curves.
results.table$goodfit <- ifelse((results.table$step_DTi<0.1 & 
                                   results.table$AdjR>0.97 & results.table$DTi > 5),1,0)

r <- rle(results.table$goodfit)
sequence <- unlist(sapply(r$lengths, seq))
results.table$tdecay <- sequence
results.table$tdecay[results.table$goodfit <= 0] <- 0
results.table$tdecay[is.na(results.table$goodfit)] <- NA

## offsetting by 3. 
results.table$tdecay_lagged <- results.table$tdecay - 3


## remove shorter than one hour lengths of observations.
# r$values[ r$lengths <= 12 & r$values == 1 ] <- 0
# results.table$coolingzone<-inverse.rle(r)

# results.table$coolingzone <- ifelse(results.table$coolingzone == 1,results.table$Ibutton_W,0)
# results.table$activezone <- ifelse(results.table$coolingzone == 0 & results.table$Ibutton_W>=40,results.table$Ibutton_W,0)
# 
# results.table$classification <- ifelse(results.table$coolingzone == 0 & results.table$activezone == 0, "No activity",
#                                        ifelse(results.table$activezone == 0, "cooling zone", "active heating zone"))
# 
# results.table$hour <- x$hour[1:length(results.table$AdjR)]


### temperature before the start of the decay.
results.table$T0 <- NA

for( i in 1:length(results.table$X.1)){
  
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
##########################################################

results.table <-  do.call(data.frame,lapply(results.table, function(x) replace(x, is.infinite(x),NA)))

## all results - slope for model:

ggplot(results.table) +
  geom_point(aes(y = log_normDTi, x = tdecay_lagged, colour = factor(Date))) +
  stat_smooth(aes(y = log_normDTi, x = tdecay_lagged, colour = factor(Date)), method = "lm", se = F) + 
  ggtitle(" \nHOUSE6")+
  theme_bw()+ theme(legend.title = element_blank(), legend.position = "bottom")

ggplot(results.table) +
  geom_point(aes(y = log_normDTi, x = tdecay_lagged)) +
  stat_smooth(aes(y = log_normDTi, x = tdecay_lagged), method = "lm", se = F) + 
  ggtitle(" \nHOUSE6")+
  theme_bw() + theme(legend.title = element_blank(), legend.position = "bottom")




#results.table$tdecay_lagged_hour <- results.table$tdecay_lagged*5/60

temp_model <- lm(formula = logDTi ~ tdecay_lagged, data = results.table[which(results.table$no.days ==10),])

summary(temp_model)$adj.r.squared
summary(temp_model)$coef
summary(temp_model)

#write.csv(results.table, "results.table_h22.csv")
##############################
## plotting bit:

## plotting bit

df1 <- results.table[2150:2200,]
plot(df1$datetime,df1$Ibutton_W, type = "l", lwd = 0.1)
lines(df1$datetime, df1$goodfit*df1$Ibutton_W,col = "blue", lwd= 2,type = "S")
lines(df1$datetime, df1$activezone,col = "red", lwd = 2, type = "S")
plot(df1$log_normDTi~df1$tdecay_lagged, type = "o")


df2 <- results.table[4000:4500,]

plot(df2$datetime, df2$Ibutton_W, type = "l", lwd = 0.1)
lines(df2$datetime, df2$goodfit*df2$Ibutton_W,col = "blue", lwd= 2,type = "S")
#lines(df2$datetime, df2$activezone,col = "red", lwd = 2, type = "S")
plot(df2$log_normDTi~df2$tdecay_lagged)

df3 <- results.table[1:1000,]
plot(df3$datetime, df3$Ibutton_W, type = "l", lwd = 0.1)
lines(df3$datetime,df3$goodfit*df3$Ibutton_W,col = "blue", lwd= 2,type = "S")
#lines(df3$datetime, df3$activezone,col = "red", lwd = 2, type = "S")

plot(df3$log_normDTi~df3$tdecay_lagged)