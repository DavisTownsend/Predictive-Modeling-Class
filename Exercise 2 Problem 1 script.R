library(mosaic)
library(foreach)
library(lubridate)
library(ggplot2)
library(plyr)
attach(airplane)

#Seems like people tend to fly less during winter months (1-3, 12)
# plot(Dest, Month)

#Setup
#######################################################################################################
#dataframe of frequency of total flights by carrier
airplane = read.csv("ABIA.csv", header=TRUE)
carriercount = count(airplane$UniqueCarrier)
colnames(carriercount) <- c("UniqueCarrier","TotalFlights")


#######################################################################################################
#Delays
#######################################################################################################

#first create new dataframe combining total flight count and total delay time
x = data.frame(airplane$UniqueCarrier, airplane$CarrierDelay)
y = aggregate(cbind(airplane$CarrierDelay) ~ UniqueCarrier, data=x, FUN=sum)
colnames(y) <- c("UniqueCarrier","TotDelayTime")

new = data.frame(carriercount$UniqueCarrier, carriercount$TotalFlights, y$TotDelayTime)

#define colr of columns vector for use in graphs
cols <- c('gold4', 'blue', 'blue', 'blue','blue', 'blue', 'blue','blue', 
          'blue', 'blue','blue', 'blue', 'blue', 'blue', 'blue', 'gold4')

#now plot total delay time per carrier in a graph
newsorted <- new[order(new$y.TotDelayTime), ]
revdfsorted <- newsorted[rev(rownames(newsorted)),]

barplot(revdfsorted$y.TotDelayTime, names.arg=revdfsorted$carriercount.UniqueCarrier, col='blue', ylim=c(0,70000), 
        ylab = 'Total Delay Time (in minutes)', xlab = 'Airlines', main = 'Total Time Delay of Flights by Airline')


barplot(revdfsorted$y.TotDelayTime, names.arg=revdfsorted$carriercount.UniqueCarrier, col=cols, ylim=c(0,70000), 
        ylab = 'Total Delay Time (in minutes)', xlab = 'Airlines', main = 'Total Time Delay of Flights by Airline')

#now calculate average delay time
new["AvgTimeDelay"] <- new$y.TotDelayTime / new$carriercount.TotalFlights

#sort the airlines by delay time percentage
test <- new[order(new$AvgTimeDelay), ]
revdf <- test[rev(rownames(test)),]

#now plot this in a bar graph
barplot(revdf$AvgTimeDelay, names.arg=revdf$carriercount.UniqueCarrier, col='blue', ylim=c(0,10), 
        ylab = 'Average Time Delay (in minutes)', xlab = 'Airlines', main = 'Average Time Delay of Flights by Airline')

#Highlight airlines with most and least avg of delayed flights
barplot(revdf$AvgTimeDelay, names.arg=revdf$carriercount.UniqueCarrier, col=cols, ylim=c(0,10), 
        ylab = 'Average Time Delay (in minutes)', xlab = 'Airlines', main = 'Average Time Delay of Flights by Airline')

## We see that Mesa Airlines, Inc.(YV) has the highest average delay times, while Envoy Air(MQ) has the least average delay time


#######################################################################################################
#Cancellations
#######################################################################################################

#which carrier had most cancellations
x1 = data.frame(airplane$UniqueCarrier, airplane$Cancelled)
y1 = aggregate(cbind(airplane$Cancelled) ~ airplane$UniqueCarrier, data=x1, FUN=sum)
colnames(y1) <- c("UniqueCarrier","TotalCancellations")

#first create new dataframe combining total flight count and total cancellations
new1 = data.frame(carriercount$UniqueCarrier, carriercount$TotalFlights, y1$TotalCancellations)

#now plot total delay time per carrier in a graph
new1sorted <- new1[order(new1$y1.TotalCancellations), ]
revdfsorted1 <- new1sorted[rev(rownames(new1sorted)),]

barplot(revdfsorted1$y1.TotalCancellations, names.arg=revdfsorted1$carriercount.UniqueCarrier, col='blue', ylim=c(0,600), 
        ylab = 'Total Cancellations', xlab = 'Airlines', main = 'Total Cancellations by Airline')

barplot(revdfsorted1$y1.TotalCancellations, names.arg=revdfsorted1$carriercount.UniqueCarrier, col=cols, ylim=c(0,600), 
        ylab = 'Total Cancellations', xlab = 'Airlines', main = 'Total Cancellations by Airline')

#now find and plot average cancellations per carrier
new1["AvgCancellation"] <- new1$y1.TotalCancellations / new$carriercount.TotalFlights

#sort by Average Cancellation
test1 <- new1[order(new1$AvgCancellation), ]
revdf1 <- test1[rev(rownames(test1)),]

#now plot this in a bar graph
barplot(revdf1$AvgCancellation, names.arg=revdf1$carriercount.UniqueCarrier, col='blue', ylim=c(0,.1), 
        ylab = 'Average Cancellation Percent', xlab = 'Airlines', main = 'Average Cancellation Rate by Airline')

#Highlight airlines with most and least avg of delayed flights
barplot(revdf1$AvgCancellation, names.arg=revdf1$carriercount.UniqueCarrier, col=cols, ylim=c(0,.1), 
        ylab = 'Average Cancellation Percent', xlab = 'Airlines', main = 'Average Cancellation Rate by Airline')
#Envoy Air(MQ) had the highest cancellation rate, while Northwest Airlines (NW) had the lowest cancellation rate at 0!