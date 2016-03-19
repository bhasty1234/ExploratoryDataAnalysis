#Open File
dat<-read.table("household_power_consumption.txt", header = TRUE, sep = ";")

#Inital exploration
head(dat)

#Transform Date and Time class
dat$time2<- strptime(dat$Time,"%H:%M:%S")
dat$Date2 <- as.Date(dat$Date, "%d/%m/%Y")

#Subset by Date
dat2<-dat[dat$Date2 %in% as.Date(c('2007-02-01', '2007-02-02')),]

#Plot 1
png("plot1.png",width=480,height=480,units="px")
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
dat2$Global_active_power<-as.numeric.factor(dat2$Global_active_power)

with(dat2, hist(Global_active_power, col="red", main="Global Active Power",
                xlab="Global Active Power (kilowatts)", ylab="Frequency"))
dev.off()

#Plot 2
#Create datetime variable
png("plot2.png",width=480,height=480,units="px")
dat2$datetime<- paste(dat2$Date2, dat2$Time)
dat2$datetime<- strptime(dat2$datetime, "%Y-%m-%d %H:%M:%S")

with(dat2, plot(datetime, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power"))
dev.off()

#Plot 3
png("plot3.png",width=480,height=480,units="px")
with(dat2, plot(datetime, Sub_metering_1, type = "l", ylab = "Energy sub metering", yaxt="n", xlab = ""))
x<- c(0, 10, 20, 30)
axis(2, at=x,labels=x, col.axis="black")
lines(dat2$datetime, dat2$Sub_metering_2, type = "l", col = "red") 
lines(dat2$datetime, dat2$Sub_metering_3, type = "l", col = "blue") 

legend('topright', names(dat2[ ,6:9])[-1] , 
       lty=1, col=c('black', 'red', 'blue'), cex=.75)
dev.off()


#Plot 4

#set 2x2 container
png("plot4.png",width=480,height=480,units="px")
par(mfrow=c(2,2))
with(dat2, plot(datetime, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power"))
with(dat2, plot(datetime, Voltage, type = "l"))
with(dat2, plot(datetime, Sub_metering_1, type = "l", ylab = "Energy sub metering", yaxt="n", xlab = ""))
x<- c(0, 10, 20, 30)
axis(2, at=x,labels=x, col.axis="black")
lines(dat2$datetime, dat2$Sub_metering_2, type = "l", col = "red") 
lines(dat2$datetime, dat2$Sub_metering_3, type = "l", col = "blue") 

legend('topright', names(dat2[ ,6:9])[-1] , 
       lty=1, col=c('black', 'red', 'blue'), bty="n", cex=.75)
with(dat2, plot(datetime, Global_reactive_power, type = "l"))
dev.off()


