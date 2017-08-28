library(datasets)
data(cars)
with(cars, plot(speed, dist))


library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)


hist(airquality$Ozone)

boxplot(Ozone ~ Month, airquality)


## download and unzip
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(fileUrl,destfile="./data/Course4_Project1.zip")
unzip(zipfile="./data/Course4_Project1.zip",exdir="./data")

## read the file data and subset the required rowss
allData <- read.table("./data/household_power_consumption.txt", header=TRUE, sep=";", stringsAsFactors=FALSE, dec=".")
subSetData <- data[allData$Date %in% c("1/2/2007","2/2/2007") ,]

## create plot 1
png("plot1.png", width=480, height=480)
hist(as.numeric(subSetData$Global_active_power), col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
dev.off()


## create plot 2
dateTime <- strptime(paste(subSetData$Date, subSetData$Time, sep=" "), "%d/%m/%Y %H:%M:%S")
png("plot2.png", width=480, height=480)
plot(dateTime , as.numeric(subSetData$Global_active_power), type="l", xlab="", ylab="Global Active Power (kilowatts)")
dev.off()


## create plot 3
dateTime <- strptime(paste(subSetData$Date, subSetData$Time, sep=" "), "%d/%m/%Y %H:%M:%S")
png("plot3.png", width=480, height=480)
plot(dateTime, as.numeric(subSetData$Sub_metering_1), type="n", ylab="Energy Submetering", xlab="")
lines(dateTime, as.numeric(subSetData$Sub_metering_1), type="l", col="black")
lines(dateTime, as.numeric(subSetData$Sub_metering_2), type="l", col="red")
lines(dateTime, as.numeric(subSetData$Sub_metering_3), type="l", col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, col=c("black", "red", "blue"))
dev.off()


## create plot 4

png("plot4.png", width=480, height=480)
par(mfrow = c(2, 2)) 

dateTime <- strptime(paste(subSetData$Date, subSetData$Time, sep=" "), "%d/%m/%Y %H:%M:%S")

## Row 1, column 1
plot(dateTime, as.numeric(subSetData$Global_active_power), type="l", xlab="", ylab="Global Active Power")
## Row 1, column 2
plot(dateTime, as.numeric(subSetData$Voltage), type="l", xlab="datetime", ylab="Voltage")
## Row 2, column 1
plot(dateTime, as.numeric(subSetData$Sub_metering_1), type="n", ylab="Energy Submetering", xlab="")
lines(dateTime, as.numeric(subSetData$Sub_metering_1), type="l", col="black")
lines(dateTime, as.numeric(subSetData$Sub_metering_2), type="l", col="red")
lines(dateTime, as.numeric(subSetData$Sub_metering_3), type="l", col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, col=c("black", "red", "blue"), bty="n")
## Row 2, column 2
plot(dateTime, as.numeric(subSetData$Global_reactive_power), type="l", xlab="datetime", ylab="Global_reactive_power")

dev.off()


