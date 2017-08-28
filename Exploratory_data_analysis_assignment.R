## download and unzip
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(fileUrl,destfile="./data/Assignment4.zip")
unzip(zipfile="./data/Assignment4.zip",exdir="./data")

# read files

NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")


# Q1
# plot total emission by each year
# yearly totals
totalPM25ByYear <- tapply(NEI$Emissions, NEI$year, sum)
png("plot1.png", width=480, height=480)
plot(names(totalPM25ByYear), totalPM25ByYear, type="l", xlab = "Year", ylab = "Total PM25 Emissions")
dev.off()


#Q2
#Subset data for Baltimore
NEIBaltimore <- subset(NEI, fips == 24510)
totalPM25ByYearBM <- tapply(NEIBaltimore$Emissions, NEIBaltimore$year, sum)
png("plot2.png", width=480, height=480)
plot(names(totalPM25ByYearBM), totalPM25ByYearBM, type="l", xlab = "Year", ylab = "Total PM25 Emissions for Baltimore")
dev.off()

#Q3
library(ggplot2)
# sum of emission per year per type for Baltimore
NEIBaltimore <- subset(NEI, fips == 24510)
Q3data <- aggregate(Emissions ~ year + type, NEIBaltimore, sum)

# plotting
png("plot3.png", width=480, height=480)
qplot(year, Emissions, data = Q3data, color=type, geom ="line")
dev.off()


#Q4
# subset SCC for coal realted sources only
coal  <- grepl("coal", SCC$Short.Name, ignore.case=T)
SCCCoal <- SCC[coal, ]
# Combine dataframes
NEICoal <- merge(NEI, SCCCoal, by="SCC")
# sum of emission per year for coal
totalCoal <- tapply(NEICoal$Emissions, NEICoal$year, sum)
# plot
png("plot4.png", width=480, height=480)
plot(names(totalCoal), totalCoal, type="l", xlab = "Year", ylab = "Total PM25 Emissions from coal sources")
dev.off()

#Q5
#subset baltimore data
NEIBaltimore <- subset(NEI, fips == 24510)
# subset SCC for motor vehicle sources sources only
mv  <- grepl("vehicle", SCC$Short.Name, ignore.case=T)
SCCMv <- SCC[mv, ]
# Combine dataframes
NEIBaltimoreMv <- merge(NEIBaltimore, SCCMv, by="SCC")
# sum of emission per year for coal
totalMv <- tapply(NEIBaltimoreMv$Emissions, NEIBaltimoreMv$year, sum)
# plot
png("plot5.png", width=480, height=480)
plot(names(totalMv), totalMv, type="l", xlab = "Year", ylab = "Total PM25 Emissions from motor vehicle sources for Baltimore")
dev.off()


#Q6
library(ggplot2)

#subset Baltimore and LA data
NEILA <- subset(NEI, fips == "06037") 
NEIBALT <- subset(NEI, fips == 24510)
# subset SCC for motor vehicle sources sources only
mv  <- grepl("vehicle", SCC$Short.Name, ignore.case=T)
SCCMv <- SCC[mv, ]
# Combine dataframes
NEIBaltMv <- merge(NEIBalt, SCCMv, by="SCC")
NEILAMv <- merge(NEILA, SCCMv, by="SCC")
NEIBaltLAMv <- rbind(NEIBaltMv, NEILAMv)

# sum of emission per year per county for motor vehicle sources 
Q6data <- aggregate(Emissions ~ year + fips, NEIBaltLAMv, sum)
Q6data$county <- ifelse(Q6data$fips == 24510, "Baltimore", "Los Angeles")

#plot
png("plot6.png", width=480, height=480)
g <- ggplot(Q6data, aes(year, Emissions, color = county))
g + geom_line() +
  xlab("Year") +
  ylab(expression("Total PM25 Emissions")) 
dev.off()



