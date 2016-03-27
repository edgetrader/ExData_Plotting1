## Exploratory Data Analysis Course Project 1
## by Chan Chee-Foong 
## on 27 Mar 2016

## Install required packages if necessary

if(!is.element('lubridate', installed.packages()[,1])) { 
    install.packages('lubridate')
}

## Load the required libraries

library(lubridate)

## Turn on to set Working directory
## setwd("C:/Users/Win7/Dropbox/GitHub/ExData_Plotting1")

## Function to clean up data by removing records with '?' and 'NA'.  Also to convert all numeric
## columns from character to numeric

CleanData <- function(x) {
    
    for (i in 3:ncol(x)) { x <- x[!is.na(x[,i]),] }     ## Remove records with NA
    for (i in 3:ncol(x)) { x <- x[x[,i] != '?',] }      ## Remove records with ?
    for (i in 3:ncol(x)) { x[,i] <- as.numeric(x[,i]) } ## Set column 3 and beyond to numeric
    x
}

## Download and unzip file if data file does not exist

datadir <- "./exdata_data_household_power_consumption"
datafile <- "household_power_consumption.txt"
zipfile <- "exdata_data_household_power_consumption.zip"

datadirfile <- paste(datadir, datafile, sep="/")
zipdirfile <- paste(datadir, zipfile, sep="/")

if (!file.exists(datadirfile)) {
    dir.create(datadir)
    url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    download.file(url, destfile = zipdirfile, mode = 'wb')
    unzip (zipdirfile, exdir = datadir)
}



## Read in all the data in the download text file and then further subset it based on the dates required
## by the project.  Clean the data and the set the Date to the correct date class using lubridate

myDataFull <- read.table("exdata_data_household_power_consumption/household_power_consumption.txt",
                         sep=";", header = TRUE, stringsAsFactors = FALSE)

myData <- subset(myDataFull, dmy(Date) >= dmy('1 Feb 2007') & dmy(Date) < dmy('3 Feb 2007'))
myData <- CleanData(myData)
myData$Date <- dmy(myData$Date)
myData$DateTime <- ymd_hms(paste(myData$Date, myData$Time))



## Ready to plot.  Set the correct parameters and then print the plot to screen for checking purpose and a same version to 
## a png file as required by the project

par(mfrow = c(1, 1))

## Print to Screen

with(myData, plot(DateTime, Global_active_power, 
                  xlab = '', 
                  ylab = 'Global Active Power (kilowatts)', 
                  type = 'n'))
lines(myData$DateTime, myData$Global_active_power)


## Print to PNG

png(filename = 'plot2.png')

with(myData, plot(DateTime, Global_active_power, 
                  xlab = '', 
                  ylab = 'Global Active Power (kilowatts)', 
                  type = 'n'))
lines(myData$DateTime, myData$Global_active_power)

dev.off()

