##  Exploratory Data Analysis - exdata-013  Project 1 - Plot 1

##  April 2015 RJ Christensen

##  Project 1 - Read in Household Power Consumption Data Feb 1-2, 2007
##  Construct Plot 1 and save as plot1.png
##  Data URL  --  https://d396qusza40orc.cloudfront.net/
##  exdata%2Fdata%2Fhousehold_power_consumption.zip

## data saved to working directory (wd)
## wd:  "C:/Users/Renee/Documents/Coursera/Exploratory Data Analysis/Data"

## specify classes to make read.table run faster. Read in first two columnes
##      (Date and Time) as "character" and the last 7 columns as "numeric"

plot1 <- function() {
        
        classes <- c(rep("character", 2), rep("numeric", 7))

        power_data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";",
                        colClasses = classes, na.strings = "?")

        ##  date column - convert to date
        power_data$Date <- as.Date(power_data$Date, "%d/%m/%Y")
        
        ## extract data for 01 FEB 2007 and 02 FEB 207
        data_ext1 <- grep("2007-02-01", power_data[,1], fixed = TRUE)
        data_ext2 <- grep("2007-02-02", power_data[,1], fixed = TRUE)
        data_extract <- c(data_ext1, data_ext2)
        power_data <- power_data[data_extract, ]

        ## Assume data collected in France time zone is "Europe/Berlin"
        ## Need to join date and time together
        power_data$Date <- paste(power_data$Date, power_data$Time, sep = " ")

        ## turn column into date/time format
        power_data$Date <- strptime(power_data$Date, format = "%Y-%m-%d %H:%M:%S", 
                            tz = "Europe/Berlin")

        ## Create and save Plot 1 - Histogram of Global Active Power
        png(file = "plot1.png")
        with(power_data, hist(power_data$Global_active_power, main = "Global Active Power",
                      col = "red", xlab = "Global Active Power (kilowatts)"))
dev.off()
}