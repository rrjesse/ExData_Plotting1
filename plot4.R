##  Exploratory Data Analysis - exdata-013  Project 1 - Plot 4

##  April 2015 RJ Christensen

##  Project 1 - Read in Household Power Consumption Data Feb 1-2, 2007
##  Construct Plot 4 and save as plot4.png
##  Data URL  --  https://d396qusza40orc.cloudfront.net/
##  exdata%2Fdata%2Fhousehold_power_consumption.zip

## data saved to working directory (wd)
## wd:  "C:/Users/Renee/Documents/Coursera/Exploratory Data Analysis/Data"

## specify classes to make read.table run faster. Read in first two columnes
##      (Date and Time) as "character" and the last 7 columns as "numeric"

plot4 <- function() {
        
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

        
        ## Create and Save Plot 4 - Set of 4 plots
        ## Global Active Power vs Time     Voltage vs time
        ## Energy sub metering vs Time     Global Reactive Power vs Time
        
        png(file = "plot4.png")
        ## set up layout for 2x2 graphs
        par(mfrow = c(2,2))
        par(mar = c(4,4,3,2))
        par(oma = c(2,2,1,1))
        
        ## Global Active Power vs Time
        with(power_data, plot(power_data$Date, power_data$Global_active_power, type = "n",
                              xlab = " " , ylab = "Global Active Power"))
        with(power_data, lines(power_data$Date, power_data$Global_active_power))
        
        ## Voltage vs Time
        with(power_data, plot(power_data$Date, power_data$Voltage, type = "n",
                              xlab = "datetime" , ylab = "Voltage"))
        with(power_data, lines(power_data$Date, power_data$Voltage))
        
        ## Energy sub metering vs Time
        with(power_data, plot(power_data$Date, power_data$Sub_metering_1, type = "n", 
                              xlab = " ", ylab = "Energy sub metering"))
        with(power_data, lines(power_data$Date, power_data$Sub_metering_2, col = "red"))
        with(power_data, lines(power_data$Date, power_data$Sub_metering_3, col = "blue"))
        with(power_data, lines(power_data$Date, power_data$Sub_metering_1))           
        ## Create Legend - bty = "n" means no legend box (bty = "n")
        legend("topright", lty = c(1,1,1), col = c("black", "red", "blue"), bty = "n",
               legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        
        ## Global Reactive Power vs Time
        with(power_data, plot(power_data$Date, power_data$Global_reactive_power, type = "n",
                              xlab = "datetime" , ylab = "Global_reactive_power"))
        with(power_data, lines(power_data$Date, power_data$Global_reactive_power))
       
dev.off()
}