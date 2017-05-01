plot1 <- function() {
        # Stores the txt file in a dataframe
        hpc <- read.table("./household_power_consumption.txt",header=TRUE,na.strings = "?",sep=";")
        
        # Casts Date to Date type
        hpc$Date <- as.Date(hpc$Date, "%d/%m/%Y")
        # Casts Global_active_power to numeric type
        hpc$Global_active_power <- as.numeric(as.character(hpc$Global_active_power))
        
        # Filters out all the rows with Dates different from 2007-02-01 or 2007-02-02
        hpcs <- subset(hpc, Date>= as.Date("2007-02-01") & Date<=as.Date("2007-02-02"))
        
        # Opens the png device 
        png(file="plot1.png",width=480,height=480)
        par(mfrow = c(1, 1), mar = c(4, 4, 4, 4))
        hist(hpcs$Global_active_power, ylim=c(0,1200),col="red", xlab="Global Active Power(kilowatts)",main="Global Active Power")
        
        # Closes the png device
        dev.off()
}