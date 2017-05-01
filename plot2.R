plot2 <- function() {
        # Sets the lenguage to English
        Sys.setlocale("LC_TIME", "English")
        # Stores the txt file in a dataframe
        hpc <- read.table("./household_power_consumption.txt",header=TRUE,na.strings = "?",sep=";")
        # Casts Date to Date type
        hpc$Date <- as.Date(hpc$Date, "%d/%m/%Y")
        # Casts Global_active_power to numeric type
        hpc$Global_active_power <- as.numeric(hpc$Global_active_power)
        
        # Filters out all the rows with Dates different from 2007-02-01 or 2007-02-02
        hpcs <- subset(hpc, Date>= as.Date("2007-02-01") & Date<=as.Date("2007-02-02"))
        
        # Creates and adds a new column combining Date and Time
        hpcsb <- cbind(hpcs,strptime(paste(hpcs$Date,hpcs$Time,sep=" "),format="%Y-%m-%d %H:%M:%S"))
        names(hpcsb)[10] <- "DateTime"
        
        # Opens the png device
        png(file="plot2.png",width=480,height=480)
        par(mfrow = c(1, 1), mar = c(4, 4, 1, 4))
        plot(hpcsb$Global_active_power~hpcsb$DateTime,type="l",xlab="", ylab= "Global Active Power (kilowatts)")
        
        # Closes the png device
        dev.off()
}