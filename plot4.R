plot4 <- function() {
        # Sets the lenguage to English
        Sys.setlocale("LC_TIME", "English")
        # Stores the txt file in a dataframe
        hpc <- read.table("./household_power_consumption.txt",header=TRUE,na.strings = "?",sep=";")
        # Casts Date to Date type
        hpc$Date <- as.Date(hpc$Date, "%d/%m/%Y")
        # Casts Global_active_power to numeric type
        hpc$Global_active_power <- as.numeric(as.character(hpc$Global_active_power))
        # Casts Global_reactive_power to numeric type
        hpc$Global_reactive_power <- as.numeric(as.character(hpc$Global_reactive_power))
        # Casts Voltage to numeric type
        hpc$Voltage<- as.numeric(as.character(hpc$Voltage))
        # Casts Sub_metering_1 to numeric type
        hpc$Sub_metering_1 <- as.numeric(as.character(hpc$Sub_metering_1))
        # Casts Sub_metering_2 to numeric type
        hpc$Sub_metering_2 <- as.numeric(as.character(hpc$Sub_metering_2))
        # Casts Sub_metering_3 to numeric type
        hpc$Sub_metering_3 <- as.numeric(as.character(hpc$Sub_metering_3))
        
        # Filters out all the rows with Dates different from 2007-02-01 or 2007-02-02
        hpcs <- subset(hpc, Date>= as.Date("2007-02-01") & Date<=as.Date("2007-02-02"))
        # Creates and adds a new column combining Date and Time
        hpcsb <- cbind(hpcs,strptime(paste(hpcs$Date,hpcs$Time,sep=" "),format="%Y-%m-%d %H:%M:%S"))
        names(hpcsb)[10] <- "DateTime"
        
        # Opens the png device
        png(file="plot4.png",width=480,height=480)
        par(mfrow = c(2, 2))
        plot(hpcsb$Global_active_power~hpcsb$DateTime,type="n",xlab="", ylab= "Global Active Power")
        lines(hpcsb$Global_active_power~hpcsb$DateTime,lwd=1)
        plot(hpcsb$Voltage~hpcsb$DateTime,type="n",xlab="datetime", ylab= "Voltage")
        lines(hpcsb$Voltage~hpcsb$DateTime,lwd=1)
        plot(hpcsb$Sub_metering_1~hpcsb$DateTime,type="n",xlab="", ylab= "Energy sub metering")
        lines(hpcsb$Sub_metering_1~hpcsb$DateTime,lwd=1)
        lines(hpcsb$Sub_metering_2~hpcsb$DateTime,lwd=1,col="red")
        lines(hpcsb$Sub_metering_3~hpcsb$DateTime,lwd=1,col="blue")
        legend(x="topright", lty=1,lwd=2, bty="n", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3" ))
        plot(hpcsb$Global_reactive_power~hpcsb$DateTime,type="n",xlab="datetime", ylab= "Global_reactive_Power")
        lines(hpcsb$Global_reactive_power~hpcsb$DateTime,lwd=1)
        
        # Closes the png device
        dev.off()
}