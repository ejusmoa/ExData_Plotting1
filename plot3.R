plot3 <- function() {
        # Sets the lenguage to English
        Sys.setlocale("LC_TIME", "English")
        # Stores the txt file in a dataframe
        hpc <- read.table("./household_power_consumption.txt",header=TRUE,na.strings = "?",sep=";")
        # Casts Date to Date type
        hpc$Date <- as.Date(hpc$Date, "%d/%m/%Y")
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
        png(file="plot3.png",width=480,height=480)
        par(mfrow = c(1, 1), mar = c(3,5,1,1))
        plot(hpcsb$Sub_metering_1~hpcsb$DateTime,type="n",xlab="", ylab= "Energy sub metering")
        legend("topright", col = c("black","red","blue"), legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"),lty=1,xjust=1,yjust=1)
        lines(hpcsb$Sub_metering_1~hpcsb$DateTime,lwd=1)
        lines(hpcsb$Sub_metering_2~hpcsb$DateTime,lwd=1,col="red")
        lines(hpcsb$Sub_metering_3~hpcsb$DateTime,lwd=1,col="blue")
        
        # Closses the png device
        dev.off()
}