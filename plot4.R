## @arg: dataFile is the path of data file
## @usage: plot4("datafile.txt") where "datafile.txt" is an arbitrary data file 

plot4 <- function(file) {
  ## the file is read into R and assigned to the power data frame
  power <- read.table(file, header=T, sep=";")
  power$Date <- as.Date(power$Date, format="%d/%m/%Y")
  
  ## the two days of interest are subsetted out of the power data frame 
  df <- power[(power$Date=="2007-02-01") | (power$Date=="2007-02-02"),]
  
  ## the Sub_metering, Voltage, Global_active_power, and Global_reactive_power
  ## cols are reformatted as numeric vectors
  df$Sub_metering_1 <- as.numeric(as.character(df$Sub_metering_1))
  df$Sub_metering_2 <- as.numeric(as.character(df$Sub_metering_2))
  df$Sub_metering_3 <- as.numeric(as.character(df$Sub_metering_3))
  df$Global_active_power <- as.numeric(as.character(df$Global_active_power))
  df$Global_reactive_power <- as.numeric(as.character(df$Global_reactive_power))
  df$Voltage <- as.numeric(as.character(df$Voltage))
  
  ## a new column titled "timestamp" is added to df.
  df <- transform(df, 
                  timestamp=as.POSIXct(paste(Date, Time)), 
                  "%d/%m/%Y %H:%M:%S")
  
  par(mfrow=c(2,2))
  
  ##PLOT 1
  plot(df$timestamp,
       df$Global_active_power, 
       type="l", 
       xlab="", 
       ylab="Global Active Power")
  
  ##PLOT 2
  plot(df$timestamp,
       df$Voltage, 
       type="l", 
       xlab="datetime", 
       ylab="Voltage")
  
  ##PLOT 3
  plot(df$timestamp,
       df$Sub_metering_1, 
       type="l", 
       xlab="", 
       ylab="Energy sub metering")
  
  lines(df$timestamp,
        df$Sub_metering_2,
        col="red")
  lines(df$timestamp,
        df$Sub_metering_3,
        col="blue")
  legend("topright", 
         col=c("black","red","blue"), 
         c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),
         lty=c(1,1), bty="n", cex=.5)
  
  ## PLOT 4
  plot(df$timestamp,
       df$Global_reactive_power, 
       type="l", 
       xlab="datetime", 
       ylab="Global_reactive_power")
  
  
  dev.copy(png, file="plot4.png", width=480, height=480)
  dev.off()
}