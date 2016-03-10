## @arg: dataFile is the path of data file
## @usage: plot3("datafile.txt") where "datafile.txt" is an arbitrary data file 

plot3 <- function(dataFile) {
  ## Reading Data
  power <- read.table(dataFile, header=T, sep=";")
  power$Date <- as.Date(power$Date, format="%d/%m/%Y")
  
  ## the two days of interest are subsetted out of the power data frame 
  df <- power[(power$Date=="2007-02-01") | (power$Date=="2007-02-02"),]
  
  df$Sub_metering_1 <- as.numeric(as.character(df$Sub_metering_1))
  df$Sub_metering_2 <- as.numeric(as.character(df$Sub_metering_2))
  df$Sub_metering_3 <- as.numeric(as.character(df$Sub_metering_3))
  
  ## a new column titled "timestamp" is added to df.
  df <- transform(df, timestamp=as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")
  
  plot(df$timestamp,
       df$Sub_metering_1, 
       type="l", 
       xlab="", 
       ylab="Energy sub metering")
  
  lines(df$timestamp,df$Sub_metering_2,col="red")
  lines(df$timestamp,df$Sub_metering_3,col="blue")
  
  legend("topright", 
         col=c("black","red","blue"), 
         c("Sub_metering_1  ", "Sub_metering_2  ", "Sub_metering_3  "),
         lty=1, 
         lwd=2.5)
  
  ## Creating PNG file
  dev.copy(png, file="plot3.png", width=480, height=480)
  dev.off()
}