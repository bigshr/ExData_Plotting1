## @arg: dataFile is the path of data file
## @usage: plot2("datafile.txt") where "datafile.txt" is an arbitrary data file 

plot2 <- function(dataFile) {
  ## Reading data
  power <- read.table(dataFile, header=TRUE, sep=";")
  power$Date <- as.Date(power$Date, format="%d/%m/%Y")
  
  ## the two days of interest are subsetted out of the power data frame 
  df <- power[(power$Date=="2007-02-01") | (power$Date=="2007-02-02"),]
  
  df$Global_active_power <- as.numeric(as.character(df$Global_active_power))
  
  ## a new column titled "timestamp" is added to df.
  df <- transform(df, 
                  timestamp=as.POSIXct(paste(Date, Time)), 
                  "%d/%m/%Y %H:%M:%S")
  
  plot(df$timestamp,
       df$Global_active_power, 
       type="l", 
       xlab="", 
       ylab="Global Active Power (kilowatts)")
  
  ## Creating PNG file
  dev.copy(png, 
           file="plot2.png", 
           width=480, 
           height=480)
  dev.off()
}