## @arg: dataFile is the path of data file
## @usage: plot1("datafile.txt") where "datafile.txt" is an arbitrary data file 

plot1 <- function(dataFile) {
  ## Reading data
  power <- read.table(dataFile, header=T, sep=";")
  power$Date <- as.Date(power$Date, format="%d/%m/%Y")
  
  ## the two days of interest are subsetted out of the power data frame 
  df <- power[(power$Date == "2007-02-01") | (power$Date == "2007-02-02"),]
  
  df$Global_active_power <- as.numeric(as.character(df$Global_active_power))
  
  ## Plotting histogram
  hist(df$Global_active_power, 
       main = paste("Global Active Power"), 
       col="red", 
       xlab="Global Active Power (kilowatts)")
  
  ## Creating PNG file
  dev.copy(png, 
           file="plot1.png", 
           width=480, 
           height=480)
  dev.off()
}
