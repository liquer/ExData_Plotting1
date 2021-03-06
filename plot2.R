## Prepare for reading the data
colnames <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
findstart <- FALSE
findend <- FALSE
nskip <- 1
data <- NULL
## Search the date ranged from "2007-02-01" to "2007-02-02"
while(!(findstart & findend)){
  data1 <- read.table("household_power_consumption.txt", skip = nskip, col.names = colnames, sep = ";", na.strings = "?", nrows = 1000)
  data1$Date <- as.character(data1$Date)
  flag <- data1$Date == "1/2/2007" | data1$Date == "2/2/2007"
  data2 <- data.frame(data1$Date[flag], data1$Time[flag], data1$Global_active_power[flag])
  if(sum(flag) != 0){
    if(length(data) == 0){
      data <- data2
    }
    else{
      data <- rbind(data, data2)
    }
  }
  if(!findstart){
    if(sum(flag) != 0){
      findstart <- TRUE
    }
  }
  else if(!findend){
    if(sum(flag) == 0){
      findend <- TRUE
    }
  }
  nskip <- nskip + 1000
  if(nskip >= 2075260){
    findstart <- TRUE
    findend <-TRUE
  }
}
names(data) <- c("Date", "Time", "Global_active_power")
## Release the memory
rm("data1")
rm("data2")
rm("colnames")
rm("flag")
rm("nskip")
rm("findstart")
rm("findend")
## Plot the graph
png(filename = "plot2.png", width = 480, height = 480)
wday <- strptime(paste(data$Date, data$Time), "%d/%m/%Y %H:%M:%S")
plot(wday, data$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power(kilowatts)")
dev.off()