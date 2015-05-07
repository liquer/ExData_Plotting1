## Prepare for reading the data
colnames <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
findstart <- FALSE
findend <- FALSE
nskip <- 1
data <- NULL
## Search the date ranged from "2007-02-01" to "2007-02-02"
while(!(findstart & findend)){
  data1 <- read.table("household_power_consumption.txt", skip = nskip, col.names = colnames, sep = ";", na.strings = "?", nrows = 1000)
  data1$Date <- as.Date(data1$Date, "%d/%m/%Y")
  flag <- data1$Date == "2007-02-01" | data1$Date == "2007-02-02"
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
png(filename = "plot1.png", width = 480, height = 480)
with(data, hist(Global_active_power, main = "Global Active Power", xlab = "Global Active Power(kilowatts)", col = 498))
dev.off()