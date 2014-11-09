# Exploratory Data Analysis
# Course project 1 
# Plot 4

###################### To read and modify the dataset  #####################################
setwd("/Users/Amilcar/Desktop/DataScience/datasciencecoursera/Exploratory Data Analysis")

# To read the househol power consumption dataset
power<- read.table("power_consumption.txt", sep=";", header=TRUE)

# To substract only the dates that followed
power.com<- power[which(power$Date=='1/2/2007' | power$Date=="2/2/2007"),]

power.com$Global_active_power<-as.numeric(as.character(power.com$Global_active_power))

power.com$Date<-as.Date(power.com$Date, format="%d/%m/%Y") # set v.Date as Date class
Date.Time<-as.POSIXct(paste(power.com$Date, power.com$Time), format="%Y-%m-%d %H:%M:%S")


power.com.tim<- data.frame(power.com, Date.Time)
#### Change class on Variables
power.com.tim$Sub_metering_1<-as.numeric(as.character(power.com.tim$Sub_metering_1))
power.com.tim$Sub_metering_2<-as.numeric(as.character(power.com.tim$Sub_metering_2))
power.com.tim$Sub_metering_3<-as.numeric(as.character(power.com.tim$Sub_metering_3))
power.com.tim$Voltage<-as.numeric(as.character(power.com.tim$Voltage))
power.com.tim$Global_reactive_power<-as.numeric(as.character(power.com.tim$Global_reactive_power))



############ Create the plot 4

png(filename = "plot4.png", width = 480, height = 480)

par(mfrow=c(2,2))

plot(power.com.tim$Date.Time, power.com.tim$Global_active_power, ylab="Global Active Power(killowatts)",xlab="",type="n")
lines(power.com.tim$Date.Time,power.com.tim$Global_active_power)

plot(power.com.tim$Date.Time, power.com.tim$Voltage, ylab="Voltage",xlab="datetime",type="n")
lines(power.com.tim$Date.Time,power.com.tim$Voltage)


plot(power.com.tim$Date.Time, power.com.tim$Sub_metering_1, ylab="Energy Sub Metering",xlab="",type="n")
lines(power.com.tim$Date.Time, power.com.tim$Sub_metering_1)
lines(power.com.tim$Date.Time, power.com.tim$Sub_metering_2, col="red")
lines(power.com.tim$Date.Time, power.com.tim$Sub_metering_3,col="blue")
legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=1,col=c("black","red","blue"))

plot(power.com.tim$Date.Time, power.com.tim$Global_reactive_power, ylab="Global_reactive_power",xlab="datetime",type="n")
lines(power.com.tim$Date.Time,power.com.tim$Global_reactive_power)


dev.off()

