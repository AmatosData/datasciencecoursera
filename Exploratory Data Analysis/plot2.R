# Exploratory Data Analysis
# Course project 1 
# Plot 2

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




############ Create the plot 2

png(filename = "plot2.png", width = 480, height = 480)

plot(power.com.tim$Date.Time, power.com.tim$Global_active_power, ylab="Global Active Power(killowatts)",xlab="",type="n")
lines(power.com.tim$Date.Time,power.com.tim$Global_active_power)

dev.off()



