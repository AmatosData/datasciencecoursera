# Exploratory Data Analysis
# Course project 1 
# Plot 1

###################### To read and modify the dataset  #####################################
setwd("/Users/Amilcar/Desktop/DataScience/datasciencecoursera/Exploratory Data Analysis")

# To read the househol power consumption dataset
power<- read.table("power_consumption.txt", sep=";", header=TRUE)

# To substract only the dates that followed
power.com<- power[which(power$Date=='1/2/2007' | power$Date=="2/2/2007"),]

power.com$Global_active_power<-as.numeric(as.character(power.com$Global_active_power))


############ Create the plot 1
png(filename = "plot1.png", width = 480, height = 480)
hist(power.com$Global_active_power, col="red", main="Global Active Power", xlab="Global Active Power(killowatts)")
dev.off()




