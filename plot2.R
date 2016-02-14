#plot2.R
# R script is part of week-1 assignemnt submission of exploratory data anlysis course. 
# Script does following thing,
# - Reads hard coded UCI electric metre reading data from UCI ML data set.
# - does basic data cleansing and col rearrangements 
# - plot requested graph. (line graph)

plot2 <- function(){
  df.data <- downloadAndReadFile()
  
  #filter on requried date before processing any further
  df.data <- subset(df.data,grepl("^[12]\\/2\\/2007",df.data$Date))
  
  #transform 
  # - character to numeric for col 3 onwards if they are upgraded by fread
  # - create new field datetime based on 1st col date & 2nd col time 
  df.data <- transform(df.data, 
                      datetime = as.POSIXct(paste(Date,Time),format="%d/%m/%Y %H:%M:%S"),
                      Global_active_power = as.numeric(Global_active_power),
                      Global_reactive_power = as.numeric(Global_reactive_power),
                      Voltage = as.numeric(Voltage),
                      Global_intensity = as.numeric(Global_intensity),
                      Sub_metering_1 = as.numeric(Sub_metering_1),
                      Sub_metering_2 = as.numeric(Sub_metering_2),
                      Sub_metering_3 = as.numeric(Sub_metering_3)
                       )
  
  #df.data <- subset(df.data,datetime)
  #remove Date and Time as they are now populated as datetime
  df.data$Date = NULL
  df.data$Time = NULL

  #set png device with rquired specs
  png(width = 480,height = 480, units = "px",filename = "plot2.png")
    
  #draw emplty plot
  plot(df.data$Global_active_power ~ df.data$datetime,
            #col="red",
            ylab="Gobal Active Power (kilowatts)",
            xlab="",
            main="",
            type="n",
            mfrow=c(1,1))
  #draw lines
  lines(df.data$Global_active_power ~ df.data$datetime)

  #reset device
  dev.off()
  
  #df.data
}

#function 
# - download's file in data directory (if not it creates one and then download the zip from URL)
# - unzip donwloaded file into tmp1 directory with overwrite option.
# - reads data from unziped file using fread auto mode 
# - deletes unziped file
# - returns data.
downloadAndReadFile <- function(dfile = "household_power_consumption.zip"){
  unzipDir <- "data/tmp1/" 
  destFile <- paste("data/",dfile,sep="")
  unzipFile <- paste(unzipDir,"household_power_consumption.txt",sep="")
  
  message(sprintf("File downloaded [%s]",destFile))
  message(sprintf("unzipFile file [%s]",unzipFile))
  
  if(!dir.exists("data")){
    message("creating data directory!")
    dir.create("data")
  }
  
  if(!file.exists(destFile)){
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", 
                  destfile = destFile,method="libcurl")
  }
  
  unzip( destFile, exdir=unzipDir, overwrite=TRUE)
  data <- fread(unzipFile,header=TRUE,sep=";",
                colClasses = c("character","character",rep("numeric",7)))
  unlink(unzipFile)
  
  data
}