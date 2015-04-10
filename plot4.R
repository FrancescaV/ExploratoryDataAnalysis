plot4 <- function(){
    #read the data frame
    mydf <-read.csv("household_power_consumption.txt",sep = ';', stringsAsFactors = FALSE,na.strings = "?")
    
    #using tbl_df
    library(dplyr)
    dat <- tbl_df(mydf)
    
    #spring cleaning
    rm("mydf")
    
    #filter the dates I want and store them in two separate data frames
    dat_1<-filter(dat, Date == "1/2/2007")
    dat_2<-filter(dat, Date == "2/2/2007")
    
    #bind the two data frames in one single data frame
    dat <- rbind(dat_1, dat_2)
    
    #use lubridate to handle date/time
    library(lubridate)
    
    #add an additional column for the date/time combination
    dat_withDateTime <- dat %>% mutate(Date_time = paste(Date, Time, sep = " "))
    
    dat_withDateTime <- dat_withDateTime %>% mutate(Date_time = dmy_hms(Date_time))
    
    dat_withDateTime <- dat_withDateTime %>% mutate(Weekday = wday(ymd_hms(Date_time), label = TRUE))


    png(filename = "plot4.png",width = 480, height = 480)
    
    par(mfrow = c(2,2), mar = c(5,5,5,2))
    with(dat_withDateTime, {
        plot(Date_time, Global_active_power, xlab = "", ylab = "Global Active Power", pch = ".")
        lines(Date_time, Global_active_power)
        plot(Date_time, Voltage, xlab = "datetime", ylab = "Voltage", pch = ".")
        lines(Date_time, Voltage)

        plot(Date_time, Sub_metering_1, pch = ".", xlab = "", ylab = "Energy sub metering")
        lines(Date_time, Sub_metering_1)
        points(Date_time, Sub_metering_2, pch = ".", col='red')
        lines(Date_time, Sub_metering_2, col = 'red')
        points(Date_time, Sub_metering_3, pch = ".", col='blue')
        lines(Date_time, Sub_metering_3, col = 'blue')
        legend("topright", lty=1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), bty = "n")
        plot(Date_time, Global_reactive_power, xlab = "datetime", ylab = "Global_reactive_power", pch = ".")
        lines(Date_time, Global_reactive_power)
    })
    dev.off()
    
}



