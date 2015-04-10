plot3 <- function(){
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


    png(filename = "plot3.png",width = 480, height = 480)
    with(dat_withDateTime, plot(Date_time, Sub_metering_1, type = 'n'))
    with(dat_withDateTime, plot(Date_time, Sub_metering_1, pch = ".", xlab = "", ylab = "Energy sub metering"))
    with(dat_withDateTime, lines(Date_time, Sub_metering_1))
    with(dat_withDateTime, points(Date_time, Sub_metering_2, pch = ".", col='red'))
    with(dat_withDateTime, lines(Date_time, Sub_metering_2, col = 'red'))
    with(dat_withDateTime, points(Date_time, Sub_metering_3, pch = ".", col='blue'))
    with(dat_withDateTime, lines(Date_time, Sub_metering_3, col = 'blue'))
    legend("topright", lty=1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    dev.off()
    
}



