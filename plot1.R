plot1 <- function(){
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

  png(filename = "plot1.png",width = 480, height = 480)

  hist(dat_withDateTime$Global_active_power, col="red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power", bg = 'white')
  
  dev.off()

}



