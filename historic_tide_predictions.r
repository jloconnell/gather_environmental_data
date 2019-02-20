##Download 6 min water level predictions from NOAA for station 8675761
#Timezone is either EDT or EST as written, other zones are possible
##Daymark #185, Rockdedundy River Entrance, GA - Station ID: 8675761
#Time is Local standard time.
#STation info:
# Mean Range:  6.93 ft.
# Diurnal Range:	7.52 ft.
# Latitude	31? 22.4' N
# Longitude	81? 20.0' W
# NOAA Chart#:	11510
# Met Site Elevation:	N/A
# Datum mean lower low water

library(stringr); library(tidyverse)

##Tell R where to save the file; below is the directory I created on my computer
setwd("/Path/to/folder/historic_tides")

start<-seq(from =1957, to=2017, by=9)
end<-c(start[1:6]+8, 2017)
for (i in 1:length(start)){
##grab tide data from NOAA
  #local daylight savings time
uri = paste0("http://tidesandcurrents.noaa.gov/api/datagetter?begin_date=", start[i],
             "0101%2000:00&end_date=", end[i],
             "1231%2023:54&station=8675761&product=predictions&datum=mllw&units=metric&time_zone=lst_ldt&application=web_services&format=xml")

#local standard time
#uri = "http://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20130916%2010:00&end_date=20140701%2010:24&station=8675761&product=predictions&datum=mllw&units=metric&time_zone=lst&application=web_services&format=xml"

#create a timezone variable; sets to "EST" for local standard or "EDT" for daylight
tzone<-ifelse (uri=="http://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20130916%2010:00&end_date=20140701%2010:24&station=8675761&product=predictions&datum=mllw&units=metric&time_zone=lst_ldt&application=web_services&format=xml",
      "EDT", "EST")
tides<- readLines(uri)
tides<-tides[grep('pr t=', tides)]
tides<-str_split(tides, ' ')
date<-sapply(tides, "[[", 2)#Grab nth element from items in list 
date<-substr(date, 4, 15)
time<-sapply(tides, "[[", 3)#Grab nth element from items in list 
time<-substr(time, 1, 5)
date_time<-as.POSIXlt(strptime(paste(date, time, sep=" "),"%Y-%m-%d %H:%M"))
water_cm<-sapply(tides, "[[", 5)#Grab nth element from items in list 
water_cm<-as.numeric(substr(water_cm, 4, 8))
timezone<-rep(tzone, length(time))
tides<-data.frame(date, time, date_time, timezone, water_cm)

##write out a copy, properly named
write.csv(tides, paste0("tides_6min_", start[i],"_", end[i], ".csv"), eol="\n", row.names=FALSE)

}


####create combined file with daily summary####
files<-list.files()
files<-files[grep("6min",files)]
tides<-read_csv(files[1])
tides$flood_time<-ifelse(tides$water_cm>1.6,1,0)
tides$date<-as.Date(tides$date)

tidemeans <- tides%>% group_by(date) %>% 
    dplyr::summarise (tide=mean(water_cm, na.rm=T), 
               flood_time=sum(flood_time, na.rm=T))
  
for (i in 2:length(files)){
    tides<-read_csv(files[i])
    tides$flood_time<-ifelse(tides$water_cm>1.3,1,0)
    tides$date<-as.Date(tides$date)
    ####create marshmeans####
    x<- tides%>% group_by(date) %>% 
        dplyr::summarise (tide=mean(water_cm, na.rm=T), 
                          flood_time=sum(flood_time, na.rm=T))
    
    tidemeans<-rbind(tidemeans, x)
}
write_csv(tidemeans, "tide_daily_summary_1957_2017.csv")



##fort pulanski water level data
start<-seq.Date(from =as.Date("1957-01-01"), to=as.Date("2017-10-01"), by="months")
end<-seq.Date(from =as.Date("1957-02-01"), to=as.Date("2017-11-01"), by="months")
end<-end-1
for (i in 1:length(start)){
    ##grab tide data from NOAA
    #local daylight savings time
    uri = paste0("https://www.tidesandcurrents.noaa.gov/api/datagetter?product=hourly_height&application=NOS.COOPS.TAC.WL&begin_date=",
        format(start[i], "%Y%m%d"),"&end_date=",
        format(end[i], "%Y%m%d"), 
        "&datum=MLLW&station=8670870&time_zone=lst_ldt&units=metric&interval=h&format=csv")
    download.file(uri, paste0("waterlevel_fortpulanski_", format(start[i], "%Y%m%d"), 
                              "_", format(end[i], "%Y%m%d"), ".csv"))
}

##process the files into one file and calculate summary variables
files<-list.files()
files<-files[grep("waterlevel",files)]
tides<-read_csv(files[1])
tides$date<-as.Date(tides$`Date Time`)

for (i in 2:length(files)){
    x<-read_csv(files[i])
    num<-nrow(x)
    if(num>1){
        x$date<-as.Date(x$`Date Time`)
        ####create marshmeans####
        tides<-rbind(tides, x)
    }
}
tides$year<-format(tides$date, "%Y")
####create combined file with daily summary####
maxtide <- tides%>% dplyr::group_by(year) %>% 
    dplyr::summarise (maxtide=quantile(`Water Level`, 0.95, na.rm=T))

x<-tides%>% dplyr::group_by(year) %>% 
    dplyr::summarise ( flood_time=mean(flood_time, na.rm=T))
plot(x$year, x$flood_time)

##interesting, not that 65% of maxtide is still an increasing value over time
plot(maxtide$year, maxtide$maxtide-maxtide$maxtide*0.65)

tides<-merge(tides, maxtide, by="year")

##estiamted from marsh flooding script that tides flood marsh 30% at about 65% of maximum height
##assuming that marsh has kept pace with sea level rise and is in the same relative space in the tidal frame
tides$flood_time<-ifelse(tides$`Water Level` >0.65*tides$maxtide,1,0)

tidemeans <- tides%>% dplyr::group_by(date) %>% 
    dplyr::summarise (tide=mean(`Water Level`, na.rm=T), maxtide=max(maxtide, na.rm=T),
                      flood_time=sum(flood_time, na.rm=T))


tidemeans$year<-format(tidemeans$date, "%Y")
table(tidemeans$year)
plot(tidemeans$year, tidemeans$flood_time)

write_csv(tidemeans, "tide_daily_summary_1957_2017.csv")
