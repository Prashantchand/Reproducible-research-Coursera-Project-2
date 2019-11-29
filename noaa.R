path<-getwd()
filename<-"StormData.csv.bz2"
if(!file.exists(filename)){
  url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(url,destfile = paste(path,filename))
}
file<-"StormData.csv.bz2"
if(!file.exists(file)){
  unzip(filename)
}
library(ggplot2)
library(dplyr)
library(wrapr)
library(lubridate)
stormdata<-read.csv("StormData.csv")
colnames(stormdata)
required_data<-stormdata[,c("BGN_DATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]
required_data<-filter(required_data,(FATALITIES|INJURIES|PROPDMG|CROPDMG)>0)
required_data$PROPDMGEXP<-toupper(required_data$PROPDMGEXP)
required_data$CROPDMGEXP<-toupper(required_data$CROPDMGEXP)
storms <- c("Astronomical Low Tide", "Avalanche", "Blizzard", "Coastal Flood", "Cold/Wind Chill", "Debris Flow", "Dense Fog", "Dense Smoke", "Drought", "Dust Devil", "Dust Storm", "Excessive Heat", "Extreme Cold/Wind Chill", "Flash Flood", "Flood", "Frost/Freeze", "Funnel Cloud", "Freezing Fog", "Hail", "Heat", "Heavy Rain", "Heavy Snow", "High Surf", "High Wind", "Hurricane", "Ice Storm", "Lake-Effect Snow", "Lakeshore Flood", "Lightning", "Marine Hail", "Marine High Wind", "Marine Strong Wind", "Marine Thunderstorm Wind", "Rip Current", "Seiche", "Sleet", "Storm Surge/Tide", "Strong Wind", "Thunderstorm Wind", "Tornado", "Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash", "Waterspout", "Wildfire", "Winter Storm", "Winter Weather")
for (i in 1:length(storms)){
  required_data[regexpr(paste("^", storms[i], "\\b", sep = ""), required_data$EVTYPE, ignore.case=TRUE)>0, "EVENT"] <- storms[i]
}
required_data <- required_data %>% mutate(DATE = as.Date(BGN_DATE, format="%m/%d/%Y")) %>% select(-BGN_DATE) %>% filter(year(DATE) >= 1996)
map<-qc(c("","-","+","?","0","2","3","4","5","6","7","K","M","B")):= qc(c(10^0,10^0,10^1,10^0,10^0,10^2,10^3,10^4,10^5,10^6,10^7,10^3,10^6,10^9))
required_data$CROPDMGEXP<-map[required_data$CROPDMGEXP]
required_data$PROPDMGEXP<-map[required_data$PROPDMGEXP]
required_data[is.na(required_data$CROPDMGEXP),]$CROPDMGEXP<-10^0
required_data[is.na(required_data$PROPDMGEXP),]$PROPDMGEXP<-10^0
required_data$PROPDMGEXP<-as.numeric(as.character(required_data$PROPDMGEXP))
required_data$CROPDMGEXP<-as.numeric(as.character(required_data$CROPDMGEXP))
required_data<-mutate(required_data,propCost=PROPDMG*PROPDMGEXP,cropCost=CROPDMG*CROPDMGEXP)
prop_dmg<-aggregate(propCost~EVENT,data = required_data,FUN = sum)
crop_dmg<-aggregate(cropCost~EVENT,data=required_data,FUN = sum)
total_dmg<-merge(prop_dmg,crop_dmg,by="EVENT")
total_dmg<-mutate(total_dmg,total_loss=propCost+cropCost)
total_dmg<-total_dmg[order(-total_dmg$total_loss),]
total_dmg<-total_dmg[1:10,]
fatalities<-aggregate(FATALITIES~EVENT,data = required_data,FUN = sum)
injuries<-aggregate(INJURIES~EVENT,data=required_data,FUN = sum)

total_injuries<-merge(fatalities,injuries,by="EVENT")
total_injuries<-mutate(total_injuries,total=FATALITIES+INJURIES)
total_injuries<-total_injuries[order(-total_injuries$total),]
total_injuries<-total_injuries[1:10,]
g<-ggplot(total_injuries,aes(x=EVENT,y=total))
g+geom_bar(aes(fill=EVENT),stat="identity")+labs(x="Event Type",y="Total Fatalities",title = "Total Deaths Caused by Natural Calamities in US")+theme(axis.text.x = element_text(angle=45, hjust=1))
g<-ggplot(total_dmg,aes(x=EVENT,y=total_loss/10^9))
g+geom_bar(aes(fill=EVENT),stat="identity")+labs(x="Event Type",y="Total Cost(in billion dollars)",title = "Total Property and Crop Damage Caused by Natural Calamities in US")+theme(axis.text.x = element_text(angle=45, hjust=1))

