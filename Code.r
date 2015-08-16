library(rjson)
library(lubridate)
library(dplyr)
library(tidyr)
library(magrittr)

setwd('C:\\Users\\Ziwen.Yu\\Documents\\Class\\OptiRTC\\')

#Constants
height=6 #in
in2ft=1/12
Ft2_2_m3=0.02831685
Po=0.25
arae=1260 #sqft
Volumn=arae*height*in2ft*Ft2_2_m3  #m3

#Read in Data
Jsonfile='https://raw.githubusercontent.com/OptiRTC/Product-Dev-Test/master/GreenRoofStorms.json'

JsonData=Jsonfile %>% 
      readLines() %>%
      paste0() %>%
      fromJSON()

Data=do.call(rbind, JsonData)%>%
      as.data.frame() %>%
      lapply(.,unlist)%>%
      data.frame(.,stringsAsFactors =F) %>%
      mutate(Time_UTC=ymd_hms(timeValue_UTC)) %>%
      arrange(Time_UTC) 

#Clean data
#remove data with year of 0001
Data_Clean=Data %>%
      mutate(Year=year(Time_UTC)) %>%
      filter(Year==2015) %>%
      select(-Year,-timeValue_UTC) %>%
      mutate(ID=as.integer(row.names(.)))

with(Data_Clean,
     plot(Time_UTC,Rain_in)
     )

#---------------Get precipitation events
#select only precipitation hours
# Add previous and post hours to each row
# Calculate the interval previous and post
# Inter-event period =4 hours 
Precip=Data_Clean %>%
      filter(Rain_in>0) %>%
      select(Time_UTC,Rain_in,ID) %>%
      arrange(Time_UTC) %>%
      mutate(Pre=lag(with_tz(Time_UTC,'UTC'),1),
             Post=lead(with_tz(Time_UTC,'UTC'),1))

EndRw=dim(Precip)[1]
Precip$Pre[1]=with_tz(Precip$Time_UTC[1]-hours(10),'UTC')
Precip$Post[EndRw]=with_tz(Precip$Time_UTC[EndRw]+hours(10),'UTC')

Precip %<>%
      mutate(PreInt=Pre %--% Time_UTC,PostInt=Time_UTC %--% Post) %>%
      mutate(PreInt=PreInt/hours(1),PostInt=PostInt/hours(1)) 

#  Get the start and end of a rain period
EventSt=Precip %>%
      filter(PreInt>4) %>%
      select(ID)
EventEd=Precip %>%
      filter(PostInt>4) %>%
      select(ID)
Event=data.frame(Start=EventSt$ID,End=EventEd$ID)
Event %<>%
      mutate(Dur=Data_Clean$Time_UTC[Start] %--% Data_Clean$Time_UTC[End]) %>%
      mutate(Length_min=Dur/minutes(1)+5) %>%
      mutate(EvtID=as.integer(row.names(.))) %>%
      cbind(.,EvtRain_in=mapply(function(x,y) sum(Data_Clean$Rain_in[x:y]),Event$Start,Event$End))

#------------------ Start of moisture change
#Add delta moisture for all probes

Data_Clean %<>%
      mutate(NorthCentral_lag=lag(NorthCentralSoilMoisture_m3_per_m3,1),
             SouthCentral_lag=lag(SouthCentralSoilMoisture_m3_per_m3,1),
             North_lag=lag(NorthSoilMoisture_m3_per_m3,1),
             South_lag=lag(SouthSoilMoisture_m3_per_m3,1)) %>%
      mutate(NorthCentral_Delta=NorthCentralSoilMoisture_m3_per_m3-NorthCentral_lag,
             SouthCentral_Delta=SouthCentralSoilMoisture_m3_per_m3-SouthCentral_lag,
             North_Delta=NorthSoilMoisture_m3_per_m3-North_lag,
             South_Delta=SouthSoilMoisture_m3_per_m3-South_lag)

# observation realted to rain events
Data_Split=apply(Event[,c(1,2,5)],1,function(x) Data_Clean %>% filter(ID>=x[1],ID<=x[2]) %>% mutate(EvtID=x[3],EvtSt=x[1]))
Event_related=lapply(Data_Split,function(x) x$ID)
Event_related=unlist(Event_related)
Event_unrelated=Data_Clean[ !(Data_Clean$ID %in% Event_related), ]

# Get threshold increase of moistures to determine 
# the reaction to rain
Moist_threshold=data.frame(NorthCentral=max(Event_unrelated$NorthCentral_Delta[!is.na(Event_unrelated$NorthCentral_Delta)])
                           ,SouthCentral=max(Event_unrelated$SouthCentral_Delta[!is.na(Event_unrelated$SouthCentral_Delta)])
                           ,North=max(Event_unrelated$North_Delta[!is.na(Event_unrelated$North_Delta)])
                           ,South=max(Event_unrelated$South_Delta[!is.na(Event_unrelated$South_Delta)]))

#Reaction lag time min
React_lag=function(x)
{
      #NorthCentral
      NorthCentral_React_lag_min=x %>%
            filter(NorthCentral_Delta>Moist_threshold$NorthCentral) %>%
            mutate(React_lag_min=(ID-EvtSt)*5) %>%
            select(React_lag_min) %>%
            min()
      
      #SouthCentral
      SouthCentral_React_lag_min=x %>%
            filter(SouthCentral_Delta>Moist_threshold$SouthCentral) %>%
            mutate(React_lag_min=(ID-EvtSt)*5) %>%
            select(React_lag_min) %>%
            min()
      
      #North
      North_React_lag_min=x %>%
            filter(North_Delta>Moist_threshold$North) %>%
            mutate(React_lag_min=(ID-EvtSt)*5) %>%
            select(React_lag_min) %>%
            min()
      
      #South
      South_React_lag_min=x %>%
            filter(South_Delta>Moist_threshold$South) %>%
            mutate(React_lag_min=(ID-EvtSt)*5) %>%
            select(React_lag_min) %>%
            min()
      
      data.frame(NorthCentral_React_lag_min,
                 SouthCentral_React_lag_min,
                 North_React_lag_min,
                 South_React_lag_min)
}

React_lag0=function(x)
{
      #NorthCentral
      NorthCentral_React_lag_min=x %>%
            filter(NorthCentral_Delta>0) %>%
            mutate(React_lag_min=(ID-EvtSt)*5) %>%
            select(React_lag_min) %>%
            min()
      
      #SouthCentral
      SouthCentral_React_lag_min=x %>%
            filter(SouthCentral_Delta>0) %>%
            mutate(React_lag_min=(ID-EvtSt)*5) %>%
            select(React_lag_min) %>%
            min()
      
      #North
      North_React_lag_min=x %>%
            filter(North_Delta>0) %>%
            mutate(React_lag_min=(ID-EvtSt)*5) %>%
            select(React_lag_min) %>%
            min()
      
      #South
      South_React_lag_min=x %>%
            filter(South_Delta>0) %>%
            mutate(React_lag_min=(ID-EvtSt)*5) %>%
            select(React_lag_min) %>%
            min()
      
      data.frame(NorthCentral_React_lag_min,
                 SouthCentral_React_lag_min,
                 North_React_lag_min,
                 South_React_lag_min)
}

Event %<>% cbind(do.call(rbind,lapply(Data_Split,React_lag0)))
Event %<>% cbind(do.call(rbind,lapply(Data_Split,React_lag)))

#-------------------- running 15-minute change in stored
RunningSumWater= function(x)
{
      x %>%
            #select valid moisture increase reaction for all probes
            mutate(NorthCentral_trueincrease=NorthCentral_Delta*(NorthCentral_Delta>Moist_threshold$NorthCentral),
                   SouthCentral_trueincrease=SouthCentral_Delta*(SouthCentral_Delta>Moist_threshold$SouthCentral),
                   North_trueincrease=North_Delta*(North_Delta>Moist_threshold$North),
                   South_trueincrease=South_Delta*(South_Delta>Moist_threshold$South)) %>%
            #Average the moisture increase
            mutate(AvgMoist_increase=(NorthCentral_trueincrease+
                                            SouthCentral_trueincrease+
                                            North_trueincrease+
                                            South_trueincrease)/4) %>%
            #Calculate water volumn m3
            mutate(stored_water_m3=AvgMoist_increase*Volumn) %>%
            #Running sum of 15 min
            mutate(stored_water_m3_15min=rollsum(x=stored_water_m3,k=3,align = "right",fill=NA)) %>%
            select(Time_UTC,Rain_in,stored_water_m3_15min)
}

RunningSumWater0= function(x)
{
      x %>%
            #select valid moisture increase reaction for all probes
            mutate(NorthCentral_trueincrease=NorthCentral_Delta*(NorthCentral_Delta>0),
                   SouthCentral_trueincrease=SouthCentral_Delta*(SouthCentral_Delta>0),
                   North_trueincrease=North_Delta*(North_Delta>0),
                   South_trueincrease=South_Delta*(South_Delta>0)) %>%
            #Average the moisture increase
            mutate(AvgMoist_increase=(NorthCentral_trueincrease+
                                            SouthCentral_trueincrease+
                                            North_trueincrease+
                                            South_trueincrease)/4) %>%
            #Calculate water volumn m3
            mutate(stored_water_m3=AvgMoist_increase*Volumn) %>%
            #Running sum of 15 min
            mutate(stored_water_m3_15min=rollsum(x=stored_water_m3,k=3,align = "right",fill=NA)) %>%
            select(Time_UTC,Rain_in,stored_water_m3_15min)
}




x=lapply(Data_Split,RunningSumWater0)

ggplot(data=x[[3]])+
      geom_line(aes(x=Time_UTC,y=stored_water_m3_15min))+
      geom_bar(aes(x=Time_UTC,y=Rain_in),stat="identity")+
      scale_x_datetime(breaks = date_breaks("1 hours"),
                       labels = date_format("%Y-%m-%d"))
