library(data.table)
library(lubridate)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(scales)
library(zoo)
library(Coalesce)
library(knitr)


Plot_theme=theme_bw()+
  theme(axis.text=element_text(size=14, 
                               color="grey12", 
                               face="bold"),
        axis.title=element_text(size=18,
                                face="bold"),
        plot.title=element_text(size=18,
                                face="bold"),
        legend.position="bottom",
        legend.text=element_text(size=14, 
                                 color="grey12", 
                                 face="bold"),
        legend.title=element_text(size=14, 
                                  color="grey12", 
                                  face="bold"),
        strip.text.y = element_text(size = 12,
                                    face='bold'),
        strip.text.x = element_text(size = 12,
                                    face='bold'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x= element_text(size=0))

path='C:\\Users\\zy32\\Documents\\Projects\\Opti\\Data\\'
file='golda-meir-(east)-green-roof-more-data2015~2016-04_snd down.csv'

Raw_dt=fread(paste0(path,file),sep=',',header=T)


colnames(Raw_dt)=c('Time_UTC',
                   'South_SoilT_dgF',
                   'AirP_inHg',
                   'Rain_in',
                   'North_SoilT_dgF',
                   'North_SoilM',
                   'AirT_dgF',
                   'South_SoilM',
                   'North_SoilM_est')

Raw_dt %<>% mutate(Time=ymd_hms(Time_UTC))

South_SoilT=Raw_dt %>% select(Time,South_SoilT_dgF)
South_SoilM=Raw_dt %>% select(Time,South_SoilM)
North_SoilT=Raw_dt %>% select(Time,North_SoilT_dgF)
North_SoilM=Raw_dt %>% select(Time,North_SoilM)
North_SoilM_est=Raw_dt %>% select(Time,North_SoilM_est)
AirP=Raw_dt %>% select(Time,AirP_inHg)
AirT=Raw_dt %>% select(Time,AirT_dgF)



file='golda-meir-(east)-green-roof-2014~2015.csv'
Raw_dt=fread(paste0(path,file),sep=',',header=T)

colnames(Raw_dt)=c('Time_UTC',
                   'Rain_in',
                   'SoilM')

Raw_dt %<>% 
  mutate(Time_Stamp=paste0(substr(Time_UTC,1,15),'0:00'))  %>% 
  mutate(Time_Stamp=ymd_hms(Time_Stamp),Time=ymd_hms(Time_UTC))

Rain_in=Raw_dt %>% select(Time,Rain_in)


# Complete Time index ------------------------
AirT %>% 
  arrange(Time) %>% 
  select(Time) %>% 
  summarize(min(Time),max(Time)) -> Dt_rng

colnames(Dt_rng)=c('MinDt','MaxDt')

# round time

Round_5min=function(Datetime)
{
  mins=ceiling(
        difftime(Datetime,
                 as.Date(Datetime),
                 units='mins') %>% 
          as.numeric /5)*5
  

  return(as.POSIXct((as.Date(Datetime)+minutes(mins)), 
                    format = "%Y-%m-%d %H:%M:%S")
         )
      
}


Dt_rng %>% 
  mutate(MinDt=Round_5min(MinDt),
         MaxDt=Round_5min(MaxDt))

#Generate time stamps with regular interval of 5 min
Tm_srs=seq(
  from=Dt_rng$MinDt[1],
  to=Dt_rng$MaxDt[1],
  by=300
) 
rm(Dt_rng)

Dt_mutate=data.frame(
  Time=Tm_srs,
  Rain_in=na.approx(Rain_in$Rain_in,x=Rain_in$Time,xout=Tm_srs,na.rm=F),
  South_SoilT_dgF=na.approx(South_SoilT$South_SoilT_dgF,x=South_SoilT$Time,xout=Tm_srs,na.rm=F),
  South_SoilM=na.approx(South_SoilM$South_SoilM,x=South_SoilM$Time,xout=Tm_srs,na.rm=F),
  North_SoilT_dgF=na.approx(North_SoilT$North_SoilT_dgF,x=North_SoilT$Time,xout=Tm_srs,na.rm=F),
  North_SoilM=na.approx(North_SoilM$North_SoilM,x=North_SoilM$Time,xout=Tm_srs,na.rm=F),
  North_SoilM_est=na.approx(North_SoilM_est$North_SoilM_est,x=North_SoilM_est$Time,xout=Tm_srs,na.rm=F),
  AirP=na.approx(AirP$AirP_inHg,x=AirP$Time,xout=Tm_srs,na.rm=F),
  AirT=na.approx(AirT$AirT_dgF,x=AirT$Time,xout=Tm_srs,na.rm=F)
)
