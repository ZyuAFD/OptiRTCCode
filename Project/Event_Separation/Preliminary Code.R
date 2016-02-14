
path='C:\\Users\\ziwen\\Documents\\Opti\\Data\\'
file='20160214-031509.csv'

library(data.table)
library(lubridate)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)

Raw_dt=fread(paste0(path,file),sep=',',header=T)

colnames(Raw_dt)=c('Time_UTC',
                   'Rain_Inten_in_s',
                   'SoilM',
                   'SoilWC_chge_ft3')

Raw_dt %<>% 
  mutate(Time_Stamp=paste0(substr(Time_UTC,1,15),'0:00'))  %>% 
  mutate(Time_Stamp=ymd_hms(Time_Stamp),Time=ymd_hms(Time_UTC))

#Get the time series with 10 min interval
Raw_dt %>% 
  filter(!is.na(SoilM)) %>% 
  select(Time_Stamp) %>% 
  summarize(min(Time_Stamp),max(Time_Stamp)) -> Dt_rng

colnames(Dt_rng)=c('MinDt','MaxDt')
  
Tm_srs=seq(
  from=Dt_rng$MinDt[1],
  to=Dt_rng$MaxDt[1],
  by=600
) 
  



#question about rain intensity unit
Raw_dt %>% 
  select(Rain_Inten_in_s) %>% 
  summary

Raw_dt %>% 
  filter(Time>ymd('2015-10-01'), Time<ymd('2015-10-26')) %>% 
  ggplot()+
  geom_point(aes(x=Time,y=SoilM))+
  stat_smooth(aes(x=Time,y=SoilM),n=200)+
  geom_bar(aes(x=Time,y=Rain_Inten_in_s),stat="identity",color='red')
