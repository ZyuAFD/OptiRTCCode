

path='C:\\Users\\zy32\\Documents\\Projects\\Opti\\Data\\'
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

#find duplicated data
Raw_dt[duplicated(Raw_dt$Time_UTC)]
#remove the duplicated data
Raw_dt %<>% 
  filter(!duplicated(Time_UTC))


Raw_dt %<>% 
  mutate(Time_Stamp=paste0(substr(Time_UTC,1,15),'0:00'))  %>% 
  mutate(Time_Stamp=ymd_hms(Time_Stamp),Time=ymd_hms(Time_UTC))



#question about rain intensity unit
Raw_dt %>% 
  select(Rain_Inten_in_s) %>% 
  
  summary

Raw_dt %>% 
  filter(Time>ymd('2015-10-01'), Time<ymd('2015-10-26'),Rain_Inten_in_s>0) %>% 
  ggplot()+
  geom_point(aes(x=Time,y=SoilM))+
  stat_smooth(aes(x=Time,y=SoilM),n=200)+
  geom_bar(aes(x=Time,y=Rain_Inten_in_s),stat="identity",color='red')




#Get the time series with 10 min interval
Raw_dt %>% 
  filter(!is.na(SoilM)) %>% 
  select(Time_Stamp) %>% 
  summarize(min(Time_Stamp),max(Time_Stamp)) -> Dt_rng

colnames(Dt_rng)=c('MinDt','MaxDt')

#Generate time stamps with regular interval of 10 min
Tm_srs=seq(
  from=Dt_rng$MinDt[1],
  to=Dt_rng$MaxDt[1],
  by=600
) 




library(zoo)
library(Coalesce)


Raw_dt %>% 
  filter(!is.na(SoilM)) %>% 
  zoo->x

autoplot(Vld_SoilM_srs,facet=NULL)

  
Raw_dt %>% 
  filter(!is.na(SoilM)) %>% 
  select(SoilM,Time) %>% 
  arrange(Time)-> Vld_SoilM_dt

Mdl_SoilM_dt=data.frame(SoilM=rep(NA,length(Tm_srs)),
                        Time=Tm_srs)


Vld_SoilM_srs=zoo(Vld_SoilM_dt$SoilM,Vld_SoilM_dt$Time)

# use missing data approxmiting to get the regular time series of 10 min interval
SoilM_srs=na.approx(Vld_SoilM_srs,xout=Tm_srs)

# decompose time series into seasonality, trend and remainder
coredata(SoilM_srs) %>% 
  ts(.,frequency=144) %>% 
  stl(.,s.window='per')->SoilM_srs.stl

#Trend Series
SoilM_srs_tnd=data.frame(SoilM=SoilM_srs.stl$time.series[,'trend'],Time=Tm_srs)
#Seasonality Series
SoilM_srs_snl=data.frame(SoilM=SoilM_srs.stl$time.series[,'seasonal'],Time=Tm_srs)
#Remainder Series
SoilM_srs_rmd=data.frame(SoilM=SoilM_srs.stl$time.series[,'remainder'],Time=Tm_srs)


#Fit daily fluctuation
SoilM_srs_snl %>% 
  mutate(Time1=strftime(Time, format="%H:%M",tz='GMT'),
         Date=strftime(Time, format="%m-%d",tz='GMT')) %>% 
  head(144) %>% 
ggplot(aes(x=Time1,y=SoilM))+
  geom_line(group=1)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


SoilM_srs_snl %>% 
  mutate(Time1=strftime(Time, format="%H:%M",tz='GMT'),
         Date=strftime(Time, format="%m-%d",tz='GMT')) %>% 
  mutate(mdl_x=as.numeric(strftime(Time, format="%H",tz='GMT'))*6+
           as.numeric(strftime(Time, format="%M",tz='GMT'))/10+1) %>% 
  head(144)->Snd_fit_dt

Daily_flux_mdl=lm(data=Snd_fit_dt,
                  SoilM~sin(2*pi*mdl_x/144)+cos(2*pi*mdl_x/144))
plot(Snd_fit_dt$SoilM~Snd_fit_dt$mdl_x)
pred=predict(Daily_flux_mdl
             ,newdata=data.frame(mdl_x=Snd_fit_dt$mdl_x))
lines(Snd_fit_dt$mdl_x,pred,col='red')

#Fit trend 
SoilM_srs_tnd %>% 
  mutate(Time1=strftime(Time, format="%H:%M",tz='GMT'),
         Date=strftime(Time, format="%m-%d",tz='GMT')) %>% 
  mutate(mdl_x=difftime(Time,strftime(Time, format="%Y-01-01",tz='GMT'),units='days')*144+
           as.numeric(strftime(Time, format="%H",tz='GMT'))*6+
           as.numeric(strftime(Time, format="%M",tz='GMT'))/10+1) %>% 
  mutate(mdl_x=as.numeric(mdl_x))->Tnd_fit_dt

plot(data=Tnd_fit_dt,as.numeric(SoilM)~mdl_x)
Tnd_flux_mdl=lm(data=Tnd_fit_dt,
                SoilM~sin(2*pi*mdl_x/52704)+cos(2*pi*mdl_x/52704))
pred=predict(Tnd_flux_mdl,
             newdata=data.frame(mdl_x=Tnd_fit_dt$mdl_x))
lines(Tnd_fit_dt$mdl_x,pred,col='red')

#Get the remainder series
Raw_dt %>% 
  mutate(tnd_mdl_x=difftime(Time,strftime(Time, format="%Y-01-01",tz='GMT'),units='days')*144+
           as.numeric(strftime(Time, format="%H",tz='GMT'))*6+
           as.numeric(strftime(Time, format="%M",tz='GMT'))/10+1,
         snl_mdl_x=as.numeric(strftime(Time, format="%H",tz='GMT'))*6+
           as.numeric(strftime(Time, format="%M",tz='GMT'))/10+1) %>% 
  mutate(tnd_mdl_x=as.numeric(tnd_mdl_x),
         snl_mdl_x=as.numeric(snl_mdl_x))->Raw_dt_mdl


Raw_dt_mdl$Tnd_flux_fitted=predict(Tnd_flux_mdl,
                                     newdata=data.frame(mdl_x=Raw_dt_mdl$tnd_mdl_x))
Raw_dt_mdl$snl_flux_fitted=predict(Daily_flux_mdl,
                                     newdata=data.frame(mdl_x=Raw_dt_mdl$snl_mdl_x))

  Raw_dt_mdl %>% 
    mutate(Rmd_srs=SoilM-Tnd_flux_fitted-snl_flux_fitted) %>% 
    filter(!is.na(SoilM )) %>% 
    #top_n(20000,Time) %>% 
    ggplot()+
    geom_point(aes(y=SoilM,x=Time)) +
    geom_point(aes(y=Tnd_flux_fitted,x=Time,color='trend'))+
    geom_line(aes(y=snl_flux_fitted,x=Time,color='seasonal',group=1))+
    geom_line(aes(y=Rmd_srs,x=Time,color='red',group=1))+
    geom_bar(aes(y=Rain_Inten_in_s/10,x=Time,color='rain'),alpha=0.2,stat="identity")+
    ylim(-0.2,0.75)




