



# Curation functions --------------------------------
FindGaps=function(Dt,minGap=6)
    #Dt dataframe with time column
    #minGap: min gap of hours  (6 for defaulst)
{
    Dt %>% 
        arrange(Time_Stamp) %>% 
        mutate(lagT=lag(Time_Stamp)) %>% 
        filter(as.numeric(Time_Stamp-lagT,unit='hours')>minGap) ->gaps
    
    if (nrow(gaps)==0) return(data.frame(St=NULL,End=NULL))
    
    gaps %>% 
        select(Time_Stamp,lagT) %>% 
        mutate(Time_Stamp=Round_5min(Time_Stamp),
               lagT=Round_5min(lagT)) %>% 
        rename(St=lagT,End=Time_Stamp) %>% 
        return
}

# Regular 5 min time interval

Regular_5min=function(Dt)
    # Dt: data frame with columns
    #       Time_Stamp
    #       Temp_F
    #       Inst_Rain
    #       SoilM
{
    # Aggregate Rain amoutn on curated 5 min interval
    Dt %>% 
        mutate(Time=Round_5min(Time_Stamp)) %>% 
        group_by(Time) %>% 
        summarise(Inst_Rain=sum(Inst_Rain,na.rm=T))->DtRain
    
    #Find the time range of the dataset
    Dt %>% 
        select(Time_Stamp) %>% 
        summarize(min(Time_Stamp),max(Time_Stamp)) -> Dt_rng
    
    colnames(Dt_rng)=c('MinDt','MaxDt')
    
    #Round to the 5 min time step
    Dt_rng %<>% 
        mutate(MinDt=Round_5min(MinDt),
               MaxDt=Round_5min(MaxDt))
    
    #form up the complete time step series
    Tm_srs=seq(
        from=Dt_rng$MinDt[1],
        to=Dt_rng$MaxDt[1],
        by=300
    ) 
    rm(Dt_rng)
    
    
    data.frame(
        Time=Tm_srs,
        Temp_F=na.approx(Dt$Temp_F,x=Dt$Time_Stamp,xout=Tm_srs,na.rm=F),
        SoilM=na.approx(Dt$SoilM,x=Dt$Time_Stamp,xout=Tm_srs,na.rm=F)) %>% 
        left_join(.,DtRain,by=c('Time'='Time')) ->Dt_Cure
    
    
    # Check gaps in original dataset
    
    gaps=FindGaps(Dt)
    
    if (nrow(gaps)>0) 
    {
        for (i in 1:nrow(gaps))    Dt_Cure %<>% filter(!between(Time,gaps$St[i],gaps$End[i]))    
    }
    
    # data.frame(Time=Tm_srs) %>% 
        # left_join(.,Dt_Cure,by=c('Time'='Time')) %>% return
    
    Dt_Cure %>% return
}


# Get precipitation events

Labeling_Evts= function(dt,IntEvt=360)
    # header: 
    # Trnd, 
    # Evt_n,
    # Inst_Rain
    
    #IntEvt : minutes (6 hours by default)
{
    
    dt %>% 
        filter(!is.na(Inst_Rain),Inst_Rain>0) %>% 
        arrange(Time) %>% 
        mutate(lag=as.numeric(Time-lag(Time),units='mins'),
               lead=as.numeric(Time-lead(Time),units='mins')) %>% 
        mutate(Evt_n=ifelse(lag>IntEvt,1,0)) %>% 
        mutate(Evt_n=ifelse(is.na(Evt_n),1,Evt_n)) %>% 
        mutate(Evt_n_Ed=ifelse(lead(Evt_n)==1,1,0)) %>% 
        select(Time,
               Evt_n) ->Evt_n
    
    dt %<>% 
        left_join(.,Evt_n,by='Time') %>% 
        mutate(Evt_n=ifelse(is.na(Evt_n),0,Evt_n)) %>% 
        mutate(Evt_n=cumsum(Evt_n)) 
    
    Evts=1:max(dt$Evt_n)
    
    
    gaps=FindGaps(dt %>% rename(Time_Stamp=Time))
    
    if (nrow(gaps)>0) 
    {
        for (i in 1:nrow(gaps))
        {
            dt %>% 
                filter(between(Time,gaps$St[i],gaps$End[i])) %>% 
                select(Evt_n) %>% 
                distinct %>% 
                .$Evt_n->Evt_nF
            
            Evts=Evts[!(Evts %in% Evt_nF)]
        }
    }
    
    
    
    dt %>% 
        filter(Evt_n %in% Evts) %>% 
        return
}


# DTW functions--------------------------------------------------------------------------------------------


# Parallel calculate alignment
align_pal=function(Dt,Tslist_val)
{
    library(foreach)
    library(doParallel)
    
    #setup parallel backend to use many processors
    cores=detectCores()
    cl <- makeCluster(cores[1]-2) #not to overload your computer
    registerDoParallel(cl)
    
    
    Dists=foreach(n=  1:length(Tslist_val),
                  .combine=list,
                  .multicombine = TRUE,
                  .packages=c('dtw','dtwclust')) %dopar%
                  {
                      Dist=dtw(zscore(Dt),zscore(Tslist_val[[n]]),keep=TRUE
                               ,open.end = T
                      )$normalizedDistance
                      
                      Dist
                  } 
    
    #stop cluster
    stopCluster(cl)
    
    Dists %>% 
        unlist %>% 
        return
}

# Calculate Distance

EvtDist_Cal=function(Tslist)
{
    Dim=length(Tslist)

    lapply(Tslist,align_pal,Tslist_val=Tslist) %>% 
        unlist(.) %>% 
        matrix(.,nrow=Dim,ncol=Dim) %>% 
        data.frame(.) %>% 
        mutate(Evt=row_number()) %>% 
        gather(.,'Ref','Dist',1:Dim) %>% 
        mutate(Ref=as.numeric(substr(Ref,2,nchar(Ref)))) %>% 
        return

}

# Apriori functions ---------------------------------------------------------------------------------------

Labeling_Evt_Character=function(RefID,Evt_Chara)
{
    
    
    ApplyQuintiles <- function(x) {
        cut(x, breaks=c(quantile(x, probs = seq(0, 1, by = 0.20))), 
            labels=c("0-20","20-40","40-60","60-80","80-100"), include.lowest=TRUE)
    }
    
    
    Ref=NULL
    for (j in 1:nrow(Evt_Chara))
    {
        Ref=rbind(Ref,Evt_Chara[RefID,] %>% rename(Ref_Evt=Evt_n))
    }
    
    data.frame(Evt_n=Evt_Chara$Evt_n,
               Ref_Evt=Ref$Ref_Evt,
               Evt_Chara[,-1]-Ref[,-1]) %>% 
        left_join(Dist.df,by=c('Evt_n'='Evt','Ref_Evt'='Ref')) %>% 
        mutate(DryPd_hr=ApplyQuintiles(DryPd_hr),
               RainPd_hr=ApplyQuintiles(RainPd_hr),
               Dur_hr=ApplyQuintiles(Dur_hr),
               AvgT=ApplyQuintiles(AvgT),
               EvtP=ApplyQuintiles(EvtP),
               Jday=ifelse(abs(Jday)<=45,'in_season','out_season')) %>% 
        filter(Dist>0) %>% 
        mutate(DryPd_hr_diff=paste0('DryPd_hr_diff=',DryPd_hr),
               RainPd_hr_diff=paste0('RainPd_hr_diff=',RainPd_hr),
               Dur_hr_diff=paste0('Dur_hr_diff=',Dur_hr),
               AvgT_diff=paste0('AvgT_diff=',AvgT),
               EvtP_diff=paste0('EvtP_diff=',EvtP),
               Jday_diff=paste0('Jday_diff=',Jday),
               Dist=paste0('Dist=',Dist<0.15)) %>% 
        
        return
}


TrueEvtPtn=function(Evt_Charas)
{
    library(arules)
    library(stringr)
    
    Event_info=lapply(1:max(Evt_Charas$Evt_n),Labeling_Evt_Character, Evt_Chara=Evt_Charas) %>% 
        rbindlist(.) 
    
    
    
    Event_info %>%
        mutate(Items=paste(DryPd_hr_diff,RainPd_hr_diff,Dur_hr_diff,AvgT_diff,EvtP_diff,Jday_diff,Dist,sep=','),
               Patn=paste(AvgT_diff,DryPd_hr_diff,Dur_hr_diff,EvtP_diff,Jday_diff,RainPd_hr_diff)) %>% 
        select(Evt_n,Ref_Evt,Items,Patn,Dist)->Soil_Seq_ts
    
    
    Soil_Pt=as(lapply(Soil_Seq_ts %>%
                          select(Items) %>%
                          unlist %>%
                          as.list,
                      function(x) strsplit(x,',') %>% unlist),'transactions')
    
    
    Pt_feq = apriori(Soil_Pt, parameter=list(support=0.00001, confidence=0.02,target='rules',minlen=7),
                     appearance = list(rhs = c("Dist=TRUE"),
                                       default="lhs"))
    
    
    
    Pt_freq_df=data.frame(
        lhs=sapply(1:Pt_feq@lhs@data@Dim[2],function(x) paste(Pt_feq@lhs@itemInfo$labels[Pt_feq@lhs@data[,x]],collapse = '.*')),
        rhs=sapply(1:Pt_feq@rhs@data@Dim[2],function(x) paste(Pt_feq@rhs@itemInfo$labels[Pt_feq@rhs@data[,x]],collapse = '.*'))) %>% 
        cbind(Pt_feq@quality)
    
    
    # Get high probability pattern
    Pt_freq_df %>% 
        filter(grepl("Jday_diff=in_season",lhs) ) %>% 
        arrange(-confidence) %>% 
        return
}



# Application


Patn_Match=function(dt)
    #Time_Stamp
    #Temp_F
    #Inst_Rain
    #SoilM
{
    Dt_5min_cure=Regular_5min(dt) %>% 
        Labeling_Evts(.)
    
    
    Dt_5min_cure %>%
        select(Evt_n,SoilM) %>% 
        filter(!is.na(SoilM)) %>% 
        group_by(Evt_n) %>% 
        do(vals=.$SoilM) %>% 
        select(vals) %>% 
        .$vals->Tslist
    
    
    Dt_5min_cure %>%
        filter(!is.na(SoilM)) %>% 
        mutate(Rain_Tm=ifelse(Inst_Rain>0,Time,NA)) %>% 
        group_by(Evt_n) %>% 
        mutate(DryPd_hr=as.numeric(max(Time,na.rm=T)-max(Rain_Tm,na.rm=T))/3600,
               RainPd_hr=(max(Rain_Tm,na.rm=T)-as.numeric(min(Time,na.rm=T)))/3600) %>% 
        summarise(DryPd_hr=max(DryPd_hr),
                  RainPd_hr=max(RainPd_hr),
                  Dur_hr=n()/12,
                  St=min(Time),
                  AvgT=mean(Temp_F),
                  EvtP=sum(Inst_Rain,na.rm=T)
        ) %>% 
        filter(Evt_n>0) %>% 
        mutate(Jday=round(as.numeric(St-ymd(year(St)*10000+101),unit='days'))) %>% 
        select(-St)-> Evt_Chara
    
    
    Dist.df=EvtDist_Cal(Tslist)
    TruePtn=TrueEvtPtn(Evt_Chara)
    return(TruePtn)
}

                   
                   
# A tibble: 118,502 × 4
#            Time_Stamp Temp_F Inst_Rain      SoilM
#                <dttm>  <dbl>     <dbl>      <dbl>
#1  2015-08-21 19:34:00 71.100         0        NaN
#2  2015-08-21 19:37:00    NaN       NaN        NaN
#3  2015-08-21 19:39:00 70.585         0        NaN
#4  2015-08-21 19:40:00    NaN       NaN 0.04918874
#5  2015-08-21 19:42:00    NaN       NaN        NaN
#6  2015-08-21 19:44:00 70.070         0        NaN
#7  2015-08-21 19:45:00    NaN       NaN 0.04915797
                   
                   
GoldaMeir_Dt_Feb2017 %>% 
    select(Time_Stamp,Temp_F,Inst_Rain_in,Est_SouthCtr_SoilM) %>% 
    rename(Inst_Rain=Inst_Rain_in,SoilM=Est_SouthCtr_SoilM) %>% 
    filter(Time_Stamp<ymd('2016-1-06'))%>% 
    Patn_Match
