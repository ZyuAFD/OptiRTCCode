#function to get extra dimentions for clustering------------------------
Get_Lag_Lead=function(Dt_mdl){
  
    # Data should only contain Time and Data columns
  stopifnot(ncol(Dt_mdl) == 2)
  colnames(Dt_mdl)=c('Time','Data')
    # assume all these scenarios are 50 data points
  for (i in (1:(1*50)))
  {
    varval_for=paste0('(Data-dplyr::lag(Data,',as.character(i),
                      '))/as.numeric((Time-dplyr::lag(Time,',as.character(i),')))')
    Dt_mdl %<>% 
      mutate_(.dots=setNames(list(varval_for), paste0('ford_lag',as.character(i))))
      #mutate_(.dots=setNames(list(paste0('scale(',varval_for,')')), paste0('ford_lag',as.character(i))))
  }
  return(Dt_mdl)
  
}
