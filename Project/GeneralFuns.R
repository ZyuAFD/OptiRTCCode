# round time to 5 min step

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
