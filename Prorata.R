Allocate = function(CC, CR, Year){
  # This calculates the pro-rata percentage of a policy lies in a year of interest
  # e.g. a policy starts on 30 jun 2015, then it returns 0.5 for the year 2015 and 0 for 2014
  # CC the policy start dates
  # CR the policy end dates
  # the year of interest
  CCdate = as.Date(as.character(CC),"%Y%m%d")
  CRdate = as.Date(as.character(CR),"%Y%m%d")
  n = length(CRdate)
  Start.date = as.Date(paste(as.character(Year),"0101"), format = "%Y%m%d")
  End.date = as.Date(paste(as.character(Year+1),"0101"), format = "%Y%m%d")
  interval = apply(rbind(as.numeric(apply(rbind(CRdate,rep(End.date,n)),MARGIN = 2,FUN = min)-apply(rbind(CCdate, rep(Start.date,n)),MARGIN = 2,FUN = max))+1,rep(0,n)),MARGIN = 2, FUN = max)/as.numeric(End.date-Start.date)
  return(interval)
}