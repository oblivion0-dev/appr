# Function : Calculating statistic criterias for piezometry (and discharge data (coming soon))

f_StatisticCriterias<-function(vecObs,vecSim,type,startTime,endTime){
  
  # Creating sub-vectors associated with the criteria calculation time period
  serieObs <- vecObs[startTime:endTime]
  serieSim <- vecSim[startTime:endTime]
  
  if (type=='piezo'){
    stat_rmse<-rmse(serieObs, serieSim, na.rm = TRUE)
    stat_kge<-KGE(serieObs, serieSim, na.rm = TRUE)
    
  } else if (type=='discharge'){
    
  }
  else{
    printf('Unknown type argument in f_StatisticCriterias() !')
    break
  }

  nobs <- length(serieObs[!is.na(serieObs)])
  moy_obs <- mean(serieObs, na.rm = TRUE)
  moy_sim <- mean(serieSim, na.rm = TRUE)
  
  # Creating function's return list
  statAttributes <- c('n'=nobs,'mobs'=moy_obs,'msim'=moy_sim,'rmse'=stat_rmse,'kge'=stat_kge)
  
  return(statAttributes)
}