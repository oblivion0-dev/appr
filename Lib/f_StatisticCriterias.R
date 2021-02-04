# Function : Calculating statistic criterias for piezometry and discharge data

f_StatisticCriterias<-function(vecObs,vecSim,type,startTime,endTime){
  
  # Creating sub-vectors associated with the criteria calculation time period
  serieObs <- vecObs[startTime:endTime]
  serieSim <- vecSim[startTime:endTime]
  
  if (type == 'piezo')
  {
    stat_rmse<-rmse(serieSim, serieObs, na.rm = TRUE)
    stat_kge<-KGE(serieSim, serieObs, na.rm = TRUE)
	stat_ccorr_pearson<-cor(serieSim, serieObs, use="pairwise.complete", method = "pearson")
    stat_mean_err <- me(serieSim, serieObs, na.rm=TRUE)
	stat_rsd <- rSD(serieSim, serieObs, na.rm=TRUE)
  } 
  else if (type=='discharge')
  {
    stat_NSeff<-NSeff(serieSim, serieObs, na.rm = TRUE) #Nash Sutcliffe efficiency
    stat_kge<-KGE(serieSim, serieObs, na.rm = TRUE)
	stat_ccorr_pearson<-cor(serieSim, serieObs, use="pairwise.complete", method = "pearson")
	stat_logNSE<- 0 #NSE(serieSim, serieObs, na.rm = TRUE, FUN=log2, epsilon="Pushpalatha2012")  # Ln NSE 
  }
  else{
    printf('Unknown type argument in f_StatisticCriterias() !')
    break
  }

  nobs <- length(serieObs[!is.na(serieObs)])
  moy_obs <- mean(serieObs, na.rm = TRUE)
  moy_sim <- mean(serieSim, na.rm = TRUE)
  
  # Creating function's return list
  if (type=='piezo')
  {
    statAttributes <- c('n'=nobs,'mobs'=moy_obs,'msim'=moy_sim,'rmse'=stat_rmse,'kge'=stat_kge,'cpearson'=stat_ccorr_pearson,'me'=stat_mean_err,'rsd'=stat_rsd)
  } else if (type=='discharge')
  {
    statAttributes <- c('n'=nobs,'mobs'=moy_obs,'msim'=moy_sim,'nash'=stat_NSeff,'lnNash'=stat_logNSE,'kge'=stat_kge,'cpearson'=stat_ccorr_pearson)
  }
  else{
    printf('Unknown type argument in f_StatisticCriterias() !')
    break
  }
  
  return(statAttributes)
}
