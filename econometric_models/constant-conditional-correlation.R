# Install package
#install.packages("fGarch")
# Load library for time-varying volatility models
library(fGarch) 

estimate_expected_returns_and_covariances_of_returns_based_on_Constant_Conditional_Correlation_Model <- function(econometric_data) {
  
  stocks_returns <- econometric_data[, 6:8]
  names(stocks_returns) <- c("Apple_Returns", "Exxon_Mobil_Returns", "Microsoft_Returns")
  
  Y = cbind(stocks_returns$Apple_Returns, stocks_returns$Exxon_Mobil_Returns, stocks_returns$Microsoft_Returns)
  
  T = nrow(Y)  # number of data in time
  N = ncol(Y)  # number of assets
  #==========================================================================
  
  #==========================================================================
  # Estimate Univariate GARCH models
  #==========================================================================
  standResid <- matrix(0,T,N)
  ResMod <- matrix(0,T,N)
  CondSdMod <- matrix(0,T,N)
  Yfor <- rep(0,N)
  SDfor <- rep(0,N)
  
  for (i in 1:N)
  { 
    # Estimate univariate GARCH models
    m1garch=garchFit(~garch(1,1),data=Y[,i],trace=F) # trace = F   reduces the summary
    # Compute the estimated residuals
    ResMod[,i] = m1garch@residuals
    CondSdMod[,i] = m1garch@sigma.t    # CondSdMod[,i] = sqrt(m1garch@fit$series$h )
    # Compute forecasts
    Yfor[i] = predict(m1garch,1)$meanForecast
    SDfor[i] = predict(m1garch,1)$standardDeviation
    # Compute Standardised Residuals
    standResid[,i] = ResMod[,i]/CondSdMod[,i]
  }
  
  # Estimate Correlation matrix
  Rmat = cor(standResid)
  # Construct Covariance matrix of the Returns
  CovRet = diag(SDfor)%*%Rmat%*%diag(SDfor)
  #Compute Expected Returns
  ExpRet = Yfor
  #==========================================================================
  # Then, ExpRet and CovRet can be used for constructing optimal portfolios
  # i.e. can be used in a portfolio optimization problem
  #==========================================================================
  
  return(list(ExpRet, CovRet))
}