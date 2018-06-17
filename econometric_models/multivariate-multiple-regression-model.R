estimate_expected_returns_and_covariances_of_returns_based_on_Multivariate_Multiple_Regression_Model <- function(econometric_data) {
  
  stocks_returns <- econometric_data[, 6:8]
  names(stocks_returns) <- c("Apple_Returns", "Exxon_Mobil_Returns", "Microsoft_Returns")
  
  multi_factors <- econometric_data[, 10:23]
  names(multi_factors) <- c("sp500ret", "tbill", "chtbill", "term", "yield", "credit", "ExGVT", "ExWGBI", "ExBHY", "ExCOM", "ExFRBI", "INFL",
                            "ChINFL", "INPROD")
  
  Y = cbind(stocks_returns$Apple_Returns, stocks_returns$Exxon_Mobil_Returns, stocks_returns$Microsoft_Returns)
  X = cbind(multi_factors$sp500ret, multi_factors$tbill, multi_factors$chtbill, multi_factors$term, multi_factors$yield, multi_factors$credit,
            multi_factors$ExGVT, multi_factors$ExWGBI, multi_factors$ExBHY, 
            multi_factors$ExCOM, multi_factors$ExFRBI, multi_factors$INFL, multi_factors$ChINFL, multi_factors$INPROD)
  
  T = nrow(Y)  # number of data in time
  N = ncol(Y)  # number of assets
  k = ncol(X)  # number of regressors
  
  #==========================================================================
  # Estimate multiple regression model parameters
  #==========================================================================
  # Model: Y   =   X   *   B   +   E
  #      (TxN) = (Txk) * (kxN) + (TxN)
  #==========================================================================
  # Compute design matrix
  DM <- matrix(0,T,k+1)
  ones <- rep(1,T)
  DM <- cbind(ones,X) 
  # Estimate multivariate regression coefficients: B = (X'X)^(-1) * X'*Y
  Bhat = solve(t(DM)%*%DM)%*%t(DM)%*%Y
  # Compute residuals: E = Y - X*B
  Ehat=Y-(DM%*%Bhat)
  # Estimate covariance matrix of errors: Sigma = E'E/T
  Sigma=(t(Ehat)%*%Ehat)/(T)
  ExpRet= (DM%*%Bhat)
  ExpRet <- apply(ExpRet, 2, mean)

  return(list(ExpRet, Sigma))
}