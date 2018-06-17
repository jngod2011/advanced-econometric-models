estimate_expected_returns_and_covariances_of_returns_based_on_Single_Index_Model <- function(data) {
  
  data_factors <- data[, 9:23]
  names(data_factors) <- c("Date", "sp500ret", "tbill", "chtbill", "term", "yield", "credit", "ExGVT", "ExWGBI", "ExBHY", "ExCOM", "ExFRBI", "INFL",
                           "ChINFL", "INPROD")
  
  Y = data[, 6:8]
  X = data_factors
  
  
  dim(Y)
  dim(X)
  T = nrow(Y)
  N = ncol(Y)
  #==========================================================================
  
  #==========================================================================
  # Estimate Single Index Model model parameters
  #==========================================================================
  # Model: Rit   =   alpha_i   +   beta_i * R_Mt   +   et
  #==========================================================================
  ones <- rep(1,T)
  DM <- cbind(ones,X[,"sp500ret"]) # construct design matrix
  alphas_i <- NULL
  betas_i <- NULL
  sigmas2_i <- NULL
  ehat <- NULL
  for (i in 1:N)
  {
    bhat = solve(t(DM)%*%DM)%*%t(DM)%*%Y[,i]     # estimate model parameters (ai,bi)
    alphas_i[i] = bhat[1]
    betas_i[i] = bhat[2]
    ehat = Y[,i] - DM%*%bhat    # estimate residuals
    sigmas2_i[i] = t(ehat)%*%ehat/(T) # sigmas2_i[i] = t(ehat)%*%ehat/(T-2)
  }
  alphas_i
  betas_i
  sigmas2_i
  
  
  #==========================================================================
  
  #==========================================================================
  # Estimate Expected Returns and Covariances of Returns based on
  # Single Index Model
  #==========================================================================
  # E(Rit) = alpha_i   +   beta_i * E(R_Mt)
  # V(Rit) = (beta_i)^2 * (sigma_M)^2 + (sigma_ie)^2
  # Cov(Rit,Rjt) = (beta_i)*(beta_j)*(sigma_M)^2
  # Cov(Rit,Rjt) = (sigma_M)^2 * B * B' + D
  #==========================================================================
  # Compute Expected Returns
  ExpRet = alphas_i + betas_i * mean(X[, "sp500ret"])
  # Compute Variances of the returns
  VarRet = betas_i^2 * var(X[,"sp500ret"]) + sigmas2_i
  
  
  # Construct Covariance matrix of the Returns (Second way: matrix oriented)
  CovRet2 = var(X[,"sp500ret"]) * betas_i%*%t(betas_i) + diag(sigmas2_i,nrow=length(sigmas2_i))

  return(list(ExpRet, CovRet2))
}
