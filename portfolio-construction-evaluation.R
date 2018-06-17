library(quadprog)

construct_optimal_portfolios_and_evaluate_them <- function(method, portfolio, out_of_sample_months, initial_sample, k, targetreturn, data) {
  # Empty matrices for the Results of MINIMUM VARIANCE PORTFOLIO
  keep_PortRisk_static_MINVAR <- NULL
  keepPortWeights_static_MINVAR <- NULL
  Realized_Ret_static_MINVAR <- NULL
  Cum_Realized_Ret_static_MINVAR <- NULL
  keep_CSR_static_MINVAR <- NULL
  
  # Empty matrices for the Results of MEAN VARIANCE PORTFOLIO
  keep_PortRisk_static_MEANVAR <- NULL
  keepPortWeights_static_MEANVAR <- NULL
  Realized_Ret_static_MEANVAR <- NULL
  Cum_Realized_Ret_static_MEANVAR <- NULL
  keep_CSR_static_MEANVAR <- NULL
  
  # Start the out of sample performance of the models
  keep_meanvec_static <- keep_covmat_static <- NULL
  
  for (i in 1:out_of_sample_months){
    data_analyse <- NULL
    data_analyse <- portfolio[1:(initial_sample+i-1), 2:4]
    
    if (method == "Sample Estimate") {
      #Question 1
      keep_meanvec_static[[i]] <- apply(data_analyse, 2, mean)
      keep_covmat_static[[i]] <- cov(data_analyse)
    }
    else if (method == "SIM"){
      #Question 2
      current_model <- estimate_expected_returns_and_covariances_of_returns_based_on_Single_Index_Model(data[1:(initial_sample+i-1),])
      keep_meanvec_static[[i]] <- current_model[[1]]
      keep_covmat_static[[i]] <- current_model[[2]]
    }
    else if (method == "Multivariate Multiple Regression"){
      #Question 3
      current_model <- estimate_expected_returns_and_covariances_of_returns_based_on_Multivariate_Multiple_Regression_Model(data[1:(initial_sample+i-1),])
      keep_meanvec_static[[i]] <- current_model[[1]]
      keep_covmat_static[[i]] <- current_model[[2]]
    }
    else {
      #Question 4
      current_model <- estimate_expected_returns_and_covariances_of_returns_based_on_Constant_Conditional_Correlation_Model(data[1:(initial_sample+i-1),])
      keep_meanvec_static[[i]] <- current_model[[1]]
      keep_covmat_static[[i]] <- current_model[[2]]
    }
    m_vec <- keep_meanvec_static[[i]]
    cov_mat <- keep_covmat_static[[i]]
    
    # Construct optimal minimum variance portfolios
    # Set matrices with constraints
    D.mat <- 2*cov_mat
    d.vec <- rep(0, k)
    A.mat <- cbind(rep(1,k), diag(k))
    b.vec <- c(1, rep(0,k))
    # Solve the Quadratic Programming Problem
    qp.Result <- solve.QP(Dmat=D.mat, dvec=d.vec, Amat=A.mat, bvec=b.vec, meq=1)
    # x_static_MINVAR are the optimal portfolio weights (k x 1 vector)
    x_static_MINVAR <- as.matrix(round(qp.Result$solution,5), k, 1)
    
    #keep portfolio weights across time
    keepPortWeights_static_MINVAR[[i]] <- x_static_MINVAR
    
    #calculate Out of Sample Returns
    RR_static_MINVAR <- as.matrix(portfolio[initial_sample+i,2:4])%*%x_static_MINVAR;
    Realized_Ret_static_MINVAR[i] <- RR_static_MINVAR;
    
    #calculate Port Risk (portfolio standard deviation)
    PR_static_MINVAR <- sqrt(t(x_static_MINVAR)%*%cov_mat%*%x_static_MINVAR)   
    keep_PortRisk_static_MINVAR[i] <- PR_static_MINVAR
    
    #calculate Conditional Sharp Ratio (CSR)
    CSR_static_MINVAR <- RR_static_MINVAR/PR_static_MINVAR
    keep_CSR_static_MINVAR[i] <- CSR_static_MINVAR
    
    #=====================================================================
    # Find optimal Mean Variance portfolio
    #=====================================================================
    # Set matrices with constraints
    D.mat <- 2*cov_mat
    d.vec <- rep(0, k)
    A.mat = cbind(rep(1,k), m_vec, diag(k))
    b.vec = c(1, targetreturn, rep(0,k))
    # Solve the Quadratic Programming Problem
    qp.Result <- solve.QP(Dmat=D.mat, dvec=d.vec, Amat=A.mat, bvec=b.vec, meq=1)
    # x_static_MEANVAR are the optimal portfolio weights (k x 1 vector)
    x_static_MEANVAR <- as.matrix(round(qp.Result$solution,5), k, 1)
    
    #keep portfolio weights across time
    keepPortWeights_static_MEANVAR[[i]] <- x_static_MEANVAR
    
    #calculate Out of Sample Returns
    RR_static_MEANVAR <- as.matrix(portfolio[initial_sample+i,2:4])%*%x_static_MEANVAR;
    Realized_Ret_static_MEANVAR[i] <- RR_static_MEANVAR;
    
    #calculate Port Risk (portfolio standard deviation)
    PR_static_MEANVAR <- sqrt(t(x_static_MEANVAR)%*%cov_mat%*%x_static_MEANVAR)   
    keep_PortRisk_static_MEANVAR[i] <- PR_static_MEANVAR
    
    #calculate Conditional Sharp Ratio (CSR)
    CSR_static_MEANVAR <- RR_static_MEANVAR/PR_static_MEANVAR
    keep_CSR_static_MEANVAR[i] <- CSR_static_MEANVAR
  }
  
  #calculate Cumulative Returns
  Cum_Realized_Ret_static_MINVAR <- cumsum(Realized_Ret_static_MINVAR)
  Cum_Realized_Ret_static_MEANVAR <- cumsum(Realized_Ret_static_MEANVAR)
  
  MINVAR <- cbind(mean(Realized_Ret_static_MINVAR), 
                  mean(keep_PortRisk_static_MINVAR), 
                  Cum_Realized_Ret_static_MINVAR[out_of_sample_months],
                  mean(keep_CSR_static_MINVAR))
  
  MEANVAR <- cbind(mean(Realized_Ret_static_MEANVAR), 
                   mean(keep_PortRisk_static_MEANVAR), 
                   Cum_Realized_Ret_static_MEANVAR[out_of_sample_months],
                   mean(keep_CSR_static_MEANVAR))
  
  Res <- data.frame(round(rbind(MINVAR, MEANVAR), 4))
  names(Res) <- c("Mean Return", "Volatility", "Cumulative Return", "Conditional Sharpe Ratio")
  row.names(Res) <- c("Minimum Variance", "Mean Variance")
  print(Res)
  
}