#install.packages("xlsx")
#install.packages("olsrr")
library("xlsx")
library("olsrr")


# Declare some variables about dataset
numberOfAssets <- 3
data <- read.xlsx("/home/eraikakou/jenny/AUEB_Master/Quarter6/Advanced-Econometric-Models-for-Finance/Documents-INF381/Assignment-2018/Data_Advanced_Econometrics_Models_2018.xls", 1, header = FALSE, startRow=6)  # read first sheet
factors <- data[, 9:23]
names(factors) <- c("Date", "sp500ret", "tbill", "chtbill", "term", "yield", "credit", "ExGVT", "ExWGBI", "ExBHY", "ExCOM", "ExFRBI", "INFL",
                    "ChINFL", "INPROD")

stocks_list <- c("Apple_Returns", "Exxon_Mobil_Returns", "Microsoft_Returns")
three_assets <- data[, 6:8]

stocks <- data[, 5:8]
names(stocks) <- c("Date", "Apple_Returns", "Exxon_Mobil_Returns", "Microsoft_Returns")
assets_and_factors <- merge(stocks, factors, by="Date")


risk_free_rate_mean = mean(factors[, "tbill"])
#==========================================================================
# Calculate the SHARPE RATIO of these three stocks
#==========================================================================
m_vec <- apply(three_assets,2,mean)
var_vec <- apply(three_assets,2,var)
std_vec = sqrt(var_vec)
Sharpe_Ratio=(m_vec - risk_free_rate_mean)/std_vec
# Print the results in Descending Order
Sharpe_Ratio_evaluation_results <- data.frame(stocks_list, Sharpe_Ratio)
names(Sharpe_Ratio_evaluation_results) <- c("Stocks", "Sharpe_Ratio")
Sharpe_Ratio_evaluation_results <- Sharpe_Ratio_evaluation_results[order(-Sharpe_Ratio),]
cat("\n")
print(Sharpe_Ratio_evaluation_results)

#==========================================================================
# Calculate the TREYNOR RATIO of these three stocks
#==========================================================================
# Compute security betas (different for each fund)
betas<-NULL
for (i in 1:numberOfAssets) 
{
  y=three_assets[,i]  
  x=factors[, "sp500ret"]
  yres <- lm(y ~ x)
  beta <- coef(yres)[2]
  betas <- cbind(betas,beta)
}
betas

# Find the top performing fund based on TREYNOR RATIO
m_vec = apply(three_assets,2,mean)
Treynor_Ratio=(m_vec - risk_free_rate_mean)/betas
# Print the results in Descending Order
Treynor_Ratio_vec = as.vector(Treynor_Ratio)
Treynor_Ratio_evaluation_results <- data.frame(stocks_list, Treynor_Ratio_vec)
names(Treynor_Ratio_evaluation_results) <- c("Stocks", "Treynor_Ratio")
Treynor_Ratio_evaluation_results <- Treynor_Ratio_evaluation_results[order(-Treynor_Ratio),]
cat("\n")
print(Treynor_Ratio_evaluation_results)

#==========================================================================
# Calculate the SORTINO RATIO of these three stocks
#==========================================================================
# Find the top performing fund based on SORTINO RATIO
deltas<-NULL
for (i in 1:numberOfAssets) 
{
  y = three_assets[,i]  
  mvalue = mean(y)
  minvec = NULL
  for (j in 1:length(y))
  {
    minvechelp = min(0,(y[j]-mvalue))
    minvec[j] = minvechelp
  }
  delta=sqrt(sum(minvec^2)/length(y))
  deltas <- cbind(deltas,delta)
}
deltas

m_vec = apply(three_assets,2,mean)
Sortino_Ratio = m_vec/deltas
# Print the results in Descending Order
Sortino_Ratio_vec = as.vector(Sortino_Ratio)
Sortino_Ratio_evaluation_results <- data.frame(stocks_list, Sortino_Ratio_vec)
names(Sortino_Ratio_evaluation_results) <- c("Stocks", "Sortino_Ratio")
Sortino_Ratio_evaluation_results <- Sortino_Ratio_evaluation_results[order(-Sortino_Ratio),]
cat("\n")
print(Sortino_Ratio_evaluation_results)


#==========================================================================
#===========================    JENSEN ALPHA   ============================
#==========================================================================
# Compute alphas from the SINGLE INDEX MODEL (different for each fund)
#==========================================================================
alphas<-NULL
for (i in 1:numberOfAssets)
{
  y=three_assets[,i] - factors[, "tbill"]
  x=factors[, "sp500ret"] - factors[, "tbill"]
  yres <- lm(y ~ x)
  alpha <- coef(yres)[1]
  alphas <- cbind(alphas,alpha)
}
alphas

alphas_vec = as.vector(alphas)
jensen_alpha_single_factor_model <- data.frame(stocks_list, alphas_vec)
names(jensen_alpha_single_factor_model) <- c("Stocks", "Jensen_Alpha")
jensen_alpha_single_factor_model <- jensen_alpha_single_factor_model[order(-alphas),]
cat("\n")
print(jensen_alpha_single_factor_model)

# Compute alphas from the Multiple regression models 
# (use stepwise selection in order to select the appropriate explanatory variables)
#==========================================================================
#Multiple Linear Regression
alphas_coef<-NULL
for (i in 1:numberOfAssets) {
  train_data <- assets_and_factors[,c(stocks_list[i], "sp500ret", "tbill", "chtbill", "term", "yield", "credit", "ExGVT", "ExWGBI", "ExBHY", "ExCOM", "ExFRBI", "INFL", "ChINFL", "INPROD")]
  colnames(train_data)[1] <- "y"
  model <- lm(y ~ ., data = train_data) 
  ols_step_both_p(model)
  alpha<-coef(model)[1]
  alphas_coef <- cbind(alphas_coef,alpha)
}

alphas_vec = as.vector(alphas_coef)
jensen_alpha_multiple_regression_model <- data.frame(stocks_list, alphas_vec)
names(jensen_alpha_multiple_regression_model) <- c("Stocks", "Jensen_Alpha")
jensen_alpha_multiple_regression_model <- jensen_alpha_multiple_regression_model[order(-alphas_coef),]

cat("\n")
print(jensen_alpha_multiple_regression_model)

# Compute alphas from the Multiple regression – GARCH
#==========================================================================
library("fGarch")
alphas_fGarch_coef<-NULL
for (i in 1:numberOfAssets) {
  train_data <- assets_and_factors[,c(stocks_list[i], "sp500ret", "tbill", "chtbill", "term", "yield", "credit", "ExGVT", "ExWGBI", "ExBHY", "ExCOM", "ExFRBI", "INFL", "ChINFL", "INPROD")]
  colnames(train_data)[1] <- "y"
  gfit.fg <- garchFit(y ~ garch(1,1), data = as.matrix(sapply(train_data, as.numeric)))
  alpha<-coef(gfit.fg)["alpha1"]
  alphas_fGarch_coef <- cbind(alphas_fGarch_coef,alpha)
}

alphas_fGarch_vec = as.vector(alphas_fGarch_coef)
jensen_alpha_multiple_garch_model <- data.frame(stocks_list, alphas_fGarch_vec)
names(jensen_alpha_multiple_garch_model) <- c("Stocks", "Jensen_Alpha")
jensen_alpha_multiple_garch_model <- jensen_alpha_multiple_garch_model[order(-alphas_fGarch_coef),]

cat("\n")
print(jensen_alpha_multiple_garch_model)

