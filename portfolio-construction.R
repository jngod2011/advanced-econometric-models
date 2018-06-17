#install.packages("quadprog")
library(quadprog)
library("xlsx")
source("/home/eraikakou/jenny/AUEB_Master/Quarter6/Advanced-Econometric-Models-for-Finance/advanced-econometric-models/econometric_models/single-index-model.R")
source("/home/eraikakou/jenny/AUEB_Master/Quarter6/Advanced-Econometric-Models-for-Finance/advanced-econometric-models/econometric_models/constant-conditional-correlation.R")
source("/home/eraikakou/jenny/AUEB_Master/Quarter6/Advanced-Econometric-Models-for-Finance/advanced-econometric-models/econometric_models/multivariate-multiple-regression-model.R")
source("/home/eraikakou/jenny/AUEB_Master/Quarter6/Advanced-Econometric-Models-for-Finance/advanced-econometric-models/portfolio-construction-evaluation.R")


data <- read.xlsx("/home/eraikakou/jenny/AUEB_Master/Quarter6/Advanced-Econometric-Models-for-Finance/Documents-INF381/Assignment-2018/Data_Advanced_Econometrics_Models_2018.xls", 1, header = FALSE, startRow=6)  # read first sheet
portfolio <- data[, 5:8]

out_of_sample_months <- 24
initial_sample <- nrow(portfolio)-out_of_sample_months;
k <- dim(portfolio)[2] - 1
targetreturn <- 0.009

alternative_models <- c("Sample Estimate", "SIM", "Multivariate Multiple Regression", "Constant Conditional Correlation")
for (i in 1:length(alternative_models))
{
  print(paste0("The estimation of the mean vector and of the covariance matrix are estimated using : ", alternative_models[i]))
  cat("\n")
  construct_optimal_portfolios_and_evaluate_them(alternative_models[i], portfolio, out_of_sample_months, initial_sample, k, targetreturn, data)
  cat("\n")
}
