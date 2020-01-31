original_data_copy <- read_excel('replica1.xlsx')


original_data_copy$npl_consumer <- original_data_copy$npl_consumer/100
original_data_copy$npl_mortgages <- original_data_copy$npl_mortgages/100
original_data_copy$npl_business <- original_data_copy$npl_business/100
original_data_copy$npl_system <- original_data_copy$npl_system/100

#Now we need to generate random data to start Monte Carlo simulation with. **footnote 17, page 31 from the paper is what this code does


#Creating dataframes used in SUR estimation to generate covariance matrices from it, and then applying Cholesky decomposition to those.

consumer_SUR_data <- data.frame(cbind(original_data_copy$npl_consumer[-last],
                                      level_predictors))
business_SUR_data <- data.frame(cbind(original_data_copy$npl_business[-last],
                                      level_predictors))
mortgages_SUR_data <- data.frame(cbind(original_data_copy$npl_mortgages[-last],
                                       level_predictors))
system_SUR_data <- data.frame(cbind(original_data_copy$npl_system[-last],
                                    level_predictors))

#Creating Cholesky decomposed covariance matrices
consumer_cov_matrix <- chol(cov(consumer_SUR_data))
business_cov_matrix <- chol(cov(business_SUR_data))
mortgages_cov_matrix <- chol(cov(mortgages_SUR_data))
system_cov_matrix <- chol(cov(system_SUR_data))

