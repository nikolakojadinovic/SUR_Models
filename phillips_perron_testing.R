#In this script I am doing Phillips-Perron testing for unit root. The procedure will be similar to the one in adf_testing script, I will
#just use the different test.


################ Phillips-Perron testing for unit root ##############################

#1st iteration - Testing for a unit root on level - 5th boolean of pp_diagnostic_list_1_iter is TRUE, meaning that the 5th variable in our dataset,
#showed stationarity, but that is just date index, so We say ALL NONSTATIONARY. I will drop that variable from further testing
pp_results_1_iter <- pp_test_results(data)
pp_diagnostic_list_1_iter <-c()
for(res in pp_results_1_iter){
  pp_diagnostic_list_1_iter <-c(pp_diagnostic_list_1_iter, pp_is_stationary(res))
}


pp_data_1st_diff <- series_differentiator(data,1)
colnames(pp_data_1st_diff) <- columnnames
pp_data_1st_diff[,5] <-NULL

#2nd iteration - Testing for a unit root on first difference - Here we end our unit root testing because all booleans in diagnostic_list evaluate
#to TRUE. Conclusion - ALL SERIES ARE INTEGRATED OF ORDER 1. 


# *********** These conclusions differ from ones given by ADF testing, which show that all series are integrated of order 3
 
#************ but we will keep the conclusions from Phillips-Perron test for the sake of economic sense of the results. The dataset that will be used for
#************ modeling is pp_data_1st_diff.
pp_results_2_iter <- pp_test_results(pp_data_1st_diff)
pp_diagnostic_list_2_iter <-c()
for(res in pp_results_2_iter){
  pp_diagnostic_list_2_iter <-c(pp_diagnostic_list_2_iter, pp_is_stationary(res))
}


#The final dataset for modeling has been stored in final_data
final_data <- data.frame(pp_data_1st_diff)
final_data[,5] <- NULL
final_data$npl_business <- NULL
final_data$npl_system <- NULL
final_data$npl_mortgages <- NULL
final_data$npl_consumer <- NULL
final_data$active_interest_rate <- NULL
final_data$pasive_interest_rate <- NULL
