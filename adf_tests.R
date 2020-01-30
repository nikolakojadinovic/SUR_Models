#In ADF testing, first thing that needs to be done is to decide which version of ADF test is going to be used. By version, I mean does
#it have to include constant only, trend only or both constant and trend in it's underlying regression which ADF test consists of.
#That is done by Stock-Watson test, which hasn't got it's own function in R or pretty much any other software that I worked with so far (except EViews)
#so I will write a function called sw_test that will do that for this purpose. SW test consists of regressing CONSTANT ONLY to a 1 period lagged series.
#It's equivalent to regressing a trend variable, which is just values 1,2,3... to a original unlagged series. If that regression turns out to be significant,
# (if the p_value <0.05), we should then include trend in ADF regression when testing for unit root.



##################################### ADF TESTING FOR UNIT ROOT ########################################


#The following procedure could've been done in a more efficient manner, but I wanted to illustrate the process.

#1st iteration - We do adf test on the level by applying function adf_test_results and we get a list o adf test results in 'adf_results_1_iter
    #-we dan declare empty list, 'diagnostic_list_1_iter' which will be filled with booleans indicating which variable in the dataset
    #is stationary (indicated by TRUE) and which is non-stationary (indicated by FALSE). The process is repeated untill we get all TRUE values in
    #a diagnostic list

adf_results_1_iter <- adf_test_results(data)
diagnostic_list_1_iter <- c()
for (res in adf_results_1_iter) {
  diagnostic_list <- c(diagnostic_list, is_stationary(res))
}

#2nd iteration - ADF test on 1st difference - ALL NONSTATIONARY
data_1st_diff <- series_differentiator(data, 1)
colnames(data_1st_diff) <- columnnames
adf_results_2_iter <- adf_test_results(data_1st_diff)
diagnostic_list_2_iter <- c()
for (res in adf_results_2_iter) {
  diagnostic_list_2_iter <- c(diagnostic_list_2_iter, is_stationary(res))
}

#3rd iteration - ADF test on 2nd difference - ALL NONSTATIONARY
data_2st_diff <- series_differentiator(data, 2)
colnames(data_2st_diff) <- columnnames
adf_results_3_iter <- adf_test_results(data_2st_diff)
diagnostic_list_3_iter <- c()
for (res in adf_results_3_iter) {
  diagnostic_list_3_iter <- c(diagnostic_list_3_iter, is_stationary(res))
}

#4th iteration - ADF test on 3rd difference - ALL STATIONARY
data_3rd_diff <- series_differentiator(data, 3)
colnames(data_3rd_diff) <- columnnames
adf_results_4_iter <- adf_test_results(data_3rd_diff)
diagnostic_list_4_iter <- c()
for (res in adf_results_4_iter) {
  diagnostic_list_4_iter <- c(diagnostic_list_4_iter, is_stationary(res))
}
