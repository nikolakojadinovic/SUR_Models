logit_transform <- function(col){
  # this fucntion does logit transformation for all values in a column. It takes a vector parameter col and returns new vector of
      #same length where values are transformed with logit transformation
  
  res_list <- c()
  for (i in 1:length(col)) {
    transformed_value <- log((1-col[i])/col[i])
    res_list <- c(res_list,transformed_value)
  }
  return(res_list)
}

get_p_value <- function(modelobject){
  #this function gets p value from 'lm' class object. Needed to speed up the process of ADF testing later on
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
  
}


set_trend <- function(series) {
  trend <- seq(1,length(series))
  return(trend)
  
}

sw_test <- function(series){
  #this function represents Stock_Watson test and it regresses it's argument series to a trend variable 
  #to check wether or not trend should be included in ADF test (explained in more detail
  #in adf_test.R script)
  trend <- set_trend(series)
  return(lm(trend ~ series))
}


difference <- function(series, n){
  #function that returns nth difference of a series. If n is given as 0, it will return series itself.
  if (n == 0) {
    return(series)
  } else{
    return(diff(series, n))
  }
}


library('urca')

adf_test_results <- function(dataset){
  #this function takes a dataset as an argument, creates and empty list adf_results where it wraps adf test results for each
  #variable in the dataset. Results are kept in a list 'adf_results' which is subsettable so for example adf_results[[1]] would
  #return ur.df class object whose attributes represent different stuff about adf test, such as for example critical values, test_statistic
  #t values for lags etc... They are all accessible with adf_results[[1]]@<attribute name>
  
 
  adf_results <- c()
  
  
  for (i in 2:ncol(dataset)) {
    ser <- dataset[,i]
    sw_res <- sw_test(ser)
    p_value <- get_p_value(sw_res)

      if (isTRUE(p_value < 0.05)) {
        #both constant and trend should be included in adf test, specified in ur.df type argument as 'trend'
        include <- "trend"
        adf_res <- ur.df(as.numeric(dataset[,i]), type = include, selectlags = "AIC")
      } else {
        #only constant is included in adf test, specifed in ur.df type argument as 'drift'
        include <- "drift"
        adf_res <- ur.df(as.numeric(dataset[,i]), type = include, selectlags = "AIC")
      }
      adf_results <- c(adf_results, adf_res)
      
    }
    
  
  return(adf_results)
  
}  
  
pp_test_results <- function(dataset){
    #this function takes a dataset as an argument, creates and empty list adf_results where it wraps adf test results for each
    #variable in the dataset. Results are kept in a list 'adf_results' which is subsettable so for example adf_results[[1]] would
    #return ur.df class object whose attributes represent different stuff about adf test, such as for example critical values, test_statistic
    #t values for lags etc... They are all accessible with adf_results[[1]]@<attribute name>
    
    
    pp_results <- c()
    
    
    for (i in 2:ncol(dataset)) {
      ser <- dataset[,i]
      sw_res <- sw_test(ser)
      p_value <- get_p_value(sw_res)
      
      if (isTRUE(p_value < 0.05)) {
        #both constant and trend should be included in adf test, specified in ur.df type argument as 'trend'
        model <- "trend"
        pp_res <- ur.pp(as.numeric(dataset[,i]), type = "Z-tau", model = model)
      } else {
        #only constant is included in adf test, specifed in ur.df type argument as 'drift'
        model <- "constant"
        pp_res <- ur.pp(as.numeric(dataset[,i]), type = "Z-tau", model = model )
      }
      pp_results <- c(pp_results, pp_res)
      
    }
    
    
    return(pp_results)
  
}

is_stationary <- function(adf_res){
  #this function takes list of class ur.df objects, each representing ADF test results for each variable of a dataset, 
  #and returns a FALSA for each non-stationary serie and TRUE for each stationary serie.
  if (adf_res@cval[1,2] < adf_res@teststat[1]) {
    return (FALSE)
  } else {
    return (TRUE)
  }
}

pp_is_stationary <- function(pp_res){
  #similar as is_stationary function, just done for Phillips_Perron test instead of ADF test
  if (pp_res@cval[2] < pp_res@teststat) {
    return (FALSE)
  } else {
    return (TRUE)
  }
}

series_differentiator <- function(dataset, n){
  #this function takes a dataset and returns a dataset of all series in it differenced with order n
  new_dataset <-c()
  for (i in 2:length(dataset)) {
    new_series <- difference(as.numeric(dataset[,i]), n)
    new_dataset <-cbind(new_dataset, new_series)
  }
  return(data.frame(new_dataset))
}




