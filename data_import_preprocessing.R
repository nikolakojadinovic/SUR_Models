data <- read_excel("replica.xlsx")
 
#Creating variable that represents difference between active and passive interest rate

data$rate <- data$active_interest_rate - data$pasive_interest_rate 

 

#Dividing all npl variables with 100 to squeeze them in a range between 0 and 1 which represents percentages 


data$npl_consumer <- data$npl_consumer/100
data$npl_mortgages <- data$npl_mortgages/100
data$npl_business <- data$npl_business/100
data$npl_system <- data$npl_system/100

#creating logit transform dependent variables for a model. function logit transfrom is defined in funtions.R script.


yt1_consumer <- logit_transform(data$npl_consumer)
yt2_business <-logit_transform(data$npl_business)
yt3_mortgages <- logit_transform(data$npl_mortgages)
yt4_system <- logit_transform(data$npl_system)



#Creating date column for our dataset and appending it, and also appending logit transfromed dependent variables
date_index <- seq(ymd('2001/01/01'), ymd('2018/9/1'), by = "month")

data <- cbind(date_index, yt1_consumer,yt2_business,yt3_mortgages,yt4_system,data)
data2 <-data

#retrieving column names for all columns except the date column, need later in ADF test
data3 <- data
data3[,1] <- NULL
columnnames <- colnames(data3)

