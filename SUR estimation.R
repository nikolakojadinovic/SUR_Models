install.packages("systemfit")
library("systemfit")

#As seen in the paper, the first difference of yt variable, that is non-performing loan (and there are 4 of those) in this study replica, is regressed
#on level series of independent variables. In this replica, level series are found in 'data' dataframe, aswell as on first difference of independent vars.
#For predictors we will use:
# ----------- employment index (expected negative impact -)
#------------ consumer price index (no defined impact 0)
#------------ gdp index sa (expected negative impact with statistical significance -) 
#------------ exchange rate index (expected positive impact +)
#------------ difference between active and passive rate (impact not expected 0)
#------------ active interest rate (expected positive impact +)
#For all of these, we get level series from 'data' dataset, and we are going to create lagged series dataset consisting only of independent variables
#Procedure is the following:
#----step 1) get all the necessary data
#----step 2) prepare the relations for systemfit for all 4 SUR models and SUR fitting
#----step 3) summarize all SUR models





#step 1) getting all necessary data

        #1.1) getting 1st difference of yt, which will be our 4 dependent variables for 4 SUR models

consumer_default_rate <- data$yt1_consumer[-1]
business_default_rate <- data$yt2_business[-1]
mortgages_default_rate <- data$yt3_mortgages[-1]
system_default_rate <- data$yt4_system[-1]

        #1.2) getting lagged series for independent variables for all of our 4 models. I will drop the first observation in order to
              #make it lagged
t1_empl_index <- data$employment_inedx[-1]
t1_cpi <- data$consumer_price_indexipc[-1]
t1_gdp <- data$gdp_index_sa[-1]
t1_exc_rate <- data$exchange_rate_index[-1]
t1_real_rate <-data$rate[-1]
t1_active_rate <- data$active_interest_rate[-1]

      # 1.3) creating cbind of predictors in one period lag
lagged_predictors <- cbind(t1_empl_index, t1_cpi, t1_gdp, t1_exc_rate, t1_real_rate, t1_active_rate)



      # 1.4) getting level series for independent variables for all of 4 models. I dropped the last element to match the month with the predictor.
last <- length(data[[1]])
empl_index <- data$employment_inedx[-last]
cpi <- data$consumer_price_indexipc[-last]
gdp <- data$gdp_index_sa[-last]
exc_rate <- data$exchange_rate_index[-last]
real_rate <- data$rate[-last]
active_rate <-data$active_interest_rate[-last]

      #1.5) creating cbind of predictors in level
level_predictors <- cbind(empl_index, cpi, gdp, exc_rate, real_rate, active_rate)




#step 2) prepare the relations for systemfit for all 4 SUR models and SUR fitting. By relations, I mean equations as arguments for a system argument
#         in systemfit. 



          #2.1)'eq(i,j)', where i = 1,4 and j = 1,2, i being 4 different dependent variables and j being 2 
          #         different sets of predictors (independent variables)
eq11 <- consumer_default_rate ~ level_predictors
eq12 <- consumer_default_rate ~ lagged_predictors

eq21 <- business_default_rate ~ level_predictors
eq22 <- business_default_rate ~ lagged_predictors

eq31 <- mortgages_default_rate ~ level_predictors
eq32 <- mortgages_default_rate ~ lagged_predictors

eq41 <- system_default_rate ~ level_predictors
eq42 <- system_default_rate ~ lagged_predictors



        #2.2) Actual fitting of the SUR model
                    #2.2.1) first we do it for the consumer default rate, representing first economic sector






system_consumer <- list(eq11 = eq11, eq12 = eq12)
consumer_SUR <- systemfit(system_consumer, method = "SUR")
summary(consumer_SUR) #eq1 represent coefficient and other information for independent variables in level, and eq2 represents the same for lagged,
                      #for all SURs this will be the case
    
                    #2.2.2) moving on to the business default rate as dependent variable

system_business <- list(eq21 = eq21, eq22 = eq22)
business_SUR <- systemfit(system_business, method = "SUR")
summary(business_SUR)
                    #2.2.3) now for the mortgages default rate

system_mortgages <- list(eq31 = eq31, eq32 = eq32)
mortgages_SUR <- systemfit(system_mortgages, method = "SUR")
summary(mortgages_SUR)

                    #2.2.3) and finally for the system default rate (don't confuse which 'system' is which)
system_sys <- list(eq41 = eq41, eq42 = eq42)
system_SUR <- systemfit(system_sys, method = "SUR")
summary(system_SUR)


