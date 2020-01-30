testval <- log((1-0.1069)/0.1069)
swtest <- sw_test(data$gdp_index_sa)


library('urca')

testdiff <- ur.df(diffx, type = "drift", selectlags = "AIC")
summary(testdiff)


col1 <- c(1,2,3,4)
col2 <- c(6,3,6,2)

df1 <- data.frame(col1,col2)


testdiff@teststat[1]
testdiff@cval[1,2]

is_stationary(testdiff)


df2 <- series_differentiator(data, 3)


teeeeest <- ur.df(df2$new_series.3, type = "trend", selectlags = "AIC")
teeeeest@cval[1,2] > teeeeest@teststat[1]


df3 <- series_differentiator(data, 4)
colnames(df3) <- columnnames
testina <- ur.df(df3$yt3_mortgages, type = "trend", selectlags = "AIC")
testina@cval[1,2] > testina@teststat[1]

rm(col1,col2,cval_pos,diffx,i,z, model_list,p)
rm(df1, df2)
rm(df3)
rm(res)



rm(test_model, test3, teeeeest, testina, x, y)
rm(testdiff)
rm(swtest)


test <- pp_results_1_iter[[1]]
test@teststat
test@cval[2]


test@cval[2] < test@teststat

testX <- cbind(data$employment_inedx, data$consumer_price_indexipc, data$exchange_rate_index, data$gdp_index_sa, data$rate,data$active_interest_rate)
colnames(testX) <- c("empl", "cpi", "exhange", "gdp", "rate", "active_rate")

testreg1<- lm(data$yt1_consumer ~ testX)
eq1 <- data$yt1_consumer ~ testX
testreg2 <- lm(data$yt2_business ~ testX)
eq2 <- data$yt2_business ~ testX
summary(testreg1)
summary(testreg2)

system <- list(eq1 = eq1, eq2 = eq2)
sur <- systemfit(system, method = "SUR", data = data)
summary(sur)
