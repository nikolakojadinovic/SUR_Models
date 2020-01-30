library('ggplot2')

plot(diffx)
trend1 <- seq(1,212)
plotdata <- cbind(trend1, diffx)

ggplot(data.frame(final_data), aes(x = set_trend(final_data$yt4_system), y = final_data$yt3_mortgages)) + geom_line()
 
