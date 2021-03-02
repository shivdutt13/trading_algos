#Parameters of interest
#No. of periods to use for estimation
estimation_period = 25
#No. of periods to project
project_period = 5
#Hypothetical function: using time only with 3 parameters
#Notice that t is the independent variable, alpha, beta, gamma are the parameters
#I use the Self-Starting Nls Logistic Model. In R, this is called using 
#SSlogis(time, alpha, beta,gamma)

#Load file. Assume that its called Stock1.csv
#Notice that I downloaded the file from GoogleStocks. The excel file format is
# [Date, Open, High, Low, Close, Volume]. The file has headers. I think this is
#the standard format. Please note that the dates are in ascending order
stock1 <- read.csv(file="stock1.csv",header=TRUE,sep=",");
#I am interested in the closing price. Extract this and give this to me in 
#for the desired number of periods
price <-stock1[,5] #closing price is in 5th column/ price <-stock1$Close also works
price <-price[1:min(length(price),estimation_period)]
#Arrange the price in descending order to feed to function
price <-rev(price)
#Note the starting price and normalize the price vector
#price_start <-price[1]
#price <- price - price_start
#Create the time vector and form data matrix
time = 1:length(price)
stock_data = cbind(price, time)
#Estimate coefficients for the function SSlogis 
library(nls2)
regression<- nls(price ~ SSlogis(time, alpha, beta,gamma), data = data.frame(stock_data))
#Extract the coefficients
alpha = coef(regression)[1]
beta = coef(regression)[2]
gamma = coef(regression)[3]
#Project for next time periods
time =1:(estimation_period+project_period)
projection = SSlogis(time, alpha, beta,gamma)
#Graph projection
g_range <- range(min(price,projection), price, projection)
x_range <-range(0,estimation_period+project_period)
plot(price, type="o", col="blue", ylim=g_range, xlim=x_range, ann=false)
lines(projection, type="o", pch=22, lty=2, col="red")
title(main="Projection for Stock1", col.main="red", font.main=4)
title(xlab="Days", col.lab=rgb(0,0.5,0))
title(ylab="Price", col.lab=rgb(0,0.5,0))
legend(1, g_range[2], c("actual","projected"), cex=0.8, 
   col=c("blue","red"), pch=21:22, lty=1:2);

#References
[1] http://www.itc.nl/~rossiter/teach/R/R_CurveFit.pdf
[2] http://stat.ethz.ch/R-manual/R-patched/library/stats/html/nls.html
[3] http://cran.r-project.org/doc/contrib/Fox-Companion/appendix-nonlinear-regression.pdf
[4] http://stat.ethz.ch/R-manual/R-patched/library/stats/html/SSlogis.html
[5] http://www.harding.edu/fmccown/r/
