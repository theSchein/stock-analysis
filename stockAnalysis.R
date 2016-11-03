## Stock Analysis
## Benjamin Scheinberg

require(quantmod)
require(xts)
# If you do not have these packages installed type: 'install.packages("quantmod")'
# & 'install.packages("xts")' into the command line

## Getting Ticker data

stock <- "tsla"  # input the ticker name to analyze
date <- "2016-01-01" # earliest date required for analysis

STOCK<- getSymbols(stock, src= "google",from=c(date),auto.assign = FALSE);
index<- getSymbols("spy", src= "google",from=c(date),auto.assign = FALSE);

## Price Chart
chartSeries(STOCK, subset="2016")
addBBands() # bollingger bands
addMACD() # moving average convergence divergence
# addCCI()

## Basic Measures
close <- STOCK[c("/",date),4]
basicMeasures <-summary(close)
mainTable<- write.csv(basicMeasures) 
# NEEDS WORK. I am trying to write all descriptive statistics to a table
variance <- var(close) # should be written to a table

## Box Plot
boxplot(as.zooreg(close), col = "red", main = c(stock,"Boxplot"))

## Histogram
par(bg = "black")
hist(close,breaks = 6, col = "dark green",border = "red",main = c(stock,'Histogram 2016') 
     , xlab = 'Price', ylab = 'Frequency', col.main='white',col.lab='white', col.axis='white')


## Volatility Chart
# volatlity is graphed with the market as reference
volStock <- volatility(STOCK[c("/",date)], calc= 'close', N= length(STOCK), n=10)
index<- getSymbols("spy", src= "google",from=c(date) )
volIndex <- volatility(SPY[c("/",date)], calc= 'close', N= 52)
par(bg='grey')
plot(as.zoo(volStock),bty='n', yaxt="n", ylab="",xlab="",main="Volatility Graph")
par(new=TRUE)               
plot(as.zoo(volIndex),col=2, bty='n', xaxt="n", yaxt="n", xlab="", ylab="")
axis(4, las=1)
legend("topleft",legend=c("Stock Volatility","S&P Volatility"),col=1:2,lty=1,cex=0.85) 

## Distributions
# to determine the distribution used in further modeling
x <- as.zoo(close)
y <- as.zoo(SPY[c("/",date),4])
par(mfrow=c(1,2),bg='white')
qqnorm(x,main=c(stock,"Q-Q Plot"))
qqline(x)
qqnorm(y, main="Index Q-Q Plot")
qqline(y)


## Correlation
t.test(x,y)
par(mfrow=c(1,1))
plot(x[,1],y[,1],xlab=c(stock,"Price"), ylab="Index Price",main="Correlation with the Market")
abline(lm(y~x), col="red") # regression line (y~x) 
#lines(lowess(x,y), col="blue") # lowess line (x,y)
legend("topleft",legend=c("R^2=",cor(x,y)))


