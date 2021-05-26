## loading, exploring, merging, aggregating and presenting
## financial time series

# setup #######################################################################
library(xts)
library(qrmdata)
library(zoo)

###############################################################################

# Stock prices ################################################################

## DJ stock price data 
data("DJ_const")
str(DJ_const) # data structure

## extract a time period and take the first 10 stocks
DJdata <- DJ_const['2006-12-29/2015-12-31', 1:10]

## use plot for zoo objects to get multiple plots
plot.zoo(DJdata, xlab = "Time", main = "DJ (10 component series)")
X <- diff(log(DJdata))[-1,] # or diff(log(DJdata))[-1,]
head(X)
plot.zoo(X, xlab = "Time", main = "Log-returns of 10 DJ component series")

## Aggregating log returns by summation for each col
## Weekly
X.w <- apply.weekly(X, FUN = colSums)
dim(X.w)
plot.zoo(X.w, type = "h", xlab = "Time", main = "Weekly log-returns")
## Monthly
X.m <- apply.monthly(X, FUN = colSums)
dim(X.m)
plot.zoo(X.m, type = "h", xlab = "Time", main = "Monthly log-returns")
## Quarterly
X.q <- apply.quarterly(X, FUN = colSums)
dim(X.q)
plot.zoo(X.q, type = "h", xlab = "Time", main = "Quarterly log-returns")

## Stock indexes ##############################################################

## load stock index data
data("SP500")
data("FTSE")
data("SMI")
plot.zoo(SP500, xlab = "Time", ylab = "S&P 500")

## merge 3 time series
all <- merge(SP500, FTSE, SMI)
nms <- c("SP 500", "FTSE", "SMI")
colnames(all) <- nms
plot.zoo(all, xlab = "Time", main = "All")

## merge and retain only days where all three time series are available
all.avail <- merge(SP500, FTSE, SMI, all = FALSE)
colnames(all.avail) <- nms
plot.zoo(all.avail, xlab = "Time", main = "All available")

## compute returns
SP500.X <- diff(log(SP500))
FTSE.X <- diff(log(FTSE))
SMI.X <- diff(log(SMI))
X <- merge(SP500.X, FTSE.X, SMI.X, all = FALSE)
colnames(X) <- nms
plot.zoo(X, xlab = "Time", main = "Log-returns")
pairs(as.zoo(X), gap = 0, cex = 0.4)

## Aggregating
## weekly
X.w <- apply.weekly(X, FUN = colSums)
plot.zoo(X.w, xlab = "Time", main = "Weekly log-returns", type = "h")
## monthly
X.m <- apply.monthly(X, FUN = colSums)
plot.zoo(X.m, xlab = "Time", main = "Monthly log-returns", type = "h")

## Exchange rates #############################################################

## load FX rates
data("GBP_USD") 
data("EUR_USD")
data("JPY_USD")
data("CHF_USD")
FX <- merge(GBP_USD, EUR_USD, JPY_USD, CHF_USD)
head(FX)
plot.zoo(FX, xlab = "Time", main = "Exchange rates to USD")
X <- diff(log(FX))
plot.zoo(X, xlab = "Time", main = "Log-returns of exchange rates to USD",
         col = c("black", "royalblue3", "maroon3", "darkorange2"))

## Zero-coupon bond yields ####################################################

## Load zero-coupon bond yield data (in USD)
## note: yield = ((face value / current bond price)^(1/maturity) - 1) * 100%
## as face value = current price * (1 + yield)^maturity
data("ZCB_USD")
dim(ZCB_USD) # 30 dimensional; each dimension is a maturity
head(ZCB_USD)
ZCB <- ZCB_USD['2002-01-02/2011-12-30']
plot.zoo(ZCB, xlab = "Time", main = "Percentage yields")

## compute differences (first row is removed)
X <- na.omit(diff(ZCB))

## pick 3 maturities
X3 <- X[, c(1, 5, 30)]
plot.zoo(X3, xlab = "Time", main = "Differences (3 maturities")
pairs(as.zoo(X3), gap = 0, cex = 0.4)

## plot the corresponding "pseudo-observations" (componentwise scaled ranks)
U3 <- apply(X3, 2, rank) / (ncol(X3) + 1)
pairs(as.zoo(U3), gap = 0, cex = 0.4)
