
# Suppose that daily log returns on a stock have a mean of 0.05/year
# and a std dev of 0.23/year. These can be converted to rates
# per trading day by dividing by 253 and sqrt(253) resp.
# What is the probability that the stock will be below
# $950,000 at the close of at least one of the next 45
# trading days?

niter = 1e5 # number of iterations
below = rep(0, niter) # set up storage
set.seed(2009)
for (i in 1:niter)
{
    r = rnorm(45, mean = 0.05/253,
        sd = 0.23/sqrt(253)) # generate random numbers
    logPrice = log(1e6) + cumsum(r)
    minlogP = min(logPrice) # minimum price over next 45 days
    below[i] = as.numeric(minlogP < log(950000))
}
mean(below)

# probability that the hedge fund makes a $100,000 profit
above = rep(0, niter)
for (i in 1:niter)
{
    r = rnorm(100, mean = 0.05/253,
        sd = 0.23/sqrt(253))
    logPrice = log(1e6) + cumsum(r)
    maxlogP = max(logPrice)
    above[i] = as.numeric(maxlogP > log(1100000))
}
mean(above)

# probability the hedge fund will suffer a loss
below = rep(0, niter) # set up storage
for (i in 1:niter)
{
    r = rnorm(100, mean = 0.05/253, # rnorm generates normally distributed random numbers
        sd = 0.23/sqrt(253)) # generate random numbers
    logPrice = log(1e6) + cumsum(r)
    minlogP = min(logPrice) # minimum price over next 45 days
    below[i] = as.numeric(minlogP < log(950000))
}
mean(below)