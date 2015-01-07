library("fts")
library("tseries")

data(NelPlo)
NelPlo = NelPlo[50:129,]
NelPlo = t(t(NelPlo) - colMeans(NelPlo))
D = dim(NelPlo)
NelPlo = NelPlo[-1,] - NelPlo[-D[1],]
D = dim(NelPlo)
Y = matrix(NelPlo[,1],D[1],1)
X = NelPlo[,c(3,14)]

ntest = floor(n * 0.1)
NelPlo

# Estimate the regressors and check significance
A = speclagreg(X,Y,lags=-5:5)
W = reglag.significance(X, Y, A, alpha = 0.05, plot=TRUE)



