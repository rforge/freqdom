library("fts")
library("mvtnorm")
source("demo/volatility.fd.R")

# split data into test and training set
n = dim(X$coef)[2]
ntest = floor(n * 0.1)
n = n - ntest
freq = pi * -100:100/100
K = 15 #speclagreg.K(t(X$coef[,1:n]),t(Y$coef[,1:n]),freq,method=1)
print(K)

#plot.alpha(t(X$coef))

# For comparison, plots are shown on the same scale
ylim = c(0,10000)

# estimate regressors and test significance
Afull = speclagreg(t(X$coef[,1:n]),t(Y$coef[,1:n]),lags=-5:5,K=K)
R = reglag.boot(t(X$coef[,1:n]),t(Y$coef[,1:n]),A,rep=5,plot=TRUE,K=K)
A = timedom.trunc(Afull,0:3)
W = reglag.significance(t(X$coef[,1:n]), t(Y$coef[,1:n]), A, alpha = 0.05, plot=TRUE)

# fiter the series
Yest = A %c% t(X$coef)

# check the accuracy on the test set
print(paste("Relative error: ", MSE(t(Y$coef[,1:ntest + n]),Yest[1:ntest + n,]) / MSE(t(Y$coef[,1:ntest + n]),0)))

ind = 2400+1:5*20
ylim = c(-0.02,0.04)
par(mfrow=c(1,2))
Yest.fd = fd(t(Yest),basis=Y$basis)
plot(Y[ind],ylim=ylim)
plot(Yest.fd[ind],ylim=ylim)
par(mfrow=c(1,1))
