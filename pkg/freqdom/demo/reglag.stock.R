#source("demo/stockdata.fd.R")
library('mvtnorm')

n = dim(X$coef)[2]
ntest = floor(n * 0.2)
n = n - ntest

K = 5
#K = speclagreg.K(t(X$coef[,1:n]),t(Y$coef[,1:n]),freq)
print(K)

#plot.alpha(t(X$coef),1:4)

# Estimate the regressors and check significance
Afull = speclagreg(t(X$coef[,1:n]),t(Y$coef[,1:n]),lags=-10:10,K=K)
  
#R = reglag.boot(t(X$coef[,1:n]),t(Y$coef[,1:n]),Afull,rep=20,plot=TRUE,K=K)
A0 = timedom.trunc(Afull,0)
A = timedom.trunc(Afull,-1:3)
#A = Afull
Alse = lagreg.est(t(X$coef[,1:n]),t(Y$coef[,1:n]),lags=-1:1)

#W = reglag.significance(t(X$coef[,1:n]), t(Y$coef[,1:n]), A, alpha = 0.05, plot=TRUE)

# Lag zero is significant. Let's check errors in practice. First all lags
Yest = A %c% t(X$coef)
Yest0 = A0 %c% t(X$coef)
YestLSE = Alse %c% t(X$coef)

Yest.fd = fd(t(Yest),basis=Y$basis)
Yest0.fd = fd(t(Yest0),basis=Y$basis)
YestLSE.fd = fd(t(YestLSE),basis=Y$basis)


ind = n + 20 + 1:5*21
print(paste("Relative error 0: ", MSE(t(Y$coef[,1:ntest + n]),Yest0[1:ntest + n,]) / MSE(t(Y$coef[,1:ntest + n]),0)))
print(paste("Relative error EX: ", MSE(t(Y$coef[,1:ntest + n]),Yest[1:ntest + n,]) / MSE(t(Y$coef[,1:ntest + n]),0)))
print(paste("Relative error LSE: ", MSE(t(Y$coef[,1:ntest + n]),YestLSE[1:ntest + n,]) / MSE(t(Y$coef[,1:ntest + n]),0)))

#ylim = c(-1.5,1.5)
ylim = c(-0.00025,0.00005) 
par(mfrow=c(2,2))

plot(Y[ind],ylim=ylim)
abline(h=0,lty=2)

plot(YestLSE.fd[ind],ylim=ylim)
abline(h=0,lty=2)

plot(Yest.fd[ind],ylim=ylim)
abline(h=0,lty=2)

plot(Yest0.fd[ind],ylim=ylim)
abline(h=0,lty=2)

par(mfrow=c(1,1))

print(1 - pnorm(TsiegiM(t(X$coef)[1:ntest + n,],t(Y$coef)[1:ntest + n,],Afull,lags=c(-1:3))))
