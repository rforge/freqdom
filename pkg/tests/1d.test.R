# Parameters
d = 1      # dimension
n = 150    # number of observation
ntest = 100 # test observations (not used for training)

# Generate AR(2) process
P = array(0,c(10,d,d))

P[1:10,1,1] = (10:1/20)

A = timedom(P,0:9)

# generate n + ntest independent random vectors X, the same for noise
X = rar(n + ntest,Psi=matrix(0,d,d),noise=rnorm)

# compute the response
Y = A %c% X
Y = linproc(X,A,noise = function(n){ rnorm(n) * 0.5 })

lags = -9:9

# Estimate using LS method and data-driven dimension choise
B.est = lagreg.est(X[1:n,],Y[1:n,],lags=lags)

# Predict
Y.est = linproc(X,B.est,noise = function(n){ rnorm(n) * 0 })

# in-sample
m1 = MSE(Y[1:n,],Y.est[1:n,])
m2 = MSE(Y[1:n,],0)
print(m1/m2)

# out-sample
m1 = MSE(Y[n + 1:ntest,],Y.est[n + 1:ntest,])
m2 = MSE(Y[n + 1:ntest,],0)
print(m1/m2)

# use spectral estimator
A.est = speclagreg(X,Y,lags=lags,p=25,q=25)

par(mfrow=c(1,1))
plot(c(rep(0,9),P),t='l',ylim=c(0,0.5))
lines(A.est$operators[,1,1],t='l',col=2)
lines(B.est$operators[,1,1],t='l',col=3)

# Predict
Y.est = linproc(X,A.est,noise = function(n){ rnorm(n) * 0 })

# in-sample
m1 = MSE(Y[1:n,],Y.est[1:n,])
m2 = MSE(Y[1:n,],0)
print(m1/m2)

# out-sample
m1 = MSE(Y[n + 1:ntest,],Y.est[n + 1:ntest,])
m2 = MSE(Y[n + 1:ntest,],0)
print(m1/m2)

