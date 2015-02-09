tc = function(X){t(Conj(X))}

#' @export
speclagreg.K = function(X,Y,freq,lags=0:0,p=10,q=10,weights="Bartlett",method = 0){  
  n = dim(X)[1]
  d = dim(X)[2]
  tr = 1:(floor(n * 0.8))
  ts = (length(tr)+1):(n)

  RES = rep(0,d+1)

  for (k in 0:d)
  {
    A = speclagreg(X[tr,],as.matrix(Y[tr,]),freq=freq,K=k,lags=lags,p=p,q=q,weights=weights)
    Yhat = A %c% X[ts,]
    RES[k+1] = sum((Yhat - as.matrix(Y[ts,]))^2)/n
  }
  
  K = which.min(RES) - 1
  K
}