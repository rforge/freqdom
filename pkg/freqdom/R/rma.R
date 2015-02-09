#' Moving average
#' @export
rma = function(n, d, A, noise=NULL)
{
  lag = rev(A$lags)[1] - A$lags[1]
  X = rar(n+lag,d=d,Psi = matrix(0,d,d), noise=noise)
  linproc(X[lag + 1:n,], A, noise=function(n){ rep(0,n)})
}

rma.old = function(n, lag=2, d=NULL, noise=NULL)
{
  rma.proc(rar(n+lag-1,d=d,noise=noise))
}

#' @export
rma.proc = function(TS, lag=2){
  n = dim(TS)[1]
  RES = c()
  for (x in 1:(n-lag+1)){
    new = TS[x + 0:(lag-1),]
    RES = rbind(RES,colMeans(new))
  }
  RES
}