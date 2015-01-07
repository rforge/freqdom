library(fda)
#load('data/stocks.rda')
#load("~/Documents/workspace/libraries/datasets/stockdata.rda")

# StockName = c("boa","citi","coca","cvx","dis","ibm","mcd","msft","wmt","xom")
index = read.table("~/Documents/workspace/libraries/freqdom/data/index.rda")
citi = read.table("~/Documents/workspace/libraries/freqdom/data/citi.rda")

Ytbl = citi
Xtbl = index[6 + 1:2511,]
Xtbl[is.na(Xtbl)] = 0

n = dim(Ytbl)[1]
ntest = 0

nbasisX = 21
nbasisY = 21

basisX = create.bspline.basis(rangeval=c(0, 1), nbasis=nbasisX) 
basisY = create.bspline.basis(rangeval=c(0, 1), nbasis=nbasisY) 

# CUMULATIVE INTRADAY RETURNS
Ytbl = 100*(log(Ytbl) - log(Ytbl[,1])) #/Ytbl[,1]
Ytbl = Ytbl[,-1]

recumulate = function(X){
  d = dim(X)
  v = c(1)
  for (i in 1:(d[1]-1)){
    v = c(v,v[length(v)] * X[i,d[2]])
  }
  plot(v)
  X * v
}

# volatility
if (TRUE) {
  Ytbl = recumulate(exp(Ytbl/100))
  Xtbl = recumulate(exp(Xtbl/100)) * 680
  
  Xtbl = abs(Xtbl[,2:389] - Xtbl[,1:388])
  Ytbl = abs(Ytbl[,2:389] - Ytbl[,1:388])
  
  #Xtbl = abs(Xtbl[,2:388] - Xtbl[,1:387])
  #Ytbl = abs(Ytbl[,2:388] - Ytbl[,1:387])
}

cols = dim(Ytbl)[2]

Xreal = Data2fd((1:cols)/cols,t(Xtbl),basisX)
Yreal = Data2fd((1:cols)/cols,t(Ytbl),basisY)

X = center.fd(Xreal)
Y = center.fd(Yreal)


