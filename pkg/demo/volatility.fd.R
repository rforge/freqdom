library(fda)
load('data/stocks.rda')
load("~/Documents/workspace/libraries/datasets/stockdata.rda")

# StockName = c("boa","citi","coca","cvx","dis","ibm","mcd","msft","wmt","xom")
st = stocks$coca
#st = stocks$ibm

#st = stocks$wmt

Ytbl = st[1:2500,]
Xtbl = index[1:2500,]

n = dim(Xtbl)[1]
ntest = 0

nbasisX = 15
nbasisY = 15

basisX = create.fourier.basis(rangeval=c(0, 1), nbasis=nbasisX) 
basisY = create.fourier.basis(rangeval=c(0, 1), nbasis=nbasisY) 
basisX = create.bspline.basis(rangeval=c(0, 1), nbasis=nbasisX) 
basisY = create.bspline.basis(rangeval=c(0, 1), nbasis=nbasisY) 

# VOLATILITY
cols = dim(Ytbl)[2]-1
Ytbl = (Ytbl[,1 + 1:(cols)] - Ytbl[,1:(cols)])
Ytbl = abs(Ytbl)
#Ytbl = Ytbl - rowMeans(Ytbl)
Ytbl = t(t(Ytbl) - colMeans(Ytbl))

Xtbl = cbind(0,Xtbl)
Xtbl = (Xtbl[,1 + 1:(cols)] - Xtbl[,1:(cols)])
Xtbl = abs(Xtbl)
#Xtbl = Xtbl - rowMeans(Xtbl)
Xtbl = t(t(Xtbl) - colMeans(Xtbl))

volXreal = Data2fd((1:cols)/cols,t(Xtbl),basisX)
volYreal = Data2fd((1:cols)/cols,t(Ytbl),basisY)

X = center.fd(volXreal)
Y = center.fd(volYreal)

