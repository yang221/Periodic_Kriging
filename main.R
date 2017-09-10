library(sp)
library(gstat)
library(plotly)
library(Metrics)
library(boot)
library(akima)

HSSurflist = list()
Result = matrix(0, 1000, 5)

xtotal <- 480
ytotal <- 160

xvec = seq(1,xtotal,3)
yvec = seq(1,ytotal,3)

loc <- list(x = rep(seq(xtotal), each = ytotal ),
            y = rep(seq(ytotal), times = xtotal))
loc.known = which(loc$x %in% seq(1,xtotal,3) & loc$y %in% seq(1,ytotal,3))
maxdist = 6
#################
kriging <- function(j){
  pt = unknownData[j,]
  
  nearPoints = knownData[which(spDistsN1(knownData,pt) <= maxdist),]
  Kdim = nrow(nearPoints)+1
  
  Kmat = matrix(1,Kdim,Kdim)
  Kmat[Kdim,Kdim] = 0
  Kmat[-Kdim,-Kdim] = CovFcn.Mix(spDists(nearPoints), coef(Mix.fit))
  Kinv = solve(Kmat)
  
  M = matrix(1,nrow = Kdim,ncol=1)
  M[-Kdim,] = CovFcn.Mix(spDistsN1(nearPoints,pt), coef(Mix.fit))
  lamda = Kinv %*% M
  tempResult@data$pred.Z[j] <<- sum(lamda[-Kdim]*nearPoints@data$Z)
  tempResult@data$pred.var[j] <<- coef(Mix.fit)['sill']-sum(lamda*M)
  return(tempResult)
}

CovFcn.Mix <- function(dist, para){
  return((para[1]-para[5])*(para[2]*exp(-(dist/para[3])^2)+(1-para[2])*besselJ(2*pi/para[4]*dist,0))+ifelse(dist==0,para[5],0))
}

#####
for(s in 1:1000){
  source('Preprocessing.R')
  source('MixKriging.R')
  source('other_method.R')
  
  HSSurflist[[s]] = HSSurf
  Result[s,1] = rmse.mix
  Result[s,2] = rmse.bic
  Result[s,3] = rmse.idw2
  Result[s,4] = rmse.bil
  Result[s,5] = rmse.nn
  
  print(paste("simulation: ",s, ", result:", Result[s,]))
}

save(HSSurflist, Result, file = 'Simu250.RData')