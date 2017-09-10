
#load("HSPreprocess.RData")

####################

Mix.fit = HS.Mix
## Predict

unknownData = HSdata.unknown
knownData = HSdata.known

KrigResult = list()
coordinates(knownData) <- ~x+y
coordinates(unknownData) <- ~x+y
KrigResult = unknownData
KrigResult@data$pred.Z = 0
KrigResult@data$pred.var = 0

tempResult = KrigResult

for(j in 1:nrow(unknownData)){
  KrigResult@data$pred.Z[j] = kriging(j)@data$pred.Z[j]
  KrigResult@data$pred.var[j]= tempResult@data$pred.var[j]
  
  if(j %% 10000 == 0){
    print(paste("simulation: ",s, ", point:", j))
  }
}

MixResult = KrigResult

rmse.mix = rmse(HSdata.unknown$Z, MixResult@data$pred.Z)

#save(MixResult,file = paste("Mix_dist_6.Rdata",sep = ""))
