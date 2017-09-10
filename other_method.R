
gstat.idw <- idw(Z ~ 1, locations = ~x+y, HSdata.known, HSdata.unknown, maxdist = 6, idp=2)
gstat.nn <- idw(Z ~ 1, locations = ~x+y, HSdata.known, HSdata.unknown, maxdist = 6, idp=2, nmax = 1)

rmse.idw2 = rmse(HSdata.unknown$Z, gstat.idw$var1.pred)
rmse.nn = rmse(HSdata.unknown$Z, gstat.nn$var1.pred)

zmat = matrix(HSdata.known$Z, nrow = length(yvec))
bil = bilinear(yvec, xvec, zmat, HSdata.unknown$y, HSdata.unknown$x)
rmse.bil = rmse(HSdata.unknown$Z, bil$z)

zmat = matrix(HSdata.known$Z, nrow = length(yvec))
bic = bicubic(yvec, xvec, zmat, HSdata.unknown$y, HSdata.unknown$x)
rmse.bic = rmse(HSdata.unknown$Z, bic$z)

