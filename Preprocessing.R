##Generate a surface with random noise
HemiSphereSurf = matrix(0, ytotal, xtotal)
for(i in 1:160){
  for(j in 1:480){
    y = i%%(20*pi)
    x = j%%(20*pi)
    r = sqrt((x-10*pi)^2+(y-10*pi)^2)
    if(r <= 2*pi){
      h = sqrt(100*pi^2-r^2)
      HemiSphereSurf[i,j] = rnorm(1,mean = h ,sd=2)
    }
    else if(r <= 6*pi){
      h = sqrt(100*pi^2-(6*pi)^2)
      HemiSphereSurf[i,j] = rnorm(1,mean = h ,sd=2)
    }else if(r <= 10*pi){
      h = sqrt(100*pi^2-r^2)
      HemiSphereSurf[i,j] = rnorm(1,mean = h ,sd=2)
    }else{
      HemiSphereSurf[i,j] = abs(rnorm(1,0,2))
    }
  }
}

#plot_ly(z =  ~ HemiSphereSurf[1:160,1:160], type = "surface")



## smooth the surface
HSSurf = matrix(0, ytotal, xtotal)

for(i in 1:160){
  for(j in 1:480){
    current_Z = HemiSphereSurf[i,j]
    neighbor = c()
    for(a in max(1,i-1):min(i+1,ytotal)){
      for(b in max(1,j-1):min(j+1,xtotal)){
        if(a != i || b != j){
          neighbor = c(neighbor, HemiSphereSurf[a,b])
        }
      }
    }
    HSSurf[i,j] = mean(neighbor)*10+rnorm(1,0,0)
  }
}

plot_ly(z =  ~ HSSurf[1:80,1:80], type = "surface")


HSdata=data.frame(Z=matrix(HSSurf, ncol = 1),x=loc$x,y=loc$y)
HSdata.known = HSdata[loc.known,]
HSdata.unknown = HSdata[-loc.known,]

##Variogram
HS.vgm <- variogram(Z ~ 1, HSdata.known,locations = ~x+y, width = 3, cutoff=160)

HS.Mix = nls(gamma ~ nug+(sill-nug)*(1-p*exp(-(dist/a)^2)-(1-p)*besselJ(2*pi/h*dist,0)), data = HS.vgm,
             start = list(sill=10000, p=0.3, a=7, h=50,nug=10), weights = dist^-2*np,
             lower = list(a = 0, nug = 0), algorithm = "port")


#plot(gamma ~ dist, data = HS.vgm ,pch=20,cex=0.4,col=1)
#d = 1:160
#lines(d, predict(HS.Mix, list(dist = d)),col=2,lwd=2)

#save.image(file = "HSPreprocess.RData")
