
d = round(100/3)


library(fGarch)
spec = garchSpec(model = list(ar = 0.5, alpha = c(0.3, 0.4), beta = 0))
sim1<- 

for(i in 1:100)
{
  x = garchSim(spec, n = 100)
  a = sample.depth.cont.for.mu(d=d,mu=0,y=as.numeric(x),length=100) - sampleDepthContForMuCPP(d,0,as.numeric(x))
  if(any(a>1e-10)) stop("ERROR")
}

t(sampleDepthContForMuCPP(d,0,as.numeric(x))) 
sample.depth.cont.for.mu(d=d,mu=0,y=as.numeric(x),length=100)

d.min<-floor(100/3)


for(i in 1:10000)
{
  x = garchSim(spec, n = 100)
a = sample.max.depth.for.mu(mu=0,y=as.numeric(x),d.min=d.min,iter=100,eps = 0.0001) - sampleMaxDepthForMuCPP(mu=0,rY=as.numeric(x),d_min=d.min,max_iter=100, eps = 0.0001)
  if(any(a>1e-10)) stop("ERROR")
}

sample.max.depth.for.mu(mu=-0.000079,y=as.numeric(x),d.min=33,iter=100,eps = 0.0001)
t(sampleMaxDepthForMuCPP(mu=-0.000079,rY=as.numeric(x),d_min=32,max_iter=100, eps = 0.0001))




for(i in 1:1000)
{
  x = garchSim(spec, n = 100)
  a = sample.max.depth(y=as.numeric(x),iter=100,eps=0.00001,p.length=10) - sampleMaxLocScaleDepthCPP(ry=as.numeric(x),iter=100,eps=0.00001,p_length=10)
  if(any(a>1e-5)) stop("ERROR")
}

sample.max.depth(y=as.numeric(x),iter=100,eps=0.00001,p.length=10)
sampleMaxLocScaleDepthCPP(ry=as.numeric(x),iter=100,eps=0.00001,p_length=10)

