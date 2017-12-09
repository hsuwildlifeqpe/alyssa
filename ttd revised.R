
############################
#Weibull
############################

setwd("C:\\Users\\db1876\\Google Drive\\alyssa")

#read in data
data<-read.csv("ttd.csv")
#str(ttd)
#rm(data)

data<-data[which(data$Survey.==1),]

data$ttd<-as.numeric(data$ttd)
data$Tmax<-as.numeric(data$Tmax)
data$SiteNumber<-as.numeric(data$SiteNumber)
data$MinOfDay<-as.numeric(data$MinOfDay)


#from simulation data
hist(data$ttd, breaks=50,col="grey",main="Observed distribution of time to detection",xlim=c(0,20),xlab="Measured time to detection")

abline(v=10, col="grey",lwd=3)

#Manage data and standardize time of day
nobs<-length(as.numeric(data$SiteCode))#number of observations
d<-as.numeric(is.na(data$ttd)) #censoring indicator
mean.tod<-mean(as.numeric(data$MinOfDay))
sd.tod<-sd(as.numeric(data$MinOfDay))
tod<-(data$MinOfDay-mean.tod)/sd.tod

#Bundle and summarize data set
str(win.data<-list(M=length(data$SiteNumber),site=1:57,
    tod=tod,ttd=data$ttd,d=d,covB=data$AveGrad,nobs=nobs,Tmax=data$Tmax))

#covA would be observer, but dont know i can use a categorical variable

#Define model
cat(file="model2.txt",
"    
model {
    
#Priors
psi~dunif(0,1)                        #Occupancy intercept
int.lambda~dgamma(0.0001,0.0001)      #Poisson rate parameter
beta1~dnorm(0,0.001)                  #Slope coefficient in logit (occupancy)
#lambda.int[1]~gamma(0.001,0.001)     #Poisson rate parameter for females
#lambda.int[2]~gamma(0.001.0.001)     #Poisson rate paramter for males
alpha1~dnorm(0,0.001)                 #Coefficient of time of day (linear)
alpha2~dnorm(0,0.001)                 #Coefficient of time of day (squared)
shape~dgamma(0.001,0.001)             #Weibul shape
#sexration~dunif(0,1)                 #Sex ratio (proportion males)

#likelihood
for(i in 1:M){                        #Model for occurrence at site level
    z[i]~dbin(psi,1)                      
} #i

for(i in 1:nobs){                     #Observation modle at observation level
  #Weibull model for time to detection ignoring censoring
  ttd[i]~dweib(shape,lambda[i])

#log(lambda[i])<-(1-males[i])*log(lambda.int[1]+male[i]*log(lambda.int[2])+alpha1
#*tod[i]+alpha2*pow(tod[i],2))
log(lambda[i])<-log(int.lambda)+alpha1*tod[i]+alpha2*pow(tod[i],2)

#model for censoring due to species absence and ttd>=Tmax
d[i]~dbin(theta[i],1)
theta[i]<-z[site[i]]*step(ttd[i]-Tmax[i])+(1-z[site[i]])

#model for sex unobserved individuals
#male[i]<dbern(sexratio)  #Will impute sex for unobserved individuals

#Derived quantiles
#n.occ<-sum(z[])           #number of occupied sites among M
} #i
} #model
")

#Inits function
zst<-rep(1,win.data$M)
ttdst<-rep(win.data$Tmax+1)
ttdst[win.data$d==0]<-NA
inits<-function(){list(z=zst,ttd=ttdst,psi=runif(1),lambda.int=runif(2),
alpha1=rnorm(1),alpha2=rnorm(1),shape=runif(1))}

#Parameters to estimate
params<-c("psi","lambda.int","alpha1","alpha2","n.occ","z","shape")

ni<-150 ; nt<-2 ; nb<- 200 ; nc<- 1

#Call JAGS from R and summarize posteriors
install.packages("rjags")
install.packages("R2jags")
library(R2jags)
out2<-jags(win.data,inits,params,"model2.txt",n.chains=nc,n.iter=ni,n.burnin=nb,n.thin=nt)

print(out2,dig=3)


#########################################
#Exponential
########################################
d<-as.numeric(is.na(data$ttd)) #censoring indicator
data1<-data("ttdPeregrine")


#bundle data
str(win.data<-list(ttd=data$ttd,d=d,covB=data$AveGrad,nobs=data$SiteNumber,data$Tmax))
#Define model
cat(file="model1.txt","
    model{
    
    #Priors
    int.psi~dunif(0,1)                    #Occupancy intercept
    int.lambda~dgamma(0.0001,0.0001)      #Poisson rate parameter
    beta1~dnorm(0,0.001)                  #Slope coefficient in logit (occupancy)
    alpha1~dnorm(0,0.001)                 #Coefficient of time of day (linear)
    
    
    #likelihood
    for (i in 1:nobs{
#Model for occurence
    z[i]~dbern(psi[i])
    logit(psi[i])<-logit(int.psi)+beta1*covB[i]

    #Observation model
    #Exponential model for time to detection ignoring censoring
    ttd[i]~dexp(lambda[i])
    log(lambda[i])<-log(int.lambda)+alpha1

    #model for censoring due to species absence and ttd>=Tmax
    d[i]<-dbern(theta[i])
    theta[i]<-z[i]*step(ttd[i]-Tmax)+(1-z[i])
}
#Derived quantities
n.occ<-sum(z[])             #Number of occupied sites
}
")

#Inits function for some params
#Initialize with z=] throughout and 
#   all NAs due to censoring, rather than non-occurence
zst<-rep(1,length(win.data$ttd))
ttdst<-rep(win.data$Tmax+1,data$SiteNumber)
ttdst[win.data$d==0]<-NA
inits<-function(){list(z=zst,ttd=ttdst,int.psi=runif(1),int.lambda=runif(1))}

#Parameters to estimate
Params<-c("int.psi","beta1","int.lambda","alpha1","n.occ")

#MCMC settings
ni<5000 ; nt<- 2 ; nb <- 2000 ; nc<- 3

#Call WinBugs
install.packages("R2WinBUGS")
library(R2WinBUGS)
out1<-bugs(win.data,inits,params,"model1.txt",n.chains=nc, n.iter=ni,n.burn=nb,
      n.thin=nt, debug=TRUE,bugs.directory=bd)
print(out1.dig=3)
