##data management
#install.packages("rjags")
#install.packages("R2jags")

setwd("C:\\Users\\db1876\\Google Drive\\alyssa")
library(R2jags)


#read in data
data<-read.csv("ttd.csv")
data<-data[which(data$Survey.==1),]

data$ttd<-as.numeric(data$ttd)
data$Tmax<-as.numeric(data$Tmax)
data$SiteNumber<-as.numeric(data$SiteNumber)
data$MinOfDay<-as.numeric(data$MinOfDay)

#observed distribution of time to detection
hist(data$ttd, breaks=50,col="grey",main="Observed distribution of time to detection",xlim=c(0,20),xlab="Measured time to detection")

#Manage data and standardize time of day
nobs<-length(as.numeric(data$SiteCode))#number of observations
d<-as.numeric(is.na(data$ttd)) #censoring indicator
mean.tod<-mean(as.numeric(data$MinOfDay))
sd.tod<-sd(as.numeric(data$MinOfDay))
tod<-(data$MinOfDay-mean.tod)/sd.tod

data2 <- data

data2$ttd[which(data$ttd>15)] <- NA
data2$Tmax[which(data$Tmax>15)] <- 15


#########################################
#Exponential NO COVARIATES
########################################

d<-as.numeric(is.na(data$ttd)) #censoring indicator

#bundle data - I had to add a nominal value to Tmax to get JAGS to play nice with this
str(jags.data<-list(ttd=data2$ttd,d=d,nobs=length(data2$SiteNumber),Tmax=data2$Tmax+0.01))

#Define model
cat(file="model1.txt","
    model{
    
    #Priors
    int.psi~dunif(0,1)                    #Occupancy intercept
    int.lambda~dgamma(0.0001,0.0001)      #Poisson rate parameter
       
    #likelihood
    for (i in 1:nobs) {
	#Model for occurence
    		z[i]~dbern(psi[i])
		logit(psi[i])<-logit(int.psi)

    #Observation model
    #Exponential model for time to detection ignoring censoring
	    ttd[i]~dexp(lambda[i])
	    log(lambda[i])<-log(int.lambda)

    #model for censoring due to species absence and ttd>=Tmax
	    d[i]~dbern(theta[i])
	    theta[i]<-z[i]*step(ttd[i]-Tmax[i])+(1-z[i])
	} #i
#Derived quantities
n.occ<-sum(z[])             #Number of occupied sites
} #model
")

#Inits function for some params
#Initialize with z=] throughout and 
#all NAs due to censoring, rather than non-occurence

	zst<-rep(1,length(jags.data$ttd))
	ttdst<-rep(jags.data$Tmax)
	ttdst[jags.data$d==0]<-NA
	inits<-function(){list(z=zst,ttd=ttdst,int.psi=runif(1),int.lambda=runif(1))}

#Parameters to estimate
	
	params<-c("int.psi","int.lambda","n.occ","z")

#MCMC settings
	
	ni<-5000 ; nt<- 2 ; nb <- 2000 ; nc<- 3

#calls to jags

	out <- jags(jags.data,inits,params,"model1.txt",n.chains=nc,n.iter=ni,n.burnin=nb,n.thin=nt)

	jm <- jags.model(file="model1.txt", data=jags.data, inits=inits, n.chains=3, n.adapt=2000)
	jo <- coda.samples(jm, params, n.iter=5000)

	plot(jo)


#########################################
#Exponential GRADIENT [now stream]
########################################

#data$AveGrad[which(is.na(data$AveGrad))] <- 0  #I did this, but don't know if it is right... can't have NAs for explanatory vars here

##added new covariate for site to replace gradient for now

#bundle data
str(jags.data<-list(ttd=data2$ttd,d=d,nobs=length(data2$SiteNumber),covB=(as.numeric(data2$Stream)-1),Tmax=data2$Tmax+0.01))

#Define model
cat(file="model1.txt","
    model{
    
    #Priors
    int.psi~dunif(0,1)                    #Occupancy intercept
    int.lambda~dgamma(0.0001,0.0001)      #Poisson rate parameter
    beta1~dnorm(0,0.001)                  #Slope coefficient in logit (occupancy)

    #likelihood
    for (i in 1:nobs) {
		#Model for occurence
		z[i]~dbern(psi[i])
		logit(psi[i])<-logit(int.psi)+beta1*covB[i]

    #Observation model
    #Exponential model for time to detection ignoring censoring
		ttd[i]~dexp(lambda[i])
		log(lambda[i])<-log(int.lambda)

    #model for censoring due to species absence and ttd>=Tmax
		d[i]~dbern(theta[i])
		theta[i]<-z[i]*step(ttd[i]-Tmax[i])+(1-z[i])
}
#Derived quantities
n.occ<-sum(z[])             #Number of occupied sites
}
")

#Inits function for some params
#Initialize with z=] throughout and 
#   all NAs due to censoring, rather than non-occurence
zst<-rep(1,length(jags.data$ttd))
ttdst<-rep(jags.data$Tmax)
ttdst[jags.data$d==0]<-NA
inits<-function(){list(z=zst,ttd=ttdst,int.psi=runif(1),int.lambda=runif(1),beta1=rnorm(1))}

#Parameters to monitor
params<-c("int.psi","int.lambda","beta1","n.occ","z")

#MCMC settings
	ni<-5000 ; nt<- 2 ; nb <- 2000 ; nc<- 3

	out2<-jags(jags.data,inits,params,"model1.txt",n.chains=nc,n.iter=ni,n.burnin=nb,n.thin=nt)

	jm2 <- jags.model(file="model1.txt", data=jags.data, inits=inits, n.chains=3, n.adapt=2000)
	jo2 <- coda.samples(jm2, params, n.iter=5000)

hist(jo2[[1]][,1]) #wtf is wrong with this beta parameter

jo2[[1]][,



