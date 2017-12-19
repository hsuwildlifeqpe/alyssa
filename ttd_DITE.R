##data management
#install.packages("rjags")
#install.packages("R2jags")
library(rjags)
library(R2jags)

setwd("C:\\Users\\db1876\\Google Drive\\alyssa\\20171219")

#read in data
data<-read.csv("Amphib.csv")
names(data)
data1<-data[which(data$Survey==1 & data$VES == 0 & data$Year==2016),]  ##fixed

data$ttd<-as.numeric(as.character(data$ttd))
data$Tmax<-as.numeric(as.character(data$Tmax))
data$SiteNumber<-as.numeric(as.character(data$SiteNumber))
data$MinOfDay<-as.numeric(as.character(data$MinOfDay))

sites <- data.frame(SiteCode=levels(droplevels(data1$SiteCode)), ttd=rep(0,240), Tmax=rep(0,240), MinOfDay=rep(0,240))

i=1

for(i in 1:length(sites$SiteCode)) {
	tmp <- sites$SiteCode[i]
	cur <- data[which(data$Survey==1 & data$VES == 0 & data$Year==2016 & data$Species == "DITE" & as.character(data$SiteCode)==as.character(tmp)),]
	if(nrow(cur) > 0) {
		sites[i,2] <- min(cur$ttd)
		sites[i,3] <- cur$Tmax[1]
		sites[i,4] <- cur$MinOfDay[1]
	} else {
		cur <- data[which(data$Survey==1 & data$VES == 0 & data$Year==2016 & as.character(data$SiteCode)==as.character(tmp)),]
		sites[i,3] <- cur$Tmax[1]
		sites[i,4] <- cur$MinOfDay[1]
	}
}

#data1<-data[which()] #changed

length(data$Survey==1&data$Year==2016&data$Species=="DITE")
length(data$Survey == 1)
length(data$Year==2016)
length(data$Species=="DITE")

data$ttd<-as.numeric(data$ttd)
data$Tmax<-as.numeric(data$Tmax)
data$SiteNumber<-as.numeric(data$SiteNumber)
data$MinOfDay<-as.numeric(data$MinOfDay)

#observed distribution of time to detection
hist(sites$ttd, breaks=50,col="grey",main="Observed distribution of time to detection",xlim=c(0,20),xlab="Measured time to detection")

#Manage data and standardize time of day
nobs<-length(sites$SiteNumber)#number of observations
d<-sites$ttd #censoring indicator
mean.tod<-mean(na.omit(sites$MinOfDay))
sd.tod<-sd(na.omit(sites$MinOfDay))
tod<-(sites$MinOfDay-mean.tod)/sd.tod

sites$ttd[which(sites$ttd ==0)] <- NA

sites$ttd[which(sites$ttd>20)] <- NA
sites$Tmax[which(sites$Tmax>20)] <- 20

sites$ttd <- sites$ttd/sites$Tmax

sites$Tmax <- rep(1,240)

quantile(na.omit(sites$ttd))

#data2 <- data

#########################################
#Exponential NO COVARIATES
########################################

d<-as.numeric(is.na(sites$ttd)) #censoring indicator

#bundle data - I had to add a nominal value to Tmax to get JAGS to play nice with this
str(jags.data<-list(ttd=sites$ttd,d=d,nobs=length(sites$Tmax),Tmax=sites$Tmax+0.01))   

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

plot(sites$Tmax, colSums(jo[[1]][,4:243])/5000)  ##plots predictions of site-level realized occupancy against survey length

sum(is.na(sites$ttd))


#######################nothing below here was tested on 12/19/2017


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
         
         