######################################################
#unmarked example
######################################################


#########################################################################
#DITE data
#########################################################################

#Importing and formatting data: csvToUMF

library(unmarked)

data <- read.csv("occdata_DITE_Robust.csv")#load your data in
data$y.1<-as.integer(data$y.1)
data$y.2<-as.integer(data$y.2)
data$y.3<-as.integer(data$y.3)
data$y.4<-as.integer(data$y.4)
data$sites<-as.integer(data$sites)

str(data)
#GW_torrent<-subset(torrent,Stream=="1")
y <- data[,2:5] #makes columns 2-3 (the occupancy data) into a variable called 'y'

siteCovs <- SiteCovs[,c("Watershed","Tier","Sediment","Coarse","WW","Over","Depth","Gradient")]


#obsCovs <- list(date=wt[,c("date.1", "date.2", "date.3")],
               # ivel=wt[,c("ivel.1", "ivel.2", "ivel.3")])
#obsCovs1<-list(observer=GW_torrent[,c("Observer.1","Observer.2")],
 #              Latency=GW_torrent[,c("Latency.1","Latency.2")])

DITE <- unmarkedFrameOccu(y=y,siteCovs = siteCovs)
summary(DITE)


#Use long = TRUE if sites dont have the same numbers of observations


#To help stabilize the numerical optimization algorithm, its recommended standardizing the covariates
#obsCovs(torrent) <- scale(obsCovs(torrent))
siteCovs(DITE) <- scale(siteCovs(DITE))


#Fitting models
fm1 <- occu(~1 ~1, DITE)
fm1
backTransform(fm1, 'state')#prob of occupancy
backTransform(fm1,'det')   #prob of detection

fm2<-occu(~Watershed ~1, DITE)
fm2
backTransform(linearComb(fm2,coefficients=c(1,0),type='det'))
backTransform(fm2,type='state')

fm3<-occu(~Tier ~1, DITE)
fm3
backTransform(linearComb(fm3,coefficients=c(1,0),type='det'))
backTransform(fm3,type='state')

fm4<-occu(~Sediment ~1, DITE)
fm4
backTransform(linearComb(fm4,coefficients=c(1,0),type='det'))
backTransform(fm4,type='state')

fm5<-occu(~Watershed + Sediment ~1, DITE)
fm5
backTransform(linearComb(fm5,coefficients=c(1,0,0),type='det'))
backTransform(fm5,type='state')

fm6<-occu(~WW ~1, DITE)
fm6
backTransform(linearComb(fm6,coefficients=c(1,0),type='det'))
backTransform(fm6,type='state')

fm7<-occu(~Gradient ~1, DITE)
fm7
backTransform(linearComb(fm6,coefficients=c(1,0),type='det'))
backTransform(fm7,type='state')

fm8<-occu(~Gradient+WW+Sediment+Depth+Over+Coarse+Watershed+Tier ~1, DITE)
fm8
backTransform(linearComb(fm8,coefficients=c(1,0,0,0,0,0,0,0,0),type='det'))
backTransform(fm8,type='state')

fm9<-occu(~Gradient+WW+Sediment+Depth+Over+Coarse+Watershed ~1, DITE)
fm9
backTransform(linearComb(fm9,coefficients=c(1,0,0,0,0,0,0,0),type='det'))
backTransform(fm9,type='state')



#Model Selection and Model Fit
fms<-fitList('psi(.)p(.)'=fm1,'psi(Watershed)p(.)'= fm2,'psi(Tier)p(.)'= fm3,
             'psi(Sediment)p(.)'= fm4, 'psi(Watershed+Sediment)p(.)'= fm5,'psi(WW)p(.)'= fm6,
             'psi(Gradient)p(.)'= fm7,'psi(Gradient+WW+Sediment+Depth+Over+Coarse+Watershed+Tier)p(.)'= fm8,
             'psi(Gradient+WW+Sediment+Depth+Over+Coarse+Watershed)p(.)'= fm9)
modSel(fms)
#model weights = probabilit ythat model k is the "best" model in the candidate set
#              = the weight of evidence in favor of model k being the actual Kullback-Leibler best model for the situation at hand

####################################################################
#Goodness of Fit# Parametric Bootstrap Method
####################################################################
chisq <- function(fm) {
  umf <- getData(fm)
  y <- getY(umf)
  y[y>1] <- 1
  sr <- fm@sitesRemoved
  if(length(sr)>0)
    y <- y[-sr,,drop=FALSE]
  fv <- fitted(fm, na.rm=TRUE)
  y[is.na(fv)] <- NA
  sum((y-fv)^2/(fv*(1-fv)), na.rm=TRUE)
}
(pb <- parboot(fm7, statistic=chisq, nsim=100, parallel=FALSE))

#################################################################
#CI
#################################################################

re <- ranef(fm1)
EBUP <- bup(re, stat="mode")
CI <- confint(re, level=0.9)
rbind(PAO = c(Estimate = sum(EBUP), colSums(CI)) / 130)

#I'm not sure how to interpret, but these estimates dont seem right

#################################################################

amphib<-read.csv("amphib.csv")
quantile(na.omit(amphib$TL))
str(amphib$SVL)
amphib$TL<-as.numeric(amphib$TL)
amphib$SVL<-as.numeric(amphib$TL)
