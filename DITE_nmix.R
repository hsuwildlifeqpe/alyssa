install.packages("unmakred")
library(unmarked)


#FORMATIING DATA
data<-read.csv("Count_DITE_Robust.csv")
data<-data[,2:5]

#Repeated count data with 4 primary periodsand
#not secondary sampling period (ie J==1)
y2<-data.matrix(data,rownames.force = NA)

#Site specific co-variates
sc2<-read.csv("SiteCovs.csv")
sc2<-sc2[,2:13]

#Observation Specific Covariates


#Primary Periods of survey
#?

umf2 <- unmarkedFramePCount(y=y2, siteCovs=sc2)

#take a look
umf2
summary(umf2)

#Fitting Models
fm1<-pcount(~1~1,umf2,K=50)
backTransform(fm1,'det')
confint(fm1,type='state')


# Real data
data(mallard)
diteUMF <- unmarkedFramePCount(data, siteCovs = sc2)
summary(diteUMF)
(fm1 <- pcount(~ 1 ~ 1,diteUMF, K=30))
(fm2 <- pcount(~ 1 ~ Tier,diteUMF, K=30))
(fm3 <- pcount(~ 1 ~ Tier+Watershed,diteUMF, K=30))
(fm4 <- pcount(~ 1 ~ Tier+Watershed+Sediment,diteUMF, K=30))
(fm5 <- pcount(~ 1 ~ Tier+Watershed+Sediment+Depth,diteUMF, K=30))
(fm6 <- pcount(~ 1 ~ Tier+Watershed+Sediment+Over,diteUMF, K=30))
(fm7 <- pcount(~ 1 ~ Tier+Watershed+Sediment+Over+Gravel,diteUMF, K=30))

plogis(coef(fm7, type="det")) 
(fm7.re <- ranef(fm4))
plot(fm7.re, subset=site %in% 1:25, xlim=c(-1,30))
sum(bup(fm7.re)) # Estimated population size
