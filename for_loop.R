
##for loop that arranges phib data into occupancy data Dan & Alyssa 12/14/2017

setwd("C:/Users/db1876/Google Drive/alyssa/")

phibs <- read.csv("Amphib.csv") #read in data
phibs<-phibs[which(phibs$VES == 0 & phibs$Survey==1|phibs$Survey==2),]


#habitat<-read.csv("Habitat.csv")
#habitat<-habitat[which(habitat$Year=="2016"&habitat$SurveyType=="GSR"|habitat$SurveyType=="GSR_SSS"),]

#phibs$site<-NA This adds another column in the dataset called 'site'

#phibs <- transform(phibs, site=match(phibs$Site.Code, unique(phibs$Site.Code)))
#write.csv(phibs,file="Phibs.csv")

sites <- unique(phibs$Site.Code) #get list of sites
#length(unique(phibs$SiteCode))

occdata <- data.frame(site=sites,y.1=rep(NA,length(sites)),y.2=rep(NA,length(sites)),y.3=rep(NA,length(sites)),y.4=rep(NA,length(sites))) #make empty data frame WITH FOUR COLUMNS NOW

spp <- "DITE"

for(i in 1:length(sites)) {
  
  site <- sites[i] ##get name of current site
  
  curdata <- phibs[which(phibs$Site.Code==site & phibs$Year==2016),] ##get data from current site in 2016
  
  if(sum(curdata$Survey==1 & curdata$Species==spp) > 0) { occdata$y.1[i] <- 1 }
  if(sum(curdata$Survey==2 & curdata$Species==spp) > 0) { occdata$y.2[i] <- 1 }
  if((sum(curdata$Survey==1 & curdata$Species==spp)==0) & (sum(curdata$Survey==1 & curdata$Species!=spp) > 0)) { occdata$y.1[i] <- 0 }
  if((sum(curdata$Survey==2 & curdata$Species==spp)==0) & (sum(curdata$Survey==2 & curdata$Species!=spp) > 0)) { occdata$y.2[i] <- 0 }
  
  curdata <- phibs[which(phibs$Site.Code==site & phibs$Year==2017),] ##get data from current site in 2016
  
  if(sum(curdata$Survey==1 & curdata$Species==spp) > 0) { occdata$y.3[i] <- 1 }
  if(sum(curdata$Survey==2 & curdata$Species==spp) > 0) { occdata$y.4[i] <- 1 }
  if((sum(curdata$Survey==1 & curdata$Species==spp)==0) & (sum(curdata$Survey==1 & curdata$Species!=spp) > 0)) { occdata$y.3[i] <- 0 }
  if((sum(curdata$Survey==2 & curdata$Species==spp)==0) & (sum(curdata$Survey==2 & curdata$Species!=spp) > 0)) { occdata$y.4[i] <- 0 }
  
}


table(occdata$y.1,occdata$y.2) ##quick look at data
write.csv(occdata,file="Occdata_DITE.csv")




#Do this one column  at a time for multi-year data
#is.na(occdata[,3])
#which(is.na(occdata[,3]))
#occdata[,3][which(is.na(occdata[,3]))]
#occdata[,3][which(is.na(occdata[,3]))] <- "."
#occdata

#is.na(occdata[,4])
#which(is.na(occdata[,4]))
#occdata[,4][which(is.na(occdata[,4]))]
#occdata[,4][which(is.na(occdata[,4]))] <- "."
#occdata

#is.na(occdata[,5])
#which(is.na(occdata[,5]))
#occdata[,5][which(is.na(occdata[,5]))]
#occdata[,5][which(is.na(occdata[,5]))] <- "."
#occdata

##add covariates
#tiers <- aggregate(phibs$Tier ~ phibs$Site.Code, FUN=max)##aggregate is your new friend
#occdata <- merge(y=tiers, x=occdata, by="Site.Code", all=FALSE)  ##and merge is its buddy

#watershed <- aggregate(Watershed ~ site, data=phibs, FUN=max)
#occdata <- merge(y=watershed,x=occdata, by="site", all=FALSE)

#distance <- aggregate(habitat$Distance ~ SiteCode, data=phibs, FUN=max)
#length(habitat$Site.Code)
#occdata <- merge(y=SiteNumber, x=occdata, by="SiteCode", all=FALSE)

#write.csv(occdata, file="occdata_ASTR_Robust.csv")

############################################################################
#SiteCovs
#############################################################################
phibs <- read.csv("Amphib.csv") #read in data
phibs<-phibs[which(phibs$VES == 0 & phibs$Survey==1|phibs$Survey==2),]

#phibs$site<-NA

#phibs <- transform(phibs, site=match(phibs$SiteCode, unique(phibs$SiteCode)))
#write.csv(phibs,file="Phibs.csv")

sites <- unique(phibs$Site.Code) #get list of sites
#length(unique(phibs$SiteCode))

SiteCovs <- data.frame(site=sites,Watershed=rep(NA,length(sites)),Tier=rep(NA,length(sites)),Gradient=rep(NA,length(sites)),Sediment=rep(NA,length(sites)),Coarse=rep(NA,length(sites)),Depth=rep(NA,length(sites)),WW=rep(NA,length(sites)),Over=rep(NA,length(sites))) #make empty data frame WITH FOUR COLUMNS NOW

tiers <- aggregate(phibs$Tier ~ phibs$Site.Code, FUN=max)##aggregate is your new friend
SiteCovs$Tier<-(tiers[,2])

Watershed <- aggregate(phibs$Watershed ~ phibs$Site.Code, FUN=max)##aggregate is your new friend
SiteCovs$Watershed<-(Watershed[,2])

###########################################
habitat<-read.csv("Habitat.csv")
habitat<-habitat[which(habitat$Year=="2016"),]
habitat<-habitat[which(habitat$Survey.Type=="GSR"|habitat$Survey.Type=="GSR_SSS"),]
###########################################

Gradient<-aggregate(habitat$Ave.Grad ~ unique(phibs$Site.Code), FUN=max)
SiteCovs$Gradient<-(Gradient[,2])
#Dont think aggregate works with missing values

Sediment<-aggregate(habitat$Per.Sed ~ unique(phibs$Site.Code),FUN=max)
SiteCovs$Sediment<-(Sediment[,2])

Coarse<-aggregate(habitat$Per.Coarse ~ unique(phibs$Site.Code),FUN=max)
SiteCovs$Coarse<-(Coarse[,2])

Depth<-aggregate(habitat$Ave.Depth ~ unique(phibs$Site.Code),FUN=max)
SiteCovs$Depth<-(Depth[,2])

WW<-aggregate(habitat$Ave.WW ~ unique(phibs$Site.Code),FUN=max)
SiteCovs$WW<-(WW[,2])

Over<-aggregate(habitat$Per.Over ~ unique(phibs$Site.Code),FUN=max)
SiteCovs$Over<-(Over[,2])

write.csv(SiteCovs,file="SiteCovs.csv")

################################################################
#Observation Covariates
################################################################
phibs <- read.csv("Amphib.csv") #read in data
phibs<-phibs[which(phibs$VES == 0 & phibs$Survey==1|phibs$Survey==2),]

sites <- unique(phibs$Site.Code) #get list of sites
#length(unique(phibs$SiteCode))

obsCovs <- data.frame(site=sites,Obs.1=rep(NA,length(sites)),Obs.2=rep(NA,length(sites)),
                      Obs.3=rep(NA,length(sites)), Obs.4=rep(NA,length(sites)),Tmax.1=rep(NA,length(sites)),Tmax.2=rep(NA,length(sites)),
                      Tmax.3=rep(NA,length(sites)), Tmax.4=rep(NA,length(sites)),MinOfDay.1=rep(NA,length(sites)),MinOfDay.2=rep(NA,length(sites)),
                      MinOfDay.3=rep(NA,length(sites)),MinOfDay.4=rep(NA,length(sites))) #make empty data frame WITH FOUR COLUMNS NOW

