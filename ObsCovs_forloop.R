
phibs <- read.csv("Amphib.csv") #read in data
phibs<-phibs[which(phibs$VES == 0 & phibs$Survey==1|phibs$Survey==2),]


sites <- unique(phibs$Site.Code) #get list of sites
#length(unique(phibs$SiteCode))

occdata <- data.frame(site=sites,Obs1=rep(NA,length(sites)),Obs2=rep(NA,length(sites)),Obs3=rep(NA,length(sites)),Obs4=rep(NA,length(sites)),
                      Tmax1=rep(NA,length(sites)),Tmax2=rep(NA,length(sites)),Tmax3=rep(NA,length(sites)),Tmax4=rep(NA,length(sites)),MinofDay1=rep(NA,length(sites)),
                      MinofDay2=rep(NA,length(sites)),MinofDay3=rep(NA,length(sites)),MinofDay4=rep(NA,length(sites))) #make empty data frame WITH FOUR COLUMNS NOW


for(i in 1:length(sites)) {
  
  site <- sites[i] ##get name of current site
  
  curdata <- phibs[which(phibs$Site.Code==site & phibs$Year==2016),] ##get data from current site in 2016
  
  if(curdata$Survey==1){ occdata$Obs1[i] <- max(curdata$Observer.1) }
  if(curdata$Survey==2){ occdata$Obs2[i] <- max(curdata$Observer.1) }


  curdata <- phibs[which(phibs$Site.Code==site & phibs$Year==2017),] ##get data from current site in 2017

  if(curdata$Survey==1) { occdata$Obs3[i] <- max(curdata$Observer.1) }
  if(curdata$Survey==2) { occdata$Obs4[i] <- max(curdata$Observer.1) }
}



table(occdata$y.1,occdata$y.2) ##quick look at data
write.csv(occdata,file="Occdata_ASTR_MNSTM.csv")

