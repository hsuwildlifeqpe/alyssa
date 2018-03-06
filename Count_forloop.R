##for loop that arranges phib data into occupancy data Dan & Alyssa 12/14/2017


phibs <- read.csv("Amphib.csv") #read in data
phibs<-phibs[which(phibs$VES == 0 & phibs$Survey==1|phibs$Survey==2),]

sites <- unique(phibs$Site.Code) #get list of sites
#length(unique(phibs$SiteCode))

occdata1 <- data.frame(site=sites,y.1=rep(NA,length(sites)),y.2=rep(NA,length(sites)),y.3=rep(NA,length(sites)),y.4=rep(NA,length(sites))) #make empty data frame WITH FOUR COLUMNS NOW

spp <- "DITE"

for(i in 1:length(sites)) {
  
  site <- sites[i] ##get name of current site
  
  curdata <- phibs[which(phibs$Site.Code==site & phibs$Year==2016),] ##get data from current site in 2016
  
  if(sum(curdata$Survey==1 & curdata$Species==spp) > 0) { occdata1$y.1[i] <- sum(curdata$Survey==1 & curdata$Species==spp) }
  if(sum(curdata$Survey==2 & curdata$Species==spp) > 0) { occdata1$y.2[i] <- sum(curdata$Survey==2 & curdata$Species==spp) }
  if((sum(curdata$Survey==1 & curdata$Species==spp)==0) & (sum(curdata$Survey==1 & curdata$Species!=spp) > 0)) { occdata1$y.1[i] <- 0 }
  if((sum(curdata$Survey==2 & curdata$Species==spp)==0) & (sum(curdata$Survey==2 & curdata$Species!=spp) > 0)) { occdata1$y.2[i] <- 0 }
  
  curdata <- phibs[which(phibs$Site.Code==site & phibs$Year==2017),] ##get data from current site in 2016
  
  if(sum(curdata$Survey==1 & curdata$Species==spp) > 0) { occdata1$y.3[i] <- sum(curdata$Survey==1 & curdata$Species==spp) }
  if(sum(curdata$Survey==2 & curdata$Species==spp) > 0) { occdata1$y.4[i] <- sum(curdata$Survey==2 & curdata$Species==spp) }
  if((sum(curdata$Survey==1 & curdata$Species==spp)==0) & (sum(curdata$Survey==1 & curdata$Species!=spp) > 0)) { occdata1$y.3[i] <- 0 }
  if((sum(curdata$Survey==2 & curdata$Species==spp)==0) & (sum(curdata$Survey==2 & curdata$Species!=spp) > 0)) { occdata1$y.4[i] <- 0 }
  
}

table(occdata1$y.1,occdata1$y.2) ##quick look at data
write.csv(occdata1,file="Count_DITE_Robust.csv")
