##for loop that arranges phib data into occupancy data Dan & Alyssa 12/14/2017

setwd("C:/Users/db1876/Google Drive/alyssa/")

phibs <- read.csv("Amphib.csv") #read in data

phibs <- phibs[-1598,] #GHOST RECORD, if you fix this, remove this

sites <- unique(phibs$SiteCode) #get list of sites

occdata <- data.frame(SiteCode=sites,v1=rep(NA,length(sites)),v2=rep(NA,length(sites)),v3=rep(NA,length(sites)),v4=rep(NA,length(sites))) #make empty data frame WITH FOUR COLUMNS NOW

spp <- "DITE"

for(i in 1:length(sites)) {

	site <- sites[i] ##get name of current site

	curdata <- phibs[which(phibs$SiteCode==site & phibs$Year==2016),] ##get data from current site in 2016

	if(sum(curdata$Survey==1 & curdata$Species==spp) > 0) { occdata$v1[i] <- 1 }
	if(sum(curdata$Survey==2 & curdata$Species==spp) > 0) { occdata$v2[i] <- 1 }
	if((sum(curdata$Survey==1 & curdata$Species==spp)==0) & (sum(curdata$Survey==1 & curdata$Species!=spp) > 0)) { occdata$v1[i] <- 0 }
	if((sum(curdata$Survey==2 & curdata$Species==spp)==0) & (sum(curdata$Survey==2 & curdata$Species!=spp) > 0)) { occdata$v2[i] <- 0 }

	curdata <- phibs[which(phibs$SiteCode==site & phibs$Year==2017),] ##get data from current site in 2016

	if(sum(curdata$Survey==1 & curdata$Species==spp) > 0) { occdata$v3[i] <- 1 }
	if(sum(curdata$Survey==2 & curdata$Species==spp) > 0) { occdata$v4[i] <- 1 }
	if((sum(curdata$Survey==1 & curdata$Species==spp)==0) & (sum(curdata$Survey==1 & curdata$Species!=spp) > 0)) { occdata$v3[i] <- 0 }
	if((sum(curdata$Survey==2 & curdata$Species==spp)==0) & (sum(curdata$Survey==2 & curdata$Species!=spp) > 0)) { occdata$v4[i] <- 0 }

}

table(occdata$v1,occdata$v2) ##quick look at data

tiers <- aggregate(Tier ~ SiteCode, data=phibs, FUN=max)  ##aggregate is your new friend

occdata <- merge(x=tiers, y=occdata, by="SiteCode", all=FALSE)  ##and merge is its buddy

write.csv("occdata.csv")
