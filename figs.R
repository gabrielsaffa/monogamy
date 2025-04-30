##########################################################################
## this script generates Figs 1-3 in the main text and Figs 1-21 in the SM


setwd("")

library(terra)
library(tidyr)
library(dplyr)
library(grDevices)
library(plotrix)
library(rethinking)


## Figure 1 (maps)
mono_data <- read.csv("mono_data.csv",header=TRUE)
MP <- ifelse(mono_data$normative_monogamy==1,1,0)
MP <- as.integer(MP)
PP <- mono_data$percent_polygyny_women
long <- mono_data$longitude
lat <- mono_data$latitude
data <- data.frame(cbind(PP,long,lat))
data <- data[complete.cases(data$PP),]

## load the raster map downloaded from: https://www.naturalearthdata.com/downloads/10m-raster-data/10m-natural-earth-2/
## continuous marriage patterns
par(mfrow=c(2,1),mar=c(1.1,1.1,1.1,1.1))
map <- rast("NE2_LR_LC_SR_W_DR.tif")
terra::plot(map,axes=FALSE,legend=FALSE,mar=c(0,0,0,0),xlim=c(-190,190),ylim=c(-60,90),maxcell=8100*16200)
bg_con <- hcl.colors(length(unique(data$PP)),"YlOrBr")
points(data$long,data$lat,pch=21,bg=bg_con[as.factor(data$PP)],cex=1.5)
color.legend(-42,-47,62,-52,legend=c("0","100"),rect.col=rev(bg_con),align="rt",xpd=NA)
mtext("A",3,line=-0.5,adj=0.03,cex=1.5)
text(10.5,-42,"% women monogamously married",cex=1,xpd=NA)

## binary marriage patterns
terra::plot(map,axes=FALSE,legend=FALSE,mar=c(0,0,0,0),xlim=c(-190,190),ylim=c(-60,90),maxcell=8100*16200)
bg_bin <- c("gray85","#682714")
points(mono_data$longitude,mono_data$latitude,pch=21,bg=bg_bin[as.factor(MP)],cex=1.5)
legend(-32,-42,legend=c("present","absent"),pch=16,col=c("#682714","gray85"),bty="n",box.col=NA,ncol=2,cex=1,xpd=NA)
mtext("B",3,line=-0.5,adj=0.03,cex=1.5)
text(5,-42,"prescribed monogamy",cex=1,xpd=NA)
dev.off()


## Figure 3
## continuous marriage patterns
af_con <- read.csv("af_con_post.csv",header=TRUE)
tr_con <- read.csv("tr_con_post.csv",header=TRUE)
mm_con <- read.csv("mm_con_post.csv",header=TRUE)
lp_con <- read.csv("lp_con_post.csv",header=TRUE)
ss_con <- read.csv("ss_con_post.csv",header=TRUE)
pd_con <- read.csv("pd_con_post.csv",header=TRUE)
al_con <- read.csv("al_con_post.csv",header=TRUE)
ic_con <- read.csv("ic_con_post.csv",header=TRUE)

## simulate frequency distributions of marriage patterns under each model and across different values of each predictor
{
  
  nr <- 2000
  par(mfrow=c(4,2),mar=c(5.1,6.1,2.6,3.1))
  
  ## assault frequency
  af_sim_min_sd <- c()
  af_sim_plus_sd <- c()
  for(i in 1:nr) {
    af_sim_min_sd[i] <- rnorm(1,af_con$mu_a[i] + af_con$mu_bPP[i]*-2 + af_con$mu_bPI[i]*1,af_con$sigma_phy)
    af_sim_plus_sd[i] <- rnorm(1,af_con$mu_a[i] + af_con$mu_bPP[i]*2 + af_con$mu_bPI[i]*1,af_con$sigma_phy)
  }
  
  hist(af_sim_min_sd,xlab="assault frequency (SD)",ylab="frequency",main="",breaks=14,xlim=c(-2,2),ylim=c(0,500),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(af_sim_plus_sd,breaks=14,col=col.alpha("#5E8C7B",0.6),add=TRUE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2.0","-1.0","0.0","1.0","2.0"),cex.axis=1.5,tick=FALSE)
  mtext("A",3,line=0.25,adj=0)
  legend(-0.5,500,legend=c("+2SD monogamous women","-2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## trade
  tr_sim_0 <- c()
  tr_sim_1 <- c()
  for(i in 1:nr) {
    tr_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(tr_con$mu_a[i] + tr_con$mu_bPP[i]*-2 + tr_con$mu_bPI[i]*1))
    tr_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(tr_con$mu_a[i] + tr_con$mu_bPP[i]*2 + tr_con$mu_bPI[i]*1))
  }
  
  tr_sim <- gather(data.frame(tr_sim_0,tr_sim_1))
  barplot(table(tr_sim),xlim=c(0,8),width=0.8,xlab=" intercommunity trade",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(3,1600,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)  
  
  
  ## military mobilization
  mm_sim_0 <- c()
  mm_sim_1 <- c()
  for(i in 1:nr) {
    mm_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(mm_con$mu_a[i] + mm_con$mu_bPP[i]*-2 + mm_con$mu_bPI[i]*1))
    mm_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(mm_con$mu_a[i] + mm_con$mu_bPP[i]*2 + mm_con$mu_bPI[i]*1))
  }
  
  mm_sim <- gather(data.frame(mm_sim_0,mm_sim_1))
  barplot(table(mm_sim),xlim=c(0,8),width=0.8,xlab="military organization",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("C",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## land privatization
  lp_sim_min_sd <- c()
  lp_sim_plus_sd <- c()
  for(i in 1:nr) {
    lp_sim_min_sd[i] <- rbeta2(1,logistic(lp_con$mu_a[i] + lp_con$mu_bLP[i]*-2 + lp_con$mu_bIA[i]*1),lp_con$theta)
    lp_sim_plus_sd[i] <- rbeta2(1,logistic(lp_con$mu_a[i] + lp_con$mu_bLP[i]*2 + lp_con$mu_bIA[i]*1),lp_con$theta)
  }
  
  hist(lp_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1500),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(lp_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("D",3,line=0.25,adj=0)
  legend(0.1,1500,legend=c("-2SD land privatization","+2SD land privatization"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## social stratification
  ss_sim_0 <- c()
  ss_sim_1 <- c()
  for(i in 1:nr) {
    ss_sim_0[i] <- rbeta2(1,logistic(ss_con$mu_a[i] + ss_con$mu_bSS[i]*0 + ss_con$mu_bLP[i]*0 + ss_con$mu_bIA[i]*1),ss_con$theta)
    ss_sim_1[i] <- rbeta2(1,logistic(ss_con$mu_a[i] + ss_con$mu_bSS[i]*1 + ss_con$mu_bLP[i]*0 + ss_con$mu_bIA[i]*1),ss_con$theta)
  }
  
  hist(ss_sim_0,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1200),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(ss_sim_1,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("E",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("low stratification","high stratification"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## population density
  pd_sim_0 <- c()
  pd_sim_1 <- c()
  for(i in 1:nr) {
    pd_sim_0[i] <- rbeta2(1,logistic(pd_con$mu_a[i] + pd_con$mu_bPD[i]*0 + pd_con$mu_bIA[i]*1),pd_con$theta)
    pd_sim_1[i] <- rbeta2(1,logistic(pd_con$mu_a[i] + pd_con$mu_bPD[i]*1 + pd_con$mu_bIA[i]*1),pd_con$theta)
  }
  
  hist(pd_sim_0,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1200),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(pd_sim_1,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("F",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("low population density","high population density"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## altitude
  al_sim_min_sd <- c()
  al_sim_plus_sd <- c()
  for(i in 1:nr) {
    al_sim_min_sd[i] <- rbeta2(1,logistic(al_con$mu_a[i] + al_con$mu_bAL[i]*-2 + al_con$mu_bIC[i]*0),al_con$theta)
    al_sim_plus_sd[i] <- rbeta2(1,logistic(al_con$mu_a[i] + al_con$mu_bAL[i]*2 + al_con$mu_bIC[i]*0),al_con$theta)
  }
  
  hist(al_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1200),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(al_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("G",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("-2SD altitude","+2SD altitude"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## incline
  ic_sim_min_sd <- c()
  ic_sim_plus_sd <- c()
  for(i in 1:nr) {
    ic_sim_min_sd[i] <- rbeta2(1,logistic(ic_con$mu_a[i] + ic_con$mu_bIC[i]*-2),ic_con$theta)
    ic_sim_plus_sd[i] <- rbeta2(1,logistic(ic_con$mu_a[i] + ic_con$mu_bIC[i]*2),ic_con$theta)
  }
  
  hist(al_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1200),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(al_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("H",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("-2SD incline","+2SD incline"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
}


## Figure 4
## binary marriage patterns
af_bin <- read.csv("af_bin_post.csv",header=TRUE)
tr_bin <- read.csv("tr_bin_post.csv",header=TRUE)
mm_bin <- read.csv("mm_bin_post.csv",header=TRUE)
lp_bin <- read.csv("lp_bin_post.csv",header=TRUE)
ss_bin <- read.csv("ss_bin_post.csv",header=TRUE)
pd_bin <- read.csv("pd_bin_post.csv",header=TRUE)
al_bin <- read.csv("al_bin_post.csv",header=TRUE)
ic_bin <- read.csv("ic_bin_post.csv",header=TRUE)


## simulate frequency distributions of marriage patterns under each model and across different values of each predictor
{
  
  par(mfrow=c(4,2),mar=c(5.1,6.1,2.6,3.1))
  names_mp <- c("absent","present")
  
  ## assault frequency
  af_sim_min_sd <- c()
  af_sim_plus_sd <- c()
  for(i in 1:nr) {
    af_sim_min_sd[i] <- rnorm(1,af_bin$mu_a[i] + af_bin$mu_bMP[i]*0 + af_bin$mu_bPI[i]*1,af_bin$sigma_phy)
    af_sim_plus_sd[i] <- rnorm(1,af_bin$mu_a[i] + af_bin$mu_bMP[i]*1 + af_bin$mu_bPI[i]*1,af_bin$sigma_phy)
  }
  
  hist(af_sim_min_sd,xlab="assault frequency (SD)",ylab="frequency",main="",breaks=14,xlim=c(-2,2),ylim=c(0,500),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(af_sim_plus_sd,breaks=14,col=col.alpha("#5E8C7B",0.6),add=TRUE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2.0","-1.0","0.0","1.0","2.0"),cex.axis=1.5,tick=FALSE)
  mtext("A",3,line=0.25,adj=0)
  legend(-0.5,500,legend=c("monogamy absent","monogamy present"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## trade
  tr_sim_0 <- c()
  tr_sim_1 <- c()
  for(i in 1:nr) {
    tr_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(tr_bin$mu_a[i] + tr_bin$mu_bMP[i]*0 + tr_bin$mu_bPI[i]*1))
    tr_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(tr_bin$mu_a[i] + tr_bin$mu_bMP[i]*1 + tr_bin$mu_bPI[i]*1))
  }
  
  tr_sim <- gather(data.frame(tr_sim_0,tr_sim_1))
  barplot(table(tr_sim),xlim=c(0,8),width=0.8,xlab=" intercommunity trade",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(3,1600,legend=c("monogamy absent","monogamy present"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)  
  
  
  ## military mobilization
  mm_sim_0 <- c()
  mm_sim_1 <- c()
  for(i in 1:nr) {
    mm_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(mm_bin$mu_a[i] + mm_bin$mu_bMP[i]*0 + mm_bin$mu_bPI[i]*1))
    mm_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(mm_bin$mu_a[i] + mm_bin$mu_bMP[i]*1 + mm_bin$mu_bPI[i]*1))
  }
  
  mm_sim <- gather(data.frame(mm_sim_0,mm_sim_1))
  barplot(table(mm_sim),xlim=c(0,8),width=0.8,xlab="military organization",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("C",3,line=0.25,adj=0)
  legend(3,1200,legend=c("monogamy absent","monogamy present"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## land privatization
  lp_sim_min_sd <- c()
  lp_sim_plus_sd <- c()
  for(i in 1:nr) {
    lp_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(lp_bin$mu_a[i] + lp_bin$mu_bLP[i]*-2 + lp_bin$mu_bIA[i]*1))
    lp_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(lp_bin$mu_a[i] + lp_bin$mu_bLP[i]*2 + lp_bin$mu_bIA[i]*1))
  }
  
  lp_sim <- gather(data.frame(lp_sim_min_sd,lp_sim_plus_sd))
  barplot(table(lp_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("D",3,line=0.25,adj=0)
  legend(3,1700,legend=c("-2SD land privatization","+2SD land privatization"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## social stratification
  ss_sim_0 <- c()
  ss_sim_1 <- c()
  for(i in 1:nr) {
    ss_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(ss_bin$mu_a[i] + ss_bin$mu_bSS[i]*0 + ss_bin$mu_bLP[i]*0 + ss_bin$mu_bIA[i]*1))
    ss_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(ss_bin$mu_a[i] + ss_bin$mu_bSS[i]*1 + ss_bin$mu_bLP[i]*0 + ss_bin$mu_bIA[i]*1))
  }
  
  ss_sim <- gather(data.frame(ss_sim_0,ss_sim_1))
  barplot(table(ss_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("E",3,line=0.25,adj=0)
  legend(3,1700,legend=c("low stratification","high stratification"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## population density
  pd_sim_0 <- c()
  pd_sim_1 <- c()
  for(i in 1:nr) {
    pd_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(pd_bin$mu_a[i] + pd_bin$mu_bPD[i]*0 + pd_bin$mu_bIA[i]*1))
    pd_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(pd_bin$mu_a[i] + pd_bin$mu_bPD[i]*1 + pd_bin$mu_bIA[i]*1))
  }
  
  pd_sim <- gather(data.frame(pd_sim_0,pd_sim_1))
  barplot(table(pd_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("F",3,line=0.25,adj=0)
  legend(3,1700,legend=c("low population density","high population density"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## altitude
  al_sim_min_sd <- c()
  al_sim_plus_sd <- c()
  for(i in 1:nr) {
    al_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(al_bin$mu_a[i] + al_bin$mu_bAL[i]*-2))
    al_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(al_bin$mu_a[i] + al_bin$mu_bAL[i]*2))
  }
  
  al_sim <- gather(data.frame(al_sim_min_sd,al_sim_plus_sd))
  barplot(table(al_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("G",3,line=0.25,adj=0)
  legend(3,1700,legend=c("-2SD altitude","+2SD altitude"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## incline
  ic_sim_min_sd <- c()
  ic_sim_plus_sd <- c()
  for(i in 1:nr) {
    ic_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(ic_bin$mu_a[i] + ic_bin$mu_bIC[i]*-2))
    ic_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(ic_bin$mu_a[i] + ic_bin$mu_bIC[i]*2))
  }
  
  ic_sim <- gather(data.frame(ic_sim_min_sd,ic_sim_plus_sd))
  barplot(table(ic_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("H",3,line=0.25,adj=0)
  legend(3,1700,legend=c("-2SD incline","+2SD incline"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
}


dev.off()


## Supplementary Figures
af_int_con <- read.csv("af_int_con_post.csv",header=TRUE)
tr_int_con <- read.csv("tr_int_con_post.csv",header=TRUE)
mm_int_con <- read.csv("mm_int_con_post.csv",header=TRUE)
pd_int_con <- read.csv("pd_int_con_post.csv",header=TRUE)
af_int_bin <- read.csv("af_int_bin_post.csv",header=TRUE)
tr_int_bin <- read.csv("tr_int_bin_post.csv",header=TRUE)
mm_int_bin <- read.csv("mm_int_bin_post.csv",header=TRUE)
pd_int_bin <- read.csv("pd_int_bin_post.csv",header=TRUE)
tr_af <- read.csv("tr_af_post.csv",header=TRUE)
mm_af <- read.csv("mm_af_post.csv",header=TRUE)
lp_pd <- read.csv("lp_pd_post.csv",header=TRUE)
lp_al <- read.csv("lp_al_post.csv",header=TRUE)
lp_ic <- read.csv("lp_ic_post.csv",header=TRUE)


## Figure 1 (continuous marriage patterns)
## assault frequency
{
  par(mfrow=c(3,1),mar=c(5.1,6.1,2.6,3.1))
  cols <- c("gray75","#5E8C7B")
  af_int_1 <- af_int_con$bPP.1
  af_int_2 <- af_int_con$bPP.2
  af_int_mu <- c(mean(af_int_1),mean(af_int_2))
  af_int_low <- as.numeric(c(PI(af_int_1,0.9)[1],PI(af_int_2,0.9)[1]))
  af_int_upp <- as.numeric(c(PI(af_int_1,0.9)[2],PI(af_int_2,0.9)[2]))
  plot(NA,xlim=c(-2,2),ylim=c(-0.5,1.5),xlab="assault frequency (SD)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2","-1","0","1","2"),cex.axis=2)
  mtext("A",3,line=-0.5,adj=0,cex=2)
  text(-1.5,1,"-0.33 [-0.52, -0.15]",cex=2.5,col="gray75")
  text(-1.5,0.7,"0.05 [-0.62, 0.69]",cex=2.5,col="#5E8C7B")
  points(af_int_mu,1:0,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:2) {
  lines(c(af_int_low[i],af_int_upp[i]),rep(c(1:0)[i],2),lwd=5,col=cols[i])
    }
  abline(v=0,lty=2,lwd=5,col="gray75")
  legend(0.25,1.5,legend=c("low social complexity","high social complexity (large states)"),col=cols,pch=21,lwd=5,bty="n",box.col=NA,cex=2,xpd=NA)

## trade
  tr_int_1 <- tr_int_con$bPP.1
  tr_int_2 <- tr_int_con$bPP.2
  tr_int_mu <- c(mean(tr_int_1),mean(tr_int_2))
  tr_int_low <- as.numeric(c(PI(tr_int_1,0.9)[1],PI(tr_int_2,0.9)[1]))
  tr_int_upp <- as.numeric(c(PI(tr_int_1,0.9)[2],PI(tr_int_2,0.9)[2]))
  plot(NA,xlim=c(-2,2),ylim=c(-0.5,1.5),xlab="log odds(high trade)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2","-1","0","1","2"),cex.axis=2)
  mtext("B",3,line=-0.5,adj=0,cex=2)
  text(-1.5,1,"0.04 [-0.54, 0.63]",cex=2.5,col="gray75")
  text(-1.5,0.7,"0.18 [-0.64, 1.01]",cex=2.5,col="#5E8C7B")
  points(tr_int_mu,1:0,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:2) {
    lines(c(tr_int_low[i],tr_int_upp[i]),rep(c(1:0)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=5,col="gray75")

## military organization
  mm_int_1 <- mm_int_con$bPP.1
  mm_int_2 <- mm_int_con$bPP.2
  mm_int_mu <- c(mean(mm_int_1),mean(mm_int_2))
  mm_int_low <- as.numeric(c(PI(mm_int_1,0.9)[1],PI(mm_int_2,0.9)[1]))
  mm_int_upp <- as.numeric(c(PI(mm_int_1,0.9)[2],PI(mm_int_2,0.9)[2]))
  plot(NA,xlim=c(-2,2),ylim=c(-0.5,1.5),xlab="log odds(high military)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2","-1","0","1","2"),cex.axis=2)
  mtext("C",3,line=-0.5,adj=0,cex=2)
  text(-1.5,1,"-0.29 [-0.84, 0.29]",cex=2.5,col="gray75")
  text(-1.5,0.7,"0.38 [-0.45, 1.18]",cex=2.5,col="#5E8C7B")
  points(mm_int_mu,1:0,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:2) {
    lines(c(mm_int_low[i],mm_int_upp[i]),rep(c(1:0)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=5,col="gray75")
}


## Figure 2 (binary marriage patterns)
## assault frequency
{
  par(mfrow=c(3,1),mar=c(5.1,6.1,2.6,3.1))
  cols <- c("gray75","#5E8C7B")
  af_int_1 <- af_int_bin$bMP.1
  af_int_2 <- af_int_bin$bMP.2
  af_int_mu <- c(mean(af_int_1),mean(af_int_2))
  af_int_low <- as.numeric(c(PI(af_int_1,0.9)[1],PI(af_int_2,0.9)[1]))
  af_int_upp <- as.numeric(c(PI(af_int_1,0.9)[2],PI(af_int_2,0.9)[2]))
  plot(NA,xlim=c(-2,2),ylim=c(-0.5,1.5),xlab="assault frequency (SD)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2","-1","0","1","2"),cex.axis=2)
  mtext("A",3,line=-0.5,adj=0,cex=2)
  text(-1.5,1,"-0.21 [-0.42, 0.00]",cex=2.5,col="gray75")
  text(-1.5,0.7,"0.12 [-0.29, 0.55]",cex=2.5,col="#5E8C7B")
  points(af_int_mu,1:0,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:2) {
    lines(c(af_int_low[i],af_int_upp[i]),rep(c(1:0)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=5,col="gray75")
  legend(0.25,1.5,legend=c("low social complexity","high social complexity (large states)"),col=cols,pch=21,lwd=5,bty="n",box.col=NA,cex=2,xpd=NA)
  
  ## trade
  tr_int_1 <- tr_int_bin$bMP.1
  tr_int_2 <- tr_int_bin$bMP.2
  tr_int_mu <- c(mean(tr_int_1),mean(tr_int_2))
  tr_int_low <- as.numeric(c(PI(tr_int_1,0.9)[1],PI(tr_int_2,0.9)[1]))
  tr_int_upp <- as.numeric(c(PI(tr_int_1,0.9)[2],PI(tr_int_2,0.9)[2]))
  plot(NA,xlim=c(-2,2),ylim=c(-0.5,1.5),xlab="log odds(high trade)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2","-1","0","1","2"),cex.axis=2)
  mtext("B",3,line=-0.5,adj=0,cex=2)
  text(-1.5,1,"0.32 [-0.30, 0.93]",cex=2.5,col="gray75")
  text(-1.5,0.7,"0.47 [-0.25, 1.18]",cex=2.5,col="#5E8C7B")
  points(tr_int_mu,1:0,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:2) {
    lines(c(tr_int_low[i],tr_int_upp[i]),rep(c(1:0)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=5,col="gray75")
  
  ## military organization
  mm_int_1 <- mm_int_bin$bMP.1
  mm_int_2 <- mm_int_bin$bMP.2
  mm_int_mu <- c(mean(mm_int_1),mean(mm_int_2))
  mm_int_low <- as.numeric(c(PI(mm_int_1,0.9)[1],PI(mm_int_2,0.9)[1]))
  mm_int_upp <- as.numeric(c(PI(mm_int_1,0.9)[2],PI(mm_int_2,0.9)[2]))
  plot(NA,xlim=c(-2,2),ylim=c(-0.5,1.5),xlab="log odds(high military)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2","-1","0","1","2"),cex.axis=2)
  mtext("C",3,line=-0.5,adj=0,cex=2)
  text(-1.5,1,"0.73 [0.06, 1.37]",cex=2.5,col="gray75")
  text(-1.5,0.7,"0.66 [-0.08, 1.42]",cex=2.5,col="#5E8C7B")
  points(mm_int_mu,1:0,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:2) {
    lines(c(mm_int_low[i],mm_int_upp[i]),rep(c(1:0)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=5,col="gray75")
}


## Figure 3 (trade ~ assault frequency)
{
  par(mfrow=c(3,1),mar=c(5.1,6.1,2.6,3.1))
  tr_af_1 <- tr_af$bAF.1
  tr_af_2 <- tr_af$bAF.2
  tr_af_mu <- c(mean(tr_af_1),mean(tr_af_2))
  tr_af_low <- as.numeric(c(PI(tr_af_1,0.9)[1],PI(tr_af_2,0.9)[1]))
  tr_af_upp <- as.numeric(c(PI(tr_af_1,0.9)[2],PI(tr_af_2,0.9)[2]))
  plot(NA,xlim=c(-2,2),ylim=c(-0.5,1.5),xlab="log odds(trade)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2","-1","0","1","2"),cex.axis=2)
  text(-1.5,1,"-0.27 [-0.83, 0.29]",cex=2.5,col="gray75")
  text(-1.5,0.7,"-0.12 [-0.96, 0.70]",cex=2.5,col="#5E8C7B")
  points(tr_af_mu,1:0,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:2) {
    lines(c(tr_af_low[i],tr_af_upp[i]),rep(c(1:0)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=5,col="gray75")
  legend(0.25,1.5,legend=c("low social complexity","high social complexity (large states)"),col=cols,pch=21,lwd=5,bty="n",box.col=NA,cex=2,xpd=NA)
}


## Figure 4 (military organization ~ assault frequency)
{
  par(mfrow=c(3,1),mar=c(5.1,6.1,2.6,3.1))
  mm_af_1 <- mm_af$bAF.1
  mm_af_2 <- mm_af$bAF.2
  mm_af_mu <- c(mean(mm_af_1),mean(mm_af_2))
  mm_af_low <- as.numeric(c(PI(mm_af_1,0.9)[1],PI(mm_af_2,0.9)[1]))
  mm_af_upp <- as.numeric(c(PI(mm_af_1,0.9)[2],PI(mm_af_2,0.9)[2]))
  plot(NA,xlim=c(-2,2),ylim=c(-0.5,1.5),xlab="log odds(military organization)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2","-1","0","1","2"),cex.axis=2)
  text(-1.5,1,"-0.24 [-0.84, 0.38]",cex=2.5,col="gray75")
  text(-1.5,0.7,"0.14 [-0.69, 0.95]",cex=2.5,col="#5E8C7B")
  points(mm_af_mu,1:0,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:2) {
    lines(c(mm_af_low[i],mm_af_upp[i]),rep(c(1:0)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=5,col="gray75")
  legend(0.25,1.5,legend=c("low social complexity","high social complexity (large states)"),col=cols,pch=21,lwd=5,bty="n",box.col=NA,cex=2,xpd=NA)
}


## Figure 5 (continuous marriage patterns ~ population density)
{
  par(mfrow=c(3,1),mar=c(5.1,6.1,2.6,3.1))
  cols <- c("gray75","#784617")
  pd_int_1 <- pd_int_con$bPD.1
  pd_int_2 <- pd_int_con$bPD.2
  pd_int_mu <- c(mean(pd_int_1),mean(pd_int_2))
  pd_int_low <- as.numeric(c(PI(pd_int_1,0.9)[1],PI(pd_int_2,0.9)[1]))
  pd_int_upp <- as.numeric(c(PI(pd_int_1,0.9)[2],PI(pd_int_2,0.9)[2]))
  plot(NA,xlim=c(-2,2),ylim=c(-0.5,1.5),xlab="log odds(women married monogamously)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2","-1","0","1","2"),cex.axis=2)
  text(-1.5,1,"-0.10 [-0.61, 0.43]",cex=2.5,col="gray75")
  text(-1.5,0.7,"0.36 [-0.05, 0.78]",cex=2.5,col="#784617")
  points(pd_int_mu,1:0,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:2) {
    lines(c(pd_int_low[i],pd_int_upp[i]),rep(c(1:0)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=5,col="gray75")
  legend(0.25,1.5,legend=c("intensive agriculture absent","intensive agriculture present"),col=cols,pch=21,lwd=5,bty="n",box.col=NA,cex=2,xpd=NA)
}


## Figure 6 (binary marriage patterns ~ population density)
{
  par(mfrow=c(3,1),mar=c(5.1,6.1,2.6,3.1))
  cols <- c("gray75","#784617")
  pd_int_1 <- pd_int_bin$bPD.1
  pd_int_2 <- pd_int_bin$bPD.2
  pd_int_mu <- c(mean(pd_int_1),mean(pd_int_2))
  pd_int_low <- as.numeric(c(PI(pd_int_1,0.9)[1],PI(pd_int_2,0.9)[1]))
  pd_int_upp <- as.numeric(c(PI(pd_int_1,0.9)[2],PI(pd_int_2,0.9)[2]))
  plot(NA,xlim=c(-2,2),ylim=c(-0.5,1.5),xlab="log odds(monogamy)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2","-1","0","1","2"),cex.axis=2)
  text(-1.5,1,"-0.14 [-0.84, 0.58]",cex=2.5,col="gray75")
  text(-1.5,0.7,"0.49 [-0.14, 1.11]",cex=2.5,col="#784617")
  points(pd_int_mu,1:0,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:2) {
    lines(c(pd_int_low[i],pd_int_upp[i]),rep(c(1:0)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=5,col="gray75")
  legend(0.25,1.5,legend=c("intensive agriculture absent","intensive agriculture present"),col=cols,pch=21,lwd=5,bty="n",box.col=NA,cex=2,xpd=NA)
}


## Figure 7 (land privatization)
## population density
{
  par(mfrow=c(3,1),mar=c(5.1,6.1,2.6,3.1))
  lp_pd <- lp_pd$bPD
  lp_pd_mu <- mean(lp_pd)
  lp_pd_low <- as.numeric(PI(lp_pd,0.9)[1])
  lp_pd_upp <- as.numeric(PI(lp_pd,0.9)[2])
  plot(NA,xlim=c(-0.5,0.5),ylim=c(0,1),xlab="land privatization (SD)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-0.5,0.5,length.out=5),labels=c("-0.5","-0.25","0","0.25","0.5"),cex.axis=2)
  mtext("A",3,line=-0.5,adj=0,cex=2)
  text(-0.25,0.75,"0.21 [0.06, 0.37]",cex=2.5,col="#784617")
  points(lp_pd_mu,0.5,col="#784617",pch=1,cex=5,lwd=5)
  lines(c(lp_pd_low,lp_pd_upp),rep(c(0.5:0.5),2),lwd=5,col="#784617")
  abline(v=0,lty=2,lwd=5,col="gray75")

  
  ## altitude
  lp_al <- lp_al$bAL
  lp_al_mu <- mean(lp_al)
  lp_al_low <- as.numeric(PI(lp_al,0.9)[1])
  lp_al_upp <- as.numeric(PI(lp_al,0.9)[2])
  plot(NA,xlim=c(-0.5,0.5),ylim=c(0,1),xlab="land privatization (SD)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-0.5,0.5,length.out=5),labels=c("-0.5","-0.25","0","0.25","0.5"),cex.axis=2)
  mtext("B",3,line=-0.5,adj=0,cex=2)
  text(-0.25,0.75,"0.05 [-0.11, 0.22]",cex=2.5,col="#784617")
  points(lp_al_mu,0.5,col="#784617",pch=1,cex=5,lwd=5)
  lines(c(lp_al_low,lp_al_upp),rep(c(0.5:0.5),2),lwd=5,col="#784617")
  abline(v=0,lty=2,lwd=5,col="gray75")
  
  ## incline
  lp_ic <- lp_ic$bIC
  lp_ic_mu <- mean(lp_ic)
  lp_ic_low <- as.numeric(PI(lp_ic,0.9)[1])
  lp_ic_upp <- as.numeric(PI(lp_ic,0.9)[2])
  plot(NA,xlim=c(-0.5,0.5),ylim=c(0,1),xlab="land privatization (SD)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-0.5,0.5,length.out=5),labels=c("-0.5","-0.25","0","0.25","0.5"),cex.axis=2)
  mtext("C",3,line=-0.5,adj=0,cex=2)
  text(-0.25,0.75,"0.09 [-0.03, 0.21]",cex=2.5,col="#784617")
  points(lp_ic_mu,0.5,col="#784617",pch=1,cex=5,lwd=5)
  lines(c(lp_ic_low,lp_ic_upp),rep(c(0.5:0.5),2),lwd=5,col="#784617")
  abline(v=0,lty=2,lwd=5,col="gray75")
}


dev.off()


## simulate frequency distributions of marriage patterns under each model and across different values of each predictor
af_con <- read.csv("af_con_post.csv",header=TRUE)
tr_con <- read.csv("tr_con_post.csv",header=TRUE)
mm_con <- read.csv("mm_con_post.csv",header=TRUE)
lp_con <- read.csv("lp_con_post.csv",header=TRUE)
ss_con <- read.csv("ss_con_post.csv",header=TRUE)
pd_con <- read.csv("pd_con_post.csv",header=TRUE)
al_con <- read.csv("al_con_post.csv",header=TRUE)
ic_con <- read.csv("ic_con_post.csv",header=TRUE)
af_bin <- read.csv("af_bin_post.csv",header=TRUE)
tr_bin <- read.csv("tr_bin_post.csv",header=TRUE)
mm_bin <- read.csv("mm_bin_post.csv",header=TRUE)
lp_bin <- read.csv("lp_bin_post.csv",header=TRUE)
ss_bin <- read.csv("ss_bin_post.csv",header=TRUE)
pd_bin <- read.csv("pd_bin_post.csv",header=TRUE)
al_bin <- read.csv("al_bin_post.csv",header=TRUE)
ic_bin <- read.csv("ic_bin_post.csv",header=TRUE)


## Figure 8 (Sino-Tibetans, continuous)
{
  
  nr <- 2000
  par(mfrow=c(4,2),mar=c(5.1,6.1,2.6,3.1))
  
  ## assault frequency
  af_sim_min_sd <- c()
  af_sim_plus_sd <- c()
  for(i in 1:nr) {
    af_sim_min_sd[i] <- rnorm(1,af_con$a.54[i] + af_con$bPP.54[i]*-2 + af_con$bPI.54[i]*1,af_con$sigma_phy)
    af_sim_plus_sd[i] <- rnorm(1,af_con$a.54[i] + af_con$bPP.54[i]*2 + af_con$bPI.54[i]*1,af_con$sigma_phy)
  }
  
  hist(af_sim_min_sd,xlab="assault frequency (SD)",ylab="frequency",main="",breaks=20,xlim=c(-3,3),ylim=c(0,800),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(af_sim_plus_sd,breaks=20,col=col.alpha("#5E8C7B",0.6),add=TRUE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2.0","-1.0","0.0","1.0","2.0"),cex.axis=1.5,tick=FALSE)
  mtext("A",3,line=0.25,adj=0)
  legend(-0.5,800,legend=c("+2SD monogamous women","-2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## trade
  tr_sim_0 <- c()
  tr_sim_1 <- c()
  for(i in 1:nr) {
    tr_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(tr_con$a.54[i] + tr_con$bPP.54[i]*-2 + tr_con$bPI.54[i]*1))
    tr_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(tr_con$a.54[i] + tr_con$bPP.54[i]*2 + tr_con$bPI.54[i]*1))
  }
  
  tr_sim <- gather(data.frame(tr_sim_0,tr_sim_1))
  barplot(table(tr_sim),xlim=c(0,8),width=0.8,xlab=" intercommunity trade",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(3,1000,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)  
  
  
  ## military mobilization
  mm_sim_0 <- c()
  mm_sim_1 <- c()
  for(i in 1:nr) {
    mm_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(mm_con$a.48[i] + mm_con$bPP.48[i]*-2 + mm_con$bPI.48[i]*1))
    mm_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(mm_con$a.48[i] + mm_con$bPP.48[i]*2 + mm_con$bPI.48[i]*1))
  }
  
  mm_sim <- gather(data.frame(mm_sim_0,mm_sim_1))
  barplot(table(mm_sim),xlim=c(0,8),width=0.8,xlab="military organization",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("C",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## land privatization
  lp_sim_min_sd <- c()
  lp_sim_plus_sd <- c()
  for(i in 1:nr) {
    lp_sim_min_sd[i] <- rbeta2(1,logistic(lp_con$a.54[i] + lp_con$bLP.54[i]*-2 + lp_con$bIA.54[i]*1),lp_con$theta)
    lp_sim_plus_sd[i] <- rbeta2(1,logistic(lp_con$a.54[i] + lp_con$bLP.54[i]*2 + lp_con$bIA.54[i]*1),lp_con$theta)
  }
  
  hist(lp_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1600),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(lp_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("D",3,line=0.25,adj=0)
  legend(0.1,1500,legend=c("-2SD land privatization","+2SD land privatization"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## social stratification
  ss_sim_0 <- c()
  ss_sim_1 <- c()
  for(i in 1:nr) {
    ss_sim_0[i] <- rbeta2(1,logistic(ss_con$a.54[i] + ss_con$bSS.54[i]*0 + ss_con$bLP.54[i]*0 + ss_con$bIA.54[i]*1),ss_con$theta)
    ss_sim_1[i] <- rbeta2(1,logistic(ss_con$a.54[i] + ss_con$bSS.54[i]*1 + ss_con$bLP.54[i]*0 + ss_con$bIA.54[i]*1),ss_con$theta)
  }
  
  hist(ss_sim_0,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1200),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(ss_sim_1,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("E",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("low stratification","high stratification"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## population density
  pd_sim_0 <- c()
  pd_sim_1 <- c()
  for(i in 1:nr) {
    pd_sim_0[i] <- rbeta2(1,logistic(pd_con$a.54[i] + pd_con$bPD.54[i]*0 + pd_con$bIA.54[i]*1),pd_con$theta)
    pd_sim_1[i] <- rbeta2(1,logistic(pd_con$a.54[i] + pd_con$bPD.54[i]*1 + pd_con$bIA.54[i]*1),pd_con$theta)
  }
  
  hist(pd_sim_0,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1300),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(pd_sim_1,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("F",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("low population density","high population density"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## altitude
  al_sim_min_sd <- c()
  al_sim_plus_sd <- c()
  for(i in 1:nr) {
    al_sim_min_sd[i] <- rbeta2(1,logistic(al_con$a.54[i] + al_con$bAL.54[i]*-2 + al_con$bIC.54[i]*0),al_con$theta)
    al_sim_plus_sd[i] <- rbeta2(1,logistic(al_con$a.54[i] + al_con$bAL.54[i]*2 + al_con$bIC.54[i]*0),al_con$theta)
  }
  
  hist(al_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1300),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(al_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("G",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("-2SD altitude","+2SD altitude"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## incline
  ic_sim_min_sd <- c()
  ic_sim_plus_sd <- c()
  for(i in 1:nr) {
    ic_sim_min_sd[i] <- rbeta2(1,logistic(ic_con$a.54[i] + ic_con$bIC.54[i]*-2),ic_con$theta)
    ic_sim_plus_sd[i] <- rbeta2(1,logistic(ic_con$a.54[i] + ic_con$bIC.54[i]*2),ic_con$theta)
  }
  
  hist(al_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1300),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(al_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("H",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("-2SD incline","+2SD incline"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
}


## Figure 9 (Sino-Tibetans, binary)
{
  
  par(mfrow=c(4,2),mar=c(5.1,6.1,2.6,3.1))
  names_mp <- c("absent","present")
  
  ## assault frequency
  af_sim_min_sd <- c()
  af_sim_plus_sd <- c()
  for(i in 1:nr) {
    af_sim_min_sd[i] <- rnorm(1,af_bin$a.63[i] + af_bin$bMP.63[i]*-2 + af_bin$bPI.63[i]*1,af_bin$sigma_phy)
    af_sim_plus_sd[i] <- rnorm(1,af_bin$a.63[i] + af_bin$bMP.63[i]*2 + af_bin$bPI.63[i]*1,af_bin$sigma_phy)
  }
  
  hist(af_sim_min_sd,xlab="assault frequency (SD)",ylab="frequency",main="",breaks=20,xlim=c(-3,3),ylim=c(0,800),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(af_sim_plus_sd,breaks=20,col=col.alpha("#5E8C7B",0.6),add=TRUE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2.0","-1.0","0.0","1.0","2.0"),cex.axis=1.5,tick=FALSE)
  mtext("A",3,line=0.25,adj=0)
  legend(-0.5,800,legend=c("+2SD monogamous women","-2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## trade
  tr_sim_0 <- c()
  tr_sim_1 <- c()
  for(i in 1:nr) {
    tr_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(tr_bin$a.63[i] + tr_bin$bMP.63[i]*-2 + tr_bin$bPI.63[i]*1))
    tr_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(tr_bin$a.63[i] + tr_bin$bMP.63[i]*2 + tr_bin$bPI.63[i]*1))
  }
  
  tr_sim <- gather(data.frame(tr_sim_0,tr_sim_1))
  barplot(table(tr_sim),xlim=c(0,8),width=0.8,xlab=" intercommunity trade",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(3,1600,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)  
  
  
  ## military mobilization
  mm_sim_0 <- c()
  mm_sim_1 <- c()
  for(i in 1:nr) {
    mm_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(mm_bin$a.55[i] + mm_bin$bMP.55[i]*-2 + mm_bin$bPI.55[i]*1))
    mm_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(mm_bin$a.55[i] + mm_bin$bMP.55[i]*2 + mm_bin$bPI.55[i]*1))
  }
  
  mm_sim <- gather(data.frame(mm_sim_0,mm_sim_1))
  barplot(table(mm_sim),xlim=c(0,8),width=0.8,xlab="military organization",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("C",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## land privatization
  lp_sim_min_sd <- c()
  lp_sim_plus_sd <- c()
  for(i in 1:nr) {
    lp_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(lp_bin$a.63[i] + lp_bin$bLP.63[i]*-2 + lp_bin$bIA.63[i]*1))
    lp_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(lp_bin$a.63[i] + lp_bin$bLP.63[i]*2 + lp_bin$bIA.63[i]*1))
  }
  
  lp_sim <- gather(data.frame(lp_sim_min_sd,lp_sim_plus_sd))
  barplot(table(lp_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("D",3,line=0.25,adj=0)
  legend(3,1700,legend=c("-2SD land privatization","+2SD land privatization"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## social stratification
  ss_sim_0 <- c()
  ss_sim_1 <- c()
  for(i in 1:nr) {
    ss_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(ss_bin$a.63[i] + ss_bin$bSS.63[i]*0 + ss_bin$bLP.63[i]*0 + ss_bin$bIA.63[i]*1))
    ss_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(ss_bin$a.63[i] + ss_bin$bSS.63[i]*1 + ss_bin$bLP.63[i]*0 + ss_bin$bIA.63[i]*1))
  }
  
  ss_sim <- gather(data.frame(ss_sim_0,ss_sim_1))
  barplot(table(ss_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("E",3,line=0.25,adj=0)
  legend(3,1700,legend=c("low stratification","high stratification"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## population density
  pd_sim_0 <- c()
  pd_sim_1 <- c()
  for(i in 1:nr) {
    pd_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(pd_bin$a.63[i] + pd_bin$bPD.63[i]*0 + pd_bin$bIA.63[i]*1))
    pd_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(pd_bin$a.63[i] + pd_bin$bPD.63[i]*1 + pd_bin$bIA.63[i]*1))
  }
  
  pd_sim <- gather(data.frame(pd_sim_0,pd_sim_1))
  barplot(table(pd_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("F",3,line=0.25,adj=0)
  legend(3,1700,legend=c("low population density","high population density"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## altitude
  al_sim_min_sd <- c()
  al_sim_plus_sd <- c()
  for(i in 1:nr) {
    al_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(al_bin$a.63[i] + al_bin$bAL.63[i]*-2))
    al_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(al_bin$a.63[i] + al_bin$bAL.63[i]*2))
  }
  
  al_sim <- gather(data.frame(al_sim_min_sd,al_sim_plus_sd))
  barplot(table(al_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("G",3,line=0.25,adj=0)
  legend(3,1700,legend=c("-2SD altitude","+2SD altitude"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## incline
  ic_sim_min_sd <- c()
  ic_sim_plus_sd <- c()
  for(i in 1:nr) {
    ic_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(ic_bin$a.63[i] + ic_bin$bIC.63[i]*-2))
    ic_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(ic_bin$a.63[i] + ic_bin$bIC.63[i]*2))
  }
  
  ic_sim <- gather(data.frame(ic_sim_min_sd,ic_sim_plus_sd))
  barplot(table(ic_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("H",3,line=0.25,adj=0)
  legend(3,1700,legend=c("-2SD incline","+2SD incline"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
}


## Figure 10 (Indo-Europeans, continuous)
{
  
  par(mfrow=c(4,2),mar=c(5.1,6.1,2.6,3.1))
  
  ## assault frequency
  af_sim_min_sd <- c()
  af_sim_plus_sd <- c()
  for(i in 1:nr) {
    af_sim_min_sd[i] <- rnorm(1,af_con$a.28[i] + af_con$bPP.28[i]*-2 + af_con$bPI.28[i]*1,af_con$sigma_phy)
    af_sim_plus_sd[i] <- rnorm(1,af_con$a.28[i] + af_con$bPP.28[i]*2 + af_con$bPI.28[i]*1,af_con$sigma_phy)
  }
  
  hist(af_sim_min_sd,xlab="assault frequency (SD)",ylab="frequency",main="",breaks=10,xlim=c(-3,3),ylim=c(0,800),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(af_sim_plus_sd,breaks=10,col=col.alpha("#5E8C7B",0.6),add=TRUE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2.0","-1.0","0.0","1.0","2.0"),cex.axis=1.5,tick=FALSE)
  mtext("A",3,line=0.25,adj=0)
  legend(-0.5,800,legend=c("+2SD monogamous women","-2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## trade
  tr_sim_0 <- c()
  tr_sim_1 <- c()
  for(i in 1:nr) {
    tr_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(tr_con$a.28[i] + tr_con$bPP.28[i]*-2 + tr_con$bPI.28[i]*1))
    tr_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(tr_con$a.28[i] + tr_con$bPP.28[i]*2 + tr_con$bPI.28[i]*1))
  }
  
  tr_sim <- gather(data.frame(tr_sim_0,tr_sim_1))
  barplot(table(tr_sim),xlim=c(0,8),width=0.8,xlab=" intercommunity trade",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(3,1600,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)  
  
  
  ## military mobilization
  mm_sim_0 <- c()
  mm_sim_1 <- c()
  for(i in 1:nr) {
    mm_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(mm_con$a.23[i] + mm_con$bPP.23[i]*-2 + mm_con$bPI.23[i]*1))
    mm_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(mm_con$a.23[i] + mm_con$bPP.23[i]*2 + mm_con$bPI.23[i]*1))
  }
  
  mm_sim <- gather(data.frame(mm_sim_0,mm_sim_1))
  barplot(table(mm_sim),xlim=c(0,8),width=0.8,xlab="military organization",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("C",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## land privatization
  lp_sim_min_sd <- c()
  lp_sim_plus_sd <- c()
  for(i in 1:nr) {
    lp_sim_min_sd[i] <- rbeta2(1,logistic(lp_con$a.28[i] + lp_con$bLP.28[i]*-2 + lp_con$bIA.28[i]*1),lp_con$theta)
    lp_sim_plus_sd[i] <- rbeta2(1,logistic(lp_con$a.28[i] + lp_con$bLP.28[i]*2 + lp_con$bIA.28[i]*1),lp_con$theta)
  }
  
  hist(lp_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1700),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(lp_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("D",3,line=0.25,adj=0)
  legend(0.1,1700,legend=c("-2SD land privatization","+2SD land privatization"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## social stratification
  ss_sim_0 <- c()
  ss_sim_1 <- c()
  for(i in 1:nr) {
    ss_sim_0[i] <- rbeta2(1,logistic(ss_con$a.28[i] + ss_con$bSS.28[i]*0 + ss_con$bLP.28[i]*0 + ss_con$bIA.28[i]*1),ss_con$theta)
    ss_sim_1[i] <- rbeta2(1,logistic(ss_con$a.28[i] + ss_con$bSS.28[i]*1 + ss_con$bLP.28[i]*0 + ss_con$bIA.28[i]*1),ss_con$theta)
  }
  
  hist(ss_sim_0,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1200),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(ss_sim_1,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("E",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("low stratification","high stratification"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## population density
  pd_sim_0 <- c()
  pd_sim_1 <- c()
  for(i in 1:nr) {
    pd_sim_0[i] <- rbeta2(1,logistic(pd_con$a.28[i] + pd_con$bPD.28[i]*0 + pd_con$bIA.28[i]*1),pd_con$theta)
    pd_sim_1[i] <- rbeta2(1,logistic(pd_con$a.28[i] + pd_con$bPD.28[i]*1 + pd_con$bIA.28[i]*1),pd_con$theta)
  }
  
  hist(pd_sim_0,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1400),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(pd_sim_1,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("F",3,line=0.25,adj=0)
  legend(0.1,1400,legend=c("low population density","high population density"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## altitude
  al_sim_min_sd <- c()
  al_sim_plus_sd <- c()
  for(i in 1:nr) {
    al_sim_min_sd[i] <- rbeta2(1,logistic(al_con$a.28[i] + al_con$bAL.28[i]*-2 + al_con$bIC.28[i]*0),al_con$theta)
    al_sim_plus_sd[i] <- rbeta2(1,logistic(al_con$a.28[i] + al_con$bAL.28[i]*2 + al_con$bIC.28[i]*0),al_con$theta)
  }
  
  hist(al_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1300),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(al_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("G",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("-2SD altitude","+2SD altitude"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## incline
  ic_sim_min_sd <- c()
  ic_sim_plus_sd <- c()
  for(i in 1:nr) {
    ic_sim_min_sd[i] <- rbeta2(1,logistic(ic_con$a.28[i] + ic_con$bIC.28[i]*-2),ic_con$theta)
    ic_sim_plus_sd[i] <- rbeta2(1,logistic(ic_con$a.28[i] + ic_con$bIC.28[i]*2),ic_con$theta)
  }
  
  hist(al_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1300),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(al_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("H",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("-2SD incline","+2SD incline"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
}


## Figure 11 (Indo-Europeans, binary)
{
  
  par(mfrow=c(4,2),mar=c(5.1,6.1,2.6,3.1))
  names_mp <- c("absent","present")
  
  ## assault frequency
  af_sim_min_sd <- c()
  af_sim_plus_sd <- c()
  for(i in 1:nr) {
    af_sim_min_sd[i] <- rnorm(1,af_bin$a.32[i] + af_bin$bMP.32[i]*-2 + af_bin$bPI.32[i]*1,af_bin$sigma_phy)
    af_sim_plus_sd[i] <- rnorm(1,af_bin$a.32[i] + af_bin$bMP.32[i]*2 + af_bin$bPI.32[i]*1,af_bin$sigma_phy)
  }
  
  hist(af_sim_min_sd,xlab="assault frequency (SD)",ylab="frequency",main="",breaks=20,xlim=c(-2,2),ylim=c(0,800),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(af_sim_plus_sd,breaks=20,col=col.alpha("#5E8C7B",0.6),add=TRUE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2.0","-1.0","0.0","1.0","2.0"),cex.axis=1.5,tick=FALSE)
  mtext("A",3,line=0.25,adj=0)
  legend(-0.5,800,legend=c("+2SD monogamous women","-2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## trade
  tr_sim_0 <- c()
  tr_sim_1 <- c()
  for(i in 1:nr) {
    tr_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(tr_bin$a.32[i] + tr_bin$bMP.32[i]*-2 + tr_bin$bPI.32[i]*1))
    tr_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(tr_bin$a.32[i] + tr_bin$bMP.32[i]*2 + tr_bin$bPI.32[i]*1))
  }
  
  tr_sim <- gather(data.frame(tr_sim_0,tr_sim_1))
  barplot(table(tr_sim),xlim=c(0,8),width=0.8,xlab=" intercommunity trade",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(3,1600,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)  
  
  
  ## military mobilization
  mm_sim_0 <- c()
  mm_sim_1 <- c()
  for(i in 1:nr) {
    mm_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(mm_bin$a.26[i] + mm_bin$bMP.26[i]*-2 + mm_bin$bPI.26[i]*1))
    mm_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(mm_bin$a.26[i] + mm_bin$bMP.26[i]*2 + mm_bin$bPI.26[i]*1))
  }
  
  mm_sim <- gather(data.frame(mm_sim_0,mm_sim_1))
  barplot(table(mm_sim),xlim=c(0,8),width=0.8,xlab="military organization",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("C",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## land privatization
  lp_sim_min_sd <- c()
  lp_sim_plus_sd <- c()
  for(i in 1:nr) {
    lp_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(lp_bin$a.32[i] + lp_bin$bLP.32[i]*-2 + lp_bin$bIA.32[i]*1))
    lp_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(lp_bin$a.32[i] + lp_bin$bLP.32[i]*2 + lp_bin$bIA.32[i]*1))
  }
  
  lp_sim <- gather(data.frame(lp_sim_min_sd,lp_sim_plus_sd))
  barplot(table(lp_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("D",3,line=0.25,adj=0)
  legend(3,1700,legend=c("-2SD land privatization","+2SD land privatization"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## social stratification
  ss_sim_0 <- c()
  ss_sim_1 <- c()
  for(i in 1:nr) {
    ss_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(ss_bin$a.32[i] + ss_bin$bSS.32[i]*0 + ss_bin$bLP.32[i]*0 + ss_bin$bIA.32[i]*1))
    ss_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(ss_bin$a.32[i] + ss_bin$bSS.32[i]*1 + ss_bin$bLP.32[i]*0 + ss_bin$bIA.32[i]*1))
  }
  
  ss_sim <- gather(data.frame(ss_sim_0,ss_sim_1))
  barplot(table(ss_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("E",3,line=0.25,adj=0)
  legend(3,1000,legend=c("low stratification","high stratification"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## population density
  pd_sim_0 <- c()
  pd_sim_1 <- c()
  for(i in 1:nr) {
    pd_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(pd_bin$a.32[i] + pd_bin$bPD.32[i]*0 + pd_bin$bIA.32[i]*1))
    pd_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(pd_bin$a.32[i] + pd_bin$bPD.32[i]*1 + pd_bin$bIA.32[i]*1))
  }
  
  pd_sim <- gather(data.frame(pd_sim_0,pd_sim_1))
  barplot(table(pd_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("F",3,line=0.25,adj=0)
  legend(3,1000,legend=c("low population density","high population density"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## altitude
  al_sim_min_sd <- c()
  al_sim_plus_sd <- c()
  for(i in 1:nr) {
    al_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(al_bin$a.32[i] + al_bin$bAL.32[i]*-2))
    al_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(al_bin$a.32[i] + al_bin$bAL.32[i]*2))
  }
  
  al_sim <- gather(data.frame(al_sim_min_sd,al_sim_plus_sd))
  barplot(table(al_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("G",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD altitude","+2SD altitude"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## incline
  ic_sim_min_sd <- c()
  ic_sim_plus_sd <- c()
  for(i in 1:nr) {
    ic_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(ic_bin$a.32[i] + ic_bin$bIC.32[i]*-2))
    ic_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(ic_bin$a.32[i] + ic_bin$bIC.32[i]*2))
  }
  
  ic_sim <- gather(data.frame(ic_sim_min_sd,ic_sim_plus_sd))
  barplot(table(ic_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("H",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD incline","+2SD incline"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
}


## Figure 12 (Japonic, continuous)
{
  
  par(mfrow=c(4,2),mar=c(5.1,6.1,2.6,3.1))
  
  ## assault frequency
  af_sim_min_sd <- c()
  af_sim_plus_sd <- c()
  for(i in 1:nr) {
    af_sim_min_sd[i] <- rnorm(1,af_con$a.30[i] + af_con$bPP.30[i]*-2 + af_con$bPI.30[i]*1,af_con$sigma_phy)
    af_sim_plus_sd[i] <- rnorm(1,af_con$a.30[i] + af_con$bPP.30[i]*2 + af_con$bPI.30[i]*1,af_con$sigma_phy)
  }
  
  hist(af_sim_min_sd,xlab="assault frequency (SD)",ylab="frequency",main="",breaks=20,xlim=c(-3,3),ylim=c(0,800),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(af_sim_plus_sd,breaks=20,col=col.alpha("#5E8C7B",0.6),add=TRUE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2.0","-1.0","0.0","1.0","2.0"),cex.axis=1.5,tick=FALSE)
  mtext("A",3,line=0.25,adj=0)
  legend(-0.5,800,legend=c("+2SD monogamous women","-2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## trade
  tr_sim_0 <- c()
  tr_sim_1 <- c()
  for(i in 1:nr) {
    tr_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(tr_con$a.30[i] + tr_con$bPP.30[i]*-2 + tr_con$bPI.30[i]*1))
    tr_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(tr_con$a.30[i] + tr_con$bPP.30[i]*2 + tr_con$bPI.30[i]*1))
  }
  
  tr_sim <- gather(data.frame(tr_sim_0,tr_sim_1))
  barplot(table(tr_sim),xlim=c(0,8),width=0.8,xlab=" intercommunity trade",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)  
  
  
  ## military mobilization
  mm_sim_0 <- c()
  mm_sim_1 <- c()
  for(i in 1:nr) {
    mm_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(mm_con$a.25[i] + mm_con$bPP.25[i]*-2 + mm_con$bPI.25[i]*1))
    mm_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(mm_con$a.25[i] + mm_con$bPP.25[i]*2 + mm_con$bPI.25[i]*1))
  }
  
  mm_sim <- gather(data.frame(mm_sim_0,mm_sim_1))
  barplot(table(mm_sim),xlim=c(0,8),width=0.8,xlab="military organization",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("C",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## land privatization
  lp_sim_min_sd <- c()
  lp_sim_plus_sd <- c()
  for(i in 1:nr) {
    lp_sim_min_sd[i] <- rbeta2(1,logistic(lp_con$a.30[i] + lp_con$bLP.30[i]*-2 + lp_con$bIA.30[i]*1),lp_con$theta)
    lp_sim_plus_sd[i] <- rbeta2(1,logistic(lp_con$a.30[i] + lp_con$bLP.30[i]*2 + lp_con$bIA.30[i]*1),lp_con$theta)
  }
  
  hist(lp_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1700),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(lp_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("D",3,line=0.25,adj=0)
  legend(0.1,1700,legend=c("-2SD land privatization","+2SD land privatization"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## social stratification
  ss_sim_0 <- c()
  ss_sim_1 <- c()
  for(i in 1:nr) {
    ss_sim_0[i] <- rbeta2(1,logistic(ss_con$a.30[i] + ss_con$bSS.30[i]*0 + ss_con$bLP.30[i]*0 + ss_con$bIA.30[i]*1),ss_con$theta)
    ss_sim_1[i] <- rbeta2(1,logistic(ss_con$a.30[i] + ss_con$bSS.30[i]*1 + ss_con$bLP.30[i]*0 + ss_con$bIA.30[i]*1),ss_con$theta)
  }
  
  hist(ss_sim_0,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1200),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(ss_sim_1,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("E",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("low stratification","high stratification"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## population density
  pd_sim_0 <- c()
  pd_sim_1 <- c()
  for(i in 1:nr) {
    pd_sim_0[i] <- rbeta2(1,logistic(pd_con$a.30[i] + pd_con$bPD.30[i]*0 + pd_con$bIA.30[i]*1),pd_con$theta)
    pd_sim_1[i] <- rbeta2(1,logistic(pd_con$a.30[i] + pd_con$bPD.30[i]*1 + pd_con$bIA.30[i]*1),pd_con$theta)
  }
  
  hist(pd_sim_0,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1400),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(pd_sim_1,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("F",3,line=0.25,adj=0)
  legend(0.1,1400,legend=c("low population density","high population density"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## altitude
  al_sim_min_sd <- c()
  al_sim_plus_sd <- c()
  for(i in 1:nr) {
    al_sim_min_sd[i] <- rbeta2(1,logistic(al_con$a.30[i] + al_con$bAL.30[i]*-2 + al_con$bIC.30[i]*0),al_con$theta)
    al_sim_plus_sd[i] <- rbeta2(1,logistic(al_con$a.30[i] + al_con$bAL.30[i]*2 + al_con$bIC.30[i]*0),al_con$theta)
  }
  
  hist(al_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1300),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(al_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("G",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("-2SD altitude","+2SD altitude"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## incline
  ic_sim_min_sd <- c()
  ic_sim_plus_sd <- c()
  for(i in 1:nr) {
    ic_sim_min_sd[i] <- rbeta2(1,logistic(ic_con$a.30[i] + ic_con$bIC.30[i]*-2),ic_con$theta)
    ic_sim_plus_sd[i] <- rbeta2(1,logistic(ic_con$a.30[i] + ic_con$bIC.30[i]*2),ic_con$theta)
  }
  
  hist(al_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1300),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(al_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("H",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("-2SD incline","+2SD incline"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
}


## Figure 13 (Japonic, binary)
{
  
  par(mfrow=c(4,2),mar=c(5.1,6.1,2.6,3.1))
  names_mp <- c("absent","present")
  
  ## assault frequency
  af_sim_min_sd <- c()
  af_sim_plus_sd <- c()
  for(i in 1:nr) {
    af_sim_min_sd[i] <- rnorm(1,af_bin$a.34[i] + af_bin$bMP.34[i]*-2 + af_bin$bPI.34[i]*1,af_bin$sigma_phy)
    af_sim_plus_sd[i] <- rnorm(1,af_bin$a.34[i] + af_bin$bMP.34[i]*2 + af_bin$bPI.34[i]*1,af_bin$sigma_phy)
  }
  
  hist(af_sim_min_sd,xlab="assault frequency (SD)",ylab="frequency",main="",breaks=20,xlim=c(-3,3),ylim=c(0,800),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(af_sim_plus_sd,breaks=10,col=col.alpha("#5E8C7B",0.6),add=TRUE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2.0","-1.0","0.0","1.0","2.0"),cex.axis=1.5,tick=FALSE)
  mtext("A",3,line=0.25,adj=0)
  legend(-0.6,800,legend=c("+2SD monogamous women","-2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## trade
  tr_sim_0 <- c()
  tr_sim_1 <- c()
  for(i in 1:nr) {
    tr_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(tr_bin$a.34[i] + tr_bin$bMP.34[i]*-2 + tr_bin$bPI.34[i]*1))
    tr_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(tr_bin$a.34[i] + tr_bin$bMP.34[i]*2 + tr_bin$bPI.34[i]*1))
  }
  
  tr_sim <- gather(data.frame(tr_sim_0,tr_sim_1))
  barplot(table(tr_sim),xlim=c(0,8),width=0.8,xlab=" intercommunity trade",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(3,1600,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)  
  
  
  ## military mobilization
  mm_sim_0 <- c()
  mm_sim_1 <- c()
  for(i in 1:nr) {
    mm_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(mm_bin$a.28[i] + mm_bin$bMP.28[i]*-2 + mm_bin$bPI.28[i]*1))
    mm_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(mm_bin$a.28[i] + mm_bin$bMP.28[i]*2 + mm_bin$bPI.28[i]*1))
  }
  
  mm_sim <- gather(data.frame(mm_sim_0,mm_sim_1))
  barplot(table(mm_sim),xlim=c(0,8),width=0.8,xlab="military organization",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("C",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## land privatization
  lp_sim_min_sd <- c()
  lp_sim_plus_sd <- c()
  for(i in 1:nr) {
    lp_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(lp_bin$a.34[i] + lp_bin$bLP.34[i]*-2 + lp_bin$bIA.34[i]*1))
    lp_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(lp_bin$a.34[i] + lp_bin$bLP.34[i]*2 + lp_bin$bIA.34[i]*1))
  }
  
  lp_sim <- gather(data.frame(lp_sim_min_sd,lp_sim_plus_sd))
  barplot(table(lp_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("D",3,line=0.25,adj=0)
  legend(3,1700,legend=c("-2SD land privatization","+2SD land privatization"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## social stratification
  ss_sim_0 <- c()
  ss_sim_1 <- c()
  for(i in 1:nr) {
    ss_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(ss_bin$a.34[i] + ss_bin$bSS.34[i]*0 + ss_bin$bLP.34[i]*0 + ss_bin$bIA.34[i]*1))
    ss_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(ss_bin$a.34[i] + ss_bin$bSS.34[i]*1 + ss_bin$bLP.34[i]*0 + ss_bin$bIA.34[i]*1))
  }
  
  ss_sim <- gather(data.frame(ss_sim_0,ss_sim_1))
  barplot(table(ss_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("E",3,line=0.25,adj=0)
  legend(3,1000,legend=c("low stratification","high stratification"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## population density
  pd_sim_0 <- c()
  pd_sim_1 <- c()
  for(i in 1:nr) {
    pd_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(pd_bin$a.34[i] + pd_bin$bPD.34[i]*0 + pd_bin$bIA.34[i]*1))
    pd_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(pd_bin$a.34[i] + pd_bin$bPD.34[i]*1 + pd_bin$bIA.34[i]*1))
  }
  
  pd_sim <- gather(data.frame(pd_sim_0,pd_sim_1))
  barplot(table(pd_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("F",3,line=0.25,adj=0)
  legend(3,1000,legend=c("low population density","high population density"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## altitude
  al_sim_min_sd <- c()
  al_sim_plus_sd <- c()
  for(i in 1:nr) {
    al_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(al_bin$a.34[i] + al_bin$bAL.34[i]*-2))
    al_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(al_bin$a.34[i] + al_bin$bAL.34[i]*2))
  }
  
  al_sim <- gather(data.frame(al_sim_min_sd,al_sim_plus_sd))
  barplot(table(al_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("G",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD altitude","+2SD altitude"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## incline
  ic_sim_min_sd <- c()
  ic_sim_plus_sd <- c()
  for(i in 1:nr) {
    ic_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(ic_bin$a.34[i] + ic_bin$bIC.34[i]*-2))
    ic_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(ic_bin$a.34[i] + ic_bin$bIC.34[i]*2))
  }
  
  ic_sim <- gather(data.frame(ic_sim_min_sd,ic_sim_plus_sd))
  barplot(table(ic_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("H",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD incline","+2SD incline"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
}


## Figure 14 (Koreanic, continuous)
{
  
  par(mfrow=c(4,2),mar=c(5.1,6.1,2.6,3.1))
  
  ## assault frequency
  af_sim_min_sd <- c()
  af_sim_plus_sd <- c()
  for(i in 1:nr) {
    af_sim_min_sd[i] <- rnorm(1,af_con$a.34[i] + af_con$bPP.34[i]*-2 + af_con$bPI.34[i]*1,af_con$sigma_phy)
    af_sim_plus_sd[i] <- rnorm(1,af_con$a.34[i] + af_con$bPP.34[i]*2 + af_con$bPI.34[i]*1,af_con$sigma_phy)
  }
  
  hist(af_sim_min_sd,xlab="assault frequency (SD)",ylab="frequency",main="",breaks=14,xlim=c(-3,3),ylim=c(0,800),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(af_sim_plus_sd,breaks=14,col=col.alpha("#5E8C7B",0.6),add=TRUE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2.0","-1.0","0.0","1.0","2.0"),cex.axis=1.5,tick=FALSE)
  mtext("A",3,line=0.25,adj=0)
  legend(-0.5,800,legend=c("+2SD monogamous women","-2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## trade
  tr_sim_0 <- c()
  tr_sim_1 <- c()
  for(i in 1:nr) {
    tr_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(tr_con$a.34[i] + tr_con$bPP.34[i]*-2 + tr_con$bPI.34[i]*1))
    tr_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(tr_con$a.34[i] + tr_con$bPP.34[i]*2 + tr_con$bPI.34[i]*1))
  }
  
  tr_sim <- gather(data.frame(tr_sim_0,tr_sim_1))
  barplot(table(tr_sim),xlim=c(0,8),width=0.8,xlab=" intercommunity trade",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(3,1600,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)  
  
  
  ## military mobilization
  mm_sim_0 <- c()
  mm_sim_1 <- c()
  for(i in 1:nr) {
    mm_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(mm_con$a.29[i] + mm_con$bPP.29[i]*-2 + mm_con$bPI.29[i]*1))
    mm_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(mm_con$a.29[i] + mm_con$bPP.29[i]*2 + mm_con$bPI.29[i]*1))
  }
  
  mm_sim <- gather(data.frame(mm_sim_0,mm_sim_1))
  barplot(table(mm_sim),xlim=c(0,8),width=0.8,xlab="military organization",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("C",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## land privatization
  lp_sim_min_sd <- c()
  lp_sim_plus_sd <- c()
  for(i in 1:nr) {
    lp_sim_min_sd[i] <- rbeta2(1,logistic(lp_con$a.34[i] + lp_con$bLP.34[i]*-2 + lp_con$bIA.34[i]*1),lp_con$theta)
    lp_sim_plus_sd[i] <- rbeta2(1,logistic(lp_con$a.34[i] + lp_con$bLP.34[i]*2 + lp_con$bIA.34[i]*1),lp_con$theta)
  }
  
  hist(lp_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1700),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(lp_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("D",3,line=0.25,adj=0)
  legend(0.1,1700,legend=c("-2SD land privatization","+2SD land privatization"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## social stratification
  ss_sim_0 <- c()
  ss_sim_1 <- c()
  for(i in 1:nr) {
    ss_sim_0[i] <- rbeta2(1,logistic(ss_con$a.34[i] + ss_con$bSS.34[i]*0 + ss_con$bLP.34[i]*0 + ss_con$bIA.34[i]*1),ss_con$theta)
    ss_sim_1[i] <- rbeta2(1,logistic(ss_con$a.34[i] + ss_con$bSS.34[i]*1 + ss_con$bLP.34[i]*0 + ss_con$bIA.34[i]*1),ss_con$theta)
  }
  
  hist(ss_sim_0,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1200),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(ss_sim_1,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("E",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("low stratification","high stratification"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## population density
  pd_sim_0 <- c()
  pd_sim_1 <- c()
  for(i in 1:nr) {
    pd_sim_0[i] <- rbeta2(1,logistic(pd_con$a.34[i] + pd_con$bPD.34[i]*0 + pd_con$bIA.34[i]*1),pd_con$theta)
    pd_sim_1[i] <- rbeta2(1,logistic(pd_con$a.34[i] + pd_con$bPD.34[i]*1 + pd_con$bIA.34[i]*1),pd_con$theta)
  }
  
  hist(pd_sim_0,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1400),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(pd_sim_1,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("F",3,line=0.25,adj=0)
  legend(0.1,1400,legend=c("low population density","high population density"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## altitude
  al_sim_min_sd <- c()
  al_sim_plus_sd <- c()
  for(i in 1:nr) {
    al_sim_min_sd[i] <- rbeta2(1,logistic(al_con$a.34[i] + al_con$bAL.34[i]*-2 + al_con$bIC.34[i]*0),al_con$theta)
    al_sim_plus_sd[i] <- rbeta2(1,logistic(al_con$a.34[i] + al_con$bAL.34[i]*2 + al_con$bIC.34[i]*0),al_con$theta)
  }
  
  hist(al_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1300),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(al_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("G",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("-2SD altitude","+2SD altitude"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## incline
  ic_sim_min_sd <- c()
  ic_sim_plus_sd <- c()
  for(i in 1:nr) {
    ic_sim_min_sd[i] <- rbeta2(1,logistic(ic_con$a.34[i] + ic_con$bIC.34[i]*-2),ic_con$theta)
    ic_sim_plus_sd[i] <- rbeta2(1,logistic(ic_con$a.34[i] + ic_con$bIC.34[i]*2),ic_con$theta)
  }
  
  hist(al_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1300),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(al_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("H",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("-2SD incline","+2SD incline"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
}


## Figure 15 (Koreanic, binary)
{
  
  par(mfrow=c(4,2),mar=c(5.1,6.1,2.6,3.1))
  names_mp <- c("absent","present")
  
  ## assault frequency
  af_sim_min_sd <- c()
  af_sim_plus_sd <- c()
  for(i in 1:nr) {
    af_sim_min_sd[i] <- rnorm(1,af_bin$a.38[i] + af_bin$bMP.38[i]*-2 + af_bin$bPI.38[i]*1,af_bin$sigma_phy)
    af_sim_plus_sd[i] <- rnorm(1,af_bin$a.38[i] + af_bin$bMP.38[i]*2 + af_bin$bPI.38[i]*1,af_bin$sigma_phy)
  }
  
  hist(af_sim_min_sd,xlab="assault frequency (SD)",ylab="frequency",main="",breaks=20,xlim=c(-3,3),ylim=c(0,800),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(af_sim_plus_sd,breaks=20,col=col.alpha("#5E8C7B",0.6),add=TRUE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2.0","-1.0","0.0","1.0","2.0"),cex.axis=1.5,tick=FALSE)
  mtext("A",3,line=0.25,adj=0)
  legend(-0.5,800,legend=c("+2SD monogamous women","-2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## trade
  tr_sim_0 <- c()
  tr_sim_1 <- c()
  for(i in 1:nr) {
    tr_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(tr_bin$a.38[i] + tr_bin$bMP.38[i]*-2 + tr_bin$bPI.38[i]*1))
    tr_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(tr_bin$a.38[i] + tr_bin$bMP.38[i]*2 + tr_bin$bPI.38[i]*1))
  }
  
  tr_sim <- gather(data.frame(tr_sim_0,tr_sim_1))
  barplot(table(tr_sim),xlim=c(0,8),width=0.8,xlab=" intercommunity trade",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(3,1600,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)  
  
  
  ## military mobilization
  mm_sim_0 <- c()
  mm_sim_1 <- c()
  for(i in 1:nr) {
    mm_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(mm_bin$a.32[i] + mm_bin$bMP.32[i]*-2 + mm_bin$bPI.32[i]*1))
    mm_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(mm_bin$a.32[i] + mm_bin$bMP.32[i]*2 + mm_bin$bPI.32[i]*1))
  }
  
  mm_sim <- gather(data.frame(mm_sim_0,mm_sim_1))
  barplot(table(mm_sim),xlim=c(0,8),width=0.8,xlab="military organization",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("C",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## land privatization
  lp_sim_min_sd <- c()
  lp_sim_plus_sd <- c()
  for(i in 1:nr) {
    lp_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(lp_bin$a.38[i] + lp_bin$bLP.38[i]*-2 + lp_bin$bIA.38[i]*1))
    lp_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(lp_bin$a.38[i] + lp_bin$bLP.38[i]*2 + lp_bin$bIA.38[i]*1))
  }
  
  lp_sim <- gather(data.frame(lp_sim_min_sd,lp_sim_plus_sd))
  barplot(table(lp_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("D",3,line=0.25,adj=0)
  legend(3,1700,legend=c("-2SD land privatization","+2SD land privatization"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## social stratification
  ss_sim_0 <- c()
  ss_sim_1 <- c()
  for(i in 1:nr) {
    ss_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(ss_bin$a.38[i] + ss_bin$bSS.38[i]*0 + ss_bin$bLP.38[i]*0 + ss_bin$bIA.38[i]*1))
    ss_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(ss_bin$a.38[i] + ss_bin$bSS.38[i]*1 + ss_bin$bLP.38[i]*0 + ss_bin$bIA.38[i]*1))
  }
  
  ss_sim <- gather(data.frame(ss_sim_0,ss_sim_1))
  barplot(table(ss_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("E",3,line=0.25,adj=0)
  legend(3,1000,legend=c("low stratification","high stratification"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## population density
  pd_sim_0 <- c()
  pd_sim_1 <- c()
  for(i in 1:nr) {
    pd_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(pd_bin$a.38[i] + pd_bin$bPD.38[i]*0 + pd_bin$bIA.38[i]*1))
    pd_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(pd_bin$a.38[i] + pd_bin$bPD.38[i]*1 + pd_bin$bIA.38[i]*1))
  }
  
  pd_sim <- gather(data.frame(pd_sim_0,pd_sim_1))
  barplot(table(pd_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("F",3,line=0.25,adj=0)
  legend(3,1000,legend=c("low population density","high population density"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## altitude
  al_sim_min_sd <- c()
  al_sim_plus_sd <- c()
  for(i in 1:nr) {
    al_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(al_bin$a.38[i] + al_bin$bAL.38[i]*-2))
    al_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(al_bin$a.38[i] + al_bin$bAL.38[i]*2))
  }
  
  al_sim <- gather(data.frame(al_sim_min_sd,al_sim_plus_sd))
  barplot(table(al_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("G",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD altitude","+2SD altitude"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## incline
  ic_sim_min_sd <- c()
  ic_sim_plus_sd <- c()
  for(i in 1:nr) {
    ic_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(ic_bin$a.38[i] + ic_bin$bIC.38[i]*-2))
    ic_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(ic_bin$a.38[i] + ic_bin$bIC.38[i]*2))
  }
  
  ic_sim <- gather(data.frame(ic_sim_min_sd,ic_sim_plus_sd))
  barplot(table(ic_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("H",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD incline","+2SD incline"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
}


## Figure 16 (Afro-Asiatic, continuous)
{
  
  par(mfrow=c(4,2),mar=c(5.1,6.1,2.6,3.1))
  
  ## assault frequency
  af_sim_min_sd <- c()
  af_sim_plus_sd <- c()
  for(i in 1:nr) {
    af_sim_min_sd[i] <- rnorm(1,af_con$a.1[i] + af_con$bPP.1[i]*-2 + af_con$bPI.1[i]*1,af_con$sigma_phy)
    af_sim_plus_sd[i] <- rnorm(1,af_con$a.1[i] + af_con$bPP.1[i]*2 + af_con$bPI.1[i]*1,af_con$sigma_phy)
  }
  
  hist(af_sim_min_sd,xlab="assault frequency (SD)",ylab="frequency",main="",breaks=20,xlim=c(-4,4),ylim=c(0,800),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(af_sim_plus_sd,breaks=20,col=col.alpha("#5E8C7B",0.6),add=TRUE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2.0","-1.0","0.0","1.0","2.0"),cex.axis=1.5,tick=FALSE)
  mtext("A",3,line=0.25,adj=0)
  legend(-1,800,legend=c("+2SD monogamous women","-2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## trade
  tr_sim_0 <- c()
  tr_sim_1 <- c()
  for(i in 1:nr) {
    tr_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(tr_con$a.1[i] + tr_con$bPP.1[i]*-2 + tr_con$bPI.1[i]*1))
    tr_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(tr_con$a.1[i] + tr_con$bPP.1[i]*2 + tr_con$bPI.1[i]*1))
  }
  
  tr_sim <- gather(data.frame(tr_sim_0,tr_sim_1))
  barplot(table(tr_sim),xlim=c(0,8),width=0.8,xlab=" intercommunity trade",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(3,1500,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)  
  
  
  ## military mobilization
  mm_sim_0 <- c()
  mm_sim_1 <- c()
  for(i in 1:nr) {
    mm_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(mm_con$a.1[i] + mm_con$bPP.1[i]*-2 + mm_con$bPI.1[i]*1))
    mm_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(mm_con$a.1[i] + mm_con$bPP.1[i]*2 + mm_con$bPI.1[i]*1))
  }
  
  mm_sim <- gather(data.frame(mm_sim_0,mm_sim_1))
  barplot(table(mm_sim),xlim=c(0,8),width=0.8,xlab="military organization",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("C",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## land privatization
  lp_sim_min_sd <- c()
  lp_sim_plus_sd <- c()
  for(i in 1:nr) {
    lp_sim_min_sd[i] <- rbeta2(1,logistic(lp_con$a.1[i] + lp_con$bLP.1[i]*-2 + lp_con$bIA.1[i]*1),lp_con$theta)
    lp_sim_plus_sd[i] <- rbeta2(1,logistic(lp_con$a.1[i] + lp_con$bLP.1[i]*2 + lp_con$bIA.1[i]*1),lp_con$theta)
  }
  
  hist(lp_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1700),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(lp_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("D",3,line=0.25,adj=0)
  legend(0.1,1700,legend=c("-2SD land privatization","+2SD land privatization"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## social stratification
  ss_sim_0 <- c()
  ss_sim_1 <- c()
  for(i in 1:nr) {
    ss_sim_0[i] <- rbeta2(1,logistic(ss_con$a.1[i] + ss_con$bSS.1[i]*0 + ss_con$bLP.1[i]*0 + ss_con$bIA.1[i]*1),ss_con$theta)
    ss_sim_1[i] <- rbeta2(1,logistic(ss_con$a.1[i] + ss_con$bSS.1[i]*1 + ss_con$bLP.1[i]*0 + ss_con$bIA.1[i]*1),ss_con$theta)
  }
  
  hist(ss_sim_0,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1200),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(ss_sim_1,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("E",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("low stratification","high stratification"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## population density
  pd_sim_0 <- c()
  pd_sim_1 <- c()
  for(i in 1:nr) {
    pd_sim_0[i] <- rbeta2(1,logistic(pd_con$a.1[i] + pd_con$bPD.1[i]*0 + pd_con$bIA.1[i]*1),pd_con$theta)
    pd_sim_1[i] <- rbeta2(1,logistic(pd_con$a.1[i] + pd_con$bPD.1[i]*1 + pd_con$bIA.1[i]*1),pd_con$theta)
  }
  
  hist(pd_sim_0,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1400),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(pd_sim_1,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("F",3,line=0.25,adj=0)
  legend(0.1,1400,legend=c("low population density","high population density"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## altitude
  al_sim_min_sd <- c()
  al_sim_plus_sd <- c()
  for(i in 1:nr) {
    al_sim_min_sd[i] <- rbeta2(1,logistic(al_con$a.1[i] + al_con$bAL.1[i]*-2 + al_con$bIC.1[i]*0),al_con$theta)
    al_sim_plus_sd[i] <- rbeta2(1,logistic(al_con$a.1[i] + al_con$bAL.1[i]*2 + al_con$bIC.1[i]*0),al_con$theta)
  }
  
  hist(al_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1300),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(al_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("G",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("-2SD altitude","+2SD altitude"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## incline
  ic_sim_min_sd <- c()
  ic_sim_plus_sd <- c()
  for(i in 1:nr) {
    ic_sim_min_sd[i] <- rbeta2(1,logistic(ic_con$a.1[i] + ic_con$bIC.1[i]*-2),ic_con$theta)
    ic_sim_plus_sd[i] <- rbeta2(1,logistic(ic_con$a.1[i] + ic_con$bIC.1[i]*2),ic_con$theta)
  }
  
  hist(al_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1300),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(al_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("H",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("-2SD incline","+2SD incline"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
}


## Figure 17 (Afro-Asiatic, binary)
{
  
  par(mfrow=c(4,2),mar=c(5.1,6.1,2.6,3.1))
  names_mp <- c("absent","present")
  
  ## assault frequency
  af_sim_min_sd <- c()
  af_sim_plus_sd <- c()
  for(i in 1:nr) {
    af_sim_min_sd[i] <- rnorm(1,af_bin$a.2[i] + af_bin$bMP.2[i]*-2 + af_bin$bPI.2[i]*1,af_bin$sigma_phy)
    af_sim_plus_sd[i] <- rnorm(1,af_bin$a.2[i] + af_bin$bMP.2[i]*2 + af_bin$bPI.2[i]*1,af_bin$sigma_phy)
  }
  
  hist(af_sim_min_sd,xlab="assault frequency (SD)",ylab="frequency",main="",breaks=20,xlim=c(-3,3),ylim=c(0,800),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(af_sim_plus_sd,breaks=20,col=col.alpha("#5E8C7B",0.6),add=TRUE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2.0","-1.0","0.0","1.0","2.0"),cex.axis=1.5,tick=FALSE)
  mtext("A",3,line=0.25,adj=0)
  legend(-0.5,800,legend=c("+2SD monogamous women","-2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## trade
  tr_sim_0 <- c()
  tr_sim_1 <- c()
  for(i in 1:nr) {
    tr_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(tr_bin$a.2[i] + tr_bin$bMP.2[i]*-2 + tr_bin$bPI.2[i]*1))
    tr_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(tr_bin$a.2[i] + tr_bin$bMP.2[i]*2 + tr_bin$bPI.2[i]*1))
  }
  
  tr_sim <- gather(data.frame(tr_sim_0,tr_sim_1))
  barplot(table(tr_sim),xlim=c(0,8),width=0.8,xlab=" intercommunity trade",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(3,1600,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)  
  
  
  ## military mobilization
  mm_sim_0 <- c()
  mm_sim_1 <- c()
  for(i in 1:nr) {
    mm_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(mm_bin$a.2[i] + mm_bin$bMP.2[i]*-2 + mm_bin$bPI.2[i]*1))
    mm_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(mm_bin$a.2[i] + mm_bin$bMP.2[i]*2 + mm_bin$bPI.2[i]*1))
  }
  
  mm_sim <- gather(data.frame(mm_sim_0,mm_sim_1))
  barplot(table(mm_sim),xlim=c(0,8),width=0.8,xlab="military organization",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("C",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## land privatization
  lp_sim_min_sd <- c()
  lp_sim_plus_sd <- c()
  for(i in 1:nr) {
    lp_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(lp_bin$a.2[i] + lp_bin$bLP.2[i]*-2 + lp_bin$bIA.2[i]*1))
    lp_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(lp_bin$a.2[i] + lp_bin$bLP.2[i]*2 + lp_bin$bIA.2[i]*1))
  }
  
  lp_sim <- gather(data.frame(lp_sim_min_sd,lp_sim_plus_sd))
  barplot(table(lp_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("D",3,line=0.25,adj=0)
  legend(3,1700,legend=c("-2SD land privatization","+2SD land privatization"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## social stratification
  ss_sim_0 <- c()
  ss_sim_1 <- c()
  for(i in 1:nr) {
    ss_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(ss_bin$a.2[i] + ss_bin$bSS.2[i]*0 + ss_bin$bLP.2[i]*0 + ss_bin$bIA.2[i]*1))
    ss_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(ss_bin$a.2[i] + ss_bin$bSS.2[i]*1 + ss_bin$bLP.2[i]*0 + ss_bin$bIA.2[i]*1))
  }
  
  ss_sim <- gather(data.frame(ss_sim_0,ss_sim_1))
  barplot(table(ss_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("E",3,line=0.25,adj=0)
  legend(3,1000,legend=c("low stratification","high stratification"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## population density
  pd_sim_0 <- c()
  pd_sim_1 <- c()
  for(i in 1:nr) {
    pd_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(pd_bin$a.2[i] + pd_bin$bPD.2[i]*0 + pd_bin$bIA.2[i]*1))
    pd_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(pd_bin$a.2[i] + pd_bin$bPD.2[i]*1 + pd_bin$bIA.2[i]*1))
  }
  
  pd_sim <- gather(data.frame(pd_sim_0,pd_sim_1))
  barplot(table(pd_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("F",3,line=0.25,adj=0)
  legend(3,1000,legend=c("low population density","high population density"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## altitude
  al_sim_min_sd <- c()
  al_sim_plus_sd <- c()
  for(i in 1:nr) {
    al_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(al_bin$a.2[i] + al_bin$bAL.2[i]*-2))
    al_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(al_bin$a.2[i] + al_bin$bAL.2[i]*2))
  }
  
  al_sim <- gather(data.frame(al_sim_min_sd,al_sim_plus_sd))
  barplot(table(al_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("G",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD altitude","+2SD altitude"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## incline
  ic_sim_min_sd <- c()
  ic_sim_plus_sd <- c()
  for(i in 1:nr) {
    ic_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(ic_bin$a.2[i] + ic_bin$bIC.2[i]*-2))
    ic_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(ic_bin$a.2[i] + ic_bin$bIC.2[i]*2))
  }
  
  ic_sim <- gather(data.frame(ic_sim_min_sd,ic_sim_plus_sd))
  barplot(table(ic_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("H",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD incline","+2SD incline"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
}


## Figure 18 (Mongolic-Khitan, continuous)
{
  
  par(mfrow=c(4,2),mar=c(5.1,6.1,2.6,3.1))
  
  ## assault frequency
  af_sim_min_sd <- c()
  af_sim_plus_sd <- c()
  for(i in 1:nr) {
    af_sim_min_sd[i] <- rnorm(1,af_con$a.40[i] + af_con$bPP.40[i]*-2 + af_con$bPI.40[i]*1,af_con$sigma_phy)
    af_sim_plus_sd[i] <- rnorm(1,af_con$a.40[i] + af_con$bPP.40[i]*2 + af_con$bPI.40[i]*1,af_con$sigma_phy)
  }
  
  hist(af_sim_min_sd,xlab="assault frequency (SD)",ylab="frequency",main="",breaks=20,xlim=c(-4,4),ylim=c(0,800),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(af_sim_plus_sd,breaks=20,col=col.alpha("#5E8C7B",0.6),add=TRUE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2.0","-1.0","0.0","1.0","2.0"),cex.axis=1.5,tick=FALSE)
  mtext("A",3,line=0.25,adj=0)
  legend(-0.5,800,legend=c("+2SD monogamous women","-2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## trade
  tr_sim_0 <- c()
  tr_sim_1 <- c()
  for(i in 1:nr) {
    tr_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(tr_con$a.40[i] + tr_con$bPP.40[i]*-2 + tr_con$bPI.40[i]*1))
    tr_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(tr_con$a.40[i] + tr_con$bPP.40[i]*2 + tr_con$bPI.40[i]*1))
  }
  
  tr_sim <- gather(data.frame(tr_sim_0,tr_sim_1))
  barplot(table(tr_sim),xlim=c(0,8),width=0.8,xlab=" intercommunity trade",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(3,1600,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)  
  
  
  ## military mobilization
  mm_sim_0 <- c()
  mm_sim_1 <- c()
  for(i in 1:nr) {
    mm_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(mm_con$a.35[i] + mm_con$bPP.35[i]*-2 + mm_con$bPI.35[i]*1))
    mm_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(mm_con$a.35[i] + mm_con$bPP.35[i]*2 + mm_con$bPI.35[i]*1))
  }
  
  mm_sim <- gather(data.frame(mm_sim_0,mm_sim_1))
  barplot(table(mm_sim),xlim=c(0,8),width=0.8,xlab="military organization",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("C",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## land privatization
  lp_sim_min_sd <- c()
  lp_sim_plus_sd <- c()
  for(i in 1:nr) {
    lp_sim_min_sd[i] <- rbeta2(1,logistic(lp_con$a.40[i] + lp_con$bLP.40[i]*-2 + lp_con$bIA.40[i]*1),lp_con$theta)
    lp_sim_plus_sd[i] <- rbeta2(1,logistic(lp_con$a.40[i] + lp_con$bLP.40[i]*2 + lp_con$bIA.40[i]*1),lp_con$theta)
  }
  
  hist(lp_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1700),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(lp_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("D",3,line=0.25,adj=0)
  legend(0.1,1700,legend=c("-2SD land privatization","+2SD land privatization"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## social stratification
  ss_sim_0 <- c()
  ss_sim_1 <- c()
  for(i in 1:nr) {
    ss_sim_0[i] <- rbeta2(1,logistic(ss_con$a.40[i] + ss_con$bSS.40[i]*0 + ss_con$bLP.40[i]*0 + ss_con$bIA.40[i]*1),ss_con$theta)
    ss_sim_1[i] <- rbeta2(1,logistic(ss_con$a.40[i] + ss_con$bSS.40[i]*1 + ss_con$bLP.40[i]*0 + ss_con$bIA.40[i]*1),ss_con$theta)
  }
  
  hist(ss_sim_0,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1200),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(ss_sim_1,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("E",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("low stratification","high stratification"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## population density
  pd_sim_0 <- c()
  pd_sim_1 <- c()
  for(i in 1:nr) {
    pd_sim_0[i] <- rbeta2(1,logistic(pd_con$a.40[i] + pd_con$bPD.40[i]*0 + pd_con$bIA.40[i]*1),pd_con$theta)
    pd_sim_1[i] <- rbeta2(1,logistic(pd_con$a.40[i] + pd_con$bPD.40[i]*1 + pd_con$bIA.40[i]*1),pd_con$theta)
  }
  
  hist(pd_sim_0,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1400),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(pd_sim_1,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("F",3,line=0.25,adj=0)
  legend(0.1,1400,legend=c("low population density","high population density"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## altitude
  al_sim_min_sd <- c()
  al_sim_plus_sd <- c()
  for(i in 1:nr) {
    al_sim_min_sd[i] <- rbeta2(1,logistic(al_con$a.40[i] + al_con$bAL.40[i]*-2 + al_con$bIC.40[i]*0),al_con$theta)
    al_sim_plus_sd[i] <- rbeta2(1,logistic(al_con$a.40[i] + al_con$bAL.40[i]*2 + al_con$bIC.40[i]*0),al_con$theta)
  }
  
  hist(al_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1300),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(al_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("G",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("-2SD altitude","+2SD altitude"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## incline
  ic_sim_min_sd <- c()
  ic_sim_plus_sd <- c()
  for(i in 1:nr) {
    ic_sim_min_sd[i] <- rbeta2(1,logistic(ic_con$a.40[i] + ic_con$bIC.40[i]*-2),ic_con$theta)
    ic_sim_plus_sd[i] <- rbeta2(1,logistic(ic_con$a.40[i] + ic_con$bIC.40[i]*2),ic_con$theta)
  }
  
  hist(al_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1300),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(al_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("H",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("-2SD incline","+2SD incline"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
}


## Figure 19 (Mongolic-Khitan, binary)
{
  
  par(mfrow=c(4,2),mar=c(5.1,6.1,2.6,3.1))
  names_mp <- c("absent","present")
  
  ## assault frequency
  af_sim_min_sd <- c()
  af_sim_plus_sd <- c()
  for(i in 1:nr) {
    af_sim_min_sd[i] <- rnorm(1,af_bin$a.46[i] + af_bin$bMP.46[i]*-2 + af_bin$bPI.46[i]*1,af_bin$sigma_phy)
    af_sim_plus_sd[i] <- rnorm(1,af_bin$a.46[i] + af_bin$bMP.46[i]*2 + af_bin$bPI.46[i]*1,af_bin$sigma_phy)
  }
  
  hist(af_sim_min_sd,xlab="assault frequency (SD)",ylab="frequency",main="",breaks=20,xlim=c(-3,3),ylim=c(0,800),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(af_sim_plus_sd,breaks=20,col=col.alpha("#5E8C7B",0.6),add=TRUE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2.0","-1.0","0.0","1.0","2.0"),cex.axis=1.5,tick=FALSE)
  mtext("A",3,line=0.25,adj=0)
  legend(-0.5,800,legend=c("+2SD monogamous women","-2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## trade
  tr_sim_0 <- c()
  tr_sim_1 <- c()
  for(i in 1:nr) {
    tr_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(tr_bin$a.46[i] + tr_bin$bMP.46[i]*-2 + tr_bin$bPI.46[i]*1))
    tr_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(tr_bin$a.46[i] + tr_bin$bMP.46[i]*2 + tr_bin$bPI.46[i]*1))
  }
  
  tr_sim <- gather(data.frame(tr_sim_0,tr_sim_1))
  barplot(table(tr_sim),xlim=c(0,8),width=0.8,xlab=" intercommunity trade",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(3,1600,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)  
  
  
  ## military mobilization
  mm_sim_0 <- c()
  mm_sim_1 <- c()
  for(i in 1:nr) {
    mm_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(mm_bin$a.39[i] + mm_bin$bMP.39[i]*-2 + mm_bin$bPI.39[i]*1))
    mm_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(mm_bin$a.39[i] + mm_bin$bMP.39[i]*2 + mm_bin$bPI.39[i]*1))
  }
  
  mm_sim <- gather(data.frame(mm_sim_0,mm_sim_1))
  barplot(table(mm_sim),xlim=c(0,8),width=0.8,xlab="military organization",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("C",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## land privatization
  lp_sim_min_sd <- c()
  lp_sim_plus_sd <- c()
  for(i in 1:nr) {
    lp_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(lp_bin$a.46[i] + lp_bin$bLP.46[i]*-2 + lp_bin$bIA.46[i]*1))
    lp_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(lp_bin$a.46[i] + lp_bin$bLP.46[i]*2 + lp_bin$bIA.46[i]*1))
  }
  
  lp_sim <- gather(data.frame(lp_sim_min_sd,lp_sim_plus_sd))
  barplot(table(lp_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("D",3,line=0.25,adj=0)
  legend(3,1700,legend=c("-2SD land privatization","+2SD land privatization"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## social stratification
  ss_sim_0 <- c()
  ss_sim_1 <- c()
  for(i in 1:nr) {
    ss_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(ss_bin$a.46[i] + ss_bin$bSS.46[i]*0 + ss_bin$bLP.46[i]*0 + ss_bin$bIA.46[i]*1))
    ss_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(ss_bin$a.46[i] + ss_bin$bSS.46[i]*1 + ss_bin$bLP.46[i]*0 + ss_bin$bIA.46[i]*1))
  }
  
  ss_sim <- gather(data.frame(ss_sim_0,ss_sim_1))
  barplot(table(ss_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("E",3,line=0.25,adj=0)
  legend(3,1000,legend=c("low stratification","high stratification"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## population density
  pd_sim_0 <- c()
  pd_sim_1 <- c()
  for(i in 1:nr) {
    pd_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(pd_bin$a.46[i] + pd_bin$bPD.46[i]*0 + pd_bin$bIA.46[i]*1))
    pd_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(pd_bin$a.46[i] + pd_bin$bPD.46[i]*1 + pd_bin$bIA.46[i]*1))
  }
  
  pd_sim <- gather(data.frame(pd_sim_0,pd_sim_1))
  barplot(table(pd_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("F",3,line=0.25,adj=0)
  legend(3,1000,legend=c("low population density","high population density"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## altitude
  al_sim_min_sd <- c()
  al_sim_plus_sd <- c()
  for(i in 1:nr) {
    al_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(al_bin$a.46[i] + al_bin$bAL.46[i]*-2))
    al_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(al_bin$a.46[i] + al_bin$bAL.46[i]*2))
  }
  
  al_sim <- gather(data.frame(al_sim_min_sd,al_sim_plus_sd))
  barplot(table(al_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("G",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD altitude","+2SD altitude"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## incline
  ic_sim_min_sd <- c()
  ic_sim_plus_sd <- c()
  for(i in 1:nr) {
    ic_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(ic_bin$a.46[i] + ic_bin$bIC.46[i]*-2))
    ic_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(ic_bin$a.46[i] + ic_bin$bIC.46[i]*2))
  }
  
  ic_sim <- gather(data.frame(ic_sim_min_sd,ic_sim_plus_sd))
  barplot(table(ic_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("H",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD incline","+2SD incline"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
}


## Figure 20 (Quechuan, continuous)
{
  
  par(mfrow=c(4,2),mar=c(5.1,6.1,2.6,3.1))
  
  ## assault frequency
  af_sim_min_sd <- c()
  af_sim_plus_sd <- c()
  for(i in 1:nr) {
    af_sim_min_sd[i] <- rnorm(1,af_con$a.50[i] + af_con$bPP.50[i]*-2 + af_con$bPI.50[i]*1,af_con$sigma_phy)
    af_sim_plus_sd[i] <- rnorm(1,af_con$a.50[i] + af_con$bPP.50[i]*2 + af_con$bPI.50[i]*1,af_con$sigma_phy)
  }
  
  hist(af_sim_min_sd,xlab="assault frequency (SD)",ylab="frequency",main="",breaks=14,xlim=c(-4,4),ylim=c(0,800),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(af_sim_plus_sd,breaks=14,col=col.alpha("#5E8C7B",0.6),add=TRUE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2.0","-1.0","0.0","1.0","2.0"),cex.axis=1.5,tick=FALSE)
  mtext("A",3,line=0.25,adj=0)
  legend(-0.5,800,legend=c("+2SD monogamous women","-2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## trade
  tr_sim_0 <- c()
  tr_sim_1 <- c()
  for(i in 1:nr) {
    tr_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(tr_con$a.50[i] + tr_con$bPP.50[i]*-2 + tr_con$bPI.50[i]*1))
    tr_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(tr_con$a.50[i] + tr_con$bPP.50[i]*2 + tr_con$bPI.50[i]*1))
  }
  
  tr_sim <- gather(data.frame(tr_sim_0,tr_sim_1))
  barplot(table(tr_sim),xlim=c(0,8),width=0.8,xlab=" intercommunity trade",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(3,1000,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)  
  
  
  ## military mobilization
  mm_sim_0 <- c()
  mm_sim_1 <- c()
  for(i in 1:nr) {
    mm_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(mm_con$a.44[i] + mm_con$bPP.44[i]*-2 + mm_con$bPI.44[i]*1))
    mm_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(mm_con$a.44[i] + mm_con$bPP.44[i]*2 + mm_con$bPI.44[i]*1))
  }
  
  mm_sim <- gather(data.frame(mm_sim_0,mm_sim_1))
  barplot(table(mm_sim),xlim=c(0,8),width=0.8,xlab="military organization",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("C",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## land privatization
  lp_sim_min_sd <- c()
  lp_sim_plus_sd <- c()
  for(i in 1:nr) {
    lp_sim_min_sd[i] <- rbeta2(1,logistic(lp_con$a.50[i] + lp_con$bLP.50[i]*-2 + lp_con$bIA.50[i]*1),lp_con$theta)
    lp_sim_plus_sd[i] <- rbeta2(1,logistic(lp_con$a.50[i] + lp_con$bLP.50[i]*2 + lp_con$bIA.50[i]*1),lp_con$theta)
  }
  
  hist(lp_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1700),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(lp_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("D",3,line=0.25,adj=0)
  legend(0.1,1700,legend=c("-2SD land privatization","+2SD land privatization"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## social stratification
  ss_sim_0 <- c()
  ss_sim_1 <- c()
  for(i in 1:nr) {
    ss_sim_0[i] <- rbeta2(1,logistic(ss_con$a.50[i] + ss_con$bSS.50[i]*0 + ss_con$bLP.50[i]*0 + ss_con$bIA.50[i]*1),ss_con$theta)
    ss_sim_1[i] <- rbeta2(1,logistic(ss_con$a.50[i] + ss_con$bSS.50[i]*1 + ss_con$bLP.50[i]*0 + ss_con$bIA.50[i]*1),ss_con$theta)
  }
  
  hist(ss_sim_0,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1200),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(ss_sim_1,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("E",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("low stratification","high stratification"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## population density
  pd_sim_0 <- c()
  pd_sim_1 <- c()
  for(i in 1:nr) {
    pd_sim_0[i] <- rbeta2(1,logistic(pd_con$a.50[i] + pd_con$bPD.50[i]*0 + pd_con$bIA.50[i]*1),pd_con$theta)
    pd_sim_1[i] <- rbeta2(1,logistic(pd_con$a.50[i] + pd_con$bPD.50[i]*1 + pd_con$bIA.50[i]*1),pd_con$theta)
  }
  
  hist(pd_sim_0,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1400),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(pd_sim_1,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("F",3,line=0.25,adj=0)
  legend(0.1,1400,legend=c("low population density","high population density"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## altitude
  al_sim_min_sd <- c()
  al_sim_plus_sd <- c()
  for(i in 1:nr) {
    al_sim_min_sd[i] <- rbeta2(1,logistic(al_con$a.50[i] + al_con$bAL.50[i]*-2 + al_con$bIC.50[i]*0),al_con$theta)
    al_sim_plus_sd[i] <- rbeta2(1,logistic(al_con$a.50[i] + al_con$bAL.50[i]*2 + al_con$bIC.50[i]*0),al_con$theta)
  }
  
  hist(al_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1300),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(al_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("G",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("-2SD altitude","+2SD altitude"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## incline
  ic_sim_min_sd <- c()
  ic_sim_plus_sd <- c()
  for(i in 1:nr) {
    ic_sim_min_sd[i] <- rbeta2(1,logistic(ic_con$a.50[i] + ic_con$bIC.50[i]*-2),ic_con$theta)
    ic_sim_plus_sd[i] <- rbeta2(1,logistic(ic_con$a.50[i] + ic_con$bIC.50[i]*2),ic_con$theta)
  }
  
  hist(al_sim_min_sd,xlab="probability of women married monogamously",ylab="frequency",main="",xlim=c(0,1),ylim=c(0,1300),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(al_sim_plus_sd,col=col.alpha("#784617",0.6),add=TRUE)
  axis(1,at=seq(0,1,length.out=6),labels=c("0.0","0.2","0.4","0.6","0.8","1.0"),cex.axis=1.5,tick=FALSE)
  mtext("H",3,line=0.25,adj=0)
  legend(0.1,1200,legend=c("-2SD incline","+2SD incline"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
}


## Figure 21 (Quechuan, binary)
{
  
  par(mfrow=c(4,2),mar=c(5.1,6.1,2.6,3.1))
  names_mp <- c("polygyny","monogamy")
  
  ## assault frequency
  af_sim_min_sd <- c()
  af_sim_plus_sd <- c()
  for(i in 1:nr) {
    af_sim_min_sd[i] <- rnorm(1,af_bin$a.59[i] + af_bin$bMP.59[i]*-2 + af_bin$bPI.59[i]*1,af_bin$sigma_phy)
    af_sim_plus_sd[i] <- rnorm(1,af_bin$a.59[i] + af_bin$bMP.59[i]*2 + af_bin$bPI.59[i]*1,af_bin$sigma_phy)
  }
  
  hist(af_sim_min_sd,xlab="assault frequency (SD)",ylab="frequency",main="",breaks=20,xlim=c(-3,3),ylim=c(0,800),col="gray75",cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(af_sim_plus_sd,breaks=20,col=col.alpha("#5E8C7B",0.6),add=TRUE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2.0","-1.0","0.0","1.0","2.0"),cex.axis=1.5,tick=FALSE)
  mtext("A",3,line=0.25,adj=0)
  legend(-0.5,800,legend=c("+2SD monogamous women","-2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## trade
  tr_sim_0 <- c()
  tr_sim_1 <- c()
  for(i in 1:nr) {
    tr_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(tr_bin$a.59[i] + tr_bin$bMP.59[i]*-2 + tr_bin$bPI.59[i]*1))
    tr_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(tr_bin$a.59[i] + tr_bin$bMP.59[i]*2 + tr_bin$bPI.59[i]*1))
  }
  
  tr_sim <- gather(data.frame(tr_sim_0,tr_sim_1))
  barplot(table(tr_sim),xlim=c(0,8),width=0.8,xlab=" intercommunity trade",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(3,1600,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)  
  
  
  ## military mobilization
  mm_sim_0 <- c()
  mm_sim_1 <- c()
  for(i in 1:nr) {
    mm_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(mm_bin$a.51[i] + mm_bin$bMP.51[i]*-2 + mm_bin$bPI.51[i]*1))
    mm_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(mm_bin$a.51[i] + mm_bin$bMP.51[i]*2 + mm_bin$bPI.51[i]*1))
  }
  
  mm_sim <- gather(data.frame(mm_sim_0,mm_sim_1))
  barplot(table(mm_sim),xlim=c(0,8),width=0.8,xlab="military organization",ylab="frequency",beside=TRUE,col=c("gray75","#5E8C7B"),names.arg=c("low","high"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("C",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD monogamous women","+2SD monogamous women"),col=c("gray75","#5E8C7B"),pch=15,box.col=NA,cex=1.5)
  
  
  ## land privatization
  lp_sim_min_sd <- c()
  lp_sim_plus_sd <- c()
  for(i in 1:nr) {
    lp_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(lp_bin$a.59[i] + lp_bin$bLP.59[i]*-2 + lp_bin$bIA.59[i]*1))
    lp_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(lp_bin$a.59[i] + lp_bin$bLP.59[i]*2 + lp_bin$bIA.59[i]*1))
  }
  
  lp_sim <- gather(data.frame(lp_sim_min_sd,lp_sim_plus_sd))
  barplot(table(lp_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("D",3,line=0.25,adj=0)
  legend(3,1700,legend=c("-2SD land privatization","+2SD land privatization"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## social stratification
  ss_sim_0 <- c()
  ss_sim_1 <- c()
  for(i in 1:nr) {
    ss_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(ss_bin$a.59[i] + ss_bin$bSS.59[i]*0 + ss_bin$bLP.59[i]*0 + ss_bin$bIA.59[i]*1))
    ss_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(ss_bin$a.59[i] + ss_bin$bSS.59[i]*1 + ss_bin$bLP.59[i]*0 + ss_bin$bIA.59[i]*1))
  }
  
  ss_sim <- gather(data.frame(ss_sim_0,ss_sim_1))
  barplot(table(ss_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("E",3,line=0.25,adj=0)
  legend(3,1000,legend=c("low stratification","high stratification"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## population density
  pd_sim_0 <- c()
  pd_sim_1 <- c()
  for(i in 1:nr) {
    pd_sim_0[i] <- rbinom(1,size=1,prob=inv_logit(pd_bin$a.59[i] + pd_bin$bPD.59[i]*0 + pd_bin$bIA.59[i]*1))
    pd_sim_1[i] <- rbinom(1,size=1,prob=inv_logit(pd_bin$a.59[i] + pd_bin$bPD.59[i]*1 + pd_bin$bIA.59[i]*1))
  }
  
  pd_sim <- gather(data.frame(pd_sim_0,pd_sim_1))
  barplot(table(pd_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("F",3,line=0.25,adj=0)
  legend(3,1000,legend=c("low population density","high population density"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## altitude
  al_sim_min_sd <- c()
  al_sim_plus_sd <- c()
  for(i in 1:nr) {
    al_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(al_bin$a.59[i] + al_bin$bAL.59[i]*-2))
    al_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(al_bin$a.59[i] + al_bin$bAL.59[i]*2))
  }
  
  al_sim <- gather(data.frame(al_sim_min_sd,al_sim_plus_sd))
  barplot(table(al_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("G",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD altitude","+2SD altitude"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
  
  ## incline
  ic_sim_min_sd <- c()
  ic_sim_plus_sd <- c()
  for(i in 1:nr) {
    ic_sim_min_sd[i] <- rbinom(1,size=1,prob=inv_logit(ic_bin$a.59[i] + ic_bin$bIC.59[i]*-2))
    ic_sim_plus_sd[i] <- rbinom(1,size=1,prob=inv_logit(ic_bin$a.59[i] + ic_bin$bIC.59[i]*2))
  }
  
  ic_sim <- gather(data.frame(ic_sim_min_sd,ic_sim_plus_sd))
  barplot(table(ic_sim),xlim=c(0,8),width=0.8,xlab="monogamy",ylab="frequency",beside=TRUE,col=c("gray75","#784617"),names.arg=names_mp,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("H",3,line=0.25,adj=0)
  legend(3,1200,legend=c("-2SD incline","+2SD incline"),col=c("gray75","#784617"),pch=15,box.col=NA,cex=1.5)
  
}


dev.off()


################################################################################