################################################################################
################################################################################


setwd("C:/R_folder/monogamy")

library(tidyr)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(ggmap)
library(rethinking)

## read posteriors and compute probabilities for estimated effects (PP)
## full sample
tr_post <- read.csv("tr_post.csv",header=TRUE)
mm_post <- read.csv("mm_post.csv",header=TRUE)
mc_post <- read.csv("mc_post.csv",header=TRUE)
ps_post <- read.csv("ps_post.csv",header=TRUE)
wi_post <- read.csv("wi_post.csv",header=TRUE)
rw_post <- read.csv("rw_post.csv",header=TRUE)
pd_post <- read.csv("pd_post.csv",header=TRUE)
dc_post <- read.csv("dc_post.csv",header=TRUE)
ai_post <- read.csv("ai_post.csv",header=TRUE)
vi_post <- read.csv("vi_post.csv",header=TRUE)
vn_post <- read.csv("vn_post.csv",header=TRUE)
tr_vareff_post <- read.csv("tr_vareff_post.csv",header=TRUE)
mm_vareff_post <- read.csv("mm_vareff_post.csv",header=TRUE)
mc_vareff_post <- read.csv("mc_vareff_post.csv",header=TRUE)
ps_vareff_post <- read.csv("ps_vareff_post.csv",header=TRUE)
wi_vareff_post <- read.csv("wi_vareff_post.csv",header=TRUE)
rw_vareff_post <- read.csv("rw_vareff_post.csv",header=TRUE)
pd_vareff_post <- read.csv("pd_vareff_post.csv",header=TRUE)
dc_vareff_post <- read.csv("dc_vareff_post.csv",header=TRUE)
ai_vareff_post <- read.csv("ai_vareff_post.csv",header=TRUE)
vi_vareff_post <- read.csv("vi_vareff_post.csv",header=TRUE)
vn_vareff_post <- read.csv("vn_vareff_post.csv",header=TRUE)


## PP
## models with phylogenetic and ethnographic covariance structures
round((sum(tr_post$bMR>0)/nrow(tr_post))*100,2) # marriage rules/trade - 73.70%
round((sum(mm_post$bMR>0)/nrow(mm_post))*100,2) # marriage rules/military mobilization - 39.15%
round((sum(mc_post$bMR<0)/nrow(mc_post))*100,2) # marriage rules/male competition - 99.85%
round((sum(ps_post$bPS>0)/nrow(ps_post))*100,2) # patrilineal succession - 47.15%
round((sum(wi_post$bWI>0)/nrow(wi_post))*100,2) # wealth inequality - 64.20%
round((sum(rw_post$bRW>0)/nrow(rw_post))*100,2) # rival wealth - 80.65%
round((sum(pd_post$bPD>0)/nrow(pd_post))*100,2) # population density - 57.30%
round((sum(dc_post$bDC<0)/nrow(dc_post))*100,2) # distance to coast - 70.45%
round((sum(ai_post$bAL>0)/nrow(ai_post))*100,2) # altitude - 70.35%
round((sum(ai_post$bIC>0)/nrow(ai_post))*100,2) # incline - 73.40%
round((sum(vi_post$bVI>0)/nrow(vi_post))*100,2) # vertical inheritance - 68.00%
round((sum(vn_post$bVN>0)/nrow(vn_post))*100,2) # virginity norms - 59.75%

## models with a varying effect of language family
round((sum(tr_vareff_post$mu_bMR>0)/nrow(tr_vareff_post))*100,2) # marriage rules/trade - 55.85%
round((sum(mm_vareff_post$mu_bMR>0)/nrow(mm_vareff_post))*100,2) # marriage rules/military mobilization - 39.05%
round((sum(mc_vareff_post$mu_bMR<0)/nrow(mc_vareff_post))*100,2) # marriage rules/male competition - 99.85%
round((sum(ps_vareff_post$mu_bPS>0)/nrow(ps_vareff_post))*100,2) # patrilineal succession - 51.75%
round((sum(wi_vareff_post$mu_bWI>0)/nrow(wi_vareff_post))*100,2) # wealth inequality - 66.45%
round((sum(rw_vareff_post$mu_bRW>0)/nrow(rw_vareff_post))*100,2) # rival wealth - 63.85%
round((sum(pd_vareff_post$mu_bPD>0)/nrow(pd_vareff_post))*100,2) # population density - 74.05%
round((sum(dc_vareff_post$mu_bDC<0)/nrow(dc_vareff_post))*100,2) # distance to coast - 64.05%
round((sum(ai_vareff_post$mu_bAL>0)/nrow(ai_vareff_post))*100,2) # altitude - 62.95%
round((sum(ai_vareff_post$mu_bIC>0)/nrow(ai_vareff_post))*100,2) # incline - 66.75%
round((sum(vi_vareff_post$mu_bVI>0)/nrow(vi_vareff_post))*100,2) # vertical inheritance - 58.90%
round((sum(vn_vareff_post$mu_bVN>0)/nrow(vn_vareff_post))*100,2) # virginity norms - 55.30%


## posteriors for models with Indo-Europeans excluded
tr_post <- read.csv("tr_noIE_post.csv",header=TRUE)
mm_post <- read.csv("mm_noIE_post.csv",header=TRUE)
mc_post <- read.csv("mc_noIE_post.csv",header=TRUE)
ps_post <- read.csv("ps_noIE_post.csv",header=TRUE)
wi_post <- read.csv("wi_noIE_post.csv",header=TRUE)
rw_post <- read.csv("rw_noIE_post.csv",header=TRUE)
pd_post <- read.csv("pd_noIE_post.csv",header=TRUE)
dc_post <- read.csv("dc_noIE_post.csv",header=TRUE)
ai_post <- read.csv("ai_noIE_post.csv",header=TRUE)
vi_post <- read.csv("vi_noIE_post.csv",header=TRUE)
vn_post <- read.csv("vn_noIE_post.csv",header=TRUE)
tr_vareff_post <- read.csv("tr_vareff_noIE_post.csv",header=TRUE)
mm_vareff_post <- read.csv("mm_vareff_noIE_post.csv",header=TRUE)
mc_vareff_post <- read.csv("mc_vareff_noIE_post.csv",header=TRUE)
ps_vareff_post <- read.csv("ps_vareff_noIE_post.csv",header=TRUE)
wi_vareff_post <- read.csv("wi_vareff_noIE_post.csv",header=TRUE)
rw_vareff_post <- read.csv("rw_vareff_noIE_post.csv",header=TRUE)
pd_vareff_post <- read.csv("pd_vareff_noIE_post.csv",header=TRUE)
dc_vareff_post <- read.csv("dc_vareff_noIE_post.csv",header=TRUE)
ai_vareff_post <- read.csv("ai_vareff_noIE_post.csv",header=TRUE)
vi_vareff_post <- read.csv("vi_vareff_noIE_post.csv",header=TRUE)
vn_vareff_post <- read.csv("vn_vareff_noIE_post.csv",header=TRUE)


## PP
## models with phylogenetic and ethnographic covariance structures
round((sum(tr_post$bMR>0)/nrow(tr_post))*100,2) # marriage rules/trade - 46.35%
round((sum(mm_post$bMR>0)/nrow(mm_post))*100,2) # marriage rules/military mobilization - 24.05%
round((sum(mc_post$bMR<0)/nrow(mc_post))*100,2) # marriage rules/male competition - 100%
round((sum(ps_post$bPS>0)/nrow(ps_post))*100,2) # patrilineal succession - 44.80%
round((sum(wi_post$bWI>0)/nrow(wi_post))*100,2) # wealth inequality - 40.85%
round((sum(rw_post$bRW>0)/nrow(rw_post))*100,2) # rival wealth - 75.05%
round((sum(pd_post$bPD>0)/nrow(pd_post))*100,2) # population density - 56.85%
round((sum(dc_post$bDC<0)/nrow(dc_post))*100,2) # distance to coast - 73.75%
round((sum(ai_post$bAL>0)/nrow(ai_post))*100,2) # altitude - 71.85%
round((sum(ai_post$bIC>0)/nrow(ai_post))*100,2) # incline - 77.35%
round((sum(vi_post$bVI>0)/nrow(vi_post))*100,2) # vertical inheritance - 64.75%
round((sum(vn_post$bVN>0)/nrow(vn_post))*100,2) # virginity norms - 56.30%

## models with a varying effect of language family
round((sum(tr_vareff_post$mu_bMR>0)/nrow(tr_vareff_post))*100,2) # marriage rules/trade - 48.80%
round((sum(mm_vareff_post$mu_bMR>0)/nrow(mm_vareff_post))*100,2) # marriage rules/military mobilization - 34.95%
round((sum(mc_vareff_post$mu_bMR<0)/nrow(mc_vareff_post))*100,2) # marriage rules/male competition - 100%
round((sum(ps_vareff_post$mu_bPS>0)/nrow(ps_vareff_post))*100,2) # patrilineal succession - 52.45%
round((sum(wi_vareff_post$mu_bWI>0)/nrow(wi_vareff_post))*100,2) # wealth inequality - 66.30%
round((sum(rw_vareff_post$mu_bRW>0)/nrow(rw_vareff_post))*100,2) # rival wealth - 57.80%
round((sum(pd_vareff_post$mu_bPD>0)/nrow(pd_vareff_post))*100,2) # population density - 72.45%
round((sum(dc_vareff_post$mu_bDC<0)/nrow(dc_vareff_post))*100,2) # distance to coast - 65.50%
round((sum(ai_vareff_post$mu_bAL>0)/nrow(ai_vareff_post))*100,2) # altitude - 63.55%
round((sum(ai_vareff_post$mu_bIC>0)/nrow(ai_vareff_post))*100,2) # incline - 69.35%
round((sum(vi_vareff_post$mu_bVI>0)/nrow(vi_vareff_post))*100,2) # vertical inheritance - 58.45%
round((sum(vn_vareff_post$mu_bVN>0)/nrow(vn_vareff_post))*100,2) # virginity norms - 54.55%


## posteriors for models with Indo-Europeans only
tr_post <- read.csv("tr_IE_post.csv",header=TRUE)
mm_post <- read.csv("mm_IE_post.csv",header=TRUE)
mc_post <- read.csv("mc_IE_post.csv",header=TRUE)
ps_post <- read.csv("ps_IE_post.csv",header=TRUE)
wi_post <- read.csv("wi_IE_post.csv",header=TRUE)
rw_post <- read.csv("rw_IE_post.csv",header=TRUE)
pd_post <- read.csv("pd_IE_post.csv",header=TRUE)
dc_post <- read.csv("dc_IE_post.csv",header=TRUE)
ai_post <- read.csv("ai_IE_post.csv",header=TRUE)
vi_post <- read.csv("vi_IE_post.csv",header=TRUE)
vn_post <- read.csv("vn_IE_post.csv",header=TRUE)


## PP
## models with phylogenetic and ethnographic covariance structures
round((sum(tr_post$bMR>0)/nrow(tr_post))*100,2) # marriage rules/trade - 63.15%
round((sum(mm_post$bMR>0)/nrow(mm_post))*100,2) # marriage rules/military mobilization - 64.95%
round((sum(mc_post$bMR<0)/nrow(mc_post))*100,2) # marriage rules/male competition - 40.95%
round((sum(ps_post$bPS>0)/nrow(ps_post))*100,2) # patrilineal succession - 51.15%
round((sum(wi_post$bWI>0)/nrow(wi_post))*100,2) # wealth inequality - 57.95%
round((sum(rw_post$bRW>0)/nrow(rw_post))*100,2) # rival wealth - 53.85%
round((sum(pd_post$bPD>0)/nrow(pd_post))*100,2) # population density - 50.00%
round((sum(dc_post$bDC<0)/nrow(dc_post))*100,2) # distance to coast - 46.00%
round((sum(ai_post$bAL>0)/nrow(ai_post))*100,2) # altitude - 49.20%
round((sum(ai_post$bIC>0)/nrow(ai_post))*100,2) # incline - 45.10%
round((sum(vi_post$bVI>0)/nrow(vi_post))*100,2) # vertical inheritance - 52.10%
round((sum(vn_post$bVN>0)/nrow(vn_post))*100,2) # virginity norms - 49.55%


######################################################
## compute slope coefficients for each language family - full sample
## we'll do that by summing up the population mean of slope coefficient ("fixed effect") and residual for each language family ("random effect")
## trade
tr_var_eff <- tr_vareff_post[,89:170]
tr_coefs_sum <- matrix(NA,nrow=nrow(tr_vareff_post),ncol=max(family))
tr_coefs_mu <- matrix(NA,nrow=max(family),ncol=1)
tr_coefs_ci <- matrix(NA,nrow=max(family),ncol=2)
for(j in 1:ncol(tr_coefs_sum)) {
  for(i in 1:nrow(tr_vareff_post)) {
    tr_coefs_sum[i,j] <- tr_vareff_post$mu_bMR[i] + tr_var_eff[i,j]
  }
}
tr_coefs_mu <- apply(tr_coefs_sum,2,mean)
tr_coefs_ci <- apply(tr_coefs_sum,2,PI)
round(cbind(tr_coefs_mu,tr_coefs_ci[1,],tr_coefs_ci[2,]),2)


## military mobilization
mm_var_eff <- mm_vareff_post[,76:147]
mm_coefs_sum <- matrix(NA,nrow=nrow(mm_vareff_post),ncol=max(mono_data_MM$family))
mm_coefs_mu <- matrix(NA,nrow=max(mono_data_MM$family),ncol=1)
mm_coefs_ci <- matrix(NA,nrow=max(mono_data_MM$family),ncol=2)
for(j in 1:ncol(mm_coefs_sum)) {
  for(i in 1:nrow(mm_vareff_post)) {
    mm_coefs_sum[i,j] <- mm_vareff_post$mu_bMR[i] + mm_var_eff[i,j]
  }
}
mm_coefs_mu <- apply(mm_coefs_sum,2,mean)
mm_coefs_ci <- apply(mm_coefs_sum,2,PI)
round(cbind(mm_coefs_mu,mm_coefs_ci[1,],mm_coefs_ci[2,]),2)


## male competition
mc_var_eff <- mc_vareff_post[,84:165]
mc_coefs_sum <- matrix(NA,nrow=nrow(mc_vareff_post),ncol=max(family))
mc_coefs_mu <- matrix(NA,nrow=max(family),ncol=1)
mc_coefs_ci <- matrix(NA,nrow=max(family),ncol=2)
for(j in 1:ncol(mc_coefs_sum)) {
  for(i in 1:nrow(mc_vareff_post)) {
    mc_coefs_sum[i,j] <- mc_vareff_post$mu_bMR[i] + mc_var_eff[i,j]
  }
}
mc_coefs_mu <- apply(mc_coefs_sum,2,mean)
mc_coefs_ci <- apply(mc_coefs_sum,2,PI)
round(cbind(mc_coefs_mu,mc_coefs_ci[1,],mc_coefs_ci[2,]),2)


## patrilineal succession
ps_var_eff <- ps_vareff_post[,88:169]
ps_coefs_sum <- matrix(NA,nrow=nrow(ps_vareff_post),ncol=max(family))
ps_coefs_mu <- matrix(NA,nrow=max(family),ncol=1)
ps_coefs_ci <- matrix(NA,nrow=max(family),ncol=2)
for(j in 1:ncol(ps_coefs_sum)) {
  for(i in 1:nrow(ps_vareff_post)) {
    ps_coefs_sum[i,j] <- ps_vareff_post$mu_bPS[i] + ps_var_eff[i,j]
  }
}
ps_coefs_mu <- apply(ps_coefs_sum,2,mean)
ps_coefs_ci <- apply(ps_coefs_sum,2,PI)
round(cbind(ps_coefs_mu,ps_coefs_ci[1,],ps_coefs_ci[2,]),2)


## wealth inequality
wi_var_eff <- wi_vareff_post[,88:169]
wi_coefs_sum <- matrix(NA,nrow=nrow(wi_vareff_post),ncol=max(family))
wi_coefs_mu <- matrix(NA,nrow=max(family),ncol=1)
wi_coefs_ci <- matrix(NA,nrow=max(family),ncol=2)
for(j in 1:ncol(wi_coefs_sum)) {
  for(i in 1:nrow(wi_vareff_post)) {
    wi_coefs_sum[i,j] <- wi_vareff_post$mu_bWI[i] + wi_var_eff[i,j]
  }
}
wi_coefs_mu <- apply(wi_coefs_sum,2,mean)
wi_coefs_ci <- apply(wi_coefs_sum,2,PI)
round(cbind(wi_coefs_mu,wi_coefs_ci[1,],wi_coefs_ci[2,]),2)


## rival wealth
rw_var_eff <- rw_vareff_post[,88:169]
rw_coefs_sum <- matrix(NA,nrow=nrow(rw_vareff_post),ncol=max(family))
rw_coefs_mu <- matrix(NA,nrow=max(family),ncol=1)
rw_coefs_ci <- matrix(NA,nrow=max(family),ncol=2)
for(j in 1:ncol(rw_coefs_sum)) {
  for(i in 1:nrow(rw_vareff_post)) {
    rw_coefs_sum[i,j] <- rw_vareff_post$mu_bRW[i] + rw_var_eff[i,j]
  }
}
rw_coefs_mu <- apply(rw_coefs_sum,2,mean)
rw_coefs_ci <- apply(rw_coefs_sum,2,PI)
round(cbind(rw_coefs_mu,rw_coefs_ci[1,],rw_coefs_ci[2,]),2)


## population density
pd_var_eff <- pd_vareff_post[,88:169]
pd_coefs_sum <- matrix(NA,nrow=nrow(pd_vareff_post),ncol=max(family))
pd_coefs_mu <- matrix(NA,nrow=max(family),ncol=1)
pd_coefs_ci <- matrix(NA,nrow=max(family),ncol=2)
for(j in 1:ncol(pd_coefs_sum)) {
  for(i in 1:nrow(pd_vareff_post)) {
    pd_coefs_sum[i,j] <- pd_vareff_post$mu_bPD[i] + pd_var_eff[i,j]
  }
}
pd_coefs_mu <- apply(pd_coefs_sum,2,mean)
pd_coefs_ci <- apply(pd_coefs_sum,2,PI)
round(cbind(pd_coefs_mu,pd_coefs_ci[1,],pd_coefs_ci[2,]),2)


## distance to coast
dc_var_eff <- dc_vareff_post[,88:169]
dc_coefs_sum <- matrix(NA,nrow=nrow(dc_vareff_post),ncol=max(family))
dc_coefs_mu <- matrix(NA,nrow=max(family),ncol=1)
dc_coefs_ci <- matrix(NA,nrow=max(family),ncol=2)
for(j in 1:ncol(dc_coefs_sum)) {
  for(i in 1:nrow(dc_vareff_post)) {
    dc_coefs_sum[i,j] <- dc_vareff_post$mu_bDC[i] + dc_var_eff[i,j]
  }
}
dc_coefs_mu <- apply(dc_coefs_sum,2,mean)
dc_coefs_ci <- apply(dc_coefs_sum,2,PI)
round(cbind(dc_coefs_mu,dc_coefs_ci[1,],dc_coefs_ci[2,]),2)


## altitude
ai_var_eff <- ai_vareff_post[,88:169]
ai_coefs_sum <- matrix(NA,nrow=nrow(ai_vareff_post),ncol=max(family))
ai_coefs_mu <- matrix(NA,nrow=max(family),ncol=1)
ai_coefs_ci <- matrix(NA,nrow=max(family),ncol=2)
for(j in 1:ncol(ai_coefs_sum)) {
  for(i in 1:nrow(ai_vareff_post)) {
    ai_coefs_sum[i,j] <- ai_vareff_post$mu_bAL[i] + ai_var_eff[i,j]
  }
}
ai_coefs_mu <- apply(ai_coefs_sum,2,mean)
ai_coefs_ci <- apply(ai_coefs_sum,2,PI)
round(cbind(ai_coefs_mu,ai_coefs_ci[1,],ai_coefs_ci[2,]),2)


## incline
ic_var_eff <- ai_vareff_post[,170:251]
ic_coefs_sum <- matrix(NA,nrow=nrow(ai_vareff_post),ncol=max(family))
ic_coefs_mu <- matrix(NA,nrow=max(family),ncol=1)
ic_coefs_ci <- matrix(NA,nrow=max(family),ncol=2)
for(j in 1:ncol(ic_coefs_sum)) {
  for(i in 1:nrow(ai_vareff_post)) {
    ic_coefs_sum[i,j] <- ai_vareff_post$mu_bIC[i] + ic_var_eff[i,j]
  }
}
ic_coefs_mu <- apply(ic_coefs_sum,2,mean)
ic_coefs_ci <- apply(ic_coefs_sum,2,PI)
round(cbind(ic_coefs_mu,ic_coefs_ci[1,],ic_coefs_ci[2,]),2)


## vertical inheritance
vi_var_eff <- vi_vareff_post[,62:117]
vi_coefs_sum <- matrix(NA,nrow=nrow(vi_vareff_post),ncol=max(mono_data_MI$family))
vi_coefs_mu <- matrix(NA,nrow=max(mono_data_MI$family),ncol=1)
vi_coefs_ci <- matrix(NA,nrow=max(mono_data_MI$family),ncol=2)
for(j in 1:ncol(vi_coefs_sum)) {
  for(i in 1:nrow(vi_vareff_post)) {
    vi_coefs_sum[i,j] <- vi_vareff_post$mu_bVI[i] + vi_var_eff[i,j]
  }
}
vi_coefs_mu <- apply(vi_coefs_sum,2,mean)
vi_coefs_ci <- apply(vi_coefs_sum,2,PI)
round(cbind(vi_coefs_mu,vi_coefs_ci[1,],vi_coefs_ci[2,]),2)


## virginity norms
vn_var_eff <- vn_vareff_post[,62:117]
vn_coefs_sum <- matrix(NA,nrow=nrow(vn_vareff_post),ncol=max(mono_data_MI$family))
vn_coefs_mu <- matrix(NA,nrow=max(mono_data_MI$family),ncol=1)
vn_coefs_ci <- matrix(NA,nrow=max(mono_data_MI$family),ncol=2)
for(j in 1:ncol(vn_coefs_sum)) {
  for(i in 1:nrow(vn_vareff_post)) {
    vn_coefs_sum[i,j] <- vn_vareff_post$mu_bVN[i] + vn_var_eff[i,j]
  }
}
vn_coefs_mu <- apply(vn_coefs_sum,2,mean)
vn_coefs_ci <- apply(vn_coefs_sum,2,PI)
round(cbind(vn_coefs_mu,vn_coefs_ci[1,],vn_coefs_ci[2,]),2)


## Figure 1
mono_data <- read.csv("monogamy_NEW.csv",header=TRUE)
original <- c(1,2,3,4,5)
new <- c(5,4,3,2,1)
MR <- new[match(mono_data$normative_monogamy,original)] # invert the scale

# map
labs_mono <- c("polygyny preferred by most men (N=48)","polygyny preferred by wealthy men (N=33)","polygyny preferred by leaders (N=44)","monogamy preferred (N=31)","monogamy prescribed (N=30)")
cols_mono <- c("#043e7d","#046ec4","#5791b3","#83b1cf","#b5d1e2")
world <- map_data("world")
map <- ggplot() + geom_polygon(data=world,aes(x=long,y=lat,group=group),fill="gray85",color="gray85") + theme_nothing() + ggtitle("a") + labs(x="",y="") + coord_fixed(1.3) + geom_point(data=mono_data,aes(x=longitude,y=latitude,fill=as.factor(MR)),pch=21,color="gray25",size=3) + ylim(-65,85) + theme(legend.position="bottom",legend.title=element_blank(),legend.text=element_text(size=16),axis.text.x=element_text(size=16),axis.text.y=element_text(size=16),text=element_text(size=16),plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) + scale_fill_manual(labels=labs_mono,values=cols_mono)
map


## plot posterior distribution of each effect
## models with phylogenetic and ethnographic covariance structures
tr_dens <- density(tr_post$bMR)
mm_dens <- density(mm_post$bMR)
mc_dens <- density(mc_post$bMR)
ps_dens <- density(ps_post$bPS)
wi_dens <- density(wi_post$bWI)
rw_dens <- density(rw_post$bRW)
pd_dens <- density(pd_post$bPD)
dc_dens <- density(dc_post$bDC)
al_dens <- density(ai_post$bAL)
ic_dens <- density(ai_post$bIC)
vi_dens <- density(vi_post$bVI)
vn_dens <- density(vn_post$bVN)

eff_dens <- cbind(tr_dens,mm_dens,mc_dens,ps_dens,wi_dens,rw_dens,pd_dens,dc_dens,al_dens,ic_dens,vi_dens,vn_dens)
names <- c("marriage rules (trade)","marriage rules (military mobilization)","marriage rules (male competition)","patrilineal succession","wealth inequality","rival wealth","population density","distance to coast","altitude","incline","vertical inheritance","virginity norms")
cols <- c("#440154FF","#440154FF","#440154FF","#5D4037","yellow2","yellow2","#388E3C","#388E3C","#388E3C","#388E3C","#FD5901","#FD5901")
PP <- c("73.70","39.15",">99","47.15","64.20","80.65","57.30","70.45","70.35","73.40","68.00","59.75")
xlim <- cbind(tr_post$bMR,mm_post$bMR,mc_post$bMR,ps_post$bPS,wi_post$bWI,rw_post$bRW,pd_post$bPD,dc_post$bDC,ai_post$bAL,ai_post$bIC,vi_post$bVI,vn_post$bVN)
ylim <- cbind(tr_dens$y,mm_dens$y,mc_dens$y,ps_dens$y,wi_dens$y,rw_dens$y,pd_dens$y,dc_dens$y,al_dens$y,ic_dens$y,vi_dens$y,vn_dens$y)

## plot the densities
par(mfrow=c(6,2),mar=c(5.1,5.1,1.5,1.1))
for(j in 1:ncol(eff_dens)) {
  plot(NULL,xlim=range(xlim[,j]),ylim=range(ylim[,j]),xlab=names[j],ylab="density", main="",lwd=3,cex.lab=1.75,cex.axis=1.75)
  lines(eff_dens[,j], col=cols[j], lwd=3)
  polygon(eff_dens[,j], col=col.alpha(cols[j], alpha=0.75), border=NA)
  mtext(PP[j],line=-2.5,adj=0.975,col=cols[j],cex=1.5)
  abline(v=0,lty=2,lwd=2,col="gray75")
}


## models with a varying effect of language family
tr_dens <- density(tr_vareff_post$mu_bMR)
mm_dens <- density(mm_vareff_post$mu_bMR)
mc_dens <- density(mc_vareff_post$mu_bMR)
ps_dens <- density(ps_vareff_post$mu_bPS)
wi_dens <- density(wi_vareff_post$mu_bWI)
rw_dens <- density(rw_vareff_post$mu_bRW)
pd_dens <- density(pd_vareff_post$mu_bPD)
dc_dens <- density(dc_vareff_post$mu_bDC)
al_dens <- density(ai_vareff_post$mu_bAL)
ic_dens <- density(ai_vareff_post$mu_bIC)
vi_dens <- density(vi_vareff_post$mu_bVI)
vn_dens <- density(vn_vareff_post$mu_bVN)

eff_dens <- cbind(tr_dens,mm_dens,mc_dens,ps_dens,wi_dens,rw_dens,pd_dens,dc_dens,al_dens,ic_dens,vi_dens,vn_dens)
PP <- c("55.85","39.05",">99","51.75","66.45","63.85","74.05","64.05","62.95","66.75","58.90","55.30")
xlim <- cbind(tr_vareff_post$mu_bMR,mm_vareff_post$mu_bMR,mc_vareff_post$mu_bMR,ps_vareff_post$mu_bPS,wi_vareff_post$mu_bWI,rw_vareff_post$mu_bRW,pd_vareff_post$mu_bPD,dc_vareff_post$mu_bDC,ai_vareff_post$mu_bAL,ai_vareff_post$mu_bIC,vi_vareff_post$mu_bVI,vn_vareff_post$mu_bVN)
ylim <- cbind(tr_dens$y,mm_dens$y,mc_dens$y,ps_dens$y,wi_dens$y,rw_dens$y,pd_dens$y,dc_dens$y,al_dens$y,ic_dens$y,vi_dens$y,vn_dens$y)

## plot the densities
for(j in 1:ncol(eff_dens)) {
  plot(NULL,xlim=range(xlim[,j]),ylim=range(ylim[,j]),xlab=names[j],ylab="density", main="",lwd=3,cex.lab=1.75,cex.axis=1.75)
  lines(eff_dens[,j], col=cols[j], lwd=3)
  polygon(eff_dens[,j], col=col.alpha(cols[j], alpha=0.7), border=NA)
  mtext(PP[j],line=-2.5,adj=0.975,col=cols[j],cex=1.5)
  abline(v=0,lty=2,lwd=2,col="gray75")
}


## plot phylogenetic and ethnographic covariance for each model
eta_phy <- cbind(tr_post$eta_phy,mm_post$eta_phy,mc_post$eta_phy,ps_post$eta_phy,wi_post$eta_phy,rw_post$eta_phy,pd_post$eta_phy,dc_post$eta_phy,ai_post$eta_phy,vi_post$eta_phy,vn_post$eta_phy)
eta_ep <- cbind(tr_post$eta_ep,mm_post$eta_ep,mc_post$eta_ep,ps_post$eta_ep,wi_post$eta_ep,rw_post$eta_ep,pd_post$eta_ep,dc_post$eta_ep,ai_post$eta_ep,vi_post$eta_ep,vn_post$eta_ep)
rho_phy <- cbind(tr_post$rho_phy,mm_post$rho_phy,mc_post$rho_phy,ps_post$rho_phy,wi_post$rho_phy,rw_post$rho_phy,pd_post$rho_phy,dc_post$rho_phy,ai_post$rho_phy,vi_post$rho_phy,vn_post$rho_phy)
rho_ep <- cbind(tr_post$rho_ep,mm_post$rho_ep,mc_post$rho_ep,ps_post$rho_ep,wi_post$rho_ep,rw_post$rho_ep,pd_post$rho_ep,dc_post$rho_ep,ai_post$rho_ep,vi_post$rho_ep,vn_post$rho_ep)
names <- c("marriage rules (TR)","marriage rules (MM)","marriage rules (MC)","patrilineal succession","wealth inequality","rival wealth","population density","distance coast","altitude/incline","vertical inheritance","virginity norms")
x_seq <- seq(from=0,to=10,length.out=100)

par(mfrow=c(2,6),mar=c(4.6,5.1,3.1,1.1))
for(j in 1:11) {
plot(NULL,xlim=c(0,1),ylim=c(0,10),xlab="",ylab="",main="",xaxt="n",cex.axis=1.75,cex.main=1.5)
axis(1,at=seq(from=0,to=1,length.out=5),labels=c("0.0","0.25","0.5","0.75","1.0"),cex.axis=1.75)
mtext(names[j],3,line=0.25,adj=0,cex=1.25)
mtext("covariance",2,line=2.75,cex=1.25)
mtext("distance",1,line=2.75,cex=1.25)
legend(0.35,10,lwd=5,legend=c("phylo","ethno"),col=c("#E99204","#009C95"),cex=1.5,box.col=NA)

phy_cov <- sapply(x_seq,function(x) eta_phy[,j]*exp(-rho_phy[,j]*x))
phy_cov_mu <- apply(phy_cov,2,mean)
shade(apply(phy_cov,2,PI),x_seq,col=col.alpha("#E99204",0.4))
lines(x_seq,phy_cov_mu,lwd=5,col="#E99204")

ep_cov <- sapply(x_seq,function(x) eta_ep[,j]*exp(-rho_ep[,j]*x))
ep_cov_mu <- apply(ep_cov,2,mean)
shade(apply(ep_cov,2,PI),x_seq,col=col.alpha("#009C95",0.4))
lines(x_seq,ep_cov_mu,lwd=5,col="#009C95")
}


## Figure 3
## simulate frequency distributions of monogamy under each model and across different values of each predictor
## phylogenetic models
{

nr <- 2000
par(mfrow=c(6,2),mar=c(5.1,8.1,2.6,5.1))
names_mr <- c("full polygyny","2","3","4","normative monogamy")

## trade
tr_sim_1 <- c()
tr_sim_5 <- c()
for(i in 1:nr) {
tr_sim_1[i] <- rordlogit(1,tr_post$bMR[i]*1,tr_post[i,2:6])
tr_sim_5[i] <- rordlogit(1,tr_post$bMR[i]*5,tr_post[i,2:6])
}

tr_sim <- gather(data.frame(tr_sim_1,tr_sim_5))
barplot(table(tr_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,axes=TRUE,col=c("gray75","#440154FF"),border=NA,names.arg=c("no trade","2","3","4","5",">50% of food"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
mtext("A",3,line=0.25,adj=0)
legend(12.5,700,legend=c("full polygyny","normative monogamy"),col=c("gray75","#440154FF"),pch=15,box.col=NA,cex=1.5)


## military mobilization
mm_sim_1 <- c()
mm_sim_5 <- c()
for(i in 1:nr) {
  mm_sim_1[i] <- rordlogit(1,mm_post$bMR[i]*1,mm_post[i,2:3])
  mm_sim_5[i] <- rordlogit(1,mm_post$bMR[i]*5,mm_post[i,2:3])
}

mm_sim <- gather(data.frame(mm_sim_1,mm_sim_5))
barplot(table(mm_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#440154FF"),border=NA,names.arg=c("no military organization","2","standing armies"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
mtext("B",3,line=0.25,adj=0)
legend(6,1000,legend=c("full polygyny","normative monogamy"),col=c("gray75","#440154FF"),pch=15,box.col=NA,cex=1.5)


## male competition
mc_sim_1 <- c()
mc_sim_5 <- c()
for(i in 1:nr) {
  mc_sim_1[i] <- rnorm(1,mc_post$a[i] + mc_post$bMR[i]*1,mc_post$sigma[i])
  mc_sim_5[i] <- rnorm(1,mc_post$a[i] + mc_post$bMR[i]*5,mc_post$sigma[i])
}

mc_sim <- cbind(mc_sim_1,mc_sim_5)
hist(mc_sim_1,xlab="simulated outcome",ylab="frequency",main="",xlim=c(-2.5,2.5),col="gray75",border=NA,cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
hist(mc_sim_5,col="#440154FF",border=NA,add=TRUE)
axis(1,at=seq(-2,2,length.out=5),labels=c("-2","-1","0","1","2"),cex.axis=1.5,tick=FALSE)
mtext("C",3,line=0.25,adj=0)
legend(0.85,500,legend=c("full polygyny","normative monogamy"),col=c("gray75","#440154FF"),pch=15,box.col=NA,cex=1.5)


## patrilineal succession
ps_sim_0 <- c()
ps_sim_1 <- c()
for(i in 1:nr) {
  ps_sim_0[i] <- rordlogit(1,ps_post$bPS[i]*0,ps_post[i,2:5])
  ps_sim_1[i] <- rordlogit(1,ps_post$bPS[i]*1,ps_post[i,2:5])
}

ps_sim <- gather(data.frame(ps_sim_0,ps_sim_1))
barplot(table(ps_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#6D4C41"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
mtext("D",3,line=0.25,adj=0)
legend(10,700,legend=c("non-patrilineal","patrilineal"),col=c("gray75","#6D4C41"),pch=15,box.col=NA,cex=1.5)


## wealth inequality
wi_sim_1 <- c()
wi_sim_5 <- c()
for(i in 1:nr) {
  wi_sim_1[i] <- rordlogit(1,wi_post$bWI[i]*1,wi_post[i,2:5])
  wi_sim_5[i] <- rordlogit(1,wi_post$bWI[i]*5,wi_post[i,2:5])
}

wi_sim <- gather(data.frame(wi_sim_1,wi_sim_5))
barplot(table(wi_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","yellow2"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
mtext("E",3,line=0.25,adj=0)
legend(10,650,legend=c("low inequality","high inequality"),col=c("gray75","yellow2"),pch=15,box.col=NA,cex=1.5)


## rival wealth
rw_sim_min_sd <- c()
rw_sim_plus_sd <- c()
for(i in 1:nr) {
  rw_sim_min_sd[i] <- rordlogit(1,rw_post$bRW[i]*-2,rw_post[i,2:5])
  rw_sim_plus_sd[i] <- rordlogit(1,rw_post$bRW[i]*2,rw_post[i,2:5])
}

rw_sim <- gather(data.frame(rw_sim_min_sd,rw_sim_plus_sd))
barplot(table(rw_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","yellow2"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
mtext("F",3,line=0.25,adj=0)
legend(10,700,legend=c("-2SD in rival wealth","+2SD in rival wealth"),col=c("gray75","yellow2"),pch=15,box.col=NA,cex=1.5)


## population density
pd_sim_min_sd <- c()
pd_sim_plus_sd <- c()
for(i in 1:nr) {
  pd_sim_min_sd[i] <- rordlogit(1,pd_post$bPD[i]*-2,pd_post[i,2:5])
  pd_sim_plus_sd[i] <- rordlogit(1,pd_post$bPD[i]*2,pd_post[i,2:5])
}

pd_sim <- gather(data.frame(pd_sim_min_sd,pd_sim_plus_sd))
barplot(table(pd_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#388E3C"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
mtext("G",3,line=0.25,adj=0)
legend(9,700,legend=c("-2SD in population density","+2SD in population density"),col=c("gray75","#388E3C"),pch=15,box.col=NA,cex=1.5)


## distance to coast
dc_sim_min_sd <- c()
dc_sim_plus_sd <- c()
for(i in 1:nr) {
  dc_sim_min_sd[i] <- rordlogit(1,dc_post$bDC[i]*-2,dc_post[i,2:5])
  dc_sim_plus_sd[i] <- rordlogit(1,dc_post$bDC[i]*2,dc_post[i,2:5])
}

dc_sim <- gather(data.frame(dc_sim_min_sd,dc_sim_plus_sd))
barplot(table(dc_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("#388E3C","gray75"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
mtext("H",3,line=0.25,adj=0)
legend(9,700,legend=c("-2SD in distance to coast","+2SD in distance to coast"),col=c("#388E3C","gray75"),pch=15,box.col=NA,cex=1.5)


## altitude
al_sim_min_sd <- c()
al_sim_plus_sd <- c()
for(i in 1:nr) {
  al_sim_min_sd[i] <- rordlogit(1,ai_post$bAL[i]*-2,ai_post[i,2:5])
  al_sim_plus_sd[i] <- rordlogit(1,ai_post$bAL[i]*2,ai_post[i,2:5])
}

al_sim <- gather(data.frame(al_sim_min_sd,al_sim_plus_sd))
barplot(table(al_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#388E3C"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
mtext("I",3,line=0.25,adj=0)
legend(10,700,legend=c("-2SD in altitude","+2SD in altitude"),col=c("gray75","#388E3C"),pch=15,box.col=NA,cex=1.5)


## incline
ic_sim_min_sd <- c()
ic_sim_plus_sd <- c()
for(i in 1:nr) {
  ic_sim_min_sd[i] <- rordlogit(1,ai_post$bIC[i]*-2,ai_post[i,2:5])
  ic_sim_plus_sd[i] <- rordlogit(1,ai_post$bIC[i]*2,ai_post[i,2:5])
}

ic_sim <- gather(data.frame(ic_sim_min_sd,ic_sim_plus_sd))
barplot(table(ic_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#388E3C"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
mtext("J",3,line=0.25,adj=0)
legend(10,700,legend=c("-2SD in incline","+2SD in incline"),col=c("gray75","#388E3C"),pch=15,box.col=NA,cex=1.5)


## vertical inheritance
vi_sim_0 <- c()
vi_sim_1 <- c()
for(i in 1:nr) {
  vi_sim_0[i] <- rordlogit(1,vi_post$bVI[i]*0,vi_post[i,2:5])
  vi_sim_1[i] <- rordlogit(1,vi_post$bVI[i]*1,vi_post[i,2:5])
}

vi_sim <- gather(data.frame(vi_sim_0,vi_sim_1))
barplot(table(vi_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#FD5901"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
mtext("K",3,line=0.25,adj=0)
legend(10,600,legend=c("no vertical inheritance","vertical inheritance"),col=c("gray75","#FD5901"),pch=15,box.col=NA,cex=1.5)


## virginity norms
vn_sim_0 <- c()
vn_sim_1 <- c()
for(i in 1:nr) {
  vn_sim_0[i] <- rordlogit(1,vn_post$bVN[i]*0,vn_post[i,2:5])
  vn_sim_1[i] <- rordlogit(1,vn_post$bVN[i]*1,vn_post[i,2:5])
}

vn_sim <- gather(data.frame(vn_sim_0,vn_sim_1))
barplot(table(vn_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#FD5901"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
mtext("L",3,line=0.25,adj=0)
legend(10,600,legend=c("virginity not required","virginity required"),col=c("gray75","#FD5901"),pch=15,box.col=NA,cex=1.5)

}


## Figure 4
## non-phylogenetic models
{
  
  ## trade
  tr_sim_1 <- c()
  tr_sim_5 <- c()
  for(i in 1:nr) {
    tr_sim_1[i] <- rordlogit(1,tr_vareff_post$mu_bMR[i]*1,tr_vareff_post[i,2:6])
    tr_sim_5[i] <- rordlogit(1,tr_vareff_post$mu_bMR[i]*5,tr_vareff_post[i,2:6])
  }
  
  tr_sim <- gather(data.frame(tr_sim_1,tr_sim_5))
  barplot(table(tr_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,axes=TRUE,col=c("gray75","#440154FF"),border=NA,names.arg=c("no trade","2","3","4","5",">50% of food"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("A",3,line=0.25,adj=0)
  legend(12.5,800,legend=c("full polygyny","normative monogamy"),col=c("gray75","#440154FF"),pch=15,box.col=NA,cex=1.5)
  
  
  ## military mobilization
  mm_sim_1 <- c()
  mm_sim_5 <- c()
  for(i in 1:nr) {
    mm_sim_1[i] <- rordlogit(1,mm_vareff_post$mu_bMR[i]*1,mm_vareff_post[i,2:3])
    mm_sim_5[i] <- rordlogit(1,mm_vareff_post$mu_bMR[i]*5,mm_vareff_post[i,2:3])
  }
  
  mm_sim <- gather(data.frame(mm_sim_1,mm_sim_5))
  barplot(table(mm_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#440154FF"),border=NA,names.arg=c("no military organization","2","standing armies"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(6,1100,legend=c("full polygyny","normative monogamy"),col=c("gray75","#440154FF"),pch=15,box.col=NA,cex=1.5)
  
  
  ## male competition
  mc_sim_1 <- c()
  mc_sim_5 <- c()
  for(i in 1:nr) {
    mc_sim_1[i] <- rnorm(1,mc_vareff_post$mu_a[i] + mc_vareff_post$mu_bMR[i]*1,mc_vareff_post$sigma[i])
    mc_sim_5[i] <- rnorm(1,mc_vareff_post$mu_a[i] + mc_vareff_post$mu_bMR[i]*5,mc_vareff_post$sigma[i])
  }
  
  mc_sim <- cbind(mc_sim_1,mc_sim_5)
  hist(mc_sim_1,xlab="simulated outcome",ylab="frequency",main="",xlim=c(-2.5,2.5),col="gray75",border=NA,cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(mc_sim_5,col="#440154FF",border=NA,add=TRUE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2","-1","0","1","2"),cex.axis=1.5,tick=FALSE)
  mtext("C",3,line=0.25,adj=0)
  legend(0.85,300,legend=c("full polygyny","normative monogamy"),col=c("gray75","#440154FF"),pch=15,box.col=NA,cex=1.5)
  
  
  ## patrilineal succession
  ps_sim_0 <- c()
  ps_sim_1 <- c()
  for(i in 1:nr) {
    ps_sim_0[i] <- rordlogit(1,ps_vareff_post$mu_bPS[i]*0,ps_vareff_post[i,2:5])
    ps_sim_1[i] <- rordlogit(1,ps_vareff_post$mu_bPS[i]*1,ps_vareff_post[i,2:5])
  }
  
  ps_sim <- gather(data.frame(ps_sim_0,ps_sim_1))
  barplot(table(ps_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#6D4C41"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("D",3,line=0.25,adj=0)
  legend(10,700,legend=c("non-patrilineal","patrilineal"),col=c("gray75","#6D4C41"),pch=15,box.col=NA,cex=1.5)
  
  
  ## wealth inequality
  wi_sim_1 <- c()
  wi_sim_5 <- c()
  for(i in 1:nr) {
    wi_sim_1[i] <- rordlogit(1,wi_vareff_post$mu_bWI[i]*1,wi_vareff_post[i,2:5])
    wi_sim_5[i] <- rordlogit(1,wi_vareff_post$mu_bWI[i]*5,wi_vareff_post[i,2:5])
  }
  
  wi_sim <- gather(data.frame(wi_sim_1,wi_sim_5))
  barplot(table(wi_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","yellow2"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("E",3,line=0.25,adj=0)
  legend(10,700,legend=c("low inequality","high inequality"),col=c("gray75","yellow2"),pch=15,box.col=NA,cex=1.5)
  
  
  ## rival wealth
  rw_sim_min_sd <- c()
  rw_sim_plus_sd <- c()
  for(i in 1:nr) {
    rw_sim_min_sd[i] <- rordlogit(1,rw_vareff_post$mu_bRW[i]*-2,rw_vareff_post[i,2:5])
    rw_sim_plus_sd[i] <- rordlogit(1,rw_vareff_post$mu_bRW[i]*2,rw_vareff_post[i,2:5])
  }
  
  rw_sim <- gather(data.frame(rw_sim_min_sd,rw_sim_plus_sd))
  barplot(table(rw_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","yellow2"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("F",3,line=0.25,adj=0)
  legend(10,800,legend=c("-2SD in rival wealth","+2SD in rival wealth"),col=c("gray75","yellow2"),pch=15,box.col=NA,cex=1.5)
  
  
  ## population density
  pd_sim_min_sd <- c()
  pd_sim_plus_sd <- c()
  for(i in 1:nr) {
    pd_sim_min_sd[i] <- rordlogit(1,pd_vareff_post$mu_bPD[i]*-2,pd_vareff_post[i,2:5])
    pd_sim_plus_sd[i] <- rordlogit(1,pd_vareff_post$mu_bPD[i]*2,pd_vareff_post[i,2:5])
  }
  
  pd_sim <- gather(data.frame(pd_sim_min_sd,pd_sim_plus_sd))
  barplot(table(pd_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#388E3C"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("G",3,line=0.25,adj=0)
  legend(9,700,legend=c("-2SD in population density","+2SD in population density"),col=c("gray75","#388E3C"),pch=15,box.col=NA,cex=1.5)
  
  
  ## distance to coast
  dc_sim_min_sd <- c()
  dc_sim_plus_sd <- c()
  for(i in 1:nr) {
    dc_sim_min_sd[i] <- rordlogit(1,dc_vareff_post$mu_bDC[i]*-2,dc_vareff_post[i,2:5])
    dc_sim_plus_sd[i] <- rordlogit(1,dc_vareff_post$mu_bDC[i]*2,dc_vareff_post[i,2:5])
  }
  
  dc_sim <- gather(data.frame(dc_sim_plus_sd,dc_sim_min_sd))
  barplot(table(dc_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("#388E3C","gray75"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("H",3,line=0.25,adj=0)
  legend(9,800,legend=c("-2SD in distance to coast","+2SD in distance to coast"),col=c("#388E3C","gray75"),pch=15,box.col=NA,cex=1.5)
  
  
  ## altitude
  al_sim_min_sd <- c()
  al_sim_plus_sd <- c()
  for(i in 1:nr) {
    al_sim_min_sd[i] <- rordlogit(1,ai_vareff_post$mu_bAL[i]*-2,ai_vareff_post[i,2:5])
    al_sim_plus_sd[i] <- rordlogit(1,ai_vareff_post$mu_bAL[i]*2,ai_vareff_post[i,2:5])
  }
  
  al_sim <- gather(data.frame(al_sim_min_sd,al_sim_plus_sd))
  barplot(table(al_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#388E3C"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("I",3,line=0.25,adj=0)
  legend(10,800,legend=c("-2SD in altitude","+2SD in altitude"),col=c("gray75","#388E3C"),pch=15,box.col=NA,cex=1.5)
  
  
  ## incline
  ic_sim_min_sd <- c()
  ic_sim_plus_sd <- c()
  for(i in 1:nr) {
    ic_sim_min_sd[i] <- rordlogit(1,ai_vareff_post$mu_bIC[i]*-2,ai_vareff_post[i,2:5])
    ic_sim_plus_sd[i] <- rordlogit(1,ai_vareff_post$mu_bIC[i]*2,ai_vareff_post[i,2:5])
  }
  
  ic_sim <- gather(data.frame(ic_sim_min_sd,ic_sim_plus_sd))
  barplot(table(ic_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#388E3C"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("J",3,line=0.25,adj=0)
  legend(10,800,legend=c("-2SD in incline","+2SD in incline"),col=c("gray75","#388E3C"),pch=15,box.col=NA,cex=1.5)
  
  
  ## vertical inheritance
  vi_sim_0 <- c()
  vi_sim_1 <- c()
  for(i in 1:nr) {
    vi_sim_0[i] <- rordlogit(1,vi_vareff_post$mu_bVI[i]*0,vi_vareff_post[i,2:5])
    vi_sim_1[i] <- rordlogit(1,vi_vareff_post$mu_bVI[i]*1,vi_vareff_post[i,2:5])
  }
  
  vi_sim <- gather(data.frame(vi_sim_0,vi_sim_1))
  barplot(table(vi_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#FD5901"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("K",3,line=0.25,adj=0)
  legend(10,700,legend=c("no vertical inheritance","vertical inheritance"),col=c("gray75","#FD5901"),pch=15,box.col=NA,cex=1.5)
  
  
  ## virginity norms
  vn_sim_0 <- c()
  vn_sim_1 <- c()
  for(i in 1:nr) {
    vn_sim_0[i] <- rordlogit(1,vn_vareff_post$mu_bVN[i]*0,vn_vareff_post[i,2:5])
    vn_sim_1[i] <- rordlogit(1,vn_vareff_post$mu_bVN[i]*1,vn_vareff_post[i,2:5])
  }
  
  vn_sim <- gather(data.frame(vn_sim_0,vn_sim_1))
  barplot(table(vn_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#FD5901"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("L",3,line=0.25,adj=0)
  legend(10,700,legend=c("virginity not required","virginity required"),col=c("gray75","#FD5901"),pch=15,box.col=NA,cex=1.5)
  
}


## Figure 5
## generate distributions of monogamy for hypothetical IE societies
{
  
  ## trade
  tr_sim_1 <- c()
  tr_sim_5 <- c()
  for(i in 1:nr) {
    tr_sim_1[i] <- rordlogit(1,tr_vareff_post$bMR.32[i]*1,tr_vareff_post[i,2:6])
    tr_sim_5[i] <- rordlogit(1,tr_vareff_post$bMR.32[i]*5,tr_vareff_post[i,2:6])
  }
  
  tr_sim <- gather(data.frame(tr_sim_1,tr_sim_5))
  barplot(table(tr_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,axes=TRUE,col=c("gray75","#440154FF"),border=NA,names.arg=c("no trade","2","3","4","5",">50% of food"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("A",3,line=0.25,adj=0)
  legend(12.5,800,legend=c("full polygyny","normative monogamy"),col=c("gray75","#440154FF"),pch=15,box.col=NA,cex=1.5)
  
  
  ## military mobilization
  mm_sim_1 <- c()
  mm_sim_5 <- c()
  for(i in 1:nr) {
    mm_sim_1[i] <- rordlogit(1,mm_vareff_post$bMR.32[i]*1,mm_vareff_post[i,2:3])
    mm_sim_5[i] <- rordlogit(1,mm_vareff_post$bMR.32[i]*5,mm_vareff_post[i,2:3])
  }
  
  mm_sim <- gather(data.frame(mm_sim_1,mm_sim_5))
  barplot(table(mm_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#440154FF"),border=NA,names.arg=c("no military organization","2","standing armies"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(6,1000,legend=c("full polygyny","normative monogamy"),col=c("gray75","#440154FF"),pch=15,box.col=NA,cex=1.5)
  
  
  ## male competition
  mc_sim_1 <- c()
  mc_sim_5 <- c()
  for(i in 1:nr) {
    mc_sim_1[i] <- rnorm(1,mc_vareff_post$mu_a[i] + mc_vareff_post$bMR.32[i]*1,mc_vareff_post$sigma[i])
    mc_sim_5[i] <- rnorm(1,mc_vareff_post$mu_a[i] + mc_vareff_post$bMR.32[i]*5,mc_vareff_post$sigma[i])
  }
  
  mc_sim <- cbind(mc_sim_1,mc_sim_5)
  hist(mc_sim_1,xlab="simulated outcome",ylab="frequency",main="",xlim=c(-2.5,2.5),col="gray75",border=NA,cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(mc_sim_5,col="#440154FF",border=NA,add=TRUE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2","-1","0","1","2"),cex.axis=1.5,tick=FALSE)
  mtext("C",3,line=0.25,adj=0)
  legend(0.85,300,legend=c("full polygyny","normative monogamy"),col=c("gray75","#440154FF"),pch=15,box.col=NA,cex=1.5)
  
  
  ## patrilineal succession
  ps_sim_0 <- c()
  ps_sim_1 <- c()
  for(i in 1:nr) {
    ps_sim_0[i] <- rordlogit(1,ps_vareff_post$bPS.32[i]*0,ps_vareff_post[i,2:5])
    ps_sim_1[i] <- rordlogit(1,ps_vareff_post$bPS.32[i]*1,ps_vareff_post[i,2:5])
  }
  
  ps_sim <- gather(data.frame(ps_sim_0,ps_sim_1))
  barplot(table(ps_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#6D4C41"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("D",3,line=0.25,adj=0)
  legend(10,700,legend=c("non-patrilineal","patrilineal"),col=c("gray75","#6D4C41"),pch=15,box.col=NA,cex=1.5)
  
  
  ## wealth inequality
  wi_sim_1 <- c()
  wi_sim_5 <- c()
  for(i in 1:nr) {
    wi_sim_1[i] <- rordlogit(1,wi_vareff_post$bWI.32[i]*1,wi_vareff_post[i,2:5])
    wi_sim_5[i] <- rordlogit(1,wi_vareff_post$bWI.32[i]*5,wi_vareff_post[i,2:5])
  }
  
  wi_sim <- gather(data.frame(wi_sim_1,wi_sim_5))
  barplot(table(wi_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","yellow2"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("E",3,line=0.25,adj=0)
  legend(10,700,legend=c("low inequality","high inequality"),col=c("gray75","yellow2"),pch=15,box.col=NA,cex=1.5)
  
  
  ## rival wealth
  rw_sim_min_sd <- c()
  rw_sim_plus_sd <- c()
  for(i in 1:nr) {
    rw_sim_min_sd[i] <- rordlogit(1,rw_vareff_post$bRW.32[i]*-2,rw_vareff_post[i,2:5])
    rw_sim_plus_sd[i] <- rordlogit(1,rw_vareff_post$bRW.32[i]*2,rw_vareff_post[i,2:5])
  }
  
  rw_sim <- gather(data.frame(rw_sim_min_sd,rw_sim_plus_sd))
  barplot(table(rw_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","yellow2"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("F",3,line=0.25,adj=0)
  legend(10,800,legend=c("-2SD in rival wealth","+2SD in rival wealth"),col=c("gray75","yellow2"),pch=15,box.col=NA,cex=1.5)
  
  
  ## population density
  pd_sim_min_sd <- c()
  pd_sim_plus_sd <- c()
  for(i in 1:nr) {
    pd_sim_min_sd[i] <- rordlogit(1,pd_vareff_post$bPD.32[i]*-2,pd_vareff_post[i,2:5])
    pd_sim_plus_sd[i] <- rordlogit(1,pd_vareff_post$bPD.32[i]*2,pd_vareff_post[i,2:5])
  }
  
  pd_sim <- gather(data.frame(pd_sim_min_sd,pd_sim_plus_sd))
  barplot(table(pd_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#388E3C"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("G",3,line=0.25,adj=0)
  legend(9,700,legend=c("-2SD in population density","+2SD in population density"),col=c("gray75","#388E3C"),pch=15,box.col=NA,cex=1.5)
  
  
  ## distance to coast
  dc_sim_min_sd <- c()
  dc_sim_plus_sd <- c()
  for(i in 1:nr) {
    dc_sim_min_sd[i] <- rordlogit(1,dc_vareff_post$bDC.32[i]*-2,dc_vareff_post[i,2:5])
    dc_sim_plus_sd[i] <- rordlogit(1,dc_vareff_post$bDC.32[i]*2,dc_vareff_post[i,2:5])
  }
  
  dc_sim <- gather(data.frame(dc_sim_plus_sd,dc_sim_min_sd))
  barplot(table(dc_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("#388E3C","gray75"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("H",3,line=0.25,adj=0)
  legend(9,800,legend=c("-2SD in distance to coast","+2SD in distance to coast"),col=c("#388E3C","gray75"),pch=15,box.col=NA,cex=1.5)
  
  
  ## altitude
  al_sim_min_sd <- c()
  al_sim_plus_sd <- c()
  for(i in 1:nr) {
    al_sim_min_sd[i] <- rordlogit(1,ai_vareff_post$bAL.32[i]*-2,ai_vareff_post[i,2:5])
    al_sim_plus_sd[i] <- rordlogit(1,ai_vareff_post$bAL.32[i]*2,ai_vareff_post[i,2:5])
  }
  
  al_sim <- gather(data.frame(al_sim_min_sd,al_sim_plus_sd))
  barplot(table(al_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#388E3C"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("I",3,line=0.25,adj=0)
  legend(10,800,legend=c("-2SD in altitude","+2SD in altitude"),col=c("gray75","#388E3C"),pch=15,box.col=NA,cex=1.5)
  
  
  ## incline
  ic_sim_min_sd <- c()
  ic_sim_plus_sd <- c()
  for(i in 1:nr) {
    ic_sim_min_sd[i] <- rordlogit(1,ai_vareff_post$bIC.32[i]*-2,ai_vareff_post[i,2:5])
    ic_sim_plus_sd[i] <- rordlogit(1,ai_vareff_post$bIC.32[i]*2,ai_vareff_post[i,2:5])
  }
  
  ic_sim <- gather(data.frame(ic_sim_min_sd,ic_sim_plus_sd))
  barplot(table(ic_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#388E3C"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("J",3,line=0.25,adj=0)
  legend(10,800,legend=c("-2SD in incline","+2SD in incline"),col=c("gray75","#388E3C"),pch=15,box.col=NA,cex=1.5)
  
  
  ## vertical inheritance
  vi_sim_0 <- c()
  vi_sim_1 <- c()
  for(i in 1:nr) {
    vi_sim_0[i] <- rordlogit(1,vi_vareff_post$bVI.32[i]*0,vi_vareff_post[i,2:5])
    vi_sim_1[i] <- rordlogit(1,vi_vareff_post$bVI.32[i]*1,vi_vareff_post[i,2:5])
  }
  
  vi_sim <- gather(data.frame(vi_sim_0,vi_sim_1))
  barplot(table(vi_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#FD5901"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("K",3,line=0.25,adj=0)
  legend(10,700,legend=c("no vertical inheritance","vertical inheritance"),col=c("gray75","#FD5901"),pch=15,box.col=NA,cex=1.5)
  
  
  ## virginity norms
  vn_sim_0 <- c()
  vn_sim_1 <- c()
  for(i in 1:nr) {
    vn_sim_0[i] <- rordlogit(1,vn_vareff_post$bVN.32[i]*0,vn_vareff_post[i,2:5])
    vn_sim_1[i] <- rordlogit(1,vn_vareff_post$bVN.32[i]*1,vn_vareff_post[i,2:5])
  }
  
  vn_sim <- gather(data.frame(vn_sim_0,vn_sim_1))
  barplot(table(vn_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#FD5901"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("L",3,line=0.25,adj=0)
  legend(10,700,legend=c("virginity not required","virginity required"),col=c("gray75","#FD5901"),pch=15,box.col=NA,cex=1.5)
  
}


## SI Figure 4
## generate distributions of monogamy for models without IE
## phylogenetic models
{
  
  ## trade
  tr_sim_1 <- c()
  tr_sim_5 <- c()
  for(i in 1:nr) {
    tr_sim_1[i] <- rordlogit(1,tr_post$bMR[i]*1,tr_post[i,2:6])
    tr_sim_5[i] <- rordlogit(1,tr_post$bMR[i]*5,tr_post[i,2:6])
  }
  
  tr_sim <- gather(data.frame(tr_sim_1,tr_sim_5))
  barplot(table(tr_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,axes=TRUE,col=c("gray75","#440154FF"),border=NA,names.arg=c("no trade","2","3","4","5",">50% of food"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("A",3,line=0.25,adj=0)
  legend(12.5,700,legend=c("full polygyny","normative monogamy"),col=c("gray75","#440154FF"),pch=15,box.col=NA,cex=1.5)
  
  
  ## military mobilization
  mm_sim_1 <- c()
  mm_sim_5 <- c()
  for(i in 1:nr) {
    mm_sim_1[i] <- rordlogit(1,mm_post$bMR[i]*1,mm_post[i,2:3])
    mm_sim_5[i] <- rordlogit(1,mm_post$bMR[i]*5,mm_post[i,2:3])
  }
  
  mm_sim <- gather(data.frame(mm_sim_1,mm_sim_5))
  barplot(table(mm_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#440154FF"),border=NA,names.arg=c("no military organization","2","standing armies"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(6,1000,legend=c("full polygyny","normative monogamy"),col=c("gray75","#440154FF"),pch=15,box.col=NA,cex=1.5)
  
  
  ## male competition
  mc_sim_1 <- c()
  mc_sim_5 <- c()
  for(i in 1:nr) {
    mc_sim_1[i] <- rnorm(1,mc_post$a[i] + mc_post$bMR[i]*1,mc_post$sigma[i])
    mc_sim_5[i] <- rnorm(1,mc_post$a[i] + mc_post$bMR[i]*5,mc_post$sigma[i])
  }
  
  mc_sim <- cbind(mc_sim_1,mc_sim_5)
  hist(mc_sim_1,xlab="simulated outcome",ylab="frequency",main="",xlim=c(-2.5,2.5),col="gray75",border=NA,cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(mc_sim_5,col="#440154FF",border=NA,add=TRUE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2","-1","0","1","2"),cex.axis=1.5,tick=FALSE)
  mtext("C",3,line=0.25,adj=0)
  legend(0.85,500,legend=c("full polygyny","normative monogamy"),col=c("gray75","#440154FF"),pch=15,box.col=NA,cex=1.5)
  
  
  ## patrilineal succession
  ps_sim_0 <- c()
  ps_sim_1 <- c()
  for(i in 1:nr) {
    ps_sim_0[i] <- rordlogit(1,ps_post$bPS[i]*0,ps_post[i,2:5])
    ps_sim_1[i] <- rordlogit(1,ps_post$bPS[i]*1,ps_post[i,2:5])
  }
  
  ps_sim <- gather(data.frame(ps_sim_0,ps_sim_1))
  barplot(table(ps_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#6D4C41"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("D",3,line=0.25,adj=0)
  legend(10,700,legend=c("non-patrilineal","patrilineal"),col=c("gray75","#6D4C41"),pch=15,box.col=NA,cex=1.5)
  
  
  ## wealth inequality
  wi_sim_1 <- c()
  wi_sim_5 <- c()
  for(i in 1:nr) {
    wi_sim_1[i] <- rordlogit(1,wi_post$bWI[i]*1,wi_post[i,2:5])
    wi_sim_5[i] <- rordlogit(1,wi_post$bWI[i]*5,wi_post[i,2:5])
  }
  
  wi_sim <- gather(data.frame(wi_sim_1,wi_sim_5))
  barplot(table(wi_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","yellow2"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("E",3,line=0.25,adj=0)
  legend(10,650,legend=c("low inequality","high inequality"),col=c("gray75","yellow2"),pch=15,box.col=NA,cex=1.5)
  
  
  ## rival wealth
  rw_sim_min_sd <- c()
  rw_sim_plus_sd <- c()
  for(i in 1:nr) {
    rw_sim_min_sd[i] <- rordlogit(1,rw_post$bRW[i]*-2,rw_post[i,2:5])
    rw_sim_plus_sd[i] <- rordlogit(1,rw_post$bRW[i]*2,rw_post[i,2:5])
  }
  
  rw_sim <- gather(data.frame(rw_sim_min_sd,rw_sim_plus_sd))
  barplot(table(rw_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","yellow2"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("F",3,line=0.25,adj=0)
  legend(10,700,legend=c("-2SD in rival wealth","+2SD in rival wealth"),col=c("gray75","yellow2"),pch=15,box.col=NA,cex=1.5)
  
  
  ## population density
  pd_sim_min_sd <- c()
  pd_sim_plus_sd <- c()
  for(i in 1:nr) {
    pd_sim_min_sd[i] <- rordlogit(1,pd_post$bPD[i]*-2,pd_post[i,2:5])
    pd_sim_plus_sd[i] <- rordlogit(1,pd_post$bPD[i]*2,pd_post[i,2:5])
  }
  
  pd_sim <- gather(data.frame(pd_sim_min_sd,pd_sim_plus_sd))
  barplot(table(pd_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#388E3C"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("G",3,line=0.25,adj=0)
  legend(9,700,legend=c("-2SD in population density","+2SD in population density"),col=c("gray75","#388E3C"),pch=15,box.col=NA,cex=1.5)
  
  
  ## distance to coast
  dc_sim_min_sd <- c()
  dc_sim_plus_sd <- c()
  for(i in 1:nr) {
    dc_sim_min_sd[i] <- rordlogit(1,dc_post$bDC[i]*-2,dc_post[i,2:5])
    dc_sim_plus_sd[i] <- rordlogit(1,dc_post$bDC[i]*2,dc_post[i,2:5])
  }
  
  dc_sim <- gather(data.frame(dc_sim_plus_sd,dc_sim_min_sd))
  barplot(table(dc_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("#388E3C","gray75"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("H",3,line=0.25,adj=0)
  legend(9,700,legend=c("-2SD in distance to coast","+2SD in distance to coast"),col=c("#388E3C","gray75"),pch=15,box.col=NA,cex=1.5)
  
  
  ## altitude
  al_sim_min_sd <- c()
  al_sim_plus_sd <- c()
  for(i in 1:nr) {
    al_sim_min_sd[i] <- rordlogit(1,ai_post$bAL[i]*-2,ai_post[i,2:5])
    al_sim_plus_sd[i] <- rordlogit(1,ai_post$bAL[i]*2,ai_post[i,2:5])
  }
  
  al_sim <- gather(data.frame(al_sim_min_sd,al_sim_plus_sd))
  barplot(table(al_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#388E3C"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("I",3,line=0.25,adj=0)
  legend(10,700,legend=c("-2SD in altitude","+2SD in altitude"),col=c("gray75","#388E3C"),pch=15,box.col=NA,cex=1.5)
  
  
  ## incline
  ic_sim_min_sd <- c()
  ic_sim_plus_sd <- c()
  for(i in 1:nr) {
    ic_sim_min_sd[i] <- rordlogit(1,ai_post$bIC[i]*-2,ai_post[i,2:5])
    ic_sim_plus_sd[i] <- rordlogit(1,ai_post$bIC[i]*2,ai_post[i,2:5])
  }
  
  ic_sim <- gather(data.frame(ic_sim_min_sd,ic_sim_plus_sd))
  barplot(table(ic_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#388E3C"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("J",3,line=0.25,adj=0)
  legend(10,700,legend=c("-2SD in incline","+2SD in incline"),col=c("gray75","#388E3C"),pch=15,box.col=NA,cex=1.5)
  
  
  ## vertical inheritance
  vi_sim_0 <- c()
  vi_sim_1 <- c()
  for(i in 1:nr) {
    vi_sim_0[i] <- rordlogit(1,vi_post$bVI[i]*0,vi_post[i,2:5])
    vi_sim_1[i] <- rordlogit(1,vi_post$bVI[i]*1,vi_post[i,2:5])
  }
  
  vi_sim <- gather(data.frame(vi_sim_0,vi_sim_1))
  barplot(table(vi_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#FD5901"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("K",3,line=0.25,adj=0)
  legend(10,600,legend=c("no vertical inheritance","vertical inheritance"),col=c("gray75","#FD5901"),pch=15,box.col=NA,cex=1.5)
  
  
  ## virginity norms
  vn_sim_0 <- c()
  vn_sim_1 <- c()
  for(i in 1:nr) {
    vn_sim_0[i] <- rordlogit(1,vn_post$bVN[i]*0,vn_post[i,2:5])
    vn_sim_1[i] <- rordlogit(1,vn_post$bVN[i]*1,vn_post[i,2:5])
  }
  
  vn_sim <- gather(data.frame(vn_sim_0,vn_sim_1))
  barplot(table(vn_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#FD5901"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("L",3,line=0.25,adj=0)
  legend(10,600,legend=c("virginity not required","virginity required"),col=c("gray75","#FD5901"),pch=15,box.col=NA,cex=1.5)
  
}


## SI Figure 5
## non-phylogenetic models
{
  
  ## trade
  tr_sim_1 <- c()
  tr_sim_5 <- c()
  for(i in 1:nr) {
    tr_sim_1[i] <- rordlogit(1,tr_vareff_post$mu_bMR[i]*1,tr_vareff_post[i,2:6])
    tr_sim_5[i] <- rordlogit(1,tr_vareff_post$mu_bMR[i]*5,tr_vareff_post[i,2:6])
  }
  
  tr_sim <- gather(data.frame(tr_sim_1,tr_sim_5))
  barplot(table(tr_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,axes=TRUE,col=c("gray75","#440154FF"),border=NA,names.arg=c("no trade","2","3","4","5",">50% of food"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("A",3,line=0.25,adj=0)
  legend(12.5,800,legend=c("full polygyny","normative monogamy"),col=c("gray75","#440154FF"),pch=15,box.col=NA,cex=1.5)
  
  
  ## military mobilization
  mm_sim_1 <- c()
  mm_sim_5 <- c()
  for(i in 1:nr) {
    mm_sim_1[i] <- rordlogit(1,mm_vareff_post$mu_bMR[i]*1,mm_vareff_post[i,2:3])
    mm_sim_5[i] <- rordlogit(1,mm_vareff_post$mu_bMR[i]*5,mm_vareff_post[i,2:3])
  }
  
  mm_sim <- gather(data.frame(mm_sim_1,mm_sim_5))
  barplot(table(mm_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#440154FF"),border=NA,names.arg=c("no military organization","2","standing armies"),cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(6,1100,legend=c("full polygyny","normative monogamy"),col=c("gray75","#440154FF"),pch=15,box.col=NA,cex=1.5)
  
  
  ## male competition
  mc_sim_1 <- c()
  mc_sim_5 <- c()
  for(i in 1:nr) {
    mc_sim_1[i] <- rnorm(1,mc_vareff_post$mu_a[i] + mc_vareff_post$mu_bMR[i]*1,mc_vareff_post$sigma[i])
    mc_sim_5[i] <- rnorm(1,mc_vareff_post$mu_a[i] + mc_vareff_post$mu_bMR[i]*5,mc_vareff_post$sigma[i])
  }
  
  mc_sim <- cbind(mc_sim_1,mc_sim_5)
  hist(mc_sim_1,xlab="simulated outcome",ylab="frequency",main="",xlim=c(-2.5,2.5),col="gray75",border=NA,cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt="n")
  hist(mc_sim_5,col="#440154FF",border=NA,add=TRUE)
  axis(1,at=seq(-2,2,length.out=5),labels=c("-2","-1","0","1","2"),cex.axis=1.5,tick=FALSE)
  mtext("C",3,line=0.25,adj=0)
  legend(0.85,300,legend=c("full polygyny","normative monogamy"),col=c("gray75","#440154FF"),pch=15,box.col=NA,cex=1.5)
  
  
  ## patrilineal succession
  ps_sim_0 <- c()
  ps_sim_1 <- c()
  for(i in 1:nr) {
    ps_sim_0[i] <- rordlogit(1,ps_vareff_post$mu_bPS[i]*0,ps_vareff_post[i,2:5])
    ps_sim_1[i] <- rordlogit(1,ps_vareff_post$mu_bPS[i]*1,ps_vareff_post[i,2:5])
  }
  
  ps_sim <- gather(data.frame(ps_sim_0,ps_sim_1))
  barplot(table(ps_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#6D4C41"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("D",3,line=0.25,adj=0)
  legend(10,700,legend=c("non-patrilineal","patrilineal"),col=c("gray75","#6D4C41"),pch=15,box.col=NA,cex=1.5)
  
  
  ## wealth inequality
  wi_sim_1 <- c()
  wi_sim_5 <- c()
  for(i in 1:nr) {
    wi_sim_1[i] <- rordlogit(1,wi_vareff_post$mu_bWI[i]*1,wi_vareff_post[i,2:5])
    wi_sim_5[i] <- rordlogit(1,wi_vareff_post$mu_bWI[i]*5,wi_vareff_post[i,2:5])
  }
  
  wi_sim <- gather(data.frame(wi_sim_1,wi_sim_5))
  barplot(table(wi_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","yellow2"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("E",3,line=0.25,adj=0)
  legend(10,700,legend=c("low inequality","high inequality"),col=c("gray75","yellow2"),pch=15,box.col=NA,cex=1.5)
  
  
  ## rival wealth
  rw_sim_min_sd <- c()
  rw_sim_plus_sd <- c()
  for(i in 1:nr) {
    rw_sim_min_sd[i] <- rordlogit(1,rw_vareff_post$mu_bRW[i]*-2,rw_vareff_post[i,2:5])
    rw_sim_plus_sd[i] <- rordlogit(1,rw_vareff_post$mu_bRW[i]*2,rw_vareff_post[i,2:5])
  }
  
  rw_sim <- gather(data.frame(rw_sim_min_sd,rw_sim_plus_sd))
  barplot(table(rw_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","yellow2"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("F",3,line=0.25,adj=0)
  legend(10,800,legend=c("-2SD in rival wealth","+2SD in rival wealth"),col=c("gray75","yellow2"),pch=15,box.col=NA,cex=1.5)
  
  
  ## population density
  pd_sim_min_sd <- c()
  pd_sim_plus_sd <- c()
  for(i in 1:nr) {
    pd_sim_min_sd[i] <- rordlogit(1,pd_vareff_post$mu_bPD[i]*-2,pd_vareff_post[i,2:5])
    pd_sim_plus_sd[i] <- rordlogit(1,pd_vareff_post$mu_bPD[i]*2,pd_vareff_post[i,2:5])
  }
  
  pd_sim <- gather(data.frame(pd_sim_min_sd,pd_sim_plus_sd))
  barplot(table(pd_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#388E3C"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("G",3,line=0.25,adj=0)
  legend(9,700,legend=c("-2SD in population density","+2SD in population density"),col=c("gray75","#388E3C"),pch=15,box.col=NA,cex=1.5)
  
  
  ## distance to coast
  dc_sim_min_sd <- c()
  dc_sim_plus_sd <- c()
  for(i in 1:nr) {
    dc_sim_min_sd[i] <- rordlogit(1,dc_vareff_post$mu_bDC[i]*-2,dc_vareff_post[i,2:5])
    dc_sim_plus_sd[i] <- rordlogit(1,dc_vareff_post$mu_bDC[i]*2,dc_vareff_post[i,2:5])
  }
  
  dc_sim <- gather(data.frame(dc_sim_plus_sd,dc_sim_min_sd))
  barplot(table(dc_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("#388E3C","gray75"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("H",3,line=0.25,adj=0)
  legend(9,800,legend=c("-2SD in distance to coast","+2SD in distance to coast"),col=c("#388E3C","gray75"),pch=15,box.col=NA,cex=1.5)
  
  
  ## altitude
  al_sim_min_sd <- c()
  al_sim_plus_sd <- c()
  for(i in 1:nr) {
    al_sim_min_sd[i] <- rordlogit(1,ai_vareff_post$mu_bAL[i]*-2,ai_vareff_post[i,2:5])
    al_sim_plus_sd[i] <- rordlogit(1,ai_vareff_post$mu_bAL[i]*2,ai_vareff_post[i,2:5])
  }
  
  al_sim <- gather(data.frame(al_sim_min_sd,al_sim_plus_sd))
  barplot(table(al_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#388E3C"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("I",3,line=0.25,adj=0)
  legend(10,800,legend=c("-2SD in altitude","+2SD in altitude"),col=c("gray75","#388E3C"),pch=15,box.col=NA,cex=1.5)
  
  
  ## incline
  ic_sim_min_sd <- c()
  ic_sim_plus_sd <- c()
  for(i in 1:nr) {
    ic_sim_min_sd[i] <- rordlogit(1,ai_vareff_post$mu_bIC[i]*-2,ai_vareff_post[i,2:5])
    ic_sim_plus_sd[i] <- rordlogit(1,ai_vareff_post$mu_bIC[i]*2,ai_vareff_post[i,2:5])
  }
  
  ic_sim <- gather(data.frame(ic_sim_min_sd,ic_sim_plus_sd))
  barplot(table(ic_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#388E3C"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("J",3,line=0.25,adj=0)
  legend(10,800,legend=c("-2SD in incline","+2SD in incline"),col=c("gray75","#388E3C"),pch=15,box.col=NA,cex=1.5)
  
  
  ## vertical inheritance
  vi_sim_0 <- c()
  vi_sim_1 <- c()
  for(i in 1:nr) {
    vi_sim_0[i] <- rordlogit(1,vi_vareff_post$mu_bVI[i]*0,vi_vareff_post[i,2:5])
    vi_sim_1[i] <- rordlogit(1,vi_vareff_post$mu_bVI[i]*1,vi_vareff_post[i,2:5])
  }
  
  vi_sim <- gather(data.frame(vi_sim_0,vi_sim_1))
  barplot(table(vi_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#FD5901"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("K",3,line=0.25,adj=0)
  legend(10,700,legend=c("no vertical inheritance","vertical inheritance"),col=c("gray75","#FD5901"),pch=15,box.col=NA,cex=1.5)
  
  
  ## virginity norms
  vn_sim_0 <- c()
  vn_sim_1 <- c()
  for(i in 1:nr) {
    vn_sim_0[i] <- rordlogit(1,vn_vareff_post$mu_bVN[i]*0,vn_vareff_post[i,2:5])
    vn_sim_1[i] <- rordlogit(1,vn_vareff_post$mu_bVN[i]*1,vn_vareff_post[i,2:5])
  }
  
  vn_sim <- gather(data.frame(vn_sim_0,vn_sim_1))
  barplot(table(vn_sim),xlab="simulated outcome",ylab="frequency",beside=TRUE,col=c("gray75","#FD5901"),border=NA,names.arg=names_mr,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("L",3,line=0.25,adj=0)
  legend(10,700,legend=c("virginity not required","virginity required"),col=c("gray75","#FD5901"),pch=15,box.col=NA,cex=1.5)
  
}


################################################################################
################################################################################