################################################################################
################################################################################


## re-do the figures but for models without Indo-Europeans
## read posteriors
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


## compute posterior probability of each effect
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


######################################################
## compute slope coefficients for each language family
## we'll do that by summing up the population mean of slope coefficient ("fixed effect") and residual for each language family ("random effect")
## trade
tr_var_eff <- tr_vareff_post[,88:168]
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
mm_var_eff <- mm_vareff_post[,75:145]
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
mc_var_eff <- mc_vareff_post[,83:163]
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
ps_var_eff <- ps_vareff_post[,87:167]
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
wi_var_eff <- wi_vareff_post[,87:167]
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
rw_var_eff <- rw_vareff_post[,87:167]
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
pd_var_eff <- pd_vareff_post[,87:167]
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
dc_var_eff <- dc_vareff_post[,87:167]
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
ai_var_eff <- ai_vareff_post[,87:167]
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
ic_var_eff <- ai_vareff_post[,168:248]
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
vi_var_eff <- vi_vareff_post[,61:115]
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
vn_var_eff <- vn_vareff_post[,61:115]
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


## plot posterior distribution of each effect
## models with phylogenetic and ethnographic covariance structures
mr_tr_dens <- density(tr_post$bMR)
mr_mm_dens <- density(mm_post$bMR)
mr_mc_dens <- density(mc_post$bMR)
ps_dens <- density(ps_post$bPS)
wi_dens <- density(wi_post$bWI)
rw_dens <- density(rw_post$bRW)
pc1_dens <- density(pc1_post$bPC1)
pc2_dens <- density(pc2_post$bPC2)
vi_dens <- density(vi_post$bVI)
vn_dens <- density(vn_post$bVN)

eff_dens <- cbind(mr_tr_dens,mr_mm_dens,mr_mc_dens,ps_dens,wi_dens,rw_dens,pc1_dens,pc2_dens,vi_dens,vn_dens)
names <- c("marriage rules (trade)","marriage rules (military mobilization)","marriage rules (male competition)","patrilineal succession","wealth inequality","rival wealth","land shortage-I","land shortage-II","vertical inheritance","virginity norms")
cols <- c("#346B6D","#346B6D","#346B6D","#2A445E","#F3AA20","#F3AA20","#841E62","#841E62","#58094F","#58094F")
PP <- c("46.35","24.05",">99","44.80","40.85","74.60","13.10","94.05","63.70","56.30")
xlim <- cbind(tr_post$bMR,mm_post$bMR,mc_post$bMR,ps_post$bPS,wi_post$bWI,rw_post$bRW,pc1_post$bPC1,pc2_post$bPC2,vi_post$bVI,vn_post$bVN)
ylim <- cbind(mr_tr_dens$y,mr_mm_dens$y,mr_mc_dens$y,ps_dens$y,wi_dens$y,rw_dens$y,pc1_dens$y,pc2_dens$y,vi_dens$y,vn_dens$y)

## plot the densities
par(mfrow=c(5,2),mar=c(5.1,5.1,1.5,1.1))
for(j in 1:ncol(eff_dens)) {
  plot(NULL,xlim=range(xlim[,j]),ylim=range(ylim[,j]),xlab=names[j],ylab="density", main="",lwd=3,cex.lab=1.75,cex.axis=1.75)
  lines(eff_dens[,j], col=cols[j], lwd=3)
  polygon(eff_dens[,j], col=col.alpha(cols[j], alpha=0.75), border=NA)
  mtext(PP[j],line=-2.5,adj=0.975,col=cols[j],cex=1.5)
  abline(v=0,lty=2,lwd=2,col="gray75")
}


## models with a varying effect of language family
mr_tr_dens <- density(tr_vareff_post$mu_bMR)
mr_mm_dens <- density(mm_vareff_post$mu_bMR)
mr_mc_dens <- density(mc_vareff_post$mu_bMR)
ps_dens <- density(ps_vareff_post$mu_bPS)
wi_dens <- density(wi_vareff_post$mu_bWI)
rw_dens <- density(rw_vareff_post$mu_bRW)
pc1_dens <- density(pc1_vareff_post$mu_bPC1)
pc2_dens <- density(pc2_vareff_post$mu_bPC2)
vi_dens <- density(vi_vareff_post$mu_bVI)
vn_dens <- density(vn_vareff_post$mu_bVN)

eff_dens <- cbind(mr_tr_dens,mr_mm_dens,mr_mc_dens,ps_dens,wi_dens,rw_dens,pc1_dens,pc2_dens,vi_dens,vn_dens)
PP <- c("48.80","34.95",">99","52.45","66.30","60.75","27.80","77.45","59.55","54.55")
xlim <- cbind(tr_vareff_post$mu_bMR,mm_vareff_post$mu_bMR,mc_vareff_post$mu_bMR,ps_vareff_post$mu_bPS,wi_vareff_post$mu_bWI,rw_vareff_post$mu_bRW,pc1_vareff_post$mu_bPC1,pc2_vareff_post$mu_bPC2,vi_vareff_post$mu_bVI,vn_vareff_post$mu_bVN)
ylim <- cbind(mr_tr_dens$y,mr_mm_dens$y,mr_mc_dens$y,ps_dens$y,wi_dens$y,rw_dens$y,pc1_dens$y,pc2_dens$y,vi_dens$y,vn_dens$y)

## plot the densities
par(mfrow=c(5,2),mar=c(5.1,5.1,1.5,1.1))
for(j in 1:ncol(eff_dens)) {
  plot(NULL,xlim=range(xlim[,j]),ylim=range(ylim[,j]),xlab=names[j],ylab="density", main="",lwd=3,cex.lab=1.75,cex.axis=1.75)
  lines(eff_dens[,j], col=cols[j], lwd=3)
  polygon(eff_dens[,j], col=col.alpha(cols[j], alpha=0.75), border=NA)
  mtext(PP[j],line=-2.5,adj=0.975,col=cols[j],cex=1.5)
  abline(v=0,lty=2,lwd=2,col="gray75")
}


## plot the total effect of each predictor by language family
## compute the sum of monogamy instances in each language family 
MR_bin <- ifelse(MR=="5",1,0)
mono_family_sums <- tapply(MR_bin, as.factor(family), sum)
mono_family_sums[mono_family_sums!=0]

{
  
  par(mfrow=c(5,2),mar=c(5.1,5.1,1.5,1.1))
  
  ## trade
  mr_tr_fam <- tr_vareff_post[,88:168]
  mr_tr_fam_mono <- tr_vareff_post[,c("bMR.2","bMR.10","bMR.11","bMR.12","bMR.13","bMR.27","bMR.32","bMR.33","bMR.37","bMR.42","bMR.45","bMR.51","bMR.53","bMR.58","bMR.62","bMR.71","bMR.74","bMR.75","bMR.81")]
  mr_tr_fam_dens <- list()
  
  tr_coefs_sum_mono <- matrix(NA,nrow=2000,ncol=ncol(mr_tr_fam_mono))
  for(j in 1:ncol(tr_coefs_sum_mono)) {
    for(i in 1:nrow(tr_coefs_sum_mono)) {
      tr_coefs_sum_mono[i,j] <- tr_vareff_post$mu_bMR[i] + mr_tr_fam_mono[i,j]
    }
  }
  mr_tr_fam_mono <- apply(tr_coefs_sum_mono,2,mean)
  mr_tr_fam_mu <- mean(mr_tr_fam_mono)
  
  tr_coefs_sum <- matrix(NA,nrow=nrow(tr_vareff_post),ncol=ncol(mr_tr_fam))
  for(j in 1:ncol(tr_coefs_sum)) {
    for(i in 1:nrow(tr_coefs_sum)) {
      tr_coefs_sum[i,j] <- tr_vareff_post$mu_bMR[i] + mr_tr_fam[i,j]
    }
  }
  
  plot(NULL,xlim=c(-2,2),ylim=c(0,0.4),xlab="marriage rules (trade)",ylab="density", main="",lwd=3,cex.lab=1.75,cex.axis=1.75)
  for(j in 1:ncol(tr_coefs_sum)) {
    mr_tr_fam_dens[[j]] <- (density(tr_coefs_sum[,j]))
    lines(mr_tr_fam_dens[[j]], col=col.alpha("#346B6D",0.3),lwd=2)
    abline(v=mean(tr_vareff_post$mu_bMR),lty=2,lwd=2,col="#346B6D")
  }
  abline(v=mr_tr_fam_mu,lty=2,lwd=2,col="gray85")
  legend(-2.1,0.425,lty=2,lwd=2,legend=c("all families","w/monogamy (no IE)"),col=c("#346B6D","gray85"),cex=1.25,box.col=NA)
  
  
  ## military mobilization
  MR_bin_MM <- ifelse(mono_data_MM$MR=="5",1,0)
  mono_family_sums_MM <- tapply(MR_bin_MM, as.factor(mono_data_MM$family), sum)
  mono_family_sums_MM[mono_family_sums_MM!=0]
  
  mr_mm_fam <- mm_vareff_post[,75:145]
  mr_mm_fam_mono <- mm_vareff_post[,c("bMR.2","bMR.10","bMR.11","bMR.12","bMR.22","bMR.26","bMR.27","bMR.31","bMR.38","bMR.45","bMR.50","bMR.54","bMR.62","bMR.66","bMR.71")]
  mr_mm_fam_dens <- list()
  
  mm_coefs_sum_mono <- matrix(NA,nrow=2000,ncol=ncol(mr_mm_fam_mono))
  for(j in 1:ncol(mm_coefs_sum_mono)) {
    for(i in 1:nrow(mm_coefs_sum_mono)) {
      mm_coefs_sum_mono[i,j] <- mm_vareff_post$mu_bMR[i] + mr_mm_fam_mono[i,j]
    }
  }
  mr_mm_fam_mono <- apply(mm_coefs_sum_mono,2,mean)
  mr_mm_fam_mu <- mean(mr_mm_fam_mono)
  
  mm_coefs_sum <- matrix(NA,nrow=nrow(mm_vareff_post),ncol=ncol(mr_mm_fam))
  for(j in 1:ncol(mm_coefs_sum)) {
    for(i in 1:nrow(mm_coefs_sum)) {
      mm_coefs_sum[i,j] <- mm_vareff_post$mu_bMR[i] + mr_mm_fam[i,j]
    }
  }
  
  plot(NULL,xlim=c(-2,2),ylim=c(0,0.4),xlab="marriage rules (military mobilization)",ylab="density", main="",lwd=3,cex.lab=1.75,cex.axis=1.75)
  for(j in 1:ncol(mm_coefs_sum)) {
    mr_mm_fam_dens[[j]] <- (density(mm_coefs_sum[,j]))
    lines(mr_mm_fam_dens[[j]], col=col.alpha("#346B6D",0.3),lwd=2)
    abline(v=mean(mm_vareff_post$mu_bMR),lty=2,lwd=2,col="#346B6D")
  }
  abline(v=mr_mm_fam_mu,lty=2,lwd=2,col="gray85")
  legend(-2.1,0.425,lty=2,lwd=2,legend=c("all families","w/monogamy (no IE)"),col=c("#346B6D","gray85"),cex=1.25,box.col=NA)
  
  
  ## male competition
  mr_mc_fam <- mc_vareff_post[,83:163]
  mr_mc_fam_mono <- mc_vareff_post[,c("bMR.2","bMR.10","bMR.11","bMR.12","bMR.13","bMR.27","bMR.32","bMR.33","bMR.37","bMR.42","bMR.45","bMR.51","bMR.53","bMR.58","bMR.62","bMR.71","bMR.74","bMR.75","bMR.81")]
  mr_mc_fam_dens <- list()
  
  mc_coefs_sum_mono <- matrix(NA,nrow=2000,ncol=ncol(mr_mc_fam_mono))
  for(j in 1:ncol(mc_coefs_sum_mono)) {
    for(i in 1:nrow(mc_coefs_sum_mono)) {
      mc_coefs_sum_mono[i,j] <- mc_vareff_post$mu_bMR[i] + mr_mc_fam_mono[i,j]
    }
  }
  mr_mc_fam_mono <- apply(mc_coefs_sum_mono,2,mean)
  mr_mc_fam_mu <- mean(mr_mc_fam_mono)
  
  mc_coefs_sum <- matrix(NA,nrow=nrow(mc_vareff_post),ncol=ncol(mr_mc_fam))
  for(j in 1:ncol(mc_coefs_sum)) {
    for(i in 1:nrow(mc_coefs_sum)) {
      mc_coefs_sum[i,j] <- mc_vareff_post$mu_bMR[i] + mr_mc_fam[i,j]
    }
  }
  
  plot(NULL,xlim=c(-0.3,0.05),ylim=c(0,10),xlab="marriage rules (male competition)",ylab="density", main="",lwd=3,cex.lab=1.75,cex.axis=1.75)
  for(j in 1:ncol(mc_coefs_sum)) {
    mr_mc_fam_dens[[j]] <- (density(mc_coefs_sum[,j]))
    lines(mr_mc_fam_dens[[j]], col=col.alpha("#346B6D",0.3),lwd=2)
    abline(v=mean(mc_vareff_post$mu_bMR),lty=2,lwd=2,col="#346B6D")
  }
  abline(v=mr_mc_fam_mu,lty=2,lwd=2,col="gray85")
  legend(-0.31,10.5,lty=2,lwd=2,legend=c("all families","w/monogamy (no IE)"),col=c("#346B6D","gray85"),cex=1.25,box.col=NA)
  
  
  ## patrilineal succession
  ps_fam <- ps_vareff_post[,87:167]
  ps_fam_mono <- ps_vareff_post[,c("bPS.2","bPS.10","bPS.11","bPS.12","bPS.13","bPS.27","bPS.32","bPS.33","bPS.37","bPS.42","bPS.45","bPS.51","bPS.53","bPS.58","bPS.62","bPS.71","bPS.74","bPS.75","bPS.81")]
  ps_fam_dens <- list()
  
  ps_coefs_sum_mono <- matrix(NA,nrow=2000,ncol=ncol(ps_fam_mono))
  for(j in 1:ncol(ps_coefs_sum_mono)) {
    for(i in 1:nrow(ps_coefs_sum_mono)) {
      ps_coefs_sum_mono[i,j] <- ps_vareff_post$mu_bPS[i] + ps_fam_mono[i,j]
    }
  }
  ps_fam_mono <- apply(ps_coefs_sum_mono,2,mean)
  ps_fam_mu <- mean(ps_fam_mono)
  
  ps_coefs_sum <- matrix(NA,nrow=nrow(ps_vareff_post),ncol=ncol(ps_fam))
  for(j in 1:ncol(ps_coefs_sum)) {
    for(i in 1:nrow(ps_coefs_sum)) {
      ps_coefs_sum[i,j] <- ps_vareff_post$mu_bPS[i] + ps_fam[i,j]
    }
  }
  
  plot(NULL,xlim=c(-2,2),ylim=c(0,0.4),xlab="patrilineal succession",ylab="density", main="",lwd=3,cex.lab=1.75,cex.axis=1.75)
  for(j in 1:ncol(ps_coefs_sum)) {
    ps_fam_dens[[j]] <- (density(ps_coefs_sum[,j]))
    lines(ps_fam_dens[[j]], col=col.alpha("#2A445E",0.3),lwd=2)
    abline(v=mean(ps_vareff_post$mu_bPS),lty=2,lwd=2,col="#2A445E")
  }
  abline(v=ps_fam_mu,lty=2,lwd=2,col="gray85")
  legend(-2.1,0.425,lty=2,lwd=2,legend=c("all families","w/monogamy (no IE)"),col=c("#2A445E","gray85"),cex=1.25,box.col=NA)
  
  
  ## wealth inequality
  wi_fam <- wi_vareff_post[,87:167]
  wi_fam_mono <- wi_vareff_post[,c("bWI.2","bWI.10","bWI.11","bWI.12","bWI.13","bWI.27","bWI.32","bWI.33","bWI.37","bWI.42","bWI.45","bWI.51","bWI.53","bWI.58","bWI.62","bWI.71","bWI.74","bWI.75","bWI.81")]
  wi_fam_dens <- list()
  
  wi_coefs_sum_mono <- matrix(NA,nrow=2000,ncol=ncol(wi_fam_mono))
  for(j in 1:ncol(wi_coefs_sum_mono)) {
    for(i in 1:nrow(wi_coefs_sum_mono)) {
      wi_coefs_sum_mono[i,j] <- wi_vareff_post$mu_bWI[i] + wi_fam_mono[i,j]
    }
  }
  wi_fam_mono <- apply(wi_coefs_sum_mono,2,mean)
  wi_fam_mu <- mean(wi_fam_mono)
  
  wi_coefs_sum <- matrix(NA,nrow=nrow(wi_vareff_post),ncol=ncol(wi_fam))
  for(j in 1:ncol(wi_coefs_sum)) {
    for(i in 1:nrow(wi_coefs_sum)) {
      wi_coefs_sum[i,j] <- wi_vareff_post$mu_bWI[i] + wi_fam[i,j]
    }
  }
  
  plot(NULL,xlim=c(-2,2),ylim=c(0,0.4),xlab="wealth inequality",ylab="density", main="",lwd=3,cex.lab=1.75,cex.axis=1.75)
  for(j in 1:ncol(wi_coefs_sum)) {
    wi_fam_dens[[j]] <- (density(wi_coefs_sum[,j]))
    lines(wi_fam_dens[[j]], col=col.alpha("#F3AA20",0.3),lwd=2)
    abline(v=mean(wi_vareff_post$mu_bWI),lty=2,lwd=2,col="#F3AA20")
  }
  abline(v=wi_fam_mu,lty=2,lwd=2,col="gray85")
  legend(-2.1,0.425,lty=2,lwd=2,legend=c("all families","w/monogamy (no IE)"),col=c("#F3AA20","gray85"),cex=1.25,box.col=NA)
  
  
  ## rival wealth
  rw_fam <- rw_vareff_post[,87:167]
  rw_fam_mono <- rw_vareff_post[,c("bRW.2","bRW.10","bRW.11","bRW.12","bRW.13","bRW.27","bRW.32","bRW.33","bRW.37","bRW.42","bRW.45","bRW.51","bRW.53","bRW.58","bRW.62","bRW.71","bRW.74","bRW.75","bRW.81")]
  rw_fam_dens <- list()
  
  rw_coefs_sum_mono <- matrix(NA,nrow=2000,ncol=ncol(rw_fam_mono))
  for(j in 1:ncol(rw_coefs_sum_mono)) {
    for(i in 1:nrow(rw_coefs_sum_mono)) {
      rw_coefs_sum_mono[i,j] <- rw_vareff_post$mu_bRW[i] + rw_fam_mono[i,j]
    }
  }
  rw_fam_mono <- apply(rw_coefs_sum_mono,2,mean)
  rw_fam_mu <- mean(rw_fam_mono)
  
  rw_coefs_sum <- matrix(NA,nrow=nrow(rw_vareff_post),ncol=ncol(rw_fam))
  for(j in 1:ncol(rw_coefs_sum)) {
    for(i in 1:nrow(rw_coefs_sum)) {
      rw_coefs_sum[i,j] <- rw_vareff_post$mu_bRW[i] + wi_fam[i,j]
    }
  }
  
  plot(NULL,xlim=c(-2,2),ylim=c(0,0.4),xlab="rival wealth",ylab="density", main="",lwd=3,cex.lab=1.75,cex.axis=1.75)
  for(j in 1:ncol(rw_coefs_sum)) {
    rw_fam_dens[[j]] <- (density(rw_coefs_sum[,j]))
    lines(rw_fam_dens[[j]], col=col.alpha("#F3AA20",0.3),lwd=2)
    abline(v=mean(rw_vareff_post$mu_bRW),lty=2,lwd=2,col="#F3AA20")
  }
  abline(v=rw_fam_mu,lty=2,lwd=2,col="gray85")
  legend(-2.1,0.425,lty=2,lwd=2,legend=c("all families","w/monogamy (no IE)"),col=c("#F3AA20","gray85"),cex=1.25,box.col=NA)
  
  
  ## land shortage-I
  pc1_fam <- pc1_vareff_post[,87:167]
  pc1_fam_mono <- pc1_vareff_post[,c("bPC1.2","bPC1.10","bPC1.11","bPC1.12","bPC1.13","bPC1.27","bPC1.32","bPC1.33","bPC1.37","bPC1.42","bPC1.45","bPC1.51","bPC1.53","bPC1.58","bPC1.62","bPC1.71","bPC1.74","bPC1.75","bPC1.81")]
  pc1_fam_dens <- list()
  
  pc1_coefs_sum_mono <- matrix(NA,nrow=2000,ncol=ncol(pc1_fam_mono))
  for(j in 1:ncol(pc1_coefs_sum_mono)) {
    for(i in 1:nrow(pc1_coefs_sum_mono)) {
      pc1_coefs_sum_mono[i,j] <- pc1_vareff_post$mu_bPC1[i] + pc1_fam_mono[i,j]
    }
  }
  pc1_fam_mono <- apply(pc1_coefs_sum_mono,2,mean)
  pc1_fam_mu <- mean(pc1_fam_mono)
  
  pc1_coefs_sum <- matrix(NA,nrow=nrow(pc1_vareff_post),ncol=ncol(pc1_fam))
  for(j in 1:ncol(pc1_coefs_sum)) {
    for(i in 1:nrow(pc1_coefs_sum)) {
      pc1_coefs_sum[i,j] <- pc1_vareff_post$mu_bPC1[i] + pc1_fam[i,j]
    }
  }
  
  plot(NULL,xlim=c(-2,2),ylim=c(0,0.4),xlab="land shortage-I",ylab="density", main="",lwd=3,cex.lab=1.75,cex.axis=1.75)
  for(j in 1:ncol(pc1_coefs_sum)) {
    pc1_fam_dens[[j]] <- (density(pc1_coefs_sum[,j]))
    lines(pc1_fam_dens[[j]], col=col.alpha("#841E62",0.3),lwd=2)
    abline(v=mean(pc1_vareff_post$mu_bPC1),lty=2,lwd=2,col="#841E62")
  }
  abline(v=pc1_fam_mu,lty=2,lwd=2,col="gray85")
  legend(-2.1,0.425,lty=2,lwd=2,legend=c("all families","w/monogamy (no IE)"),col=c("#841E62","gray85"),cex=1.25,box.col=NA)
  
  
  ## land shortage-II
  pc2_fam <- pc2_vareff_post[,87:167]
  pc2_fam_mono <- pc2_vareff_post[,c("bPC2.2","bPC2.10","bPC2.11","bPC2.12","bPC2.13","bPC2.27","bPC2.32","bPC2.33","bPC2.37","bPC2.42","bPC2.45","bPC2.51","bPC2.53","bPC2.58","bPC2.62","bPC2.71","bPC2.74","bPC2.75","bPC2.81")]
  pc2_fam_dens <- list()
  
  pc2_coefs_sum_mono <- matrix(NA,nrow=2000,ncol=ncol(pc2_fam_mono))
  for(j in 1:ncol(pc2_coefs_sum_mono)) {
    for(i in 1:nrow(pc2_coefs_sum_mono)) {
      pc2_coefs_sum_mono[i,j] <- pc2_vareff_post$mu_bPC2[i] + pc2_fam_mono[i,j]
    }
  }
  pc2_fam_mono <- apply(pc2_coefs_sum_mono,2,mean)
  pc2_fam_mu <- mean(pc2_fam_mono)
  
  pc2_coefs_sum <- matrix(NA,nrow=nrow(pc2_vareff_post),ncol=ncol(pc2_fam))
  for(j in 1:ncol(pc2_coefs_sum)) {
    for(i in 1:nrow(pc2_coefs_sum)) {
      pc2_coefs_sum[i,j] <- pc2_vareff_post$mu_bPC2[i] + pc2_fam[i,j]
    }
  }
  
  plot(NULL,xlim=c(-2,2),ylim=c(0,0.4),xlab="land shortage-II",ylab="density", main="",lwd=3,cex.lab=1.75,cex.axis=1.75)
  for(j in 1:ncol(pc2_coefs_sum)) {
    pc2_fam_dens[[j]] <- (density(pc2_coefs_sum[,j]))
    lines(pc2_fam_dens[[j]], col=col.alpha("#841E62",0.3),lwd=2)
    abline(v=mean(pc2_vareff_post$mu_bPC2),lty=2,lwd=2,col="#841E62")
  }
  abline(v=pc2_fam_mu,lty=2,lwd=2,col="gray85")
  legend(-2.1,0.425,lty=2,lwd=2,legend=c("all families","w/monogamy (no IE)"),col=c("#841E62","gray85"),cex=1.25,box.col=NA)
  
  
  ## vertical inheritance
  MR_bin_MI <- ifelse(mono_data_MI$MR=="5",1,0)
  mono_family_sums_MI <- tapply(MR_bin_MI, as.factor(mono_data_MI$family), sum)
  mono_family_sums_MI[mono_family_sums_MI!=0]
  
  vi_fam <- vi_vareff_post[,61:115]
  vi_fam_mono <- vi_vareff_post[,c("bVI.2","bVI.8","bVI.9","bVI.10","bVI.20","bVI.24","bVI.28","bVI.31","bVI.35","bVI.38","bVI.42","bVI.52","bVI.53")]
  vi_fam_dens <- list()
  
  vi_coefs_sum_mono <- matrix(NA,nrow=2000,ncol=ncol(vi_fam_mono))
  for(j in 1:ncol(vi_coefs_sum_mono)) {
    for(i in 1:nrow(vi_coefs_sum_mono)) {
      vi_coefs_sum_mono[i,j] <- vi_vareff_post$mu_bVI[i] + vi_fam_mono[i,j]
    }
  }
  vi_fam_mono <- apply(vi_coefs_sum_mono,2,mean)
  vi_fam_mu <- mean(vi_fam_mono)
  
  vi_coefs_sum <- matrix(NA,nrow=nrow(vi_vareff_post),ncol=ncol(vi_fam))
  for(j in 1:ncol(vi_coefs_sum)) {
    for(i in 1:nrow(vi_coefs_sum)) {
      vi_coefs_sum[i,j] <- vi_vareff_post$mu_bVI[i] + vi_fam[i,j]
    }
  }
  
  plot(NULL,xlim=c(-2,2),ylim=c(0,0.4),xlab="vertical inheritance",ylab="density", main="",lwd=3,cex.lab=1.75,cex.axis=1.75)
  for(j in 1:ncol(vi_coefs_sum)) {
    vi_fam_dens[[j]] <- (density(vi_coefs_sum[,j]))
    lines(vi_fam_dens[[j]], col=col.alpha("#58094F",0.3),lwd=2)
    abline(v=mean(vi_vareff_post$mu_bVI),lty=2,lwd=2,col="#58094F")
  }
  abline(v=vi_fam_mu,lty=2,lwd=2,col="gray85")
  legend(-2.1,0.425,lty=2,lwd=2,legend=c("all families","w/monogamy (no IE)"),col=c("#58094F","gray85"),cex=1.25,box.col=NA)
  
  
  ## virginity norms
  vn_fam <- vn_vareff_post[,61:115]
  vn_fam_mono <- vn_vareff_post[,c("bVN.2","bVN.8","bVN.9","bVN.10","bVN.20","bVN.24","bVN.28","bVN.31","bVN.35","bVN.38","bVN.42","bVN.52","bVN.53")]
  vn_fam_dens <- list()
  
  vn_coefs_sum_mono <- matrix(NA,nrow=2000,ncol=ncol(vn_fam_mono))
  for(j in 1:ncol(vn_coefs_sum_mono)) {
    for(i in 1:nrow(vn_coefs_sum_mono)) {
      vn_coefs_sum_mono[i,j] <- vn_vareff_post$mu_bVN[i] + vn_fam_mono[i,j]
    }
  }
  vn_fam_mono <- apply(vn_coefs_sum_mono,2,mean)
  vn_fam_mu <- mean(vn_fam_mono)
  
  vn_coefs_sum <- matrix(NA,nrow=nrow(vn_vareff_post),ncol=ncol(vn_fam))
  for(j in 1:ncol(vn_coefs_sum)) {
    for(i in 1:nrow(vn_coefs_sum)) {
      vn_coefs_sum[i,j] <- vn_vareff_post$mu_bVN[i] + vn_fam[i,j]
    }
  }
  
  plot(NULL,xlim=c(-2,2),ylim=c(0,0.4),xlab="virginity norms",ylab="density", main="",lwd=3,cex.lab=1.75,cex.axis=1.75)
  for(j in 1:ncol(vn_coefs_sum)) {
    vn_fam_dens[[j]] <- (density(vn_coefs_sum[,j]))
    lines(vn_fam_dens[[j]], col=col.alpha("#58094F",0.3),lwd=2)
    abline(v=mean(vn_vareff_post$mu_bVN),lty=2,lwd=2,col="#58094F")
  }
  abline(v=vn_fam_mu,lty=2,lwd=2,col="gray85")
  legend(-2.1,0.425,lty=2,lwd=2,legend=c("all families","w/monogamy (no IE)"),col=c("#58094F","gray85"),cex=1.25,box.col=NA)
  
}

