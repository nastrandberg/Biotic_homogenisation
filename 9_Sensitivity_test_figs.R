#Figures for the SI####

library(npreg)

#Similarity where only taxa within the same rank are compared####
dat1<-read.csv("Outputs/Stand1_by_rank_dat.csv", check.names = F)
dat2<-read.csv("Outputs/Stand2_by_rank_dat.csv", check.names = F)

mod1 <- ss(dat1$times, dat1$sims, nknots = 5)
mod2 <- ss(dat2$times, dat2$sims, nknots = 5)
names(mod1)

plot(mod1,level=0.95, lwd=3,xlim=c(4650,150),ylim=c(0,1),ylab="Pairwise Bray-Curtis Similarity", xlab="Cal. years BP", col="royalblue4")
points(dat1$sims ~ dat1$times,pch=21,col=("royalblue4"))
points(dat2$sims ~ dat2$times,pch=24,col=("royalblue1"))
lines(mod2, lwd=3,xlim=c(4650,150),ylim=c(0,1), col="royalblue1")
lines(mod1, lwd=3,xlim=c(4650,150),ylim=c(0,1), col="royalblue4")

#Similarity where assemblages with counts <300 are subsetted out####
dat1<-read.csv("Outputs/Stand1_subset_dat.csv", check.names = F)
dat2<-read.csv("Outputs/Stand2_subset_dat.csv", check.names = F)

mod1 <- ss(dat1$times, dat1$sims, nknots = 5)
mod2 <- ss(dat2$times, dat2$sims, nknots = 5)

plot(mod2,level=0.95, lwd=3,xlim=c(4650,150),ylim=c(0,1),ylab="Pairwise Bray-Curtis Similarity", xlab="Cal. years BP", col="royalblue4")
points(dat1$sims ~ dat1$times,pch=21,col=("royalblue4"))
points(dat2$sims ~ dat2$times,pch=24,col=("royalblue1"))
lines(mod2, lwd=3,xlim=c(4650,150),ylim=c(0,1), col="royalblue1")
lines(mod1, lwd=3,xlim=c(4650,150),ylim=c(0,1), col="royalblue4")

#Similarity excluding Cyperaceae ####
dat1<-read.csv("Outputs/Stand1_without_Cyperaceae_dat.csv", check.names = F)
dat2<-read.csv("Outputs/Stand2_without_Cyperaceae_dat.csv", check.names = F)

mod1 <- ss(dat1$times, dat1$sims, nknots = 5)
mod2 <- ss(dat2$times, dat2$sims, nknots = 5)

par(mfrow=c(1,2))
plot(mod2,level=0.95, lwd=3,xlim=c(4650,150),ylim=c(0,1),ylab="Pairwise Bray-Curtis Similarity", xlab="Cal. years BP", col="royalblue4")
points(dat1$sims ~ dat1$times,pch=21,col=("royalblue4"))
points(dat2$sims ~ dat2$times,pch=24,col=("royalblue1"))
lines(mod2, lwd=3,xlim=c(4650,150),ylim=c(0,1), col="royalblue1")
lines(mod1, lwd=3,xlim=c(4650,150),ylim=c(0,1), col="royalblue4")

plot(mod2,level=0.95, lwd=3,xlim=c(4650,150),ylim=c(0,0.2),ylab="Pairwise Bray-Curtis Similarity", xlab="Cal. years BP", col="royalblue4")
points(dat1$sims ~ dat1$times,pch=21,col=("royalblue4"))
points(dat2$sims ~ dat2$times,pch=24,col=("royalblue1"))
lines(mod2, lwd=3,xlim=c(4650,150),ylim=c(0,1), col="royalblue1")
lines(mod1, lwd=3,xlim=c(4650,150),ylim=c(0,1), col="royalblue4")
abline(h=0.12, col= "red", lwd=1, lty=5)

#Similarity excluding Poaceae ####
dat1<-read.csv("Outputs/Stand1_without_Poaceae_dat.csv", check.names = F)
dat2<-read.csv("Outputs/Stand2_without_Poaceae_dat.csv", check.names = F)

mod1 <- ss(dat1$times, dat1$sims, nknots = 5)
mod2 <- ss(dat2$times, dat2$sims, nknots = 5)

plot(mod2,level=0.95, lwd=3,xlim=c(4650,150),ylim=c(0,1),ylab="Pairwise Bray-Curtis Similarity", xlab="Cal. years BP", col="royalblue4")
points(dat1$sims ~ dat1$times,pch=21,col=("royalblue4"))
points(dat2$sims ~ dat2$times,pch=24,col=("royalblue1"))
lines(mod2, lwd=3,xlim=c(4650,150),ylim=c(0,1), col="royalblue1")
lines(mod1, lwd=3,xlim=c(4650,150),ylim=c(0,1), col="royalblue4")