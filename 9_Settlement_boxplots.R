dat<-read.csv("dat.csv")
levels(dat$group)
dat$group <- factor(dat$group , levels=c("none", "one", "both"))
boxplot(sims ~ group, data = dat, col="darkorange1", xlab = "Proportion of pair settled by humans",
        ylab = "Pairwise Bray-Curtis Similarity slope coefficients")
