#here we produce the boxplots to show the similarity
#of sites where neither, one, or both sites were
#settled by people during any particular interval

#the dat csv was group column was input my hand
#in excel
dat<-read.csv("dat.csv")
levels(dat$group)
dat$group <- factor(dat$group , levels=c("neither", "one", "both"))
boxplot(sims ~ group, data = dat, col="lightblue", xlab = "Proportion of pair settled by humans",
        ylab = "Pairwise Bray-Curtis Similarity slope coefficients")
