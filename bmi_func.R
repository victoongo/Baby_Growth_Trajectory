library(sitar)
library(ggplot2)
library(plyr)
library(nlme)

setwd("~/proj_data/bmi")
bmi<-read.csv("bmi.csv", as.is=TRUE)
dim(bmi)
names(bmi)
#keep <- complete.cases(bmi)
#wt <- wt[keep,]
bmi <- unique(bmi)
# bmi$id <- paste0(bmi$nestid, '_', bmi$mom_nestid)
bmi$id <- bmi$mom_nestid * 100000 + bmi$nestid

ggplot(aes(x=agemos), data=bmi) + geom_histogram() 
qplot(agemos, data=bmi, geom="histogram", fill=factor(sex), binwidth=4, main="titile") 
bmi60 <- bmi[bmi$agemos<=60,]
qplot(agemos, height, data=bmi60, geom=c("point","smooth"), method="gam", formula=y~ns(x,3), 
      size=weight, alpha=I(1/3), colour=factor(sex)) 
age <- bmi[bmi$agemos<0.5,]
ggplot(aes(x=agemos), data=age) + geom_histogram() 

summary(bmi)
hist(bmi$weight)
plot(bmi$agemos, bmi$weight)
ggplot(aes(x=agemos, y=weight), data=bmi) + geom_point()
table(bmi$X_bivwt)

hist(bmi$height)
plot(bmi$agemos, bmi$height)
table(bmi$X_bivht)

hist(bmi$bmi)
plot(bmi$agemos, bmi$bmi)
table(bmi$X_bivbmi)

hist(bmi$whz)
plot(bmi$agemos, bmi$whz)
table(bmi$X_bivwh)


# wt$age_mos <- wt$age_mos / 30
# if bmi$X_bivwt==-1 | bmi$X_bivwt==1 
# bmi <- bmi[bmi$X_bivwt==0,]
bmi <- bmi[bmi$X_bivht==0,]
# bmi <- bmi[bmi$X_bivbmi==0,]
# bmi <- bmi[bmi$X_bivwh==0,]

# mplot(agemos, weight, id, bmi)
# mplot(agemos, waz, id, bmi)
mplot(agemos, height, id, bmi)
mplot(agemos, haz, id, bmi)
# mplot(agemos, bmi, id, bmi)
# mplot(agemos, bmiz, id, bmi)
# mplot(agemos, whz, id, bmi)

plotclean(agemos, weight, id, bmi)

waz <- bmi[!is.na(bmi$height),]
waz <- subset(waz, select=c(height, agemos, id, ref_date, ht_source, sex))
waz <- unique(waz)

minmax <- ddply(waz, .(id), summarize, agemos_min=min(agemos), agemos_max=max(agemos))
minmax_keep <- minmax[minmax$agemos_min<=0.5 & minmax$agemos_max>=17,]
waz <- waz[waz$id %in% minmax_keep$id,]

# agemos0 <- 

#waz <- waz[1:5000,]
# waz <- waz[waz$agemos<=48 & waz$agemos>3,]
waz <- waz[waz$agemos<=25,]
mplot(agemos, height, id, waz)

# waz_freq <- table(waz$id)
# waz_keep <- data.frame(waz_freq[waz_freq>5])
# waz <- waz[waz$id %in% rownames(waz_keep),]

# ggplot(aes(x=height, fill=as.factor(sex)), data=bmi) + geom_histogram() 
# ggplot(aes(x=agemos, y=height, group=sex, color=as.factor(sex)), data=bmi) + geom_point()

outliers <- velout(agemos, height, id, waz, limit=0.5, linearise=TRUE)
nwaz <- zapvelout(outliers, icode=c(4,6,7))
# outlist <- outliers[abs(outliers$height)>=4.5,]
# table(waz[row.names(outlist),]$ht_source)
# table(bmi$ht_source)

# keep <- outliers[abs(outliers$height)<4.5,]
# nwaz <- waz[row.names(keep),]

# ag <- aggregate(height ~ id, outliers, function(x) c(mean=mean(x), sd=sd(x)))
ag <- ddply(outliers, .(id), summarize, height_mean=mean(height), height_sd=sd(height))
summary(ag)
# approach 1: drop cases with height_sd>
box_stats <- boxplot(ag$height_sd, na.rm=TRUE, col="blue", plot=FALSE)
# keep_list <- ag[ag$height_sd<box_stats$stats[5,1],]
keep_list <- ag[ag$height_sd<2.5,]
keep_list <- keep_list[!is.na(keep_list$height_sd),]
# [,keep_list$id]
nwaz <- nwaz[nwaz$id %in% keep_list$id,]
nwaz <- nwaz[!is.na(nwaz$height),]

waz_freq <- table(nwaz$id)
waz_keep <- data.frame(waz_freq[waz_freq>7])
nwaz <- nwaz[nwaz$id %in% rownames(waz_keep),]

# outliers <- velout(agemos, height, id, waz, limit=0.5, linearise=TRUE)
# nwaz <- zapvelout(outliers, icode=c(4,6,7))
# mplot(agemos, height, id, outliers)
# # codeplot(outliers, icode=c(4, 6))
# nwaz <- nwaz[!is.na(nwaz$height),]
# 
# waz_freq <- table(nwaz$id)
# waz_keep <- data.frame(waz_freq[waz_freq>10])
# nwaz <- nwaz[nwaz$id %in% rownames(waz_keep),]

mplot(agemos, height, id, nwaz)
# ggplot(aes(x=height, fill=as.factor(sex)), data=nwaz) + geom_histogram() 
# ggplot(aes(x=agemos, y=height, group=sex, color=as.factor(sex)), data=nwaz) + geom_point()

m1 <- sitar(x=agemos, y=height, id=id, data=nwaz, df=3)
## draw fitted distance and velocity curves
## with velocity curve in blue
## adding age at peak velocity
plot(m1, y2par=list(col='blue'), apv=TRUE)

## draw individually coloured growth curves adjusted for random effects
## using same x-axis limits as for previous plot
plot(m1, opt='a', col=1+as.integer(id) %% 5, xlim=xaxsd())

## add mean curve in red
lines(m1, opt='d', col='red', lwd=2)


outliers <- velout(agemos, height, id, waz, limit=2)
#lm(weight ~ agemos, data=bmi, na.omit=TRUE)
codeplot(outliers, icode=c(4, 6))
nwaz <- zapvelout(outliers, icode=6)

length(unique(epi$nestid)) == nrow(epi)
rownames(epi) <-epi$nestid




# nlme model
lm <- lme(height ~ agemos, data=nwaz, random= ~ agemos | id, method="ML")
qm <- lme(height ~ agemos + I(agemos^2), data=nwaz, random= ~ agemos | id, method="ML")
cm <- lme(height ~ agemos + I(agemos^2) + I(agemos^3), data=nwaz, random= ~ agemos | id, method="ML")
plot(lm, y2par=list(col='blue', apv=TRUE))
plot(qm, y2par=list(col='blue', apv=TRUE))
plot(cm, y2par=list(col='blue', apv=TRUE))
plot(qm, fitted(.) ~ agemos)
pwaz <- nwaz
pwaz$pred <- predict(cm, newdata=pwaz,level=0)
ggplot(pwaz,aes(x=agemos,y=height,color=as.factor(id)),alpha=I(1/3),legend=FALSE) + 
  geom_path() +
  geom_line(data=pwaz,aes(x=agemos,y=pred),color="black") +
  scale_y_continuous("height") +
  theme(legend.position="none")
plot(nwaz$agemos, nwaz$height, type='l')
ggplot(pwaz,aes(x=agemos,y=height)) + geom_line() 
qplot(agemos, height, data=pwaz, geom="path", colour=as.factor(id), alpha=I(1/2)) + scale_size_area()
anova(lm, qm)
anova(qm, cm)

qm_sex <- lme(height ~ agemos + I(agemos^2) + sex, data=nwaz, random= ~ agemos + I(agemos^2) | id, method="ML")
cm_sex <- lme(height ~ agemos + I(agemos^2) + I(agemos^3) + sex, data=nwaz, random= ~ agemos | id, method="ML")
pwaz_sex <- nwaz
pwaz_sex$pred <- predict(cm_sex, newdata=pwaz_sex,level=0)
ggplot(pwaz, aes(agemos, height, group=id, colour=factor(id)), alpha=I(1/10)) + geom_line() +
  geom_smooth(aes(group=id), methos="lm", size=0.2, colour="blue", se=F) +
  geom_smooth(aes(group=1), method="lm", formula=y~ns(x,3), size=2, se=F, colour="red")

ggplot(pwaz_sex,aes(x=agemos,y=height,color=factor(sex)), alpha=I(1/30)) + 
  geom_path() +
  geom_line(data=pwaz_sex,aes(x=agemos,y=pred,group=sex,color=factor(sex)), colour="black") +
  scale_y_continuous("height")