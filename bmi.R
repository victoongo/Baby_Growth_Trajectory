library(sitar)
library(ggplot2)

setwd("~/proj_data/bmi")
bmi<-read.csv("bmi.csv", as.is=TRUE)
dim(bmi)
names(bmi)
#keep <- complete.cases(bmi)
#wt <- wt[keep,]
bmi <- unique(bmi)
# bmi$id <- paste0(bmi$nestid, '_', bmi$mom_nestid)
bmi$id <- bmi$mom_nestid * 10000 + bmi$nestid

ggplot(aes(x=agemos), data=bmi) + geom_histogram() 

summary(bmi)
hist(bmi$weight)
plot(bmi$agemos, bmi$weight)
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
waz <- subset(waz, select=c(height, agemos, id, ref_date))
waz <- unique(waz)

# agemos0 <- 

#waz <- waz[1:5000,]
# waz <- waz[waz$agemos<=48 & waz$agemos>3,]
waz <- waz[waz$agemos<=12,]
mplot(agemos, height, id, waz)

# waz_freq <- table(waz$id)
# waz_keep <- data.frame(waz_freq[waz_freq>5])
# waz <- waz[waz$id %in% rownames(waz_keep),]

ggplot(aes(x=height, fill=as.factor(sex)), data=bmi) + geom_histogram() 
ggplot(aes(x=agemos, y=height, group=sex, color=as.factor(sex)), data=bmi) + geom_point()

outliers <- velout(agemos, height, id, waz, limit=0.5, linearise=TRUE)
nwaz <- zapvelout(outliers, icode=c(4,6,7))
keep <- outliers[abs(outliers$height)<4.5,]
nwaz <- waz[row.names(keep),]
nwaz <- nwaz[!is.na(nwaz$height),]

waz_freq <- table(nwaz$id)
waz_keep <- data.frame(waz_freq[waz_freq>7])
nwaz <- nwaz[nwaz$id %in% rownames(waz_keep),]

outliers <- velout(agemos, height, id, waz, limit=0.5, linearise=TRUE)
nwaz <- zapvelout(outliers, icode=c(4,6,7))
# codeplot(outliers, icode=c(4, 6))
nwaz <- nwaz[!is.na(nwaz$height),]

waz_freq <- table(nwaz$id)
waz_keep <- data.frame(waz_freq[waz_freq>7])
nwaz <- nwaz[nwaz$id %in% rownames(waz_keep),]

mplot(agemos, height, id, nwaz)
# ggplot(aes(x=height, fill=as.factor(sex)), data=nwaz) + geom_histogram() 
# ggplot(aes(x=agemos, y=height, group=sex, color=as.factor(sex)), data=nwaz) + geom_point()

m1 <- sitar(x=agemos, y=height, id=id, data=nwaz, df=4)
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