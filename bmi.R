library(sitar)

setwd("~/bmi")
bmi<-read.csv("bmi.csv", as.is=TRUE)
dim(bmi)
names(bmi)
#keep <- complete.cases(bmi)
#wt <- wt[keep,]
bmi <- unique(bmi)
bmi$id <- paste0(bmi$nestid, '_', bmi$mom_nestid)


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


#wt$age_mos <- wt$age_mos / 30
bmi <- bmi[bmi$X_bivwt==0,]
bmi <- bmi[bmi$X_bivht==0,]
bmi <- bmi[bmi$X_bivbmi==0,]
bmi <- bmi[bmi$X_bivwh==0,]

mplot(agemos, weight, id, bmi)
mplot(agemos, waz, id, bmi)
mplot(agemos, height, id, bmi)
mplot(agemos, haz, id, bmi)
mplot(agemos, bmi, id, bmi)
mplot(agemos, bmiz, id, bmi)
mplot(agemos, whz, id, bmi)


outliers <- velout(age_mos, weight_kg, id, wt, limit=2)
codeplot(outliers, icode=c(4, 6))

length(unique(epi$nestid)) == nrow(epi)
rownames(epi) <-epi$nestid