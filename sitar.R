library('datasets')
library('sitar')

# BICadj:
data(heights)
## fit sitar model for height
m1 <- sitar(x=age, y=height, id=id, data=heights, df=5)

## update it for log(height)
m2 <- update(m1, y=sqrt(height))

## compare variance explained in the two models
varexp(m1, m2)

## compare BIC adjusting for sqrt tranform
## the pattern matches names starting with "m" followed by a digit 
BICadj(pattern="^m[0-9]")


# bupdate(x)
## fit sitar model with b fixed effect starting value defaulting to 'mean'
m1 <- sitar(x=age, y=height, id=id, data=heights, df=5)
print(fixef(m1)['b'])

## refit with starting value chosen to minimise b-c correlation
m2 <- update(m1, bstart=bupdate(m1))
print(fixef(m2)['b'])

# cLMS:
cLMS(z = -2:2, L = 1:-1, M = 5:7, S = rep(0.1, 3))
zLMS(x = 6.5, L = 1:-1, M = 5:7, S = rep(0.1, 3))


# codeplot:
## identify outliers
outliers <- velout(age, height, id, heights, limit=2)

## plot outliers with code 4 or 6
codeplot(outliers, icode=c(4, 6))

## set the 8 outliers missing
newheights <- zapvelout(outliers, icode=6)

# heights
coplot(height ~ age | id, data = heights, panel = panel.smooth, show.given=FALSE,
       xlab = 'age (years)', ylab = 'height (cm)', pch = 19)

# lms2z
## convert girls' heights data to UK 90 z-score
data(heights)
data(uk90)
lms2z(age, height, sex = 2, heights, measure = 'ht', reg = 'uk90')

## construct table of boys weight centiles for WHO standard on 3-month age grid
data(who06)
zs <- -4:4*2/3 # z-scores for centiles
ages <- 0:12/4 # 3-month ages
v <- as.data.frame(lapply(as.list(zs), function(z) {
  v <- lms2z(ages, z, sex = 1, measure = 'tw', ref = 'who06', toz = FALSE)
}))
names(v) <- z2cent(zs)
rownames(v) <- ages
round(v, 2)

# makess:
## create smooth.spline mean height curve
data(heights)
ss <- with(heights, makess(age, height))

## and plot it
plot(ss, type = 'l', xlab = 'age', ylab = 'height')

## age at peak velocity, and peak velocity
print(ss$apv)

# mplot:
mplot(age, height, id, heights)

# plot.sitar:
## fit sitar model
m1 <- sitar(x=age, y=height, id=id, data=heights, df=7)

## draw fitted distance and velocity curves
## with velocity curve in blue
## adding age at peak velocity
plot(m1, y2par=list(col='blue'), apv=TRUE)

## draw individually coloured growth curves adjusted for random effects
## using same x-axis limits as for previous plot
plot(m1, opt='a', col=1+as.integer(id) %% 5, xlim=xaxsd())

## add mean curve in red
lines(m1, opt='d', col='red', lwd=2)

# plotclean:
plotclean(age, height, id, heights)


# sitar:
data(heights)
## fit simple model
m1 <- sitar(x=age, y=height, id=id, data=heights, df=5)

## alternatively try sqrt transform for height and increase df
m2 <- update(m1, x=age, y=sqrt(height), df=6)


# subsample:
## draw 50% random sample
s50 <- subsample(age, id, heights, prob=0.5)

## trancate age range to 7-12 for 50% of subjects
t50 <- subsample(age, id, heights, prob=0.5, xlim=c(7,12))


# velout:
outliers <- velout(age, height, id, heights, limit=3)


# xaxsd:
## generate and plot 100 data points
x <- rnorm(100)
y <- rnorm(100)
plot(x, y, pch=19)

## generate and plot 10 more
## constraining axis scales to xbe as before
x <- rnorm(10)
y <- rnorm(10)
plot(x, y, pch=19, xlim=xaxsd(), ylim=yaxsd(c(-3,3)))

## force axis extreme to be -3 and 3
plot(x, y, pch=19, xlim=xaxsd(c(-3,3)), ylim=yaxsd(c(-3,3)))


# y2plot:
## plot boys median height and weight on the UK 1990 reference
with(uk90[uk90$sex == 1,], y2plot(x=years, y1=M.ht, y2=M.wt, y2par=list(col='red')))
