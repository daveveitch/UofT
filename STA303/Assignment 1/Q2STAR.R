setwd("C:/Users/David/Google Drive/Documents/UofT/NonDegree/303/")
dat <- read.csv("assign1datamodified.csv")
dat_male <- dat[dat$sex == "Male",]
dat_female <- dat[dat$sex == "Female",]

# Draw Boxplot
boxplot(dat_male$height, dat_female$height, 
        xlab="Sex",names=c("Male","Female"),
        main="Q 2e - Boxplot - 7917")

# Conduct a Pooled T-Test
t.test(dat_male$height,dat_female$height,var.equal=T)

# Test diagnostics (checking model assumptions)

# F test for equal variances
var.test(dat_male$height,dat_female$height)

# Test Normality Histogram & QQ plot
par(mfrow=c(2,2))
qqnorm(dat_male$height,main="Male QQ - 7917")
qqline(dat_male$height)
hist(dat_male$height, main="Male Histogram - 7917", breaks=20)

qqnorm(dat_female$height,main="Female QQ - 7917")
qqline(dat_female$height)
hist(dat_female$height, main="Female Histogram - 7917", breaks=20)

# Test Normality - Shaprio 
shapiro.test(dat_male$height)
shapiro.test(dat_female$height)