## DAVID VEITCH ASSIGNMENT 2 - 1004657917

#Import CSV to variable dat
setwd("C:/Users/David/Google Drive/Documents/UofT/NonDegree/303/Assignment 2")
dat <- read.csv("bbw.csv")

# Create variables maturity & MatSmoke with Factor levels
maturity=array(0,length(dat$gestation))
MatSmoke=array(0,length(dat$smoke))
for (i in 1:length(dat$gestation))
{
  if (dat$gestation[i]<259)
  {maturity[i]=1}
  else if (dat$gestation[i]>293)
  {maturity[i]=3}
  else {maturity[i]=2}
}
for (i in 1:length(dat$smoke))
{
  if (maturity[i]==1 & dat$smoke[i]==1)
  {MatSmoke[i]="PreSmoke"}
  else if (maturity[i]==1 & dat$smoke[i]==0)
  {MatSmoke[i]="PreNoSmoke"}
  else if (maturity[i]==2 & dat$smoke[i]==1)
  {MatSmoke[i]="NorSmoke"}
  else if (maturity[i]==2 & dat$smoke[i]==0)
  {MatSmoke[i]="NorNoSmoke"}
  else if (maturity[i]==3 & dat$smoke[i]==1)
  {MatSmoke[i]="PostSmoke"}
  else {MatSmoke[i]="PostNoSmoke"}
}

# Append maturity & MatSmoke to dat
dat$MatSmoke <- MatSmoke
dat$maturity <- maturity

# QUESTION 1 - Create Boxplots for 3 factors

# BOXPLOT - Mothers Smoked/not smoked
boxplot(dat[dat$smoke==0,]$bwt, dat[dat$smoke==1,]$bwt, 
        xlab="Did Mother Smoke During Pregnancy",ylab="Birth Weight",
        names=c("Did Not Smoke","Smoked"),
        main="Q 1a - Boxplot - 7917")

# BOXPLOT - Maturity Level
boxplot(dat[maturity==1,]$bwt, dat[dat$maturity==2,]$bwt, 
        dat[dat$maturity==3,]$bwt, xlab="Gestational Age (Days)",ylab="Birth Weight",
        names=c("Age < 259","259 <= Age <= 293","Age > 293"),
        main="Q 1b - Boxplot - 7917")

# BOXPLOT - Smoking & Maturity
boxplot(dat[dat$MatSmoke=="PreNoSmoke",]$bwt, 
        dat[dat$MatSmoke=="PreSmoke",]$bwt, 
        dat[dat$MatSmoke=="NorNoSmoke",]$bwt, 
        dat[dat$MatSmoke=="NorSmoke",]$bwt, 
        dat[dat$MatSmoke=="PostNoSmoke",]$bwt, 
        dat[dat$MatSmoke=="PostSmoke",]$bwt, 
        ylab="Birth Weight",
        names=c("PreNoSmoke","PreSmoke","NorNoSmoke","NorSmoke",
                "PostNoSmoke","PostSmoke"),
        main="Q 1c - Boxplot - 7917")

# QUESTION 2 - Use T-Test to investigate if difference in weight to babies
# from mothers who were smokers to mothers who were non-smokers
t.test(dat$bwt~dat$smoke)

# QUESTION 3 - Investigate whether there is a difference in mean birth weight
# among babies classified by gestational maturity using one way ANOVA
#dat$maturity <- as.factor(dat$maturity)
aov3 = aov(dat$bwt~dat$maturity)
summary(aov3)
pairwise.t.test(dat$bwt, dat$maturity, p.adj="bonf")

# QUESTION 4 - Use a one-way analysis of variance to investigate whether there
# is a difference in mean birth weight among six categories based on maturity
# level and mothers smoking status
summary(aov(dat$bwt~dat$MatSmoke))
pairwise.t.test(dat$bwt, dat$MatSmoke, p.adj="bonf")

# QUESTION 5 - Check assumptions of one way ANOVA
# Check equal variances with Bartlett Test
bartlett.test(dat$bwt~dat$MatSmoke)

# Check normarlity with QQ-Plot
plot(lm(dat$bwt~dat$MatSmoke),which=2, main="Q 5 - 7917")

# QUESTION 7 - Number of babies per group
table(dat$MatSmoke)
