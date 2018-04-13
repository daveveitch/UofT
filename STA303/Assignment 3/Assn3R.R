# Assignment 3 David Veitch 1004657917

workdir<-("C:/Users/David/Google Drive/Documents/UofT/NonDegree/303/Assignment 3")
setwd(workdir)
dat <- read.csv("video.csv")

# 1A

# Create contingency table
cont_tab <- xtabs(~like+sex, data=dat)
cont_tab

# Run difference of proportions test and chi-sq test
prop.test(cont_tab, correct=FALSE)
fisher.test(cont_tab)

# 1B

# Create contingency tables for different grades

# Contingency tables & testsfor grade == A
cont_tab_grade_A <- xtabs(~like+sex, data=subset(dat, grade=="A"))
cont_tab_grade_A
prop.test(cont_tab_grade_A, correct=FALSE)
fisher.test(cont_tab_grade_A)

# Contingency tables & tests for grade == nA
cont_tab_grade_nA <- xtabs(~like+sex, data=subset(dat, grade=="nA"))
cont_tab_grade_nA
prop.test(cont_tab_grade_nA, correct=FALSE)
fisher.test(cont_tab_grade_nA)

# 2 A

# Create logistic regression models

# Model 2.1 to include interaction between sex and grade
model_2_1 <- glm(like~sex+grade+sex*grade,family=binomial, data=dat)
summary(model_2_1)

# Model 2.2 to not include interaction between sex and grade
model_2_2 <- glm(like~sex+grade,family=binomial, data=dat)
summary(model_2_2)

# Conduct Likelihood Ratio test using residual deviance
# Test is chi-squre with one degree of freedom
p_val = (1-pchisq(model_2_2$deviance - model_2_1$deviance,1))
p_val

# 3 A

# Create a 3-way contingency table and convert to a dataframe
cont_tab_3_way <- xtabs(~like+sex+grade, data=dat)
df_3_way <- as.data.frame(cont_tab_3_way)
df_3_way

# Create models with counts as Poisson variables

# Model 3.1 three two-way interaction terms and one three-way interaction
model_3_1 <- glm(Freq~sex+grade+like+sex*grade*like,family=poisson, data=df_3_way)
summary(model_3_1)

# Model 3.2 three two-way interaction terms, no three-way interaction
model_3_2 <- glm(Freq~sex+grade+like+ sex:grade + sex:like + grade:like,family=poisson, data=df_3_way)
summary(model_3_2)

# 3 B 

# Compare models 2_2 and 3_1, chisq with 4 df
# Model 3 statistically significantly more accurate
p_val = (1-pchisq(model_2_1$deviance - model_3_1$deviance, 4))
p_val