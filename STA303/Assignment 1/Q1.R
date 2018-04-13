setwd("C:/Users/David/Google Drive/Documents/UofT/NonDegree/303/")
juries_dat <- read.csv("juries(1).csv")

# separate data into two groups
spock_group <- juries_dat[juries_dat$JUDGE == "SPOCKS",]
others_group <- juries_dat[juries_dat$JUDGE != "SPOCKS",]

# Check for Outliers in Spocks group and other groups
# IQR rule is Q3 + 1.5IQR or Q1 - 1.5IQR

# IQR spock group
quantiles_spock <- quantile(spock_group$PERCENT,seq(.25,1,.25))
IQR_spock <- (quantiles_spock[3]-quantiles_spock[1])
outlier_spock_high_threshold = quantiles_spock[3] + 1.5*IQR_spock
outlier_spock_low_threshold = quantiles_spock[1] - 1.5*IQR_spock

outliers_spock_high <- subset(spock_group, PERCENT > outlier_spock_high_threshold)
outliers_spock_low <- subset(spock_group, PERCENT < outlier_spock_low_threshold)

# IQR of OTHERS
quantiles_others <- quantile(others_group$PERCENT,seq(.25,1,.25))
IQR_others <- quantiles_others[3]-quantiles_others[1]
outlier_others_high_threshold = quantiles_others[3] + 1.5*IQR_others
outlier_others_low_threshold = quantiles_others[1] - 1.5*IQR_others

outliers_others_high <- subset(others_group, PERCENT > outlier_others_high_threshold)
outliers_others_low <- subset(others_group, PERCENT < outlier_others_low_threshold)

# Create boxplot
boxplot(spock_group$PERCENT, others_group$PERCENT, 
        xlab="JUDGE",names=c("SPOCKS","OTHERS"),
        main="Q 1b - Boxplot - 7917",outline=FALSE)