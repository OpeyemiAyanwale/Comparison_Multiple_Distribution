
# COURSE: Introductory to Case Study Summer 2023
# PROJECT TITLE: Comparison of Multiple Distributions (Project 2)



getwd()


install.packages("DescTools")
install.packages("car")


library(readr)
library(dplyr)
library(tidyr)
library(car)
library(DescTools)


path <- "babies.csv"
babiesData <- read_csv(file=path)
babiesData <- babiesData[,-1]
View(babiesData)

subData <- babiesData[, c('smoke', 'wt')]
View(subData)


# check the structure of Data
head(subData)
str(subData)

## changed type of smoke to factor
class(subData$smoke)
subData$smoke <- as.factor(subData$smoke)
class(subData$smoke)
unique(subData$smoke)

#Check for missing data, unkown in smoke and weight
sum(is.na(subData))  #10 missing data
subData[rowSums(is.na(subData)) > 0, ]

smokeUnknown <- subData[subData$smoke == "9",]
smokeUnknown

weightUnknown <- subData[subData$wt == 999,]
weightUnknown

subData <- filter(subData, subData$smoke != "9" & !is.na(subData$wt))
unique(subData$smoke)
str(subData)

## Question 1

# Descriptive statistics by Smoke
descriptiveStat = subData %>% 
  group_by(smoke) %>% 
  summarize(n(), mean_weight = mean(wt), variance_weight = var(wt), stdd_weight = sd(wt))

View(descriptiveStat)


#Histogram of weigth
fun <- dnorm(subData$wt, mean = mean(subData$wt), sd = sd(subData$wt))
hist(subData$wt, prob = TRUE, ylim = c(0, max(fun)))
lines(density(subData$wt), col = 4, lwd = 2)

##Question 2 #ANOVA test
## Assumptions of ANOVA

## Test for normality 

#Group 0
group0 = subset(subData, smoke == "0")
qqnorm( group0$wt, ylab =" Quantiles", main=NULL)
qqline(group0$wt)

#Group 1
group1 = subset(subData, smoke == "1")
qqnorm( group1$wt, ylab =" Quantiles", main=NULL)
qqline(group1$wt)

#Group 2
group2 = subset(subData, smoke == "2")
qqnorm( group2$wt, ylab =" Quantiles", main=NULL)
qqline(group2$wt)

#Group 3
group3 = subset(subData, smoke == "3")
qqnorm( group3$wt, ylab =" Quantiles", main=NULL)
qqline(group3$wt)

##Shapiro Wilk test
## TESTING FOR NORMAILTY TOGETHER
model  <- lm(wt ~ smoke, data = subData)
qqnorm( residuals(model), ylab =" Quantiles", main=NULL)
qqline(residuals(model))
shapiro.test(residuals(model))

shapiro.test(group0$wt) ##The p-value is less than the level of significance 0.05,
shapiro.test(group1$wt) ##If the p-value is low, we can reject such a null hypothesis 
                        ## and say that the sample has not been generated from a normal distribution. 
shapiro.test(group2$wt)
shapiro.test(group3$wt)



## The p-value is less than the level of significance 0.05,
## we reject null hypothesis and conclude that the weight.

## test for homogenity of variance

# Using leveneTest()
result = leveneTest(wt ~ smoke, subData)
result

##mainData$smoke <- factor(mainData$smoke)
##mainData <- filter(subData, subData$smoke != "9" & !is.na(subData$wt))
##mainData$smoke <- factor(mainData$smoke)

##Question 3 # ANOVA Test
#anova_values = anova(lm(wt~smoke, data = subData))
#View(anova_values)

anova_values = aov(wt~smoke, data = subData)
summary(anova_values)


## The p-value is less than the level of significance 0.05,
## we reject null hypothesis and conclude that the means of the samples are not equal.

#pairwise differences (T-test)
pairwise.t.test(subData$wt, subData$smoke, data=subData, pool.sd = TRUE, var.equal = TRUE, p.adj = "none")

# Bonferroni adjustment
pairwise.t.test(subData$wt, subData$smoke, data=subData, pool.sd = TRUE, var.equal=TRUE, p.adj = "bonf")

# Turkey test
#model<-aov(wt~smoke, data = subData)

#HSD.test(model,"smoke", group=TRUE, unbalanced=TRUE, console=TRUE)

# Scheffe test since we have unequal sample size
ScheffeTest(anova_values)

scheffe_result <- ScheffeTest(anova_values)

