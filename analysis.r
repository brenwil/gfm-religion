library(haven)
library(moments)
library(tidyverse)
library(corrplot)
library("Hmisc")
library(corrgram)
library(boot)
library(semTools)
# bootstrap replication: (more than one bootstrap process used and consistency in output desirable)
set.seed(2341)

# Data:
QLS <- read_sav("./QLS_Response_Data_Set.sav")
# Subset and omit NA: 
QLSd = na.omit(QLS[c("SFR2", "SFR3", "SFR4", "SFR5", "SFR6", "SFR7", "SFR8", "SFR9", "A1_CPGITOTAL_1", "A1_GFTOTAL" )])
# view(QLSd)
GFm <- 11-QLSd$A1_GFTOTAL
describe(GFm)
sd(GFm)
# RELIABILITY CHECK: Cronbach calculated in SPSS previously: 
QLSd.GFM=na.omit(QLS[c("A1_GF1", "A1_GF2", "A1_GF3", "A1_GF4", "A1_GF5", "A1_GF6", "A1_GF7", "A1_GF8", "A1_GF9", "A1_GF10")])
library(Cronbach)
cronbach(QLSd.GFM)

# actually: calculate OMEGA: 
HS.model <- ' GFM  =~ A1_GF1+A1_GF2+A1_GF3+A1_GF4+A1_GF5+A1_GF6+A1_GF7+A1_GF8+A1_GF9+A1_GF10 '
fit <- cfa(HS.model, data = QLS)
reliability(fit)
reliability(fit, return.total = TRUE)

# create the RJRS scales: 
QLSd$Ritual=QLSd$SFR2+QLSd$SFR3
QLSd$Consequential=QLSd$SFR4+QLSd$SFR5
QLSd$Ideological=QLSd$SFR6+QLSd$SFR7
QLSd$Experiential=QLSd$SFR8+QLSd$SFR9

# GF REVERSED (interpretation easier - higher = more fallacies)
QLSd$GamblingFallacies <- 11 - QLSd$A1_GFTOTAL

correlations = cor(correlateData[sapply(correlateData, is.numeric)], use = "pairwise", method = "kendall")
correlations


library(psych)
describe(QLSd$Ritual)
describe(QLSd$Consequential)
describe (QLSd$Ideological)
describe(QLSd$Experiential)
describe(QLSd$GamblingFallacies)


summary(QLSd$Ritual)
boxplot(QLSd$Ritual, main="Ritualistic", sub=paste("Outlier rows: ", boxplot.stats(QLSd$Ritual)$out)) 
shapiro.test(QLSd$Ritual)

summary(QLSd$Consequential)
boxplot(QLSd$Consequential, main="Consequential", sub=paste("Outlier rows: ", boxplot.stats(QLSd$Consequential)$out))
shapiro.test(QLSd$Consequential)


summary (QLSd$Ideological)
boxplot(QLSd$Ideological, main="Ideological", sub=paste("Outlier rows: ", boxplot.stats(QLSd$Ideological)$out))
shapiro.test(QLSd$Ideological)

summary(QLSd$Experiential)
hist(QLSd$Experiential)
boxplot(QLSd$Experiential, main="Experiential", sub=paste("Outlier rows: ", boxplot.stats(QLSd$Experiential)$out))
shapiro.test(QLSd$Experiential)


# Center the RJRS data for LM interpretation: 
QLSd$Ritual <- scale (QLSd$Ritual, center = T, scale = T)
QLSd$Consequential <- scale (QLSd$Consequential, center = T, scale = T)
QLSd$Experiential <- scale (QLSd$Experiential, center = T, scale = T)
QLSd$Ideological <- scale (QLSd$Ideological, center = T, scale = T)

# Log transform QLSd to deal with skew
QLSd$GamblingFallacies <- log(QLSd$GamblingFallacies)+1


library("car")
lm.QLS <- lm(GamblingFallacies ~ Ritual + Consequential + Experiential + Ideological, data = QLSd, na.action = na.omit)
# anova(lm.QLS)
summary(lm.QLS, signif.stars=T, correlation=T)

