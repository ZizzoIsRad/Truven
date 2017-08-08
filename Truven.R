
TT <- read.csv(file="C:/Transformed Worksheet (roh_big_data).csv",head=TRUE,sep=",")

dim(TT)
summary(TT)



View(TT)

data.frame(test)

ls(TT)


#nrow(TT$risk_cat)
#ncol(TT$SEX)
train <- TT[1:8000,]
test <- TT[8001:40000,]




TT$Risk.Cat <- factor(TT$Risk.Cat)
TT$Aid.Cat <- factor(TT$Aid.Cat)
#TT$MDST_AGE_GRP <- factor(TT$MDST_AGE_GRP)
TT$SEX <- factor(TT$SEX)
TT$Age.Years <- factor(TT$Age.Years)




#Regression 

fit <- glm(Readmission ~ Male , data = TT , family = "binomial")


predict(LoTT, newdata = TT, interval = 'prediction')

LoTT <- glm(Readmission ~  Male+ Adult+ Child+ Senior +Teen +  Disabled + Medically.Needy + Low.Income + In.Crisis + Struggling + At.Risk + Healthy + Stable  , data = TT , family = "binomial")


summary(LoTT)

#summary(LoT)


confint(LoTT)
plot(LoTT)



## odds ratios only
#exp(coef(TT$Readmission))


## odds ratios and 95% CI
exp(cbind(OR = coef(fit), confint(fit)))



xtabs(~Readmission+ Male , data = TT)
xtabs(~ Male , data = TT)

15931+2454



predict <- predict(LoTT, type = 'response')


plot(predict)


table(TT$Readmission,predict > 0.01)



#ROCR Curve
library(gplots)
library(ROCR)

ROCRpred <- prediction(predict, TT$Readmission)

ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))



