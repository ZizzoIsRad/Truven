View(Person_record_count)
plot(Person_record_count)
roh_data[is.na(roh_data)] <- 0
summary(roh_data['Age Years'])
summary(pt_under_18['Age Years'])
gender <- data.frame(table(Export_Worksheet_asthma_patients_$`Mdst Age Grp`))
under_18 <- Export_Worksheet_asthma_patients_[Export_Worksheet_asthma_patients_$`Age Years` >= "18", ]
am.glm = glm(formula=`Adm Days Last Cnt` ~ `Age Years` + `Aid Cat`, data=Export_Worksheet_asthma_patients_, family=binomial)
dups <- data.frame(table(Export_Worksheet$`Person Key`))
dups <- data.frame(table(Export_Worksheet$`Dx Cd`))
Export_Worksheet[Export_Worksheet$`Dx Cd` == "49300", ]
Export_Worksheet$iAid_Cat= as.integer(as.factor(Export_Worksheet$`Aid Cat`))
Export_Worksheet$Readmit[Export_Worksheet$`Admit Dt` <= 30 & Export_Worksheet$`Admit Dt` > 1]<-1
Export_Worksheet$Readmit2<-ifelse(Export_Worksheet$`Admit Dt` == 30 ,1,0)


library(Rserve)
Rserve(args="--no-save")

#Code used
Export_Worksheet$Readmit<-ifelse(Export_Worksheet$`Adm Days Last Cnt` <= 30 & Export_Worksheet$`Adm Days Last Cnt` > 1,1,0)


Export_Worksheet$Age_Years<-Export_Worksheet$`Age Years`
Export_Worksheet$Risk_Cat<-Export_Worksheet$`Risk Cat`
Export_Worksheet$Aid_Cat<-Export_Worksheet$`Aid Cat`

am.glm = glm(formula=Readmit ~ SEX + Age_Years + Risk_Cat + Aid_Cat, data=Export_Worksheet, family=binomial)
summary( am.glm)
confint(am.glm)
exp(am.glm$coefficients)
exp(confint(am.glm))

newdata <- data.frame(SEX = Export_Worksheet$SEX, Age_Years = Export_Worksheet$Age_Years, Risk_Cat =  Export_Worksheet$Risk_Cat, Aid_Cat = Export_Worksheet$Aid_Cat)
Export_Worksheet$predict <- predict(am.glm,newdata,type="response")


#Code End



##Tableau

# SCRIPT_REAL("
# 
#             readmit <- .arg1
#             Age_Years <- .arg2
#             
#             fit <- glm(  readmit ~ Age_Years, family = binomial )
#             fit$fitted
#             "
#             , ATTR([Admit Dt]), ATTR([Age Years]))



############################ Research
# 
# View(Person_record_count)
# 
# 
# plot(Person_record_count)
# 
# 
# roh_data[is.na(roh_data)] <- 0
# 
# 
# summary(roh_data['Age Years'])
# 
# 
# summary(pt_under_18['Age Years'])
# 
# 
# gender <- data.frame(table(Export_Worksheet_asthma_patients_$`Mdst Age Grp`))
# 
# 
# under_18 <- Export_Worksheet_asthma_patients_[Export_Worksheet_asthma_patients_$`Age Years` >= "18", ]
# 
# 
# am.glm = glm(formula=`Adm Days Last Cnt` ~ `Age Years` + `Aid Cat`, data=Export_Worksheet_asthma_patients_, family=binomial)
# 
# 
# dups <- data.frame(table(Export_Worksheet$`Person Key`))
# 
# 
# dups <- data.frame(table(Export_Worksheet$`Dx Cd`))
# 
# Export_Worksheet[Export_Worksheet$`Dx Cd` == "49300", ]
# 
# 
# Export_Worksheet$iAid_Cat= as.integer(as.factor(Export_Worksheet$`Aid Cat`))
# Export_Worksheet$iDx_Cd= as.integer(as.factor(Export_Worksheet$`Dx Cd`))
# 
# Export_Worksheet$Readmit[Export_Worksheet$`Adm Days Last Cnt` <= 30 & Export_Worksheet$`Admit Dt` > 1]<-1
# 
# admits<- data.frame(table(Export_Worksheet$Readmit2))
# 