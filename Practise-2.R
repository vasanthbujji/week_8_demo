insurance_data<- read.csv("insurance.csv")
str(insurance_data)
insurance_data$sex<- factor(insurance_data$sex, levels = c("male","female"), ordered = FALSE)
attach(insurance_data)
insurance_data$smoker<-factor(smoker,levels = c("yes","no"), ordered = FALSE)
insurance_data$region<-factor(region, levels = c("northeast","northwest","southeast","southwest"))
View(insurance_data)

table(insurance_data$sex)
table(insurance_data$smoker)
table(insurance_data$region)

install.packages("psych")
library(psych)
windows(20,12)
pairs.panels(insurance_data, smooth=FALSE, scale=FALSE, density=TRUE,
             ellipses=FALSE, method="pearson", pch=21, lm=FALSE,
             cor=TRUE, jiggle=FALSE,hist.col = 1)

model<-lm(charges~ age+sex+bmi+children+smoker+region)
model
summary(model)
saveRDS(model,"./insurance_model.rds")

charges~11778.7+256.9* age+339.2*bmi+475.5*children-
        23848.5*smokerno -1035.0 *regionsoutheast-
        960.0*regionsouthwest