var<-lm(weight~height, data =women)
y~b0+b1*x
summary(lm(weight~height, data =women))
View(women)

a~cars
str(cars)
var1=lm(dist~speed, data =cars)
summary(lm(dist~speed, data =cars))
View(cars)

windows(20,12)
scatter.smooth(x=cars$speed,
               y=cars$dist, main ="Dist~ Speed")

windows(20,12)
par(mfrow=c(1,2))
boxplot(cars$speed, main ="CAR~SPEED")
boxplot(cars$dist, main="CAR~DISTANCE")       

install.packages()

windows(20,12)
par(mfrow =c(1,2))
plot(density(cars$speed),
     main = "Density plot:Speed",
     ylab = "Frequency")
polygon(density(cars$speed),col = "blue")

plot(density(cars$dist),
     main = "Density plot:Distance",
     ylab = "Frequency")
polygon(density(cars$dist),col = "red")

cor(cars$speed, cars$dist)
cor(cars)

attach(cars)
linearmod<-lm(dist~speed)
linearmod

no_of_records<-sample(1:nrow(cars),0.8 * nrow(cars))
str(no_of_records)

training_data<-cars[no_of_records,]
training_data

testing_data<-cars[-no_of_records,]
testing_data

lr_model<-lm(dist~speed, data=training_data)
lr_model
summary(lr_model)

dist_predicted<-predict(lr_model, testing_data) 
dist_predicted

actual_preds<-data.frame(cbind(actuals = testing_data$dist, predicted = dist_predicted))
actual_preds

head(actual_preds)

attach(actual_preds)
correlation_accuracy<- cor(actuals, predicted)
correlation_accuracy

correlation_accuracy1<-cor(actual_preds)
correlation_accuracy1

min_max_accuracy<- mean(apply(actual_preds,1,min)/ apply(actual_preds,1,max))
min_max_accuracy

mape<-mean(abs(predicted - actuals)/ actuals)
mape


#install.packages("DAAG")
library(DAAG)

windows(20,12)
cvResults<-suppressWarnings(CVlm(data = cars, form.lm = dist~speed, m=5, dots =FALSE,
                                 seed = 29, legend.pos = "topleft", printit = FALSE,
                                 main = "Small Symbols are predicted vales while bigger ones are actuals"))
cvResults

saveRDS(lr_model,"./cars_model.rds")
lr_model_1<-readRDS("./cars_model.rds")
lr_model_1
