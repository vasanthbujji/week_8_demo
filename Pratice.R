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


