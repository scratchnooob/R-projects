a=read.csv("bus.csv")
#parsing into response and covariate variable
x<-a$Car.miles.per.year..1000s.
y<-a$Expenses.per.car.mile..pence.
#plotting the data
plot(y~x)
#linear Regression
model1<-lm(y~x,data=a)
plot(x,y)
points(x,model1$fitted.values,pch=20)
abline(model1)
#coefficient estimates
model1$coefficients
#fitted values
model1$fitted.values
#sum of fitted values
sum(model1$fitted.values)
#sum of response variables
sum(y)
#residual values and sum
model1$residuals
sum(model1$residuals)
#standard errors
summary(model1)
max(y)
which.max(y)
a[21,1]

#maximum abs residual value
which.max(abs(model1$residuals))
a[18,1]
#mean of covariate and mean of response
xbar<-mean(x)
ybar<-mean(y)
points.default(xbar,ybar,col="red", pch=21, cex=1.5) #clearly xbar & ybar lie on Linear Regression
