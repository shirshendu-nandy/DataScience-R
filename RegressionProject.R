data("mtcars")
head(mtcars)
?mtcars
str(mtcars)
# Transform 'am' variable
mtcars$am   <- factor(mtcars$am,labels=c("Auto","Manual"))




#exploratory analysis 
#inference
t.test(mpg ~ am, data = mtcars)
#boxplot 
boxplot(mpg ~ am, data = mtcars, col = (c("bisque","lightgrey")), ylab = "MPG", xlab = "Type of transmission" )


#regression

fit0 <- lm(mpg ~ ., data = mtcars)
summary(fit0)

fitBest <- step(fit0, direction = "both")
summary(fitBest)
confint(fitBest)

##nested model test
fit1 <- lm(mpg ~ wt, data = mtcars)
fit2 <- update(fit1, mpg ~ wt + qsec)
fit3 <- update(fit2, mpg ~ wt + qsec + am)
anova(fit1, fit2, fit3)


#diagnostics
mar.orig <- par()$mar
par(mfrow=c(2, 2))
plot(fitBest, which = c(1:2))
par(mar = mar.orig)

ht <- hatvalues(fitBest)
summary(ht)


