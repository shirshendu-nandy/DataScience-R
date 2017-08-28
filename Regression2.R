library(UsingR)
data("father.son")
head(father.son)
fit1 <- lm(sheight ~fheight, data = father.son)
summary(fit1)
predict(fit1, newdata = data.frame(fheight = 80))
sumCoef1 <- summary(fit1)$coefficients
## confidence interval
sumCoef1[1,1]+ c(-1,1)*qt(.975, df = fit1$df)*sumCoef1[1,2]
confint(fit1)


data("mtcars")
head(mtcars)
fit2 <- lm(mpg ~ hp, data = mtcars)
summary(fit2)
fit2 <- lm(mpg ~ I(hp -mean(hp)), data = mtcars)
fit3 <- lm(mpg ~ I(wt -mean(wt)), data = mtcars)
predict(fit2, newdata = data.frame(hp = 111))
summary(mtcars)
confint(fit3)


plot(father.son$sheight, resid(fit1))
abline(h = 0, col ="red")


plot(mtcars$hp, resid(fit2))
abline(h = 0, col = "red")
summary(fit2)$sigma
