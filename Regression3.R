library(datasets)
data("Seatbelts")
head(Seatbelts)
summary(Seatbelts)
seatbelts <- as.data.frame(Seatbelts)
summary(seatbelts)
head(seatbelts)
fit1 <- lm(DriversKilled ~ kms + PetrolPrice, data = seatbelts)
summary(fit1)$coef
library(dplyr)
seatbelts <-  mutate(seatbelts
                     , pp = (PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice)
                     , mm = kms/1000
                     , mmc = mm - mean(mm))
head(seatbelts)
fit2 <- lm(DriversKilled ~ mmc + pp, data = seatbelts)
summary(fit2)$coef
fit2 <- lm(I(log(DriversKilled)) ~ mmc + pp, data = seatbelts)
1 - exp(-0.01400794)



data("swiss")
pairs(swiss)
cor(swiss)
summary(lm(Fertility ~ . , data = swiss))$coef

#model selection using nested model technique
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
fit3 <- update(fit1, Fertility ~ Agriculture + Examination + Education)
fit5 <- update(fit3, Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality)
anova(fit1, fit3, fit5)



data("InsectSprays")
head(InsectSprays)
summary(lm(count ~ spray , data = InsectSprays))$coef
# without the reference
summary(lm(count ~ spray -1 , data = InsectSprays))$coef
# relevel
spray2 <- relevel(InsectSprays$spray, "C")
summary(lm(count ~ spray2 , data = InsectSprays))$coef


data("mtcars")
head(mtcars)
fitQ1 <-  lm(mpg ~ I(factor(cyl)) + wt , data = mtcars)
summary(fitQ1)$coef
fitQ2 <-  lm(mpg ~ I(factor(cyl)), data = mtcars)
summary(fitQ2)$coef
fitQ3 <- lm(mpg ~ I(factor(cyl)*wt, data = mtcars) 
summary(fitQ2)$coef





n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
fit <- lm(y ~ x + t)
summary(fit)$coef





x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fitQ5 <- lm(y ~ x)
hatvalues(fitQ5)
dfbetas(fitQ5)
