library(datasets)
data("Seatbelts")
seatbelts <- as.data.frame(Seatbelts)
head(seatbelts)
library(dplyr)
seatbelts <-  mutate(seatbelts
                     , dkb = 1* (DriversKilled > 119)
                     , pp = (PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice)
                     , mm = kms/1000
                     , mmc = mm - mean(mm))
fit4a <- glm(dkb ~ mmc + pp + law, data = seatbelts, family = binomial)
round(summary(fit4a)$coef,3)

1 - round(exp(summary(fit4a)$coef[4]),3)
#46% decrease in odds of having more than 119 drivers killed after the inclusion of law

1 - round(exp(summary(fit4a)$coef[3]),3)
# 3% decrease odds of having more than 119 drivers killed for every additional 1000 kms driven




fit4b <- glm(cbind(DriversKilled, drivers - DriversKilled) ~ mmc +pp + law, data = seatbelts, family = binomial)
round(summary(fit4b)$coef,3)
1 - round(exp(summary(fit4b)$coef[4]),3)
# 3% increase in ratios of drivers killed of all the drivers killed or injured after the inclusion of law


fit4c1 <- glm(dkb ~ law, data = seatbelts, family = binomial)
fit4c2 <- update(fit4c1, dkb ~ law + pp)
fit4c3 <- update(fit4c2, dkb ~ law + pp + mmc)
anova(fit4c1, fit4c2, fit4c3)


library(MASS)
data("shuttle")
head(shuttle)
Q1
new_shuttle=mutate(shuttle,autobin= ifelse(use=='auto',1,0))
logfit = glm(new_shuttle$autobin~factor(new_shuttle$wind)-1,family="binomial")
coeff=summary(logfit)$coeff[,1]
ans=exp(coeff[1])/exp(coeff[2])
ans


Q2
logfit = glm(new_shuttle$autobin~factor(new_shuttle$wind)*new_shuttle$magn-1,family="binomial")
summary(logfit)$coeff
