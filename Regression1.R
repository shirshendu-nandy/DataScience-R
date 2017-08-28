library(UsingR)
data("galton")
head(galton)
str(galton)
library(reshape2)
long <- melt(galton)
head(long)
g <- ggplot(long, aes(x = value, fill = variable))
g <- g + geom_histogram(colour = "black", binwidth = 1)
g <- g + facet_grid(.~variable)
g



g1 <-  ggplot(galton, aes(x=  child))
g1 <-  g1 + geom_histogram( fill =  "salmon", colour = "black", binwidth = 1) 
g1 <-  g1 + geom_vline(xintercept = mean(galton$child, na.rm = T), size = 3)
g1


lm(I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton)


freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
g <- ggplot(freqData[freqData$freq > 0,], aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")                    
lm1 <- lm(galton$child ~ galton$parent)
g <- g + geom_abline(intercept = coef(lm1)[1], slope = coef(lm1)[2], size = 3, colour = grey(.5))
g




x <-  galton$parent
y <-  galton$child

xc <-  x - mean(x)
yc <-  y - mean(y)

xs <- xc/sd(xc)
ys <- yc/sd(yc)

sd(xs)
sd(ys)
mean(xc)
mean(yc)


cor(xs, ys)



data("father.son")
head(father.son)
fit <- lm(sheight ~ fheight, data = father.son)
fit

g2 <- ggplot(father.son, aes(x = fheight, y = sheight)) + geom_point(colour="grey50") + geom_smooth(method = lm, lwd = 2)
g2
cor(father.son)


## If a father's height was 63 inches, what would you predict the son's height to be?

b0 <- coef(fit)[1]
b1 <- coef(fit)[2]
b0 + b1 *63

predict(fit, newdata = data.frame(fheight = 63))




library(UsingR)
data(father.son)
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(x, y)
library(ggplot2)
g = ggplot(data.frame(x, y), aes(x = x, y = y))
g = g + geom_point(size = 5, alpha = .2, colour = "black")
g = g + geom_point(size = 4, alpha = .2, colour = "red")
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(position = "identity")
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2)
g = g + xlab("Father's height, normalized")
g = g + ylab("Son's height, normalized")
g