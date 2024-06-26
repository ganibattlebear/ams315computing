install.packages('mice')
library(mice)
setwd('/Users/gania/Documents/Project1/Project1_PartB_data')
getwd()
PartB <- read.csv('095768_partB.csv')
newX <- log((PartB$y))^2
newY <- (PartB$x)
plot(PartB$x ~ PartB$y, pch=1)
plot(newX ~ newY, main='Scatter : x ~ y', xlab='x', ylab='y', pch=1)
M <- lm(newX ~ newY, data=PartB)
plot(fitted(M), resid(M), abline(0,0))
summary(M)
abline(M, col='red', lty=3, lwd=2)
legend('topleft', legend='Estimated Regression Line', lty=3, lwd=2, col='red')
## start binning 
data_trans <- data.frame(xtrans=newX, ytrans=newY)
groups <- cut(data_trans$xtrans,breaks=c(-Inf,seq(min(data_trans$xtrans)+0.3, max(data_trans$xtrans)-0.3,by=0.15),Inf))
table(groups)
x <- ave(data_trans$xtrans, groups)
data_bin <- data.frame(x=x, y=data_trans$ytrans)
plot(data_bin$x, data_bin$y, pch=1)
install.packages('remotes')
library(remotes)
install_github("cran/alr3", force = TRUE)
##lack of fit test on data_bin
fit_b <- lm(y ~ x, data = data_bin)
pureErrorAnova(fit_b)
##extra analysis
summary(fitted(M))

confint(M, level = 0.95)
