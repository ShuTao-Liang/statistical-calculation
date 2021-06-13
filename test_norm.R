#####################################################
#随机数直方图和正态概率图
e = rnorm_version3(1000, 10,2)       #rnorm_version"i"根据随机数生成的算法而改变
hist(e, freq = FALSE, ylim = c(0,0.2))
curve(dnorm(x, 10, 2), 4, 16, col="red", add=TRUE)

#####################################################
#正态QQ图

qqnorm(e, main='', xlab='', ylab='', cex=0.3)
qqline(e, lwd=2)

####################################################
#Shapiro-Wilk正态检验
shapiro.test(e)
