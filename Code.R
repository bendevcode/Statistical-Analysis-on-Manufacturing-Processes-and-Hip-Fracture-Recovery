library(e1071)
processa <- c(8.5,7.0,8.2,7.8,6.8,6.4,7.8,6.4)
processb <- c(6.8,8.8,7.5,7.9,8.3,6.1,7.6,8.8)
process_all <- c(8.5,7.0,8.2,7.8,6.8,6.4,7.8,6.4,6.8,8.8,7.5,7.9,8.3,6.1,7.6,8.8)
pro <- c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2)
label <- c("process A","process B")
summary(processa)
summary(processb)
boxplot(processa,processb,names = label, main = "boxplot of processes A and B")
skewness(processa)
skewness(processb)
hist(processb)
kurtosis(processa)
kurtosis(processb)
#testing for normality
#Stating the hypothesis:
#Ho: the data could come from a normal distribution
#H1: the data could not have come from a normal distribution

shapiro.test(processa)
shapiro.test(processb)
#The p-value for process A is … 0.335 … this is greater than 0.1 therefore we 
#fail to reject the Ho and so the data could have come from a population that is 
#normally distributed.

#The p-value for process B is … 0.6293… this is greater than 0.1 therefore we 
#fail to reject the Ho and so the data could have come from a population that is 
#normally distributed.


#constant variance
#Stating the hypothesis:
#H0: the variance of Process A = the variance of Process B data 
#H1: the variances are not equal

bartlett.test(process_all~pro)

#t test
#Ho: MU of process A = MU of process B
#H1: MU of process A not = MU of process B

t.test(processa,processb, alternative = "two.sided", conf.level=0.9, var.equal=TRUE)



#question 2
speed = c(0.89,1.35,0.64,0.82,1.27,0.65,0.30,0.32,0.20,0.82,
          0.36,0.83,1.13,0.37,0.18,0.42,0.52,0.46,0.57,0.99)
boxplot(speed, ylab="speed", main="boxplot of speed data")
summary(speed)
skewness(speed)
kurtosis(speed)
plot(speed,xlab="")

#test for normality
#Stating the hypothesis:
#Ho: the data could come from a normal distribution
#H1: the data could not have come from a normal distribution

shapiro.test(speed)
#performing the t test
#Ho: speed <= 1
#H1: speed > 1

t.test(speed, alternative = "greater", mu = 1, conf.level = 0.99 )
#The p-value = 0.9999 which is greater than our level of significance of 0.01 
#so we will fail reject the Ho (i.e., it’s possible that the individuals who 
#suffered a hip fracture have fully recovered)