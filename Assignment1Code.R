#
#     R-code to generate data for Assignment 1.  This code MUST be
#     placed at the start of your own R-script.  You must edit
#     the argument to the set.seed( ) function to fit your own
#     registration number
#
#     Please also ensure you have created all required variables 
#     (Quantity1, Quantity2, Quantity3, Quantity5, Quantity6, 
#     Quantity7, Quantity8)
#     and required function Quantity4
#
RNGkind(sample.kind = "Rejection")
set.seed(9251)
#
#     Generate sample A times with censorng
#
LenA <- 50
TimeA <- sort(round(rexp(LenA, 0.48), digits = 10))
CensorA <- rbinom(LenA,1,0.5)
cbind(TimeA, CensorA)
#
#     Generate sample B times with censorng
#
LenB <- 50
TimeB <- sort(round(rexp(LenB, 0.52), digits = 10))
CensorB <- rbinom(LenB,1,0.4)
cbind(TimeB, CensorB)
#
#     Combine data
#
Time <- c(TimeA, TimeB)
Censor <- c(CensorA, CensorB)
Group <- factor(c(rep(1,LenA), rep(2,LenB)))
cbind(Time, Censor, Group)

####################################################
# Please insert your R code after this line
####################################################

######### Code to evaluate Quantity1 and Quantity 2 ##############
library(survival) # loads package survival 
KM.surv1 <- survfit(Surv(Time[1:50], Censor[1:50]) ~ 1, conf.type = "plain") #assigns fitted KM model for group A to this variable 
KM.surv2 <- survfit(Surv(Time[51:100], Censor[51:100]) ~ 1, conf.type = "plain") #assigns fitted KM model for group B to this variable 
par(mfcol=c(1,2)) #merges 2 plots side by side 
plot(KM.surv1$time,KM.surv1$surv, main = "Plot of survival functions against time", 
xlab = "Time (in years)", ylab = "Output of survival functions",col="red",type="l") #plots survival function for group A 
Quantity1=1-summary(KM.surv1,time=3.5)$surv #calculates quantity 1 
lines(KM.surv2$time,KM.surv2$surv,col="blue") # adds plot of survival function for group B vs time 
legend("topright",
c("Survival function A","Survival function B"),
col=c("red","blue"),lty=c(1,1),cex=0.65) #affixes legends to the top-right corner of the third plot


plot(KM.surv1$time,KM.surv1$lower, main = "Plot of confidence intervals' bounds against time",
xlab = "Time (in years)", ylab = "Confidence interval",col="red",type="l",lty=2) # plots lower bound of confidence interval of group A's survival function
lines(KM.surv1$time,KM.surv1$upper,col="red",lty=3)  # plots upper bound of confidence interval of group A's survival function
lines(KM.surv2$time,KM.surv2$lower,col="blue",lty=2) # plots lower bound of confidence interval of group B's survival function
lines(KM.surv2$time,KM.surv2$upper,col="blue",lty=3) # plots upper bound of confidence interval of group A's survival function
legend("topright",
c("Lower bound of confidence interval for group A","Lower bound of confidence interval for group B","Upper bound of confidence interval for group A","Upper bound of confidence interval for group B"),
col=c("red","blue","red","blue"),lty=c(2,2,3,3),cex=0.63) #affixes legends to the top-right corner of the third plot
Quantity2=summary(KM.surv2,time=1.9)$surv #calculates quantity 2



######### Code to evaluate Quantity3 and Quantity4 #############

library(survival) # loads package survival 

Group=Group[order(Time)] #arranges entries in vector according to permutation of entries in Time that corresponds to an ascending order 
Censor=Censor[order(Time)] #arranges entries in vector according to permutation of entries in Time that corresponds to an ascending order 
Time=sort(Time) #sorts entries in Time in ascending order 
Cox.fit <- coxph(Surv(Time, Censor) ~ Group) #assigns cox model to Cox.fit
Quantity3=summary(Cox.fit)$coefficients[1,1] #assigns MPLE to Quantity3 



list=list(Time=Time,Group=Group,Censor=Censor) #concatenates all 3 vectors into a list 



Partiallog=function(b) #partiallogliklihood function
{
p=exp(b)
sum=numeric(100) #vector for storing value of each term 

for(i in 1:100) #for loop for computing value of each term 
{
if(list$Censor[i]==1) #if censored 
{
if(list$Group[i]==2) #if censored and from group B
{
sum[i]=b #a=1
}
freq=as.data.frame(table(list$Group[i:100]))

sum[i]=sum[i]-log(freq[1,2]+(freq[2,2]*p)) 
}
else
{sum[i]=0} 
}

output=sum(sum)
return(output)
}

Score=function(b) #score function
{
p=exp(b)
sum=numeric(100) #vector for storing value of each term 

for(i in 1:100) #for loop for computing value of each term 
{
if(list$Censor[i]==1) #if censored 
{
if(list$Group[i]==2)  #if censored and from group B
{
sum[i]=1 #a=1
}
freq=as.data.frame(table(list$Group[i:100]))

sum[i]=sum[i]-((freq[2,2]*p)/(freq[1,2]+(freq[2,2]*p)))
}
else
{sum[i]=0}
}

output=sum(sum)
return(output)
}

Quantity4=Score #assigns score function to Quantity4

par(mfcol=c(1,2)) #merges 2 plots side by side 

y=numeric(1)
for(j in 1:length(seq(-0.3835,-0.383,0.0001)))  #transfers outputs of function into y
{
y[j]=Partiallog(seq(-0.3835,-0.383,0.0001)[j])
}

plot(seq(-0.3835,-0.383,0.0001),y,type="l",xlab="beta",ylab="Partialloglikelihood", main="Partiallog vs beta") #code for plotting partialloglikelihood against beta 


y=numeric(1)
for(j in 1:length(seq(-0.3835,-0.383,0.0001)))   #transfers outputs of function into y
{
y[j]=Score(seq(-0.3835,-0.383,0.0001)[j])
}

plot(seq(-0.3835,-0.383,0.0001),y,type="l",xlab="beta",ylab="Score function",main="Score function vs beta") #code for plotting score function against beta 


Quantity5=Cox.fit$wald.test #assigns test statistic of wald test to Quantity5
Quantity6=summary(Cox.fit)$logtest[1] #assigns test statistic of likelihood ratio test to Quantity6
Quantity7=Cox.fit$score #assigns test statistic of score test to Quantity 7
Quantity8=summary(Cox.fit)$logtest[3] #assigns p value of LRT to Quantity 8




