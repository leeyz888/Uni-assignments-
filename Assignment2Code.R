#
#     R-code to generate data for Assignment.  This code MUST be
#     placed at the start of your own R-script.  You must edit
#     the argument to set.seed( ) function to fit your own
#     registration number
#
RNGkind(sample.kind = "Rejection") 
set.seed(9251)    ##### EDIT TO THE LAST 4 DIGITS OF YOUR STUDENT ID ######
#
#     Read in CMI data and load R-funcions
#
#     NB: The files CMI_read.r, the CMI data files and R-function file
#         Test_GoF.r must be in the appropriate directory
#
source("CMI_read.r")
source("Test_GoF.r")
Year_choice <- Year[16:36]
My_Year <- sample(Year_choice, 1); My_Year  ## Year for assigmnent
AGE <- Age[Age > 39]

## Data for Part 1

DTH_MY <- Dth[Age > 39, Year == My_Year]
EXP_MY_c <- Exp[Age > 39, Year == My_Year]
EXP_MY <- EXP_MY_c + 0.5*DTH_MY

## Data for Part 2

DTH <- Dth[ Age>=40 & Age <= 90 , Year>=1950 & Year <=My_Year ]
EXP_c <- Exp[ Age>=40 & Age <= 90 , Year>=1950 & Year <=My_Year ]
EXP <- EXP_c + 0.5*DTH

######### Solutions below here  ###########

######## Code for part 1 ##########

####### Task 1 ##########
Logit <- function(x){log(x/(1-x))} # define logit function for use later
Q.x=DTH_MY/EXP_MY # estimates qx
Obs=Logit(Q.x) # calculates logit(qx)
mod1 <- glm(Q.x ~ AGE, weights = EXP_MY, family = binomial) # fits binomial GLM to data
Quantity1a=as.numeric(mod1$coefficients)[1] # assigns value of intercept to quantity 1a
Quantity1b=as.numeric(mod1$coefficients)[2] # assigns value of slope to quantity 1b

######### Task 2 ##########
 
sum((resid(mod1,type="pear"))^2) # calculates sum of residuals squared 
var=(mod1$fit*(1-mod1$fit))/EXP_MY # calculates variance
Quantity2a=sum(((DTH_MY/EXP_MY)-mod1$fit)^2/var) # assigns sum of Pearson residuals to quantity 2a
Quantity2b=Chi.Square(resid(mod1,type="pear"),2)$Sig.Pr # assigns p-value to quantity 2b
Quantity3a=Standard.Area(resid(mod1,type="pear"),4)$Chis2 #assigns test statistic to quantity 3a
Quantity3b=Standard.Area(resid(mod1,type="pear"),4)$Sig.Pr # assigns p value to quantity 3b
Quantity4a=Sign(resid(mod1,type="pear"))$N.plus # assigns test statistic to quantity 4a
Quantity4b=Sign(resid(mod1,type="pear"))$Sig.Prob # assigns p value to quantity 4b
Quantity5a=Change.Sign(resid(mod1,type="pear"))$Change # assigns test statistic to quantity 5a
Quantity5b=Change.Sign(resid(mod1,type="pear"))$Sig.Pr # assigns p value to quantity 5b
Quantity6a=Runs.test(resid(mod1,type="pear"))$g # assigns test statistic to quantity 6a
Quantity6b=Runs.test(resid(mod1,type="pear"))$Sig.Prob # assigns p value to quantity 6b
Quantity7a=Serial(resid(mod1,type="pear"))$Serial # assigns test statistic to quantity 7a
Quantity7b=Serial(resid(mod1,type="pear"))$Sig.Pr # assigns p value to quantity 7b


mod1$coefficients # displays coefficients of gompertz glm


par(mfcol=c(1,2)) # merges 2 plots into 1 window
plot(AGE,resid(mod1,type="pear"),ylab="Standardised residuals",xlab="Age",main="Pearson residuals vs age") # plots residual plots 
abline(a=0,b=0)
plot(AGE,Obs,xlab="Age",ylab="Logit(qx)",main="Logit(qx) vs age")
lines(AGE,mod1$lin)
######### Code for part 2 ########

######### Task 1 ############

library("gnm") # loads library
DTH.v=c(DTH) # defines quantities
EXP_c.v=c(EXP_c)
EXP.v=c(EXP)
qx=c(DTH/EXP)
AGE=40:90
YEAR=1950:My_Year 
Age.F=factor(rep(AGE,ncol(DTH)))
Year.F=factor(rep(YEAR,each=nrow(DTH)))
Obs=Logit(DTH/EXP)
Obs.2005=Dth[Age==65,Year>My_Year&Year<=My_Year+20]/((0.5*Dth[Age==65,Year>My_Year&Year<=My_Year+20])+(Exp[Age==65,Year>My_Year&Year<=My_Year+20]))
Act=Logit(Obs.2005)

LC.Model <-gnm( qx ~  -1+Age.F + Mult(Age.F,Year.F) , weights=EXP.v , family=binomial) # creates lee carter model

Alpha.gnm=LC.Model$coefficients[1:51] # derives coefficients of lee carter model
Beta.gnm <- LC.Model$coefficients[52:102]
Kappa.gnm <- LC.Model$coefficients[103:(My_Year-1950+103)]
n.y=length(1950:My_Year)

Kappa.m <- mean(Kappa.gnm) # derives coefficients of lee carter model which satisfy indenficability constraints
Beta.m <- mean(Beta.gnm)
Alpha.hat <- Alpha.gnm + Kappa.m * Beta.gnm
Beta.hat <- Beta.gnm / (nrow(DTH) * Beta.m)
Kappa.hat <- nrow(DTH) * Beta.m * (Kappa.gnm - Kappa.m)
sum(Kappa.hat); sum(Beta.hat) # check to see if constraints are satisfied

Fitted.M.hat = Alpha.hat + Beta.hat %*% t(Kappa.hat) # derives fitted values of logit(qxy)

layout(matrix(c(1,2,3,4),2,2,byrow=TRUE)) #plots graphs on lee carter model 
plot(AGE,Alpha.hat,xlab="Age",ylab="Values of alpha",main="alpha vs age")
plot(AGE,Beta.hat,xlab="Age",ylab="Values of beta",main="beta vs age")
plot(YEAR,Kappa.hat,xlab="Year",ylab="Values of kappa",main="kappa vs year")
plot(YEAR,Obs[26,],xlab="Year",ylab="logit(q(65,y))",main="logit(q(65,y)) vs year")
lines(YEAR,Fitted.M.hat[26,])
legend("topright",legend=c("Est.Values","Fitted"), lty=c(NA,1),pch=c(1,NA),cex=0.9)
######### Task 2 ###########
Quantity8=Alpha.hat # assigns coefficients of lee carter model to quantities 8 9 and 10
Quantity9=Beta.hat
Quantity10=Kappa.hat
N.Ahead=20 # defines no of years to project mortalities 
library("astsa") # loads asta lobrary 
Kappa.for = sarima.for(Kappa.hat, n.ahead = N.Ahead, p=0, d=1, q=0) # projects future kappa values 
z=qnorm(0.975) # assigns 97.5% quantile to z
Quantity11=Kappa.for$pred # assigns predicted kappa values to quantity 11
Sarima.out=sarima(Kappa.hat, p=0, d=1, q=0,details=FALSE)
SE.Param = (1:20) * sqrt(Sarima.out$fit$sigma2/(n.y-1)) # derives SE of delta's estimate 
Kappa.Lower=-(z*(((Kappa.for$se)^2+(SE.Param)^2)^0.5))+Quantity11 # Lower bound of 95% CI
Kappa.Upper=(z*(((Kappa.for$se)^2+(SE.Param)^2)^0.5))+Quantity11 # Upper bound of 95% CI
invlogit=function(x){x/(1+x)} # defines invlogit function 

ax=as.numeric(Alpha.hat[26]) # assigns a_65 to ax
bx=as.numeric(Beta.hat[26]) #assigns b_65 to bx
ky=as.numeric(Kappa.hat[My_Year-1950+1]) # assigns k_1985 to ky

y=ax+(bx*ky) # 
Quantity12a=log((invlogit(exp(y)))/(1-(invlogit(exp(y)))*(0.5))) # assigns fitted value of log (mu) to quantity12a
Kappa.for = sarima.for(Kappa.hat, n.ahead = 20, p=0, d=1, q=0) # projects future kappa values 
Central=Kappa.for$pred # assigns predicted values to central 
Forecast = Alpha.hat + Beta.hat %*% t(Central) # projects future mortalities 
Forecast.Up= Alpha.hat + Beta.hat %*% t(Kappa.Upper) # derives 95% CI
Forecast.Down= Alpha.hat + Beta.hat %*% t(Kappa.Lower)
Sarima.out=sarima(Kappa.hat, p=0, d=1, q=0,details=FALSE)
RANGE=(My_Year+1):(My_Year+20) #Code to plot graph of log(q(65,y)) against year
plot.row=26
plot(YEAR,Obs[plot.row,], axes = FALSE, xlab = "Year", ylab = "logit(q(65,y))",xlim=c(1950,My_Year+20)
,ylim=c(min(Act), max(Obs[plot.row, ]))
,main="Plot of logit(q(65,y)) vs year",pch=16,col="blue")
axis(1,las = 1, at = seq(1950, 2010, by = 5), tcl = -0.4)
axis(2, seq(-5,-3, by = 0.25), tcl = -0.4)
lines(RANGE, Forecast[plot.row, ], lwd = 2)
lines(RANGE, Forecast.Up[plot.row, ], lwd = 2,col="red",lty=2)
lines(RANGE, Forecast.Down[plot.row, ], lwd = 2,col="red",lty=2)
points(RANGE,Act,col="#ff1493",pch=3)
legend("bottomleft", legend = c("Observed", "Actual","Central forecast", "95% CI acounting for both parameter and stochastic error"),
       lty = c(-1,-1, 1, 2), pch = c(16,3,-1,-1), lwd = 2, bty = "n",col=c("blue","#ff1493","black","red")) 
Quantity12b=log((invlogit(exp(Forecast[26,20])))/(1-(invlogit(exp(Forecast[26,20])))*(0.5))) # assigns projected value of log (mu) to quantity12b

