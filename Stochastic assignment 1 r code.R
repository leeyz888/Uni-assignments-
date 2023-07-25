##########Code to determine average time taken for scheme 1 to achieve stability##########
n=10000 # no of customers 
v<-numeric(n) # vector to store discount levels of each customer 
lambda=0.871 # mean number of accidents 
freq=numeric(3) # vector to record the frequency of each discount level 
pi<-c((exp(0.871)-1)^2/(exp(1.742)-exp(0.871)+1),(exp(0.871)-1)/(exp(1.742)-exp(0.871)+1),1/(exp(1.742)-exp(0.871)+1))*10000 #stationary distribution multiplied by 100000 , essentially the expected frequency of each discount level 
time=0 # counting variable to count the number of time steps taken to reach stability
m=1000 # number of simulations
exptime=numeric(m) # vector to store time taken to stability from each simulation
boolean=numeric(3) #vector used to store logic values in order to track the difference between frequency and expected frequency 
for(a in 1:m) # for loop to simulate 1000 times 
{
while(sum(boolean)<3) # while each frequency is not sufficiently close to stationary distribution, update markov chain 
{
time=time+1 # for each iteration, increases time step taken by 1
for(i in 1:n) # for loop to update discount level of 10000 customers 
{
y<-rpois(1,lambda) #simulates no of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it 
{
v[i]<-min(v[i]+1,2) # if at max discount level, remain , else go up by 1
}
else
{
v[i]<-max(0,v[i]-1) # if at min discount level, remain, else decrease by 1
}
}
count<-as.data.frame(table(v))$Freq #stores frequency of each discount level into a data frame 
for(k in 1:length(count)) # for loop to transfer frequency in data frame into another vector 
{
freq[k]=count[k] #transfers kth frequency of data frame into kth entry of vector 
}
boolean<-abs(freq-pi)<30 #compares differences of each frequency with corresponding theoritical frequency 
}
exptime[a]=time # stores time taken into ath entry 
time=0 #resets counter 
freq=numeric(3) #resets vector
v=numeric(n) #resets vector
boolean=numeric(3) #resets vector 
}
time_scheme1=mean(exptime) #calculates average time taken and stores them in variable time_scheme1
##########Code to determine average time taken for scheme 2 to achieve stability##########
n=10000 # no of customers 
v<-numeric(n) # vector to store discount levels of each customer 
lambda=0.871 # mean number of accidents 
freq=numeric(4) # vector to record the frequency of each discount level 
p1=(exp(0.871)-1)^3/((exp(0.871))*((exp(0.871)-1)^2+1)) #expected proportion of first discount level
p2=(exp(0.871)-1)^2/((exp(0.871))*((exp(0.871)-1)^2+1)) #expected proportion of second discount level
p3=(exp(0.871)-1)/((exp(0.871))*((exp(0.871)-1)^2+1)) #expected proportion of third discount level
p4=1/((exp(0.871))*((exp(0.871)-1)^2+1)) #expected proportion of fourth discount level
pi<-c(p1,p2,p3,p4)*10000 #stores each expected proportion into vector before its multiplied by 10000 to derive expected frequency of each discount level 
time=0 # counting variable to count the number of time steps taken to reach stability
m=1000 # number of simulations
exptime=numeric(m) # vector to store time taken to stability from each simulation
boolean=numeric(4) #vector used to store logic values in order to track the difference between frequency and expected frequency 
for(a in 1:m) # for loop to simulate 1000 times 
{
while(sum(boolean)<4) # while each frequency are not sufficiently close to stationary distribution, update markov chain 
{
time=time+1 # for each iteration , increases time step taken by 1
for(i in 1:n) # for loop to update discount level of 10000 customers 
{
y<-rpois(1,lambda) #simulates no of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it 
{
v[i]<-min(v[i]+1,3) # if at max discount level, remain , else go up by 1
}
else
{
v[i]<-max(0,v[i]-1) # if at min discount level, remain, else decrease by 1
}
}
count<-as.data.frame(table(v))$Freq #stores frequency of each discount level into a data frame 
for(k in 1:length(count)) # for loop to transfer frequency in data frame into another vector 
{
freq[k]=count[k] #transfers kth frequency of data frame into kth entry of vector 
}
boolean<-abs(freq-pi)<30 #compares differences of each frequency with corresponding theoretical frequency 
}
exptime[a]=time # stores time taken into ath entry 
time=0 #resets counter 
freq=numeric(4) #resets vector
v=numeric(n) #resets vector
boolean=numeric(4) #resets vector 
}
time_scheme2=mean(exptime) #calculates average time taken and stores them in variable time_scheme2
###########Scheme 1 Country A##############
###########Code to stabalise markov chain (run this first)##################
n=10000 #no of customers 
v<-numeric(n) # vector to store discount levels of each customer 
lambda=0.871 # mean number of accidents
freq=numeric(3) # vector to record the frequency of each discount level 
pi<-c((exp(0.871)-1)^2/(exp(1.742)-exp(0.871)+1),(exp(0.871)-1)/(exp(1.742)-exp(0.871)+1),1/(exp(1.742)-exp(0.871)+1))*10000 #stationary distribution multiplied by 100000 , essentially the expected frequency 
boolean=numeric(3) #vector used to store logic values in order to track the difference between frequency and expected frequency 
while(sum(boolean)<3) # while each frequency are not sufficiently close to stationary distribution, update markov chain 
{
for(i in 1:n) #for loop to simulate discount level of each customer 
{
y<-rpois(1,lambda) # simulates no of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it 
{
v[i]<-min(v[i]+1,2) # if at max discount level, remain , else go up by 1
}
else
{
v[i]<-max(0,v[i]-1) # if at min discount level, remain, else decrease by 1
}
}
count<-as.data.frame(table(v))$Freq #stores frequency of each discount level into a data frame 
for(k in 1:length(count)) # for loop to transfer frequency in data frame into another vector 
{
freq[k]=count[k] #transfers kth frequency of data frame into kth entry of vector 
}
boolean<-abs(freq-pi)<30 #compares differences of each frequency with corresponding theoritical frequency and stores result as true false values into vector boolean 
}
########## Code to determine annual profit once stability is established (run this only after above code is ran)###############
s=1000 #number of simulations 
m=15 #number of years which to simulate profit 
n=10000 # no of customers 
lambda=0.871 # mean number of accidents
d<-c(0,0.15,0.3) #vector containing each discount level 
base_pre=200 #amount of base premium 
profit<-numeric(n) #vector to store simulated profits of each customer
exprofit1<-numeric(m) # vector to store aggregate profit of all customers 
sumexprofit=numeric(s) # vector to store the total profit gained in 15 years upon stabilization of Markov Chain 
for(i in 1:s) # for loop to simulate total profit earned in 10 years for 1000 times 
{
for(k in 1:m) #for loop to repeat each simulation by m times so as to simulate profit gained for m years 
{
for(j in 1:n) #for loop to update discount levels and to simulate profits of each customer 
{
y<-rpois(1,lambda) # simulates no of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it 
{
profit[j]<-((base_pre)*(1-d[v[j]+1]))-(sum(runif(y,0,400))) #evaluates profit derived from the jth customer and stores into jth entry of vector
v[j]<-min(v[j]+1,2) # if at max discount level, remain , else go up by 1
}
else
{
profit[j]<-((base_pre)*(1-d[v[j]+1]))-(sum(runif(y,0,400))) #evaluates profit derived from the jth customer and stores into jth entry of vector
v[j]<-max(0,v[j]-1) # if at min discount level, remain, else decrease by 1
}
}
exprofit1[k]<-sum(profit) #aggregrates profit gained from all customers before storing it into kth entry 
profit<-numeric(n) #resets vector 
}
sumexprofit[i]=sum(exprofit1) #stores total profit gained over 15 years as the ith entry 
exprofit1=numeric(m) # resets vector 
}
annualprofit_scheme1_countryA=mean(sumexprofit)/15 # derives average annual profit earned over 15 years by taking the mean of the total profits gained over 15 years prior to dividing it by 15 . 
################Code to calculate profitability in early years#################
s=1000 #no of simulations 
m=10 # no of years to simulate the profit
n=10000 #population size 
lambda=0.871 # mean number of accidents
Base_Pre=200 #amount of base premium 
d<-c(0,0.15,0.3) #vector containing each discount level
level=matrix(0,nrow=n,ncol=m+1) #matrix to store discount levels in each of the m years of each customer 
Profit=matrix(0,nrow=n,ncol=m) #matrix to store profit gained in each of the m years of each customers 
TotalProfit1=numeric(s) # vector to store total profit derived within 10 years from all 10000 customers 
for(k in 1:s) #for loop to iterate simulation for 1000 times 
{
for(j in 1:n) #for loop to simulate profit of each customer 
{
for(i in 1:m) # for loop to simulate profit derived in each year for each customer 
{
y<-rpois(1,lambda) # simulates no of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it 
{
Profit[j,i]=Base_Pre*(1-d[level[j,i]+1])-(sum(runif(y,0,400))) #evaluates profit derived from the jth customer and stores into jth entry of vector
level[j,i+1]=min(level[j,i]+1,2) # if at max discount level, remain , else go up by 1
}
else
{
Profit[j,i]=Base_Pre*(1-d[level[j,i]+1])-(sum(runif(y,0,400))) #evaluates profit derived from the jth customer and stores into jth entry of vector
level[j,i+1]=max(level[j,i]-1,0) # if at min discount level, remain, else decrease by 1
}
}
}
TotalProfit1[k]=sum(Profit) #aggregrates profit earned over m years from all customers and stores into kth entry 
level=matrix(0,nrow=n,ncol=m+1) #resets matrix 
Profit=matrix(0,nrow=n,ncol=m) #resets matrix 
}
profit_scheme1_countryA=mean(TotalProfit1) # takes average to derive mean total profit earned in 10 years and stores them in variable 
####### Code to simulate probability of ruin###############
n=10000 #number of customers 
lambda=0.871 # mean number of accidents
d<-c(0,0.15,0.3) #vector containing each discount level
base_pre=200 #amount of base premium
profit<-numeric(n) #vector to store simulated profits of each customer
Capital=1000000 #initial capital 
b=3000000 #upper bound of the values which the capital can take
v<-numeric(n) #vector to store discount levels of each customer 
capital<-numeric(1000) #vector to store capital derived from each simulation 
for(r in 1:1000) # for loop to iterate simulation 1000 times 
{
while(Capital>0 & Capital<b) # while capital>0 and <b, reiterate simulation of profit and update capital 
{
for(j in 1:n) #for loop to simulate profit and discount level for 10000 customers
{
y<-rpois(1,lambda) #simulates no. of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it
{
profit[j]<-((base_pre)*(1-d[v[j]+1]))-(sum(runif(y,0,400))) # simulates profit for jth customer 
v[j]<-min(v[j]+1,2) # if at max discount level, remain , else go up by 1
}
else
{
profit[j]<-((base_pre)*(1-d[v[j]+1]))-(sum(runif(y,0,400))) # simulates profit for jth customer 
v[j]<-max(0,v[j]-1) #if at min discount level, remain , else go up by 1
}
}
Capital<-Capital+sum(profit) # updates capital 
profit=numeric(n) #resets vector 
}
capital[r]<-Capital #stores final value of capital in rth entry
Capital=1000000 # reinitialises capital 
v=numeric(n) #resets vector 
}
Boolean<-capital<0 #tests if each entry of vector<0 
probability_scheme1_countryA<-sum(Boolean)/length(Boolean) #calculates probability of bankruptcy
###########Scheme 1 Country B##############
###########Code to stabalise markov chain (run this first)##################
n=10000 #no of customers 
v<-numeric(n) # vector to store discount levels of each customer 
lambda=0.871 # mean number of accidents
freq=numeric(3) # vector to record the frequency of each discount level 
pi<-c((exp(0.871)-1)^2/(exp(1.742)-exp(0.871)+1),(exp(0.871)-1)/(exp(1.742)-
exp(0.871)+1),1/(exp(1.742)-exp(0.871)+1))*10000 #stationary distribution multiplied by 100000 , essentially the expected frequency 
boolean=numeric(3) #vector used to store logic values in order to track the difference between frequency and expected frequency 
while(sum(boolean)<3) # while each frequency are not sufficiently close to stationary distribution, update markov chain 
{
for(i in 1:n) #for loop to simulate discount level of each customer 
{
y<-rpois(1,lambda) # simulates no of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it 
{
v[i]<-min(v[i]+1,2) # if at max discount level, remain , else go up by 1
}
else
{
v[i]<-max(0,v[i]-1) # if at min discount level, remain, else decrease by 1
}
}
count<-as.data.frame(table(v))$Freq #stores frequency of each discount level into a data frame 
for(k in 1:length(count)) # for loop to transfer frequency in data frame into another vector 
{
freq[k]=count[k] #transfers kth frequency of data frame into kth entry of vector 
}
boolean<-abs(freq-pi)<30 #compares differences of each frequency with corresponding theoritical frequency and stores result as true false values into vector boolean 
}
########## Code to determine annual profit once stability is established (run this only after above code is ran)###############
s=1000 #number of simulations 
m=15 #number of years which to simulate profit 
n=10000 # no of customers 
lambda=0.871 # mean number of accidents
d<-c(0,0.15,0.3) #vector containing each discount level 
base_pre=300 #amount of base premium 
profit<-numeric(n) #vector to store simulated profits of each customer
exprofit2<-numeric(m) # vector to store aggregate profit of all customers 
sumexprofit=numeric(s) # vector to store the total profit gained in 15 years upon stabilization of Markov Chain 
for(i in 1:s) # for loop to simulate total profit earned in 10 years for 1000 times 
{
for(k in 1:m) #for loop to repeat each simulation by m times so as to simulate profit gained for m years 
{
for(j in 1:n) #for loop to update discount levels and to simulate profits of each customer 
{
y<-rpois(1,lambda) # simulates no of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it 
{
profit[j]<-((base_pre)*(1-d[v[j]+1]))-(sum(runif(y,0,600))) #evaluates profit derived from the jth customer and stores into jth entry of vector
v[j]<-min(v[j]+1,2) # if at max discount level, remain , else go up by 1
}
else
{
profit[j]<-((base_pre)*(1-d[v[j]+1]))-(sum(runif(y,0,600))) #evaluates profit derived from the jth customer and stores into jth entry of vector
v[j]<-max(0,v[j]-1) # if at min discount level, remain, else decrease by 1
}
}
exprofit2[k]<-sum(profit) #aggregrates profit gained from all customers before storing it into kth entry 
profit<-numeric(n) #resets vector 
}
sumexprofit[i]=sum(exprofit2) #stores total profit gained over 15 years as the ith entry 
exprofit2=numeric(m) # resets vector 
}
annualprofit_scheme1_countryB=mean(sumexprofit)/15 # derives average annual profit earned over 15 years by taking the mean of the total profits gained over 15 years prior to dividing it by 15 . 
################Code to calculate profitability in early years#################
s=1000 #no of simulations 
m=10 # no of years to simulate the profit
n=10000 #population size 
lambda=0.871 # mean number of accidents
Base_Pre=300 #amount of base premium 
d<-c(0,0.15,0.3) #vector containing each discount level
level=matrix(0,nrow=n,ncol=m+1) #matrix to store discount levels in each of the m years of each customer 
Profit=matrix(0,nrow=n,ncol=m) #matrix to store profit gained in each of the m years of each customers 
TotalProfit2=numeric(s) # vector to store total profit derived within 10 years from all 10000 customers 
for(k in 1:s) #for loop to iterate simulation for 1000 times 
{
for(j in 1:n) #for loop to simulate profit of each customer 
{
for(i in 1:m) # for loop to simulate profit derived in each year for each customer 
{
y<-rpois(1,lambda) # simulates no of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it 
{
Profit[j,i]=Base_Pre*(1-d[level[j,i]+1])-(sum(runif(y,0,600))) #evaluates profit derived from the jth customer and stores into jth entry of vector
level[j,i+1]=min(level[j,i]+1,2) # if at max discount level, remain , else go up by 1
}
else
{
Profit[j,i]=Base_Pre*(1-d[level[j,i]+1])-(sum(runif(y,0,600))) #evaluates profit derived from the jth customer and stores into jth entry of vector
level[j,i+1]=max(level[j,i]-1,0) # if at min discount level, remain, else decrease by 1
}
}
}
TotalProfit2[k]=sum(Profit) #aggregrates profit earned over m years from all customers and stores into kth entry 
level=matrix(0,nrow=n,ncol=m+1) #resets matrix 
Profit=matrix(0,nrow=n,ncol=m) #resets matrix 
}
profit_scheme1_countryB=mean(TotalProfit2) # takes average to derive mean total profit earned in 10 years and stores them in variable 
####### Code to simulate probability of ruin###############
n=10000 #number of customers 
lambda=0.871 # mean number of accidents
d<-c(0,0.15,0.3) #vector containing each discount level
base_pre=300 #amount of base premium
profit<-numeric(n) #vector to store simulated profits of each customer
Capital=1000000 #initial capital 
b=3000000 #upper bound of the values which the capital can take
v<-numeric(n) #vector to store discount levels of each customer 
capital<-numeric(1000) #vector to store capital derived from each simulation 
for(r in 1:1000) # for loop to iterate simulation 1000 times 
{
while(Capital>0 & Capital<b) # while capital>0 and <b, reiterate simulation of profit and update capital 
{
for(j in 1:n) #for loop to simulate profit and discount level for 10000 customers
{
y<-rpois(1,lambda) #simulates no. of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it 
{
profit[j]<-((base_pre)*(1-d[v[j]+1]))-(sum(runif(y,0,600))) # simulates profit for jth customer 
v[j]<-min(v[j]+1,2) # if at max discount level, remain , else go up by 1
}
else
{
profit[j]<-((base_pre)*(1-d[v[j]+1]))-(sum(runif(y,0,600))) # simulates profit for jth customer 
v[j]<-max(0,v[j]-1) #if at min discount level, remain , else go up by 1
}
}
Capital<-Capital+sum(profit) # updates capital 
profit=numeric(n) #resets vector 
}
capital[r]<-Capital #stores final value of capital in rth entry
Capital=1000000 # reinitialises capital 
v=numeric(n) #resets vector 
}
Boolean<-capital<0 #tests if each entry of vector<0 
probability_scheme1_countryB<-sum(Boolean)/length(Boolean) #calculates probability of bankruptcy
###########Scheme 2 Country A##############
###########Code to stabalise markov chain (run this first)##################
n=10000 #no of customers 
v<-numeric(n) # vector to store discount levels of each customer 
lambda=0.871 # mean number of accidents
freq=numeric(4) # vector to record the frequency of each discount level 
p1=(exp(0.871)-1)^3/((exp(0.871))*((exp(0.871)-1)^2+1)) # expected proportion of first discount level
p2=(exp(0.871)-1)^2/((exp(0.871))*((exp(0.871)-1)^2+1)) # expected proportion of second discount level
p3=(exp(0.871)-1)/((exp(0.871))*((exp(0.871)-1)^2+1)) # expected proportion of third discount level
p4=1/((exp(0.871))*((exp(0.871)-1)^2+1)) # expected proportion of fourth discount level
pi<-c(p1,p2,p3,p4)*10000 #stores all expected proportions into variable before multiplying it by 10000 to derive expected frequency 
boolean=numeric(4) #vector used to store logic values in order to track the difference between frequency and expected frequency 
while(sum(boolean)<4) # while each frequency is not sufficiently close to stationary distribution, update markov chain 
{
for(i in 1:n) #for loop to simulate discount levels for each of the 10000 customers 
{
y<-rpois(1,lambda) # simulates no of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it 
{
v[i]<-min(v[i]+1,3) # if at max discount level, remain , else go up by 1
}
else
{
v[i]<-max(0,v[i]-1) # if at min discount level, remain, else decrease by 1
}
}
count<-as.data.frame(table(v))$Freq #stores frequency of each discount level into a data frame 
for(k in 1:length(count)) # for loop to transfer frequency in data frame into another vector 
{
freq[k]=count[k] #transfers kth frequency of data frame into kth entry of vector 
}
boolean<-abs(freq-pi)<30 #compares differences of each frequency with corresponding theoritical frequency and stores result as true false values into vector boolean 
}
########## Code to determine annual profit once stability is established (run this only after above code is ran)###############
s=1000 #no. of simulations 
m=15 #number of years which to simulate profit 
n=10000 # no of customers 
lambda=0.871 # mean number of accidents
d<-c(0,0.1,0.2,0.3) #vector containing each discount level 
base_pre=200 #amount of base premium 
profit<-numeric(n) #vector to store simulated profits of each customer
exprofit3<-numeric(m) # vector to store aggregate profit of all customers 
sumexprofit=numeric(s) # vector to store the total profit gained in 15 years upon stabilization of Markov Chain 
for(i in 1:s) # for loop to simulate total profit earned in 10 years for 1000 times 
{
for(k in 1:m) #for loop to repeat each simulation by m times so as to simulate profit gained for m years 
{
for(j in 1:n) #for loop to update discount levels and to simulate profits of each customer 
{
y<-rpois(1,lambda) # simulates no of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it 
{
profit[j]<-((base_pre)*(1-d[v[j]+1]))-(sum(runif(y,0,400))) #evaluates profit derived from the jth customer and stores into jth entry of vector
v[j]<-min(v[j]+1,3) # if at max discount level, remain , else go up by 1
}
else
{
profit[j]<-((base_pre)*(1-d[v[j]+1]))-(sum(runif(y,0,400))) #evaluates profit derived from the jth customer and stores into jth entry of vector
v[j]<-max(0,v[j]-1) # if at min discount level, remain, else decrease by 1
}
}
exprofit3[k]<-sum(profit) #aggregrates profit gained from all customers before storing it into kth entry 
profit<-numeric(n) #resets vector 
}
sumexprofit[i]=sum(exprofit3) #stores total profit gained over 15 years as the ith entry
exprofit3=numeric(m) # resets vector
}
annualprofit_scheme2_countryA=mean(sumexprofit)/15 # derives average annual profit earned over 15 years by taking the mean of the total profits gained over 15 years prior to dividing it by 15 .
################Code to calculate profitability in early years#################
s=1000 #no of simulations 
m=10 # no of years to simulate the profit
n=10000 #population size 
lambda=0.871 # mean number of accidents
Base_Pre=200 #amount of base premium 
d<-c(0,0.1,0.2,0.3) #vector containing each discount level
level=matrix(0,nrow=n,ncol=m+1) #matrix to store discount levels in each of the m years of each customer 
Profit=matrix(0,nrow=n,ncol=m) #matrix to store profit gained in each of the m years of each customers 
TotalProfit3=numeric(s) # vector to store total profit derived within 10 years from all 10000 customers 
for(k in 1:s) #for loop to iterate simulation for 1000 times 
{
for(j in 1:n) #for loop to simulate profit of each customer 
{
for(i in 1:m) # for loop to simulate profit derived in each year for each customer 
{
y<-rpois(1,lambda) # simulates no of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it 
{
Profit[j,i]=Base_Pre*(1-d[level[j,i]+1])-(sum(runif(y,0,400))) #evaluates profit derived from the jth customer and stores into jth entry of vector
level[j,i+1]=min(level[j,i]+1,3) # if at max discount level, remain , else go up by 1
}
else
{
Profit[j,i]=Base_Pre*(1-d[level[j,i]+1])-(sum(runif(y,0,400))) #evaluates profit derived from the jth customer and stores into jth entry of vector
level[j,i+1]=max(level[j,i]-1,0) # if at min discount level, remain, else decrease by 1
}
}
}
TotalProfit3[k]=sum(Profit) #aggregrates profit earned over m years from all customers and stores into kth entry 
level=matrix(0,nrow=n,ncol=m+1) #resets matrix 
Profit=matrix(0,nrow=n,ncol=m) #resets matrix 
}
profit_scheme2_countryA=mean(TotalProfit3) # takes average to derive mean total profit earned in 10 years and stores them in variable 
####### Code to simulate probability of ruin###############
n=10000 #number of customers 
lambda=0.871 # mean number of accidents
d<-c(0,0.1,0.2,0.3) #vector containing each discount level
base_pre=200 #amount of base premium
profit<-numeric(n) #vector to store simulated profits of each customer
Capital=1000000 #initial capital 
b=3000000 #upper bound of the values which the capital can take
v<-numeric(n) #vector to store discount levels of each customer 
capital<-numeric(1000) #vector to store capital derived from each simulation 
for(r in 1:1000) # for loop to iterate simulation 1000 times
{
while(Capital>0 & Capital<b) # while capital>0 and <b, reiterate simulation of profit and update capital 
{
for(j in 1:n) #for loop to simulate profit and discount level for 10000 customers
{
y<-rpois(1,lambda) #simulates no. of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it
{
profit[j]<-((base_pre)*(1-d[v[j]+1]))-(sum(runif(y,0,400))) #evaluates profit derived from the jth customer and stores into jth entry of vector
v[j]<-min(v[j]+1,3) # if at max discount level, remain , else go up by 1
}
else
{
profit[j]<-((base_pre)*(1-d[v[j]+1]))-(sum(runif(y,0,400))) #evaluates profit derived from the jth customer and stores into jth entry of vector
v[j]<-max(0,v[j]-1) # if at min discount level, remain , else go down by 1
}
}
Capital<-Capital+sum(profit) # updates capital 
profit=numeric(n) #resets vector 
}
capital[r]<-Capital #stores final value of capital in rth entry
Capital=1000000 # reinitialises capital
v=numeric(n) #resets vector 
}
Boolean<-capital<0 #tests if each entry of vector<0
probability_scheme2_countryA<-sum(Boolean)/length(Boolean) #calculates probability of bankruptcy
###########Scheme 2 Country B##############
###########Code to stabalise markov chain (run this first)##################
n=10000 #no of customers 
v<-numeric(n) # vector to store discount levels of each customer 
lambda=0.871 # mean number of accidents
freq=numeric(4) # vector to record the frequency of each discount level 
p1=(exp(0.871)-1)^3/((exp(0.871))*((exp(0.871)-1)^2+1)) # expected proportion of first discount level
p2=(exp(0.871)-1)^2/((exp(0.871))*((exp(0.871)-1)^2+1)) # expected proportion of second discount level
p3=(exp(0.871)-1)/((exp(0.871))*((exp(0.871)-1)^2+1)) # expected proportion of third discount level
p4=1/((exp(0.871))*((exp(0.871)-1)^2+1)) # expected proportion of fourth discount level
pi<-c(p1,p2,p3,p4)*10000 #stores all expected proportions into variable before multiplying it by 10000 to derive expected frequency 
boolean=numeric(4) #vector used to store logic values in order to track the difference between frequency and expected frequency 
while(sum(boolean)<4) # while each frequency is not sufficiently close to stationary distribution, update markov chain 
{
for(i in 1:n) #for loop to simulate discount levels for each of the 10000 customers 
{
y<-rpois(1,lambda) # simulates no of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it 
{
v[i]<-min(v[i]+1,3) # if at max discount level, remain , else go up by 1
}
else
{
v[i]<-max(0,v[i]-1) # if at min discount level, remain, else decrease by 1
}
}
count<-as.data.frame(table(v))$Freq #stores frequency of each discount level into a data frame 
for(k in 1:length(count)) # for loop to transfer frequency in data frame into another vector 
{
freq[k]=count[k] #transfers kth frequency of data frame into kth entry of vector 
}
boolean<-abs(freq-pi)<30 #compares differences of each frequency with corresponding theoritical frequency and stores result as true false values into vector boolean 
}
########## Code to determine annual profit once stability is established (run this only after above code is ran)###############
s=1000 #no. of simulations 
m=15 #number of years which to simulate profit 
n=10000 # no of customers 
lambda=0.871 # mean number of accidents
d<-c(0,0.1,0.2,0.3) #vector containing each discount level 
base_pre=300 #amount of base premium 
profit<-numeric(n) #vector to store simulated profits of each customer
exprofit4<-numeric(m) # vector to store aggregate profit of all customers 
sumexprofit=numeric(s) # vector to store the total profit gained in 15 years upon stabilization of Markov Chain 
for(i in 1:s) # for loop to simulate total profit earned in 10 years for 1000 times 
{
for(k in 1:m) #for loop to repeat each simulation by m times so as to simulate profit gained for m years 
{
for(j in 1:n) #for loop to update discount levels and to simulate profits of each customer 
{
y<-rpois(1,lambda) # simulates no of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it 
{
profit[j]<-((base_pre)*(1-d[v[j]+1]))-(sum(runif(y,0,600))) #evaluates profit derived from the jth customer and stores into jth entry of vector
v[j]<-min(v[j]+1,3) # if at max discount level, remain , else go up by 1
}
else
{
profit[j]<-((base_pre)*(1-d[v[j]+1]))-(sum(runif(y,0,600))) #evaluates profit derived from the jth customer and stores into jth entry of vector
v[j]<-max(0,v[j]-1) # if at min discount level, remain, else decrease by 1
}
}
exprofit4[k]<-sum(profit) #aggregrates profit gained from all customers before storing it into kth entry 
profit<-numeric(n) #resets vector 
}
sumexprofit[i]=sum(exprofit4) #stores total profit gained over 15 years as the ith entry
exprofit4=numeric(m) # resets vector
}
annualprofit_scheme2_countryB=mean(sumexprofit)/15 # derives average annual profit earned over 15 years by taking the mean of the total profits gained over 15 years prior to dividing it by 15 .
################Code to calculate profitability in early years#################
s=1000 #no of simulations 
m=10 # no of years to simulate the profit
n=10000 #population size 
lambda=0.871 # mean number of accidents
Base_Pre=300 #amount of base premium 
d<-c(0,0.1,0.2,0.3) #vector containing each discount level
level=matrix(0,nrow=n,ncol=m+1) #matrix to store discount levels in each of the m years of each customer 
Profit=matrix(0,nrow=n,ncol=m) #matrix to store profit gained in each of the m years of each customers 
TotalProfit4=numeric(s) # vector to store total profit derived within 10 years from all 10000 customers 
for(k in 1:s) #for loop to iterate simulation for 1000 times 
{
for(j in 1:n) #for loop to simulate profit of each customer 
{
for(i in 1:m) # for loop to simulate profit derived in each year for each customer 
{
y<-rpois(1,lambda) # simulates no of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it 
{
Profit[j,i]=Base_Pre*(1-d[level[j,i]+1])-(sum(runif(y,0,600))) #evaluates profit derived from the jth customer and stores into jth entry of vector
level[j,i+1]=min(level[j,i]+1,3) # if at max discount level, remain , else go up by 1
}
else
{
Profit[j,i]=Base_Pre*(1-d[level[j,i]+1])-(sum(runif(y,0,600))) #evaluates profit derived from the jth customer and stores into jth entry of vector
level[j,i+1]=max(level[j,i]-1,0) # if at min discount level, remain, else decrease by 1
}
}
}
TotalProfit4[k]=sum(Profit) #aggregrates profit earned over m years from all customers and stores into kth entry 
level=matrix(0,nrow=n,ncol=m+1) #resets matrix 
Profit=matrix(0,nrow=n,ncol=m) #resets matrix 
}
profit_scheme2_countryB=mean(TotalProfit4) # takes average to derive mean total profit earned in 10 years and stores them in variable 
####### Code to simulate probability of ruin###############
n=10000 #number of customers 
lambda=0.871 # mean number of accidents
d<-c(0,0.1,0.2,0.3) #vector containing each discount level
base_pre=300 #amount of base premium
profit<-numeric(n) #vector to store simulated profits of each customer
Capital=1000000 #initial capital 
b=3000000 #upper bound of the values which the capital can take
v<-numeric(n) #vector to store discount levels of each customer 
capital<-numeric(1000) #vector to store capital derived from each simulation 
for(r in 1:1000) # for loop to iterate simulation 1000 times
{
while(Capital>0 & Capital<b) # while capital>0 and <b, reiterate simulation of profit and update capital 
{
for(j in 1:n) #for loop to simulate profit and discount level for 10000 customers
{
y<-rpois(1,lambda) #simulates no. of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it
{
profit[j]<-((base_pre)*(1-d[v[j]+1]))-(sum(runif(y,0,600))) #evaluates profit derived from the jth customer and stores into jth entry of vector
v[j]<-min(v[j]+1,3) # if at max discount level, remain , else go up by 1
}
else
{
profit[j]<-((base_pre)*(1-d[v[j]+1]))-(sum(runif(y,0,600))) #evaluates profit derived from the jth customer and stores into jth entry of vector
v[j]<-max(0,v[j]-1) # if at min discount level, remain , else go down by 1
}
}
Capital<-Capital+sum(profit) # updates capital 
profit=numeric(n) #resets vector 
}
capital[r]<-Capital #stores final value of capital in rth entry
Capital=1000000 # reinitialises capital
v=numeric(n) #resets vector 
}
Boolean<-capital<0 #tests if each entry of vector<0
probability_scheme2_countryB<-sum(Boolean)/length(Boolean) #calculates probability of bankruptcy
################Code to simulate variation in total profit in first 10 years for combination 1################# 
m=10 # no of years to simulate the profit
n=10000 #population size 
lambda=0.871 # mean number of accidents
Base_Pre=200 #amount of base premium 
d<-c(0,0.15,0.3) #vector containing each discount level
level=matrix(0,nrow=n,ncol=m+1) #matrix to store discount levels in each of the m years of each customer 
Profit=matrix(0,nrow=n,ncol=m) #matrix to store profit gained in each of the m years of each customers 
profit1=numeric(m) # vector to store total profit derived within 10 years from all 10000 customers 
for(k in 1:m) #for loop to iterate simulation for 1000 times 
{
for(j in 1:n) #for loop to simulate profit of each customer 
{
for(i in 1:m) # for loop to simulate profit derived in each year for each customer 
{
y<-rpois(1,lambda) # simulates no of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it 
{
Profit[j,i]=Base_Pre*(1-d[level[j,i]+1])-(sum(runif(y,0,400))) #evaluates profit derived from the jth customer and stores into jth entry of vector
level[j,i+1]=min(level[j,i]+1,2) # if at max discount level, remain , else go up by 1
}
else
{
Profit[j,i]=Base_Pre*(1-d[level[j,i]+1])-(sum(runif(y,0,400))) #evaluates profit derived from the jth customer and stores into jth entry of vector
level[j,i+1]=max(level[j,i]-1,0) # if at min discount level, remain, else decrease by 1
}
}
}
profit1[k]=sum(Profit[,k]) #aggregrates profit earned in mth year from all customers and stores into kth entry 
level=matrix(0,nrow=n,ncol=m+1) #resets matrix 
Profit=matrix(0,nrow=n,ncol=m) #resets matrix 
}
################Code to simulate variation in total profit in first 10 years for combination 2################# 
m=10 # no of years to simulate the profit
n=10000 #population size 
lambda=0.871 # mean number of accidents
Base_Pre=300 #amount of base premium 
d<-c(0,0.15,0.3) #vector containing each discount level
level=matrix(0,nrow=n,ncol=m+1) #matrix to store discount levels in each of the m years of each customer 
Profit=matrix(0,nrow=n,ncol=m) #matrix to store profit gained in each of the m years of each customers 
profit2=numeric(m) # vector to store total profit derived within 10 years from all 10000 customers 
for(k in 1:m) #for loop to iterate simulation for 1000 times 
{
for(j in 1:n) #for loop to simulate profit of each customer 
{
for(i in 1:m) # for loop to simulate profit derived in each year for each customer 
{
y<-rpois(1,lambda) # simulates no of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it 
{
Profit[j,i]=Base_Pre*(1-d[level[j,i]+1])-(sum(runif(y,0,600))) #evaluates profit derived from the jth customer and stores into jth entry of vector
level[j,i+1]=min(level[j,i]+1,2) # if at max discount level, remain , else go up by 1
}
else
{
Profit[j,i]=Base_Pre*(1-d[level[j,i]+1])-(sum(runif(y,0,600))) #evaluates profit derived from the jth customer and stores into jth entry of vector
level[j,i+1]=max(level[j,i]-1,0) # if at min discount level, remain, else decrease by 1
}
}
}
profit2[k]=sum(Profit[,k]) #aggregrates profit earned in mth year from all customers and stores into kth entry 
level=matrix(0,nrow=n,ncol=m+1) #resets matrix 
Profit=matrix(0,nrow=n,ncol=m) #resets matrix 
}
################Code to simulate variation in total profit in first 10 years for combination 3################# 
m=10 # no of years to simulate the profit
n=10000 #population size 
lambda=0.871 # mean number of accidents
Base_Pre=200 #amount of base premium 
d<-c(0,0.1,0.2,0.3) #vector containing each discount level
level=matrix(0,nrow=n,ncol=m+1) #matrix to store discount levels in each of the m years of each customer 
Profit=matrix(0,nrow=n,ncol=m) #matrix to store profit gained in each of the m years of each customers 
profit3=numeric(m) # vector to store total profit derived within 10 years from all 10000 customers 
for(k in 1:m) #for loop to iterate simulation for 1000 times 
{
for(j in 1:n) #for loop to simulate profit of each customer 
{
for(i in 1:m) # for loop to simulate profit derived in each year for each customer 
{
y<-rpois(1,lambda) # simulates no of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it 
{
Profit[j,i]=Base_Pre*(1-d[level[j,i]+1])-(sum(runif(y,0,400))) #evaluates profit derived from the jth customer and stores into jth entry of vector
level[j,i+1]=min(level[j,i]+1,3) # if at max discount level, remain , else go up by 1
}
else
{
Profit[j,i]=Base_Pre*(1-d[level[j,i]+1])-(sum(runif(y,0,400))) #evaluates profit derived from the jth customer and stores into jth entry of vector
level[j,i+1]=max(level[j,i]-1,0) # if at min discount level, remain, else decrease by 1
}
}
}
profit3[k]=sum(Profit[,k]) #aggregrates profit earned in mth year from all customers and stores into kth entry 
level=matrix(0,nrow=n,ncol=m+1) #resets matrix 
Profit=matrix(0,nrow=n,ncol=m) #resets matrix 
}
################Code to simulate variation in total profit in first 10 years for combination 4################# 
m=10 # no of years to simulate the profit
n=10000 #population size 
lambda=0.871 # mean number of accidents
Base_Pre=300 #amount of base premium 
d<-c(0,0.1,0.2,0.3) #vector containing each discount level
level=matrix(0,nrow=n,ncol=m+1) #matrix to store discount levels in each of the m years of each customer 
Profit=matrix(0,nrow=n,ncol=m) #matrix to store profit gained in each of the m years of each customers 
profit4=numeric(m) # vector to store total profit derived within 10 years from all 10000 customers 
for(k in 1:m) #for loop to iterate simulation for 1000 times 
{
for(j in 1:n) #for loop to simulate profit of each customer 
{
for(i in 1:m) # for loop to simulate profit derived in each year for each customer 
{
y<-rpois(1,lambda) # simulates no of accidents 
if(y==0) #if y=0 , r increases discount level of customer, otherwise, decreases it 
{
Profit[j,i]=Base_Pre*(1-d[level[j,i]+1])-(sum(runif(y,0,600))) #evaluates profit derived from the jth customer and stores into jth entry of vector
level[j,i+1]=min(level[j,i]+1,3) # if at max discount level, remain , else go up by 1
}
else
{
Profit[j,i]=Base_Pre*(1-d[level[j,i]+1])-(sum(runif(y,0,600))) #evaluates profit derived from the jth customer and stores into jth entry of vector
level[j,i+1]=max(level[j,i]-1,0) # if at min discount level, remain, else decrease by 1
}
}
}
profit4[k]=sum(Profit[,k]) #aggregrates profit earned in mth year from all customers and stores into kth entry 
level=matrix(0,nrow=n,ncol=m+1) #resets matrix 
Profit=matrix(0,nrow=n,ncol=m) #resets matrix 
}
##########Time taken to stability for each scheme and country combination################
cbind(time_scheme1,time_scheme2)
##########Annual profit upon stabilization for each scheme and country combination############
cbind(annualprofit_scheme1_countryA,annualprofit_scheme1_countryB,annualprofit_scheme2_countryA,annualprofit_scheme2_countryB)
##############Profit in early years for each scheme and country combination############
cbind(profit_scheme1_countryA,profit_scheme1_countryB,profit_scheme2_countryA,profit_scheme2_countryB)
#########Probability of bankruptcy for each scheme and country combination###########
cbind(probability_scheme1_countryA,probability_scheme1_countryB,probability_scheme2_countryA,probability_scheme2_countryB)
par<-par(mfcol=c(1,4)) #code to position 4 individual plots side by side 
plot(profit1,type="l",main="Total Profit in 10 years vs time step",sub="Combination1",xlab="time step",ylab="Total Profit") #code to produce line plot of total profit vs time step 
plot(profit2,type="l",main="Total Profit in 10 years vs time step",sub="Combination2",xlab="time step",ylab="Total Profit") #code to produce line plot of total profit vs time step
plot(profit3,type="l",main="Total Profit in 10 years vs time step",sub="Combination3",xlab="time step",ylab="Total Profit") #code to produce line plot of total profit vs time step
plot(profit4,type="l",main="Total Profit in 10 years vs time step",sub="Combination4",xlab="time step",ylab="Total Profit") #code to produce line plot of total profit vs time step



