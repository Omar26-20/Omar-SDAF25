#Fall 25/ITC 255
#Descriptive methods
#Univar case 
#FDT 
#FDT of a QL var

#Upload the data dataset tips

dfTips=read.csv("Mohammad-Omar-Es_Haqzai-itc255.csv")
View(dfTips)

#FDT of a QL var
names(dfTips)
View(dfTips)
#smoker Distribution

AbsFreq=table(dfTips$PrimaryUse)
AbsFreq2=table(dfTips$Height)
AbsFreq
AbsFreq2

prop.table(AbsFreq)    #Abs. Freq
prop.table(AbsFreq2)

RelFreq=round(prop.table(AbsFreq), 2)
RelFreq

RelFreq2=round(prop.table(AbsFreq2), 2)
RelFreq2

CumFreq=cumsum(RelFreq)
CumFreq

FDTSmoker=cbind(AbsFreq, RelFreq, CumFreq)
FDTSmoker

#write a function that creates and FDT of a QL var


FDTQL=function(x){
  ABSFreq=table(x)
  RELFreq=round(prop.table(ABSFreq),2)
  CUMFreq=cumsum(RELFreq)
  FDTx=cbind(ABSFreq, RELFreq, CUMFreq)
  return(FDTx)
}

FDTQL(dfTips$PrimaryUse)

FDTQL(dfTips$Gender)
FDTQL(dfTips$Age)

##Construction FDT of a Quant variable 
#Loops and conditional functions work in R
#1. Transform the variable into a categorical var based a definition/we specify them

#Lets use the variable tips

summary(dfTips$Height)
head(Mohammad-Omar-Es_Haqzai-itc255)
#define catgories: small whtn tip<3 meduim when tip is 3>= but less than 7, large otherwise

#selection + Loop
catTips=c()  #create an empty vector

for (k in 1:length(dfTips$tip)) {
  if(dfTips$tip[k]<3){
    catTips[k]="AsmallTip"
  } else if (dfTips$tip[k] >=3 & dfTips$tip[k]<7) {
    catTips[k]="BmeduimTip"
  } else {
    catTips[k]="Clargetip"
  }
}

head(catTips)
head(dfTips$tip)
#apply the function for FDT of QL
FDTQL(catTips)

#++++++++++++++++++++Descriptive methods++++++++++++
#Univar case 
#Graphs 
#Categorical vars (pie and bar)

#create the FDT 
FDTQL(dfTips$smoker)[,2]

fdtSmoker=FDTQL(dfTips$smoker)[,2]
fdtSmoker

pie(fdtSmoker, 
    col = rainbow(2), 
    main = 'Smoker Distribution')

barplot(fdtSmoker, 
        col=rainbow(2), 
        main = 'Smoker distribution')

fdttip=FDTQL(catTips)[,2]
fdttip

barplot(fdttip, 
        col=rainbow(3), 
        main = 'Tip distribution')

#Descriptive methods
#Univar case 
#Graphs 
#Num vars (hist and density)
head(dfTips)

hist(dfTips$Age, 
     col='blue', 
     main = 'Tips distibution')

plot(density(dfTips$Age), 
     col='#0033FF', 
     main='Tips distribution')


plot(density(dfTips$total_bill), 
     col='#0033FF', 
     main='Total Bill distribution')

y=read.csv("timeToOffice.csv")
names(y)


hist(y$T)
plot(density(y$T))


#Descriptive methods - WEEk-3
  #Univariate case
    #Numerical methods
      #Center of distribution (mean, median)
mean(dfTips$Age)
median(dfTips$Age)

#Descriptive methods - WEEk-3
  #Univariate case
    #Numerical methods
      #variation in the distribution (range, sd, var, mad)
range(dfTips$Age)
sd(dfTips$Age)
var(dfTips$Age)
mad(dfTips$Age)


#Descriptive methods - WEEk-3
  #Univariate case
    #Numerical methods
      #location (quantiles and percentiles)
quantile(dfTips$Age)

boxplot(dfTips$Age,
        horizontal = T,
        col='#0033FF')
boxplot.stats(dfTips$Age) #outliers
quantile(dfTips$Age, .2)
