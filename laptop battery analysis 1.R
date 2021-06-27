
# invoke my dataframe#
View(com)

 # Compactly Display the Structure#
str(com)

# the number of  rows and columns#
dim(com)

#explore the my data#
nrow(com)
ncol(com)


#to see the names of varables#
names(com)

#to see first 6 rows in my data set#
head(com)
tail(com,4)

#mean and standard deviation of data set#
?scale
x<-com$`FULL CHARGE CAPACITY(mWh)`
x
scale(x)


#initial summary in varables#
summary(com)

 # LOAD THE PACKAGE (MOSAIC)#
library(mosaic)
library(tigerstats)
str(com)

?fastest



#summary for each column mean,median.........in favstats  "favorite statistics"# 
?favstats
favstats(com$`FULL CHARGE CAPACITY(mWh)`)
plot(com$`temperature in C`,com$`maximum charge storage capacity(AH) for 50 cycle`,col="red")         
plot(com$`temperature in C`,com$`for 250 cycle`,type = "l",col="red",xlab ="TEMPERATURE",ylab = "MAX CAP (AH)FOR 250 CYCLES",main="DISCHARGE CAPACITY IN BATTERY")         
plot(com$`maximum charge storage capacity(mah)`,com$`voltage of cell (V) for 2C`,type ="l",col="blue",xlab = "MAX CHARGE strg CAPACITY(mah)",ylab="VOLTAGE IN 2 CELL(V)",main="DISCHARGE MAXIMUM STORAGE CAPACITY(mah)") 

#individual commands for some statisticals#
mean(com$`PERIOD (IN DAYS)`)
sd(com$`FULL CHARGE CAPACITY(mWh)`)
IQR(com$`FULL CHARGE CAPACITY(mWh)`)
median(com$`FULL CHARGE CAPACITY(mWh)`)

#make a bar chart of computer_battery_analysis#
bargraph(~`FULL CHARGE CAPACITY(mWh)`, data = com)
bargraph(~com$`FULL CHARGE CAPACITY(mWh)`,data = com,col="green",horizontal = TRUE)


ggplot(com,aes(x=com$`FULL CHARGE CAPACITY(mWh)`))+geom_bar()
ggplot(com,aes(x=com$`PERIOD (IN DAYS)`,fill=com$`FULL CHARGE CAPACITY(mWh)`))+geom_bar(position = "dodge")
library(RPostgreSQL)
library(dplyr)
library(ggplot2)
library(lubridate)
ggplot(com)+
  aes(x=`PERIOD (IN DAYS)`,y=`FULL CHARGE CAPACITY(mWh)`)+
  geom_line(color='deeppink3',alpha=0.9)

boxplot(com$`PERIOD (IN DAYS)`~com$`FULL CHARGE CAPACITY(mWh)`)
library(ggplot2)
ggplot(com,aes(`PERIOD (IN DAYS)`,`FULL CHARGE CAPACITY(mWh)`))+geom_boxplot()
ggplot(com,aes(`PERIOD (IN DAYS)`,`FULL CHARGE CAPACITY(mWh)`))+geom_boxplot(fill="blue")


library(effects)
library(mosaic)

hist(com$`FULL CHARGE CAPACITY(mWh)`)
library(ggplot2)
head(com)
ggplot(com,aes(`PERIOD (IN DAYS)`,`FULL CHARGE CAPACITY(mWh)`))+geom_histogram()

?boxplot
boxplot(com$`PERIOD (IN DAYS)`~com$`FULL CHARGE CAPACITY(mWh)`,col="yellow")
boxplot(com$`FULL CHARGE CAPACITY(mWh)`,horizontal = T)


hist(com$`FULL CHARGE CAPACITY(mWh)`,prob=TRUE)
curve(dnorm(x,mean(com$`FULL CHARGE CAPACITY(mWh)`),sd(com$`FULL CHARGE CAPACITY(mWh)`)),col="red",add = TRUE)

qqnorm(com$`temperature in C`~com$`for 250 cycle`,col("yellow"))
qqline(com$`FULL CHARGE CAPACITY(mWh)`,col="green")
qqplot(com$`FULL CHARGE CAPACITY(mWh)`,distribution="norm")

?qqplot

summary(com$`maximum charge storage capacity(mah)`)
boxplot(com$`voltage of cell (V) for 2C`)

hist(com$`FULL CHARGE CAPACITY(mWh)`)
plot(density(com$`FULL CHARGE CAPACITY(mWh)`),main ="`FULL CHARGE CAPACITY(mWh)`")

library("datasets")

?pie
pie(table(com$`FULL CHARGE CAPACITY(mWh)`),main="capacity dropping")
?table

com$`FULL CHARGE CAPACITY(mWh)`=factor(computer_battery_analysis_new$`FULL CHARGE CAPACITY(mWh)`)
summary(com$`voltage of cell (V) for 2C`)
plot(com$`FULL CHARGE CAPACITY(mWh)`,xlab="full charge capacity",ylab="mWH")

#
?by
by(com$`PERIOD (IN DAYS)`,com$`FULL CHARGE CAPACITY(mWh)`,summary)
by(com$`PERIOD (IN DAYS)`,com$`FULL CHARGE CAPACITY(mWh)`,mean)
by(com$`PERIOD (IN DAYS)`,com$`FULL CHARGE CAPACITY(mWh)`,median)


?boxplot
boxplot(com$`PERIOD (IN DAYS)`~com$`FULL CHARGE CAPACITY(mWh)`,notch=TRUE,main="discharge capacity")

?sm
library(sm)
sm.dens(com$`FULL CHARGE CAPACITY(mWh)`,xlab="capacity")
?sm.density.compare

?xtabs
xtabs(~`FULL CHARGE CAPACITY(mWh)`+`PERIOD (IN DAYS)`,com)
plot(xtabs(~`FULL CHARGE CAPACITY(mWh)`+`PERIOD (IN DAYS)`,com),main="DISCHARGE CAPACITY")


str(com$`maximum charge storage capacity(AH) for 50 cycle`)
scatter.smooth(com$`maximum charge storage capacity(AH) for 50 cycle`,com$`for 250 cycle`)



#to deletesNA's#
com<-replace(com,TRUE,lapply(com,na.aggregate))
summary(com)
str(com)




#model bulding#
dim(com)
library(leaps)
library(CARS)

#random forest classifications#
library(tree)
library(e1071)
library(PerformanceAnalytics)
summary(com)
chart.Correlation(com[-11],col=com$`o/pvariable`)

model<-train(`o/pvariable`~.,com,
             method='rf',TuneLength=5,
             trcontrol=trainControl(
               method = 'cv',number = 12,
               classProbs = TRUE))
model$results

predictedcom<-predict(model,com,"prob")
head(predictedcom)


ouput.forest<-randomForest(com$`o/pvariable`~com$`PERIOD (IN DAYS)`+com$`FULL CHARGE CAPACITY(mWh)`+com$`DESIGN CAPACITY(mWh)`+com$`VOLTAGE(volts)`+com$`temperature in C`+com$`maximum charge storage capacity(AH) for 50 cycle`+com$`for 250 cycle`+com$`maximum charge storage capacity(mah)`+com$`voltage of cell (V) for 1C`+com$`voltage of cell (V) for 2C`,data = com)
print(ouput.forest )
randomForest::importance(ouput.forest)
plot(ouput.forest)
table(com$`o/pvariable`)/nrow(com)



#classification tree#
library(party)
library(caret)
view(com)
set.seed(123)
ind<-sample(2,nrow(com),replace = TRUE,prob = c(0.7,0.30))
train22<-com[ind==1,]
test22<-com[ind==2,]
myf<-com$`o/pvariable`~com$`FULL CHARGE CAPACITY(mWh)`+com$`DESIGN CAPACITY(mWh)`+com$`VOLTAGE(volts)`+com$`temperature in C`+com$`maximum charge storage capacity(AH) for 50 cycle`+com$`for 250 cycle`+com$`maximum charge storage capacity(mah)`+com$`voltage of cell (V) for 1C`+com$`voltage of cell (V) for 2C`
com<-ctree(myf,data = test )
com$`o/pvariable`<-as.factor(com$`o/pvariable`)
table(com$`o/pvariable`)
plot(com)
myf2<-comop.sandeep$`o/pvariable`~comop.sandeep$`PERIOD (IN DAYS)`+com$`FULL CHARGE CAPACITY(mWh)`
compt2<-ctree(myf,data=train22)
plot(compt2)


