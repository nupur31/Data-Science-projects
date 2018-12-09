install.packages("car")
install.packages("MASS")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("psych")
install.packages("jtools")
install.packages("ggstance")
install.packages("broom")
install.packages("modelr")
library(car)
library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(psych)
library(jtools)
library(ggstance)
library(broom)
library(modelr)
library(stringr)

setwd("/Users/nupur/Upgrad R/regression /Regression assignment")
car<-read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE)
str(car)

#DATA PREPARATION

#Check for duplicates
unique(car)        #total obsv is 205 so no duplicate
#Check for NA
sum(is.na(car))   #no NA in the data
summary(car)

#Checking for outliers
quantile(car$wheelbase,seq(0,1,0.01))
car$wheelbase[which(car$wheelbase>115.544)]<-115.544        #capping at 99%tile
quantile(car$carlength,seq(0,1,0.01))                         
car$carlength[which(car$carlength>192.700)]<-192.700        #capping at 94%tile
quantile(car$carwidth,seq(0,1,0.01))                        #no outliers
quantile(car$carheight,seq(0,1,0.01))
quantile(car$curbweight,seq(0,1,0.01))                      #capping lower limit at 2%tile
car$curbweight[which(car$curbweight<1874.00)]<-1874.00
quantile(car$enginesize,seq(0,1,0.01))
car$enginesize[which(car$enginesize<90.00)]<-90.00      #capping at 3%tile and 93%tile
car$enginesize[which(car$enginesize>183.00)]<-183.00
quantile(car$boreratio,seq(0,1,0.01))                   #no outliers
quantile(car$stroke,seq(0,1,0.01)) 
car$stroke[which(car$stroke<2.6400)]<-2.6400             #capping lower limit at 2%tile
quantile(car$compressionratio,seq(0,1,0.01)) 
car$compressionratio[which(car$compressionratio<7.5000)]<-7.5000   #cappint at 4%tile and 90%tile
car$enginesize[which(car$enginesize>10.9400)]<-10.9400
quantile(car$horsepower,seq(0,1,0.01)) 
car$horsepower[which(car$horsepower>184.00)]<-184.00    #capping at 97%tile
quantile(car$peakrpm,seq(0,1,0.01))                     #nooutliers
quantile(car$citympg,seq(0,1,0.01))
car$citympg[which(car$citympg>38.00)]<-38.00            #capping at 98%tile
quantile(car$highwaympg,seq(0,1,0.01))
car$highwaympg[which(car$highwaympg>43.00)]<-49.88     #capping at 99%tile

str(car)

#plotting car_ID vs price
ggplot(car, aes(car_ID,price)) +geom_line()+
  scale_x_continuous(name = "car ID", breaks = seq(0,205,20), limits = c(0,205)) +
  scale_y_continuous(name = "Price", breaks = seq(0,50000,10000), limits = c(0,50000))


#Assigning dummy variables for symboling variable
car$symboling<-as.factor(car$symboling)
car$symboling
levels(car$symboling)[1:2] <- "pretty safe"
levels(car$symboling)[2:3] <- "safe"
levels(car$symboling)[3:4] <- "risky"
#Creating dummt variable 
dummy<-model.matrix(~symboling-1, data=car)
dummy<-dummy[,-1]
#combining dummy columns with database
car1<-cbind(car[,-2], dummy)
str(car1)

#separating car name
car1<-car1%>%separate(CarName,c("CarCompany"), sep=" ", extra="drop")
car1$CarCompany<-str_replace_all(car1$CarCompany,"maxda","mazda")
car1$CarCompany<-str_replace_all(car1$CarCompany,"Nissan","nissan")
car1$CarCompany<-str_replace_all(car1$CarCompany,"porcshce","porsche")
car1$CarCompany<-str_replace_all(car1$CarCompany,"vokswagen","volkswagen")
car1$CarCompany<-str_replace_all(car1$CarCompany,"vw","volkswagen")
car1$CarCompany<-str_replace_all(car1$CarCompany,"toyouta","toyota")
car1$CarCompany


#assigning dummy variable
dummy<-model.matrix(~CarCompany-1, data=car1)
dummy<-dummy[,-1]
#combining dummy columns with database
car1<-cbind(car1[,-2], dummy)
str(car1)


#assigning level to fueltype
car1$fueltype<-as.factor(car1$fueltype)
levels(car1$fueltype)<-c(1,0)
car1$fueltype <- as.numeric(levels(car1$fueltype))[car1$fueltype]
str(car1)
#View(car1)

#assigning level to aspiration
car1$aspiration<-as.factor(car1$aspiration)
levels(car1$aspiration)<-c(1,0)
car1$aspiration <- as.numeric(levels(car1$aspiration))[car1$aspiration]
str(car1)

#assigning level to doornumber
car1$doornumber<-as.factor(car1$doornumber)
levels(car1$doornumber)<-c(1,0)
car1$doornumber <- as.numeric(levels(car1$doornumber))[car1$doornumber]
str(car1)

#assigning level to carbody
dummy<-model.matrix(~carbody-1, data=car1)
dummy<-dummy[,-1]
#combining dummy columns with database
car1<-cbind(car1[,-5], dummy)
str(car1)

#assigning dummy variable to drivewheel

car1$drivewheel<-as.factor(car1$drivewheel)

dummy<-model.matrix(~drivewheel-1, data=car1)
dummy<-dummy[,-1]
#combining dummy columns with database
car1<-cbind(car1[,-5], dummy)
str(car1)

#assigning level to enginelocation
car1$enginelocation<-as.factor(car1$enginelocation)
levels(car1$enginelocation)<-c(1,0)
car1$enginelocation <- as.numeric(levels(car1$enginelocation))[car1$enginelocation]
str(car1)

#assigning dummy var to enginetype
dummy<-model.matrix(~enginetype-1, data=car1)
dummy<-dummy[,-1]
#combining dummy columns with database
car1<-cbind(car1[,-11], dummy)
str(car1)



#assigning dummy var to cylindernumbere
dummy<-model.matrix(~cylindernumber-1, data=car1)
dummy<-dummy[,-1]
#combining dummy columns with database
car1<-cbind(car1[,-11], dummy)
str(car1)

#assigning level to fuelsytem
car1$fuelsystem<-as.factor(car1$fuelsystem)
#assigning dummy variables
dummy<-model.matrix(~fuelsystem-1, data=car1)
dummy<-dummy[,-1]
#combining dummy columns with database
car1<-cbind(car1[,-12], dummy)
str(car1)




#Derived metrics car volume
car1$carvolume<-car1$carlength*car1$carwidth*car1$carheight

#removing car_ID

str(car1)
write.csv(car1, "car1.csv")
#selecting some of the variables for checking normality and multicollinearity
car1.num<-subset(car1, select=c(doornumber,curbweight,horsepower,peakrpm,citympg,cylindernumberfour,cylindernumbersix,price))
pairs.panels(car1.num, col="red")
#From graph,curbweight, horsepower, price are skewed, thus, transforming the variables
car1$curbweight<-log10(car1$curbweight)
car1$horsepower<-log10(car1$horsepower)
car1$price<-log10(car1$price)
car1.num1<-subset(car1, select=c(doornumber,curbweight,horsepower,peakrpm,citympg,cylindernumberfour,cylindernumbersix,price))
pairs.panels(car1.num1, col="red")         #now all variables are normal
#there is a high correlation between horsepower and citympg, horsepower and curbweight,
#horsepower and cylindernumbersix,citympg and cylindernumberfour, cylindernumberfour and cylindernumbersix which needs to be kept in mind at the time of model building
########################################################################################################
#DIVIDING INTO TRAIN & TEST DATA

#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(car1), 0.7*nrow(car1))
# generate the train data set
train = car1[trainindices,]
train<-train[,-1]
str(train)
#Similarly store the rest of the observations into an object "test".
test = car1[-trainindices,]
str(test)
#######################################################################################################

#BUILDING MLR MODEL

#Building linear regression model

model_1<-lm(price~., data=train)
summary(model_1)

#Using stepAIC method
step<-stepAIC(model_1, direction="both")
step

model_2<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase + carlength + carheight + curbweight + boreratio + 
              peakrpm + citympg + CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
              CarCompanychevrolet + CarCompanydodge + CarCompanyisuzu + 
              CarCompanymazda + CarCompanymitsubishi + CarCompanypeugeot + 
              CarCompanyplymouth + CarCompanyporsche + CarCompanyrenault + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
              enginetypeohc + cylindernumberfive + cylindernumbersix + 
              fuelsystemmpfi + carvolume, data = train)
summary(model_2)

#using VIF function to find multicollinearity
vif(model_2)

#removing carvolume var with heighest vif and also less significant
model_3<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase + carlength + carheight + curbweight + boreratio + 
              peakrpm + citympg + CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
              CarCompanychevrolet + CarCompanydodge + CarCompanyisuzu + 
              CarCompanymazda + CarCompanymitsubishi + CarCompanypeugeot + 
              CarCompanyplymouth + CarCompanyporsche + CarCompanyrenault + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
              enginetypeohc + cylindernumberfive + cylindernumbersix + 
              fuelsystemmpfi, data = train)
summary(model_3)
vif(model_3)


#checking correlation between carlength and curbweight as both have high VIF
cor(car1$carlength,car1$curbweight)
#as they are highly correlated, we will drop less significant var carlength

model_4<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase + carheight + curbweight + boreratio + 
              peakrpm + citympg + CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
              CarCompanychevrolet + CarCompanydodge + CarCompanyisuzu + 
              CarCompanymazda + CarCompanymitsubishi + CarCompanypeugeot + 
              CarCompanyplymouth + CarCompanyporsche + CarCompanyrenault + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
              enginetypeohc + cylindernumberfive + cylindernumbersix + 
              fuelsystemmpfi, data = train)
summary(model_4)
vif(model_4)

#correlation between curbweight and citympg
cor(car1$curbweight,car1$citympg)
#As they are highly correlated, we will drop less significant var citympg 

model_5<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase + carheight + curbweight + boreratio + 
              peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
              CarCompanychevrolet + CarCompanydodge + CarCompanyisuzu + 
              CarCompanymazda + CarCompanymitsubishi + CarCompanypeugeot + 
              CarCompanyplymouth + CarCompanyporsche + CarCompanyrenault + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
              enginetypeohc + cylindernumberfive + cylindernumbersix + 
              fuelsystemmpfi, data = train)
summary(model_5)
vif(model_5)

#correlation between carbodysedan and carbodyhatchback as they have high VIF
cor(car1$carbodyhatchback,car1$carbodysedan)

#dropping var carbodysedan as it is highly correlated carbodyhatchback and less significant
model_6<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase + carheight + curbweight + boreratio + 
              peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
              CarCompanychevrolet + CarCompanydodge + CarCompanyisuzu + 
              CarCompanymazda + CarCompanymitsubishi + CarCompanypeugeot + 
              CarCompanyplymouth + CarCompanyporsche + CarCompanyrenault + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              carbodyhardtop + carbodyhatchback + carbodywagon + 
              enginetypeohc + cylindernumberfive + cylindernumbersix + 
              fuelsystemmpfi, data = train)
summary(model_6)
vif(model_6)

#checking correlation between variables with high VIF

car1.model3<-subset(car1, select=c(fueltype,doornumber, enginelocation, 
                                   wheelbase,carheight,curbweight,boreratio,
                                   peakrpm,enginetypeohc,cylindernumberfive,cylindernumbersix, 
                                   fuelsystemmpfi))
pairs.panels(car1.model3,col="red")


#dropping var boreratio as it has vif and highly correlated with curbweight and low significance

model_7<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase + carheight + curbweight +
              peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
              CarCompanychevrolet + CarCompanydodge + CarCompanyisuzu + 
              CarCompanymazda + CarCompanymitsubishi + CarCompanypeugeot + 
              CarCompanyplymouth + CarCompanyporsche + CarCompanyrenault + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              carbodyhardtop + carbodyhatchback + carbodywagon + 
              enginetypeohc + cylindernumberfive + cylindernumbersix + 
              fuelsystemmpfi, data = train)
summary(model_7)
vif(model_7)


#dropping cylindernumbersix with high vif and highly correlated to curbweight and no significance
model_8<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase + carheight + curbweight +
              peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
              CarCompanychevrolet + CarCompanydodge + CarCompanyisuzu + 
              CarCompanymazda + CarCompanymitsubishi + CarCompanypeugeot + 
              CarCompanyplymouth + CarCompanyporsche + CarCompanyrenault + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              carbodyhardtop + carbodyhatchback + carbodywagon + 
              enginetypeohc + cylindernumberfive+ 
              fuelsystemmpfi, data = train)
summary(model_8)
vif(model_8)

#dropping fuelsystemmpfi is highly correlated to curbweight and not significant  

model_9<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase + carheight + curbweight +
              peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
              CarCompanychevrolet + CarCompanydodge + CarCompanyisuzu + 
              CarCompanymazda + CarCompanymitsubishi + CarCompanypeugeot + 
              CarCompanyplymouth + CarCompanyporsche + CarCompanyrenault + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              carbodyhardtop + carbodyhatchback + carbodywagon + 
              enginetypeohc + cylindernumberfive, data = train)
summary(model_9)
vif(model_9)



#dropping wheelbase as its highly correlated with curbweight and less significant than curbweight
model_10<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
                carheight + curbweight +
                peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
                CarCompanychevrolet + CarCompanydodge + CarCompanyisuzu + 
                CarCompanymazda + CarCompanymitsubishi + CarCompanypeugeot + 
                CarCompanyplymouth + CarCompanyporsche + CarCompanyrenault + 
                CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodywagon + 
                enginetypeohc + cylindernumberfive, data = train)
summary(model_10)
vif(model_10)

#checking correlation between variables with high VIF
car1.model.cat<-subset(car1, select=c( doornumber,carheight,curbweight,enginelocation,CarCompanybuick,
                                       CarCompanyporsche,enginetypeohc,carbodyhatchback,cylindernumberfive))
pairs.panels(car1.model.cat,col="red")

#removing CarCompanyporsche as its correlated with enginelocation and less significant
model_11<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
               carheight + curbweight +
               peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
               CarCompanychevrolet + CarCompanydodge + CarCompanyisuzu + 
               CarCompanymazda + CarCompanymitsubishi + CarCompanypeugeot + 
               CarCompanyplymouth +CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
               carbodyhardtop + carbodyhatchback + carbodywagon + 
               enginetypeohc + cylindernumberfive, data = train)
summary(model_11)
vif(model_11)


#removing enginetypeohc as its correlated with curbweight and less significant   
model_12<-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
               carheight + curbweight +
               peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
               CarCompanychevrolet + CarCompanydodge + CarCompanyisuzu + 
               CarCompanymazda + CarCompanymitsubishi + CarCompanypeugeot + 
               CarCompanyplymouth +CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
               carbodyhardtop + carbodyhatchback + carbodywagon + 
               cylindernumberfive, data = train)
summary(model_12)
vif(model_12)


#removing doornumber as its correlated with carheight and insignificant
model_13<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               carheight + curbweight +
               peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
               CarCompanychevrolet + CarCompanydodge + CarCompanyisuzu + 
               CarCompanymazda + CarCompanymitsubishi + CarCompanypeugeot + 
               CarCompanyplymouth +CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
               carbodyhardtop + carbodyhatchback + carbodywagon + 
               cylindernumberfive, data = train)
summary(model_13)
vif(model_13)

#dropping cylindernumberfive  as its highly correlated with CarCompanybuick and less significant
model_14<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               carheight + curbweight +
               peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
               CarCompanychevrolet + CarCompanydodge + CarCompanyisuzu + 
               CarCompanymazda + CarCompanymitsubishi + CarCompanypeugeot + 
               CarCompanyplymouth +CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
               carbodyhardtop + carbodyhatchback + carbodywagon , data = train)
summary(model_14)
vif(model_14)



#dropping carbodyhatchback as its highly correlated to carheight and less significant
model_15<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               carheight + curbweight +
               peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
               CarCompanychevrolet + CarCompanydodge + CarCompanyisuzu + 
               CarCompanymazda + CarCompanymitsubishi + CarCompanypeugeot + 
               CarCompanyplymouth +CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
               carbodyhardtop+ carbodywagon , data = train)
summary(model_15)
vif(model_15)

#dropping carheight as its insignificant
model_16<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               curbweight +
               peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
               CarCompanychevrolet + CarCompanydodge + CarCompanyisuzu + 
              CarCompanyplymouth +CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
               carbodyhardtop+ carbodywagon , data = train)
summary(model_16)
vif(model_16)  #Now, multicollinearity is removed

#Now, checking and eliminating influential/leveraged observations which can distort the outcome and accuracy of a regression
cutoff <- 4/((nrow(train)-length(model_16$coefficients)-1)) # Cook's D metho, cutoff as 4/(n-k-1)
plot(model_16, which=4, cook.levels=cutoff)                        # identify cook's Distance values > cutoff
plot(model_16, which=5, cook.levels=cutoff)
train<- train[-which(rownames(train)    # Row names discovered in 2 rounds
                     %in% c("10", "75", "156")),]    
#Refitting the model
model_16<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               curbweight +
               peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
               CarCompanychevrolet + CarCompanydodge + CarCompanyisuzu + 
               CarCompanymazda + CarCompanymitsubishi + CarCompanypeugeot + 
               CarCompanyplymouth +CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
               carbodyhardtop +carbodywagon, data = train)
summary(model_16)
vif(model_16)
# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train)-length(model_16$coefficients)-1)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(model_16, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(model_16, which=5, cook.levels=cutoff)
train<- train[-which(rownames(train)    # Row names discovered in 2 rounds
                     %in% c("99", "139", "168")),]  

#Refitting the model
model_16<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               curbweight +
               peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
               CarCompanychevrolet + CarCompanydodge + CarCompanyisuzu + 
               CarCompanymazda + CarCompanymitsubishi + CarCompanypeugeot + 
               CarCompanyplymouth +CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
               carbodyhardtop +carbodywagon, data = train)
summary(model_16)
vif(model_16)

# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train)-length(model_16$coefficients)-1)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(model_16, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(model_16, which=5, cook.levels=cutoff)                      #no extreme observations


#dropping CarCompanymazda as its insignificant
model_17<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               curbweight +
               peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
               CarCompanychevrolet + CarCompanydodge + CarCompanyisuzu + 
               CarCompanymitsubishi + CarCompanypeugeot + 
               CarCompanyplymouth +CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
               carbodyhardtop +carbodywagon, data = train)
summary(model_17)
vif(model_17)

#dropping CarCompanyvolkswagen  as its insignificant
model_18<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               curbweight +
               peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
               CarCompanychevrolet + CarCompanydodge + CarCompanyisuzu + 
               CarCompanymitsubishi + CarCompanypeugeot + 
               CarCompanyplymouth +CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + 
               carbodyhardtop +carbodywagon, data = train)
summary(model_18)
vif(model_18)

#dropping carbodyhardtop   as its insignificant
model_19<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               curbweight +
               peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
               CarCompanychevrolet + CarCompanydodge + CarCompanyisuzu + 
               CarCompanymitsubishi + CarCompanypeugeot + 
               CarCompanyplymouth +CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + 
               carbodywagon, data = train)
summary(model_19)
vif(model_19)

#dropping CarCompanychevrolet   as its insignificant
model_20<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               curbweight +
               peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
               CarCompanydodge + CarCompanyisuzu + 
               CarCompanymitsubishi + CarCompanypeugeot + 
               CarCompanyplymouth +CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + 
               carbodywagon, data = train)
summary(model_20)
vif(model_20)

#dropping CarCompanysubaru  as its insignificant
model_21<-lm(formula = price ~ fueltype + aspiration +enginelocation + 
               curbweight +
               peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
               CarCompanydodge + CarCompanyisuzu + 
               CarCompanymitsubishi + CarCompanypeugeot + 
               CarCompanyplymouth +CarCompanyrenault + 
               CarCompanytoyota + 
               carbodywagon, data = train)
summary(model_21)
vif(model_21)



#dropping fueltype  is not significant 
model_22<-lm(formula = price ~ aspiration +enginelocation + 
               curbweight +
               peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
               CarCompanydodge + CarCompanyisuzu + 
               CarCompanymitsubishi + CarCompanypeugeot + 
               CarCompanyplymouth +CarCompanyrenault + 
               CarCompanytoyota + 
               carbodywagon, data = train)
summary(model_22)
vif(model_22)

#dropping CarCompanyrenault  as its insignificant
model_23<-lm(formula = price ~ aspiration +enginelocation + 
               curbweight +
               peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
               CarCompanydodge + CarCompanyisuzu + 
               CarCompanymitsubishi + CarCompanypeugeot + 
               CarCompanyplymouth +
               CarCompanytoyota + 
               carbodywagon, data = train)
summary(model_23)
vif(model_23)


#dropping aspiration   as its insignificant
model_24<-lm(formula = price ~ enginelocation + 
               curbweight +
               peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
               CarCompanydodge + CarCompanyisuzu + 
               CarCompanymitsubishi + CarCompanypeugeot + 
               CarCompanyplymouth +
               CarCompanytoyota + 
               carbodywagon, data = train)
summary(model_24)
vif(model_24)


#dropping CarCompanydodge   as its insignificant
model_25<-lm(formula = price ~ enginelocation + 
               curbweight +
               peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
               CarCompanyisuzu + 
               CarCompanymitsubishi + CarCompanypeugeot + 
               CarCompanyplymouth +
               CarCompanytoyota + 
               carbodywagon, data = train)
summary(model_25)
vif(model_25)

#dropping CarCompanyplymouth   as its insignificant
model_26<-lm(formula = price ~ enginelocation + 
               curbweight +
               peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
               CarCompanyisuzu + 
               CarCompanymitsubishi + CarCompanypeugeot +
               CarCompanytoyota + 
               carbodywagon, data = train)
summary(model_26)
vif(model_26)


#dropping CarCompanytoyota   as its insignificant
model_27<-lm(formula = price ~ enginelocation + 
               curbweight +
               peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
               CarCompanyisuzu + 
               CarCompanymitsubishi + CarCompanypeugeot +
               carbodywagon, data = train)
summary(model_27)
vif(model_27)

#dropping CarCompanyisuzu as its insignificant
model_28<-lm(formula = price ~ enginelocation + 
               curbweight +
               peakrpm +CarCompanyaudi + CarCompanybmw + CarCompanybuick +
               CarCompanymitsubishi + CarCompanypeugeot +
               carbodywagon, data = train)
summary(model_28)
vif(model_28)


#Checking model accuracy 
glance(model_28)

#Adj. R square of the model is .94, thus it's a good model that explains 94% of the variance in the model
#Sigma or RSE is 0.496 is low indicating ggood model with low residual std. error
#F-Statistic is 266 is high which indicates model is providing better fit to data than a model that contains no independent variables
#AIC and BIC are low indicating better quality of model

# Checking goodness of model by checking its Normality, Independence, Linearity, Homoscedasticity
#Plot 1: Residual V/s Fitted
ggplot(model_28, aes(.fitted, .resid)) +
  geom_ref_line(h = 0) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Residuals vs Fitted")

#Plot 1: Residual V/s Fitted Values plot checks the linearity and shows if residuals have non-linear patterns. 
#Here , residual values are randomly spread and not showing any patterns , thus there are no non-linear relationships.

#Plot 2: Normal QQ plot

qqplot<- qqnorm(model_28$residuals, main = "Normal Q-Q Plot",
                xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
                plot.it = TRUE)
qqplot<-qqline(model_28$residuals, col="blue")

qqPlot(model_28$residuals)
#Normal QQ plot checks if residuals are nornally distributed. Here, residuals follow a straight line and are normal
#Plot 3:Spread-location plot
plot(model_28, which = 3, id.n = 5)
#Spread-location plot shows if residual are spread equally along the range of predictors and checks the assumption of homoscedasticity
#Here, the model is following the assumption as points are randomly spread around the horizontal line.
#Plot 4: Residual V/s Leverage plot
par(mfrow=c(1, 2))

plot(model_28, which = 4, id.n = 5)
plot(model_28, which = 5, id.n = 5)
# Residual V/s Leverage plot helps to find influential points if any that can effect the model using Cook's distance method
#Here, All the observations ae within the threshold area 


#validating model on test data
str(test)
test$price<-10^(test$price)
test$predict_price<-10^(predict(model_28, test))
test$error<-test$price-test$predict_price
train$predict_price<-10^(predict(model_28,train))

# Checking the goodness of the model on the test set - correlation^2, RMSE and MAE
test.corr <- round(cor(test$price,test$predict_price ),2)
test.RMSE <- round(sqrt(mean((test$predict_price - test$price)^2)))
test.MAE <- round(mean(abs(test$predict_price - test$price)))
c(test.corr^2, test.RMSE, test.MAE)       
#0.9025 2297.0000 1604.0000, the model is predicting price of car with error of $1604-2297 

test
# Plot - Actual vs Predicted price model
ggplot(test, aes(car_ID,price, fill="red")) +geom_line(aes(y=test$price, color="blue"))+
  scale_x_continuous(name = "car ID", breaks = seq(0,205,15), limits = c(0,205)) +
  scale_y_continuous(name = "Price", breaks = seq(0,50000,10000), limits = c(0,50000))+ 
  geom_line(aes(x=car_ID, y=predict_price, colour="red"))+scale_color_manual(labels = c("Actual Price", "Predicted Price"), values = c("red", "green")) 
#The predicted price is almost overlapping the actual price data

# Plot Model errors
ggplot(test, aes(car_ID,error)) + geom_point() + 
  scale_x_continuous(name = "car ID", breaks = seq(0,205,15), limits = c(0,205)) +
  scale_y_continuous(name = "Error", limits = c(-7000,15000)) +
  geom_hline(yintercept = 0)
#Error points are randomly scattered like a white noise and not because of any variable

#understanding the model

summ(model_28)
#Variables significant in predicting the price are
#enginelocation,curbweight,peakrpm,CarCompanyaudi,CarCompanybmw,CarCompanymitsubishi,CarCompanypeugeot,CarCompanybuick,carbodywagon

car1.model<-subset(car1, select=c(enginelocation,curbweight,peakrpm,CarCompanyaudi,CarCompanybmw,CarCompanybuick,CarCompanymitsubishi,CarCompanypeugeot,carbodywagon,price))
pairs.panels(car1.model, col="red")


#Ploting the influence of each predictors in predicting the car price
plot_summs(model_28, scale = TRUE)
effect_plot(model_28, pred = curbweight, interval = TRUE, plot.points=TRUE, point.size=0.5)#With increase in curbweight price is increasing
effect_plot(model_28, pred = peakrpm, interval = TRUE) #increase in peakrpm increases price
effect_plot(model_28, pred = carbodywagon, interval = TRUE) #negatively effects the car price, car with wagon body are less priced
effect_plot(model_28, pred = enginelocation, interval = TRUE) #cars with engine in front have greater price
effect_plot(model_28, pred = CarCompanyaudi, interval = TRUE) #audi cars have high price in US
effect_plot(model_28, pred = CarCompanybmw, interval = TRUE)  #bmw cars have high price in US
effect_plot(model_28, pred = CarCompanymitsubishi, interval = TRUE) #mitsubhishi cars have -ve effect on car price, they are prices at lower end
effect_plot(model_28, pred = CarCompanypeugeot, interval = TRUE) # peugeot cars have -ve effect on car price, they are prices at lower end
effect_plot(model_28, pred = CarCompanybuick, interval = TRUE)   #buick cars have high price in US
