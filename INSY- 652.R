# PREDICTIVE ANALYTICS -------------------------------------------------------

# Packages-------------------------------------------------------
library("tidyverse")
library("modelr")
library("lubridate")
library("purrr")
library("broom")
library("magrittr")
library("RColorBrewer")
library("rpart")
library("rpart.plot")
library("Hmisc")
library("caTools")
library("car")   # provides VIF(collinerarity) and modelling packages
library("caret") # Machine learning + Modelling
library("GGally")
library("e1071") # various stats tools
library("dplyr") # grammer for manipulating, filtering, mutating tables.



  # Office Hours - Wed 1:30 - 3:30
  setwd("Google Drive/Courses/Predictive Analytics/INSY-652/")

# DATES -------------------------------------------------------
  # Jan 15 - Group Assignment 1
  # Jan 22 - Individual Assignment 1
  # Mar 12 - Team Project 2 
  # Mar 23 - Individual Assignment 2 
  # Mar 26 - Group Presentation 1
  # Apr 18 - Group Presentation 2
next_assignment <- function() {
  today <- Sys.Date()
  dates = list(
    list("name" = "Group Assignment 1"      , "date" = "2018-01-15"),
    list("name" = "Individual Assignment 1" , "date" = "2018-01-22"),
    list("name" = "Team Project 2"          , "date" = "2018-03-12"),
    list("name" = "Individual Assignment 2" , "date" = "2018-03-23"),
    list("name" = "Group Presentation 1"    , "date" = "2018-03-26"),
    list("name" = "Group Presentation 2"    , "date" = "2018-04-18")
  )
  for(i in 1:length(dates)) {
    dates[[i]]$"date" <- as.Date(dates[[i]]$date)
    dates[[i]]$"Days_Away" <- as.integer(dates[[i]]$date - today)
    
  }
  min_over_0 <- 1000
  closest_assignment <- list()
  for(i in 1:length(dates)) {
    if(min_over_0 > dates[[i]]$Days_Away && 0 < dates[[i]]$Days_Away) {
      min_over_0 <- dates[[i]]$Days_Away
      closest_assignment <- dates[[i]]
    }
  }
  # closest_assignment$date <- format(closest_assignment$date, format="%B %d %Y")
  print(paste("The next assignment is", closest_assignment$name,"on", format(closest_assignment$date, format="%B %d %Y"), ".", closest_assignment$Days_Away, "days from now" ))
}
next_assignment()

toDo=c("autmatically count N/A's for each row", "then check skew")


"questions = c(
  1: What is an accectable level of skewedness? Which transformations should be applied?
  2: 
)"

# Missing Values -------------------------------------------------------

  # three ways to deal with missing data
    #1: Replace with 0 or N/A
    #2: Replace with mode or mean 
    #3: Replace data with random data
  # Which is best? Depends. 


# Chapter 1  -------------------------------------------------------

'Preprossing_steps = list(
  1: `Remove if data mostly missing` ,
  2. `remove_outliers`,
  2. `check for normaliity: skewness + transformation if need be, Q-Q plot`,
  3: `categorical to flag (eg. M == 0)`
  4. `Reclassify categorical var (USA === US)`
)'


`what is [x,y]?` <- list(x:row, y:col)

skewness = function(foo) {
  skew <- 3*((mean(foo) - median(foo))) / (sd(foo))
  round(skew,digits = 2)
} 

check_skewness_transformation <- function(dataset){
  skew           <- skewness(dataset)
  sqrt_skew      <- skewness(sapply(dataset, sqrt)) # Square root
  inv_sqrt_skew  <- skewness(sapply(dataset, function(x) 1/sqrt(x))) # Inverse square root
  ln_skew        <- skewness(sapply(dataset, log)) # Natural log
  # Use the one that reduces the SKEW the MOST.
  print(paste("The Skew is:", skew))
  print(paste("Sqrt:",sqrt_skew,"Inverse sqrt:", inv_sqrt_skew,"Natural Log:", ln_skew))
}

percent_NA <- function(collumn){
  num_na     <- sum(is.na(collumn))
  percent_NA <- (num_na / length(collumn)) * 100
  
  print(paste("Percent:", percent_NA, num_na,"/",length(collumn)))
}



# Class 2 -------------------------------------------------------
    
  # hypothesis testing: " checking the P values for signifigance" T test for two, F test for 2+  

  # One Sample T test

    churn <- read.csv(file='Google Drive/Courses/Predictive Analytics/churn.txt', stringsAsFactors=TRUE)
    
    subchurn  <- subset(churn, churn$Int.l.Plan == "yes" & churn$VMail.Plan=="yes" & churn$Day.Mins>220)
    # T-Test for 1 sample
    mean.test <- t.test(x=subchurn$CustServ.Calls,mu=2.4,conf.level=0.95)
    mean.test$statistic
    mean.test$p.value
  
  # Two sample T test
  
    # Create two subgroups
    subchurn_yes <- subset(churn, Churn. == "True.")
    subchurn_no <- subset(churn, Churn. == "False.")
    
    # T-Test for two samples
    diff.test<-t.test(subchurn_yes$CustServ.Calls,subchurn_no$CustServ.Calls,conf.level=0.95)
    diff.test$p.value
    diff.test$statistic
    
  # Chi^2 test: "is the proportion of the sample the same as in the group?"
    # Create table
    table<-as.table(rbind(c(410,340,250),c(95,85,70)))
    dimnames(table)<-list(Data.Set=c("Training Set","Test Set"),Status=c("Married","Single","Other"))
    
    # Chi-Square Goodness of fit Test
    Xsq_data<-chisq.test(table)
    Xsq_data$statistic
    Xsq_data$p.value
    Xsq_data$expected
    
  # ANOVA: Are the within groups different from the whole? 
    # Create Data
    a<-c(30,40,50,60)
    b<-c(25,30,50,55)
    c<-c(25,30,40,45)
    ab<-append(a,b)
    datavalues<-append(ab,c)
    datalabels<-factor(c(rep("a",length(a)),rep("b",length(b)),rep("c",length(c))))
    # ANOVA Test
    anova.results<-aov(datavalues~datalabels)
    summary(anova.results)
    
    
  # Linear Regression
    # Create Data
    cereal  <- read.csv(file="Google Drive/Courses/Predictive Analytics/cereals.CSV",stringsAsFactors=TRUE,header=TRUE)
    sugars  <- cereal$Sugars
    fiber   <- cereal$Fiber
    rating  <- cereal$Rating
    sugars  <- na.omit(sugars)
    rating  <- rating[-58]
    fiber   <- fiber[-58]
    
    lm1   <-lm(rating~sugars)
    summary(lm1)
    plot(sugars, rating, main="Cereal Rating by Sugar Content",xlab="Sugar Content",ylab="Rating",pch=16,col="blue")
    abline(lm1,col="red")
  
    ggplot(cereal, aes(Sugars, Rating)) + geom_smooth()
    
    library(scatterplot3d)
    sp      <- scatterplot3d(z=sort(cereal$Rating), y=cereal$Sugars, x=cereal$Fiber, pch=16, xlab="Fiber", ylab="Sugars", zlab="Rating", main="3D Scatterplot")
    mreg1   <- lm(rating ~ sugar s+ fiber, data=cereal)
    summary(mreg1)
    
    vif(mreg1)
    
    # Four model selection 
      # forward selection: Start with most correleated predictor, if signififant, run multiple F tests on all predictors, use signifigant variables.
      # backwards elimination: Begin with all var, remove the smallest F stat (highest p) until all variables are predictive. 
      # stepwise selection: (?)
      # best subsets: works when predictor is not large, run all models with one predictor, pick the best (X), then run all models with X + 1
      
    
# Class 3-------------------------------------------------------
    
    
    
    # import the data and use only use first 1000 rows
    car.df <- read.csv(file="Google Drive/Courses/Predictive Analytics/INSY-652/Datasets/ToyotaCorolla.csv")
    car.df <- car.df[1:1000, ]
   # select variables for regression
    car.df$
   selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)
   # partition data
   set.seed(1) # set seed for reproducing the partition
   train.index <- sample(c(1:1000), 600)
   train.df <- car.df[train.index, selected.var]
   valid.df <- car.df[-train.index, selected.var]
   all.df <- car.df[, selected.var]

  # linear regression model
    car.lm <- lm(Price ~ ., data = train.df)
    car.lm2 <- lm(Price ~ ., data = all.df)
    summary(car.lm)
    summary(car.lm2)
    # predictive task
    
    require(forecast)
    car.lm.pred <- predict(car.lm, valid.df)
    some.residuals <- valid.df$Price[1:20] - car.lm.pred[1:20]
    data.frame("Predicted" = car.lm.pred[1:20], "Actual" = valid.df$Price[1:20],"Residual" = some.residuals)
    accuracy(car.lm.pred, valid.df$Price)

  # plotting the distribution of residual
    car.lm.pred <- predict(car.lm, valid.df)
    all.residuals <- valid.df$Price - car.lm.pred
    length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)])/400
    hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")


  # Input
    patients<-read.csv(file="Google Drive/Courses/Predictive Analytics/INSY-652/Datasets/patient.txt", header=TRUE)
  # Linear Regression vs. Logistic Regression
    lm1<-lm(disease~age,data=patients)
    lr1<-glm(disease~age,data=patients,family=binomial)
    plot(patients$age,patients$disease, xlab="Age", ylab="Disease", main="Disease vs. Age", xlim=c(20,90),pch=16)
    abline(lm1,lty=3)
    curve(predict(lr1,data.frame(age=x),type="resp"),add=TRUE,lwd=2)
    legend("topleft",legend=c("LS","Log."),lty=c(3,1),cex=.9)

    
  # Dichotomous Predictor
  # Input
    churn <- read.csv(file="Google Drive/Courses/Predictive Analytics/INSY-652/Datasets/churn.txt", stringsAsFactors=TRUE)
    table(churn$Churn,churn$VMail.Plan)
    churn$VMP.ind<-ifelse(churn$VMail.Plan=="yes",1,0)
  # Logistic Regression
    lr2<-glm(Churn. ~ VMP.ind,data=churn,family="binomial")
    summary(lr2)

  # Polytomous Predictor
  # Input
    churn <- read.csv(file="Google Drive/Courses/Predictive Analytics/INSY-652/Datasets/churn.txt", stringsAsFactors=TRUE)
    churn$CSC              <- factor(churn$CustServ.Calls)
    levels(churn$CSC)
    levels(churn$CSC)[1:2] <- "Low"
    levels(churn$CSC)[2:3] <- "Medium"
    levels(churn$CSC)[3:8] <- "High"
    churn$CSC_Med          <- ifelse(churn$CSC=="Medium", 1, 0)
    churn$CSC_Hi           <- ifelse(churn$CSC=="High", 1, 0)
  # Logistic Regression
    lr3<-glm(Churn.~CSC_Med+CSC_Hi,data=churn,family="binomial")
    summary(lr3)



# Continous prediction
  churn <- read.csv(file="C:/…/churn.txt", stringsAsFactors=TRUE)
  # Logistic Regression
  lr4<-glm(Churn.~Day.Mins,data=churn,family="binomial")
  summary(lr4)

# Multiple Logistic Regression
  #IF overall P is very low, but inidicidual P is not signifigant.There is likely MULTICOLLINEARITY! 
# Input
  churn <- read.csv(file="./datasets/churn.txt", stringsAsFactors=TRUE)
  churn$CSC<-factor(churn$CustServ.Calls)
  levels(churn$CSC)
  levels(churn$CSC)[1:2] <- "Low"
  levels(churn$CSC)[2:3] <- "Medium"
  levels(churn$CSC)[3:8] <- "High"
  churn$CSC_Med<-ifelse(churn$CSC=="Medium",1,0)
  churn$CSC_Hi<-ifelse(churn$CSC=="High",1,0)
  churn$IntlP.ind<-ifelse(churn$Int.l.Plan=="yes",1,0)
  VMP.ind<-ifelse(churn$VMail.Plan=="yes",1,0)
  
# Logistic Regression
  lr5<-glm(Churn.~IntlP.ind+VMP.ind+CSC_Hi+Day.Mins+Eve.Mins+Night.Mins+Intl.Mins,data=churn,family="binomial")
  summary(lr5)
  # G-Statistics
  install.packages("lmtest")
  library(lmtest)
  lrtest(lr5)