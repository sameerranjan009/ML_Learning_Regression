#Assignment 1 BUAN 6341 Anthony Chin aec180004


install.packages("corrplot")
install.packages("RColorBrewer")
library(corrplot)
library(RColorBrewer)

setwd("C:/Users/Wings2N/Desktop/School/Fall 2019/Applied Machine Learning BUAN 6341/Assignment 1")
Energydata<-read.csv("energydata_complete.csv")




#Data input
Energydata<-Energydata[,-1] #removing date as told not to use it
Energydata.scale<-scale(Energydata)
Energydata.scale.cor<-cor(Energydata)



#taking a look at which variables I shouldn't use
corrplot(Energydata.scale.cor, type="full", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))


#Correlation plot shows that I should not use rv1, rv2, windspeed, appliances, lights, Visibility or the first two
set.seed(1000)
selected.var<-c(1,1:18+2)  #I don't know let's just take the first 18 variables that aren't  the ones above
train.index<-sample(nrow(Energydata.scale),0.6*nrow(Energydata.scale), replace = FALSE)
train.df<-as.data.frame(Energydata.scale[train.index, selected.var])
valid.df<-as.data.frame(Energydata.scale[-train.index, selected.var])


#####Task 1 Model the energy usage of Appliances with initial parameters
#Picking variables
xi<-as.matrix(train.df[,2:NCOL(train.df)])
yi<-as.matrix(train.df[,1]) #Appliances


#Creating/Initializing variables
m<-as.numeric(NROW(xi)) #num of observations
xi <- cbind(rep(1,m), xi)
b_all <- matrix(c(1,1),ncol(xi),1)
alpha<-.005
ConvergenceThreshold<-.01
itr<-2000

#results vector creation
results<-rep(NA,NROW(b_all)+3)
names(results)<-list("alpha","iterations","cf", "b0", "b1", "...")


#initial cost function calculation
cf <- (1/(2*m)) * t(xi %*% b_all - yi) %*% (xi %*% b_all - yi)
cf_all<-cf


#loop to minimize cost function
i <- 0
cf_diff<-1

while(cf_diff>ConvergenceThreshold){
  i <- i + 1
  
  #if we hit iteration limit break out of loop
  if(i==itr){print("Iterations limit reached"); break;}
  
  #calculating yhat
  yhat <- xi %*% b_all
  
  #calculating b's
  b_all <- b_all - (alpha *(1/ m)) * (t(xi) %*% (yhat - yi))
  
  #calculating new cf
  cf_new<-(1/(2*m)) * t(xi %*% b_all - yi) %*% (xi %*% b_all - yi)
  
  #keeping a list of all cf
  cf_all <- append(cf_all, cf_new)
  
  #checking for convergence
  cf_diff <- abs(cf_all[i+1] - cf_all[i])
  
  if((cf_all[i+1] - cf_all[i]) > 0){
    print("Alpha too big?"); break;
  }
}
plot(1:NROW(cf_all),cf_all)

b_all

#Creating a results vector
if(is.na(results[1])){results<-results<-rbind(results,c(alpha,itr,cf_new,b_all)); results<-results[-1,]
}else{results<-results<-rbind(results,c(alpha,itr,cf_new,b_all))}


################################### Experimentation 1#####################################

#Experiemental Variables:
alphaExperiment<-.24

  #first run alpha c(.001,.01,.05,.1,.2,.3,.4,.5,1)
  #second run alpha seq(.15, .35, by=.01)
  #Smallest value that makes still doesn't cause huge changes .25

ConvergenceThresholdExperiment<-.000000001
  #first run Threshold c(.001,.01,.05,.1,.2,.3,.4,.5,1) Probably not small enough
  #second run Threshold c(.0000000001,.000000001,.00000001,.0000001)


#results vector creation
results<-rep(NA,NROW(b_all)+4)

a<-1
th<-1

##LOOPING for Alpha and Convergence Thresholds use formulas below to 
#for(a in 1:NROW(alphaExperiment)) #for alpha testing
#for(th in 1:NROW(ConvergenceThresholdExperiment)) #for Convergence Threshold Testing
{
#Picking variables
  xi<-as.matrix(train.df[,2:NCOL(train.df)])
  yi<-as.matrix(train.df[,1]) #Appliances
  
  xi<-as.matrix(train.df[,2:NCOL(train.df)])
  yi<-as.matrix(train.df[,1]) #Appliances
  
#Creating/Initializing variables
  m<-as.numeric(NROW(xi)) #num of observations
  xi<- cbind(rep(1,m), xi)
  b_all <- matrix(c(1,1),ncol(xi),1)
  alpha<-alphaExperiment[a]
  ConvergenceThreshold<-ConvergenceThresholdExperiment[th]
  itr<-2000



#initial cost function calculation
cf <- (1/(2*m)) * t(xi %*% b_all - yi) %*% (xi %*% b_all - yi)
cf_all<-cf


#Gradient descent
i <- 0
cf_diff<-10

while(cf_diff>ConvergenceThreshold){
  i <- i + 1
  
  #if we hit iteration limit break out of loop
  if(i==itr){print(paste("Iterations limit reached")); break;}
  
  #calculating yhat
  yhat <- xi %*% b_all
  
  #calculating b's
  b_all <- b_all - (alpha *(1/ m)) * (t(xi) %*% (yhat - yi))
  
  #calculating new cf
  cf_new<-(1/(2*m)) * t(xi %*% b_all - yi) %*% (xi %*% b_all - yi)
  
  #keeping a list of all cf
  cf_all <- append(cf_all, cf_new)
  
  #checking for convergence
  cf_diff <- abs(cf_all[i+1] - cf_all[i])
  
  #checking for non-convergence
  if((cf_all[i+1] - cf_all[i]) > 0){
    print(paste("Alpha too big? Alpha Value:",alpha)); break;
  }
}


b_all

#Creating a results vector
if(is.na(results[1])){results<-results<-rbind(results,c(alpha,i, itr,cf_new,ConvergenceThreshold,b_all)); results<-results[-1,]
}else{results<-results<-rbind(results,c(alpha,i, itr,cf_new,ConvergenceThreshold,b_all))}

}

results<-as.data.frame(results)
names(results)<-list("alpha","iterations", "max iterations","cf_new","ConvergenceThreshold", "b0", "b1", "...")

#plot for alpha
plot(results$alpha,results$cf_new, type ="b", xlab = "Alpha", ylab = "Cost Function") 

#plot for Convergence Threshold Results
plot(results$ConvergenceThreshold,results$cf_new, type ="b", xlab = "Convergence Threshold", ylab = "Cost Function")

#Results are in this order:"alpha","iterations", "max iterations","cf_new","ConvergenceThreshold", "b0", "b1", "b..."
View(results)

#to just get B coefficients
b_all
round(b_all,5) #if you want a condensed version

predict(valid.df)

#checking against lm
lm(Appliances ~ ., data = train.df)
head(train.df)


############ Experimenting with Logstics Regression ##############
library("caret")
Energydata.forlog <- read.csv("energydata_complete.csv")
summary(Energydata.forlog)
plot(Energydata.forlog$Appliances)

High = ifelse(Energydata.forlog$Appliances<=100,"No","Yes")
Energydata.forlog.High<-cbind(High, Energydata.forlog)

#Data input
Energydata.forlog.High<-Energydata.forlog.High[,-2] #removing date as told not to use it

# Split the data
set.seed(1000)
selected.var<-c(1,1:18+3)  #take the same variables from Experiment 1
train.index.log<-sample(nrow(Energydata.forlog.High),0.6*nrow(Energydata.forlog.High), replace = FALSE)
train.df.log<-as.data.frame(Energydata.forlog.High[train.index, selected.var])
valid.df.log<-as.data.frame(Energydata.forlog.High[-train.index, selected.var])

set.seed(1000)
Energy.logit<-glm(High~., data = train.df.log, family = "binomial")
options(scipen=999)
summary(Energy.logit)

#Confusion Matrix and Statistics
testresponse<-seq(.1,.9,.1)
i<-1
Energy.logit.cm.acc<-rep(-1,NROW(testresponse))
for(i in 1:NROW(testresponse))
{
set.seed(1000)
Energy.logit.cm<-confusionMatrix(table(predict(Energy.logit, newdata = valid.df.log, type="response") >= testresponse[i], valid.df.log$High == "Yes"))
Energy.logit.cm.acc[i]<-Energy.logit.cm$overall["Accuracy"]
}
#Shows all accuracy models run
Energy.logit.cm.acc

#Picks the most accurate
max(Energy.logit.cm.acc)

#Run the highest accuracy model .4
Energy.logit.cm<-confusionMatrix(table(predict(Energy.logit, newdata = valid.df.log, type="response") >= .4, valid.df.log$High == "Yes"))
Energy.logit.cm #displays the highest accuracy model


################################# Experimentation 2###################################

### WARNING THIS CODE BELOW RUNS BOTH ALPHA EXPERIMENT AND CONVERGENCE THRESHOLDS EXHAUSTIVELY AND WILL TAKE A WHILE TO RUN AS IT RUNS THROUGH 332 MODELS###
### I will provided a CSV of the results just as proof that I ran it called results.csv###
#Experiemental Variables:
alphaExperiment<-seq(.015, .35, by=.004)

#first run alpha c(.001,.01,.05,.1,.2,.3,.4,.5,1)
#second run alpha seq(.15, .35, by=.01)
#Smallest value that makes still doesn't cause huge changes .25

ConvergenceThresholdExperiment<-c(.0000000001,.000000001,.00000001,.0000001)
#first run Threshold c(.001,.01,.05,.1,.2,.3,.4,.5,1) Probably not small enough
#second run Threshold c(.0000000001,.000000001,.00000001,.0000001)


#results vector creation
results<-rep(NA,NROW(b_all)+4)

a<-1
th<-1

##LOOPING for Alpha and Convergence Thresholds use formulas below to 
for(a in 1:NROW(alphaExperiment)) #for alpha testing
{
for(th in 1:NROW(ConvergenceThresholdExperiment)) #for Convergence Threshold Testing
{
  #Picking variables
  xi<-as.matrix(train.df[,2:NCOL(train.df)])
  yi<-as.matrix(train.df[,1]) #Appliances
  
  xi<-as.matrix(train.df[,2:NCOL(train.df)])
  yi<-as.matrix(train.df[,1]) #Appliances
  
  #Creating/Initializing variables
  m<-as.numeric(NROW(xi)) #num of observations
  xi<- cbind(rep(1,m), xi)
  b_all <- matrix(c(1,1),ncol(xi),1)
  alpha<-alphaExperiment[a]
  ConvergenceThreshold<-ConvergenceThresholdExperiment[th]
  itr<-2000
  
  
  
  #initial cost function calculation
  cf <- (1/(2*m)) * t(xi %*% b_all - yi) %*% (xi %*% b_all - yi)
  cf_all<-cf
  
  
  #Gradient descent
  i <- 0
  cf_diff<-10
  
  while(cf_diff>ConvergenceThreshold){
    i <- i + 1
    
    #if we hit iteration limit break out of loop
    if(i==itr){print(paste("Iterations limit reached")); break;}
    
    #calculating yhat
    yhat <- xi %*% b_all
    
    #calculating b's
    b_all <- b_all - (alpha *(1/ m)) * (t(xi) %*% (yhat - yi))
    
    #calculating new cf
    cf_new<-(1/(2*m)) * t(xi %*% b_all - yi) %*% (xi %*% b_all - yi)
    
    #keeping a list of all cf
    cf_all <- append(cf_all, cf_new)
    
    #checking for convergence
    cf_diff <- abs(cf_all[i+1] - cf_all[i])
    
    #checking for non-convergence
    if((cf_all[i+1] - cf_all[i]) > 0){
      print(paste("Alpha too big? Alpha Value:",alpha)); break;
    }
  }
  
  
  b_all
  
  #Creating a results vector
  if(is.na(results[1])){results<-results<-rbind(results,c(alpha,i, itr,cf_new,ConvergenceThreshold,b_all)); results<-results[-1,]
  }else{results<-results<-rbind(results,c(alpha,i, itr,cf_new,ConvergenceThreshold,b_all))}
  
}
  print(a/NROW(alphaExperiment)) #percentage completion printout to see that it is still working
}
results<-as.data.frame(results)
names(results)<-list("alpha","iterations", "max iterations","cf_new","ConvergenceThreshold", "b0", "b1", "...")


#plot for Iterations vs Convergence Threshold
plot(results$iterations,results$cf_new, type ="b", xlab = "iterations", ylab = "Cost Function")

#plot for Iterations vs Cost Function
plot(results$iterations,results$cf_new, type ="b", xlab = "iterations", ylab = "Cost Function")

#plot for Iterations vs Cost Function Subset to zoom in on < 10
t<-results$iterations
t<-as.data.frame(t)
t[,2]<-results$cf_new
t<-subset(t,V2<10)
min(t$V2) #wondering if the exhaustive search is worth it

plot(t[,1],t[,2], type ="b", xlab = "iterations", ylab = "Cost Function")


#Results are in this order:"alpha","iterations", "max iterations","cf_new","ConvergenceThreshold", "b0", "b1", "b..."
View(results)
write.csv(results,"results.csv")



################################# Experiment 3##################################################################
setwd("C:/Users/Wings2N/Desktop/School/Fall 2019/Applied Machine Learning BUAN 6341/Assignment 1")
Energydata<-read.csv("energydata_complete.csv")


#Data input
Energydata<-Energydata[,-1] #removing date as told not to use it
Energydata.scale<-scale(Energydata)


#Correlation plot shows that I should not use rv1, rv2, windspeed, appliances, lights, Visibility or the first two
set.seed(1000)
random_10<-sample(2:NCOL(Energydata),10,replace = FALSE)
selected.var<-c(1,random_10)  #Random Selected Variables
train.index<-sample(nrow(Energydata.scale),0.6*nrow(Energydata.scale), replace = FALSE)
train.df<-as.data.frame(Energydata.scale[train.index, selected.var])
valid.df<-as.data.frame(Energydata.scale[-train.index, selected.var])


#Experiemental Variables:
alphaExperiment<-c(.001,.01,.05,.1,.2,.3,.4,.5,1)

#first run alpha c(.001,.01,.05,.1,.2,.3,.4,.5,1)
#second run alpha seq(.15, .35, by=.01)
#Smallest value that makes still doesn't cause huge changes .25

ConvergenceThresholdExperiment<-c(.001,.01,.05,.1,.2,.3,.4,.5,1) 
#first run Threshold c(.001,.01,.05,.1,.2,.3,.4,.5,1) Probably not small enough
#second run Threshold c(.0000000001,.000000001,.00000001,.0000001)


#results vector creation
results<-rep(NA,NROW(b_all)+4)

a<-1
th<-1

##LOOPING for Alpha and Convergence Thresholds use formulas below to 
for(a in 1:NROW(alphaExperiment)) #for alpha testing
{
  for(th in 1:NROW(ConvergenceThresholdExperiment)) #for Convergence Threshold Testing
  {
    #Picking variables
    xi<-as.matrix(train.df[,2:NCOL(train.df)])
    yi<-as.matrix(train.df[,1]) #Appliances
    
    xi<-as.matrix(train.df[,2:NCOL(train.df)])
    yi<-as.matrix(train.df[,1]) #Appliances
    
    #Creating/Initializing variables
    m<-as.numeric(NROW(xi)) #num of observations
    xi<- cbind(rep(1,m), xi)
    b_all <- matrix(c(1,1),ncol(xi),1)
    alpha<-alphaExperiment[a]
    ConvergenceThreshold<-ConvergenceThresholdExperiment[th]
    itr<-2000
    
    
    
    #initial cost function calculation
    cf <- (1/(2*m)) * t(xi %*% b_all - yi) %*% (xi %*% b_all - yi)
    cf_all<-cf
    
    
    #Gradient descent
    i <- 0
    cf_diff<-10
    
    while(cf_diff>ConvergenceThreshold){
      i <- i + 1
      
      #if we hit iteration limit break out of loop
      if(i==itr){print(paste("Iterations limit reached")); break;}
      
      #calculating yhat
      yhat <- xi %*% b_all
      
      #calculating b's
      b_all <- b_all - (alpha *(1/ m)) * (t(xi) %*% (yhat - yi))
      
      #calculating new cf
      cf_new<-(1/(2*m)) * t(xi %*% b_all - yi) %*% (xi %*% b_all - yi)
      
      #keeping a list of all cf
      cf_all <- append(cf_all, cf_new)
      
      #checking for convergence
      cf_diff <- abs(cf_all[i+1] - cf_all[i])
      
      #checking for non-convergence
      if((cf_all[i+1] - cf_all[i]) > 0){
        print(paste("Alpha too big? Alpha Value:",alpha)); break;
      }
    }
    
    
    b_all
    
    #Creating a results vector
    if(is.na(results[1])){results<-results<-rbind(results,c(alpha,i, itr,cf_new,ConvergenceThreshold,b_all)); results<-results[-1,]
    }else{results<-results<-rbind(results,c(alpha,i, itr,cf_new,ConvergenceThreshold,b_all))}
    
  }
  print(a/NROW(alphaExperiment)) #percentage completion printout to see that it is still working
}
results<-as.data.frame(results)
names(results)<-list("alpha","iterations", "max iterations","cf_new","ConvergenceThreshold", "b0", "b1", "...")


#plot for Iterations vs Convergence Threshold
plot(results$iterations,results$cf_new, type ="b", xlab = "iterations", ylab = "Cost Function")

#plot for Iterations vs Cost Function
plot(results$iterations,results$cf_new, type ="b", xlab = "iterations", ylab = "Cost Function")

#Results are in this order:"alpha","iterations", "max iterations","cf_new","ConvergenceThreshold", "b0", "b1", "b..."
View(results)
write.csv(results,"results.csv")

