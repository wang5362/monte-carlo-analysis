#Monte Carlo: Methodology
#STEP 1: declare parameters maximum and minimum values
#r ; Vl ; Vh; a ;ml ;mh; theta; e; t; i
options(scipen=5)
options(digits=5)
library(readxl)
setwd("~/Desktop/ECON/ECON 4831BCA")
data=read_xls('EasyValues.xls')
print(data)
# declare variables with values and max and min 

#STEP 2: generate random values for each parameter
table<-vector()
z<-runif(1000,min=0, max=1)
r<- runif(1000,min=0.04, max=0.08)
vl<-runif(1000,min=0.03,max=0.07)
vh<-runif(1000,min=0.4,max=0.8)
a<-runif(1000,min=0.01,max=0.05)
ml<-runif(1000,min=0.000025,max=0.000075)
mh<- runif(1000,min=0.0005,max=0.002)
theta<-runif(1000,min=0.5,max=1)
e<-runif(1000,min=0.65,max=0.85)
t<-runif(1000,min=18,max=30)
i<-runif(1000,min=0.2,max=0.3)

#STEP3 & 4:
#Compute Vh, Vl,v
x<-data$Value
Vh<- x[1]*vh*r
Vl<-vl*(1-r)*x[1]
v<-(r*vh)+(vl*(1-r))

#Compute Ca,Cs,Cenv,Cev
Ca<-(x[15]+(Vh+Vl)*x[14])/1000000 
Cs <- (a*(Vh+Vl)*((x[16]*t)+(mh*x[17])))/1000000
Cenv<- (i*{(r*x[1])*(x[16]*t+mh*x[17])+((1-r)*x[1]*(x[16]*t+ml*x[17]))})/1000000
Cev<- {(i-(theta*v*e))*((r*x[1]-e*Vh)*(x[16]*t +mh*x[17])+((1-r)*x[1]-e*Vl)*(x[16]*t +ml*x[17]))}/1000000

#Compute 3 possible NBs
NB_neither= -(Ca+Cs)
NB_p1= -(Ca+Cs)+(Cenv-Cev)
NB_p2= -(Ca+Cs)+((Cenv-Cev)/1+0.05)

table11<-cbind(r,vl,vh,a,ml,mh,theta,e,t,i, NB_neither,NB_p1,NB_p2)
#STEP 5 & 6: generate occurence of epidemy and find NB
p1<- runif(1000,min=0, max=1) #generate random p1
p2<- runif(1000,min=0, max=1)
Actual_NB<-vector()
outcome<-vector()
for (j in 1:1000){
if (p1[j]<0.4) {
  Actual_NB <- c(Actual_NB, NB_p1[j])
  outcome<-c(outcome,'p1')
  }
  else if(p1[j]>0.4 & p2[j] <0.2) { 
    Actual_NB <- c(Actual_NB, NB_p2[j])
    outcome<-c(outcome,'P2')}
  else {
    Actual_NB <- c(Actual_NB, NB_neither[j])
    outcome<-c(outcome,'None')
  }
}

table<-data.frame(r,vl,vh,a,ml,mh,theta,e,t,p1,p2,outcome,NB_neither,NB_p1,NB_p2,Actual_NB)
summary(table)
#STEP 7: generate appropriate bin

hist(Actual_NB, breaks=50 ,xlim=c(-20,35),ylim=c(0,100), main="Monte Carlo: Histogram", 
     ylab="# of Trials out of 1000",xlab="Net Benefits in increments($ million)")
