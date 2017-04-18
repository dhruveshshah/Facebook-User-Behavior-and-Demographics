#Understand Facebook user behaviour and demographics

pf<-read.csv("pseudo_facebook.tsv",sep = "\t")
names(pf)

class(pf$dob_day)

#Converting dob_day to factor
pf$dob_day<-as.factor(pf$dob_day)
names(pf)

#Histogram of users birthday
install.packages("ggplot2")
library(ggplot2)

qplot(x=dob_day,data=pf) +
  scale_x_discrete(breaks=1:31)
# To divide the histogram in 12 months
qplot(x=dob_day,data = pf) +
  scale_x_discrete(breaks=1:31)+
  facet_wrap(~dob_month,ncol = 3)

#Histogram of users friend_count and limiting the axes
qplot(x = friend_count,data=pf, bins=30) + 
  scale_x_continuous(limits=c(0,1000),breaks = seq(0,1000,50)) + 
  facet_wrap(gender ~ .) 

#Removing NA values
qplot(x = friend_count,data=subset(pf, !is.na(gender)), bins=30) + 
  scale_x_continuous(limits=c(0,1000),breaks = seq(0,1000,50)) + 
  facet_wrap(~gender) 

#Statistics by gender

table(pf$gender)
by(pf$friend_count,pf$gender,summary)

#Number of days user has been on Facebook (Tenure in days)
qplot(x=tenure,data=pf,binwidth=30,
      color=I('black'),fill=I('#99DD9'))

#Tenure in Years
qplot(x=tenure/365,data=pf,binwidth=0.25,
      x="Number of years using Facebook",
      y="Number of users in sample",
      color=I('black'),fill=I('#99DD9')) +
  scale_x_continuous(breaks = seq(1,7,1),lim=c(0,7))

# Histogram of User Ages
qplot(x=age,data=pf,binwidth=1,
      color=I("black"),fill=I('#5760AB'))
scale_x_discrete(breaks=seq(0,113,5))

qplpot(x=friend_count,data=pf)
#Checking for different transformation
summary(pf$friend_count)
summary(log10(pf$friend_count+1))
summary(sqrt(pf$friend_count))

install.packages("gridExtra")
library(gridExtra)
#Comparing both the transformations
p1<-ggplot(aes(x=friend_count), data=pf) +geom_histogram()
q1<-p1+scale_x_log10()
q2<-p1+scale_x_sqrt()

grid.arrange(p1,q1,q2,ncol=1)

#Comparing values using log10 and scale_x_log10
q4<-qplot(x=log10(friend_count),data=pf)
q5<-ggplot(aes(x=friend_count),data=pf)+
  geom_histogram()+
  scale_x_log10()

#Frequency polygon
qplot(x=friend_count,y = ..count../sum(..count..), data = subset(pf,!is.na(gender)),
      xlab="Friend_Counts",
      ylab="Proportion of Users with that Friend Count",
      binwidth=10,geom="freqpoly",color=gender)+
  scale_x_continuous(lim=c(0,1000),breaks = seq(0,1000,100))

#Frequency Polygon for number of likes
qplot(x=www_likes,data=subset(pf,!is.na(gender)),
      geom="freqpoly",colour=gender)+
  scale_x_continuous()+
  scale_x_log10()

#Who gets more likes
by(pf$www_likes,pf$gender,sum)

#Box Plot
qplot(x=gender,y=friend_count,data=subset(pf,!is.na(gender)),
      geom = "boxplot")+
coord_cartesian(ylim = c(0,250))

by(pf$friend_count,pf$gender,summary)

qplot(x=gender,y=friendships_initiated,
       data=subset(pf,!is.na(gender)),geom="boxplot")+
  coord_cartesian(ylim = c(0,250))

#Did users use the mobile checkin feature
mobile_in<-NA
pf$mobile_in<-ifelse(pf$mobile_likes>0,1,0)
pf$mobile_in<-factor(pf$mobile_in)
summary(pf$mobile_in)

