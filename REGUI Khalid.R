setwd("C:/Users/kh/Desktop/REGUI Khalid")
install.packages("readxl")

#1
library(readxl)
data <- read_excel("CHD.xlsx")
head(data)
dim(data)

#2
is.factor(data$chd)
is.factor(data$famhist)
data$chd = as.factor(data$chd)
data$famhist = as.factor(data$famhist)

#3
library(Amelia)
missmap(data)
data = na.omit(data)
missmap(data)

#4
#var----age
#indicateurs statistiques
mean(data$age,na.rm = TRUE)
median(data$age,na.rm = TRUE)
sd(data$age,na.rm = TRUE)
var(data$age,na.rm = TRUE)
quantile(data$age,na.rm = TRUE)
IQR(data$age,na.rm = TRUE)
range(data$age,na.rm = TRUE)
library(e1071)
skewness(data$age,na.rm = TRUE)  
kurtosis(data$age,na.rm = TRUE)

#représentation graphique :
#hist des effectifs
hist(data$age,col="yellow",xlab = "Age",ylab = "n")
#hist des frequences
hist(data$age,col="red",xlab = "Age",ylab = "f",main = "Distribution de la mantant",probability = T)
boxplot(data$age,col="green",main="boit à moustache de la montant")

#var chd
table(data$chd)
library(questionr)
freq(data$chd)

barplot(table(data$chd),col=c("green","red"),main="Digramme en bâtons",ylab ="n")
pie(table(data$chd),col=c("blue","black"),main="Digramme en secteur")

#5
boxplot(data$ldl)
boxplot.stats(data$ldl)$out
q=quantile(data$ldl,probs=c(0.25,0.75),na.rm=TRUE)
a=q[2]+1.5*IQR(data$ldl,na.rm=TRUE) 
b=q[1]-1.5*IQR(data$ldl,na.rm=TRUE)
data$ldl[data$ldl>a]=b
data$ldl[data$ldl<b]=a
boxplot.stats(data$ldl)$out
boxplot(data$ldl)

#6 
qqnorm(data$ldl)
qqline(data$ldl)
qqnorm(data$age)
qqline(data$age)
shapiro.test(data$ldl)
shapiro.test(data$age)


#7
#---------test de correlation entre deux var non normaux disrtrubier 
cor.test(data$ldl,data$age,method="spearman")


#8
table(data$famhist, data$chd)
barplot(table(data$famhist, data$chd) ,beside = T , col=c("red","yellow"))

#9
chisq.test(data$famhist, data$chd)

#10
#11
library(dplyr)
data%>%group_by(data$chd)%>%summarise(mean(data$age),median(data$age),sd(data$age))
boxplot(data$age~data$chd)

#12
t.test(data$age~data$chd)
t.test(data$age~data$chd, mu =0, alternative = "two.sided", paired = FALSE, conf.level = 0.95, var.equal = TRUE)



