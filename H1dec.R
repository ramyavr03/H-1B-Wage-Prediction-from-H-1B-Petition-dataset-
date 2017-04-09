#install.packages('e1071')
library (e1071)
#install.packages('caTools')
library (caTools)
#install.packages("caret")
library(caret)
#install.packages('xlsx')
library(xlsx)




dataset = read.csv('h1data.csv',header=TRUE)
dataset1<-dataset[!(dataset$employer_name=="N/A"| dataset$employer_name ==""|dataset$employer_state =="" |dataset$prevailing_wage==0),]
dataset2<-dataset1[!(dataset1$soc_code=="N/A"),]
keep<-c("case_status","visa_class","employer_name","employer_state","job_title","soc_code","prevailing_wage",
        "pw_unit_of_pay","h1b_dependent")
df=dataset2[keep]


#a<-df[df$job_title=="CHIEF EXECUTIVE OFFICER",]

#k<- df[grepl("QUALITY ASSURANCE MANAGER", df$job_title), ]
J <- df[df$visa_class %in% c("H-1B"),]

k<-J[J$job_title %in% c( "PROGRAM ANALYST","SOFTWARE ENGINEER","BUSINESS ANALYST","PROGRAMMER ANALYST","SOFTWARE DEVELOPER","APPLICATION DEVELOPER","SYSTEMS ENGINEER","SYSTEM ENGINEER","IT CONSULTANT","TECHNICAL CONSULTANT","DATA SCIENTIST","DATA ANALYST","COMPUTER AND INFORMATION SYSTEMS MANAGER","COMPUTER AND INFORMATION SYSTEM MANAGER","COMPUTER INFORMATION SYSTEM MANAGER","COMPUTER INFORMATION SYSTEMS MANAGER","COMPUTER PROGRAMMER","TEST ANALYST","QA ANALYST","NETWORK ENGINEER","PROJECT MANAGER"), ]
#l<-df[df$job_title %in% c("PROJECT MANAGER"), ]
k <- transform(k,prevailing_wage = ifelse( pw_unit_of_pay == 'Week',(prevailing_wage*52),prevailing_wage))
#k <- within(k,prevailing_wage[pw_unit_of_pay == 'Week'] <- (prevailing_wage[pw_unit_of_pay=='Week']*52))
k<-within(k,pw_unit_of_pay[pw_unit_of_pay=='Week']<-'Year')
k <- transform(k,prevailing_wage = ifelse( pw_unit_of_pay == 'Hour',(prevailing_wage*8*5*52),prevailing_wage))
k<-within(k,pw_unit_of_pay[pw_unit_of_pay=='Hour']<-'Year')
k <- transform(k,prevailing_wage = ifelse( pw_unit_of_pay == 'Month',(prevailing_wage*12),prevailing_wage))
k<-within(k,pw_unit_of_pay[pw_unit_of_pay=='Month']<-'Year')

keep1<-c("employer_name","employer_state","job_title","soc_code","prevailing_wage")
k=k[keep1]

k <-within(k,job_title[job_title =="SYSTEMS ENGINEER"] <- "SYSTEM ENGINEER")
k <-within(k,job_title[job_title =="COMPUTER INFORMATION SYSTEM MANAGER"] <- "COMPUTER AND INFORMATION SYSTEMS MANAGER")
k <-within(k,job_title[job_title =="COMPUTER SYSTEMS MANAGER"] <- "COMPUTER AND INFORMATION SYSTEMS MANAGER")
k <-within(k,job_title[job_title =="COMPUTER AND INFORMATION SYSTEM MANAGER"] <- "COMPUTER AND INFORMATION SYSTEMS MANAGER")
k <-within(k,job_title[job_title =="COMPUTER INFORMATION SYSTEMS MANAGER"] <- "COMPUTER AND INFORMATION SYSTEMS MANAGER")
#k <-within(k,prevailing_wage[prevailing_wage < 32000] <- 1)
#k <-within(k,prevailing_wage[prevailing_wage >=32000] <- 2)
k <- within(k, {prevailing_wage1 = ifelse(prevailing_wage <32000,1,2)})


new1 <- k
write.csv(k,"mydata2.csv")
set.seed(80)
split = sample.split(new1$prevailing_wage, SplitRatio = 0.80)
training_set = subset(new1, split == TRUE)
training_set
test_set = subset(new1, split == FALSE)
#feature scaling
#raining_set[-6] = scale (training_set[-6])
#est_set[-6] =scale(test_set[-6])
training_set$prevailing_wage=as.factor(training_set$prevailing_wage)
write.csv(training_set,"mydatad.csv")
write.csv(test_set,"mydataTestd.csv")
#install.packages('rpart')
library(rpart)
datasetTrain2 = read.csv('mydatad.csv',header=TRUE)
keep<-c("employer_name","employer_state","job_title","soc_code","prevailing_wage1")
dsTrain1=datasetTrain2[keep]
dsTrain1$prevailing_wage1=as.factor(dsTrain1$prevailing_wage1)
#classifier = rpart(x = dsTrain1[-5],y = dsTrain1$prevailing_wage1)
classifier = rpart(formula = dsTrain1$prevailing_wage1~.,data=dsTrain1)
 
test_pred1 <-predict(classifier,newdata =dsTrain1[-5],type = 'class')
cm1 = table(dsTrain1[, 5], test_pred1)
cm1
(accuracy <- sum(diag(cm1)) / sum(cm1))
results <-cbind(datasetTrain2,test_pred1)
result1 <- results[(results$test_pred1==1)|(results$test_pred1==2& results$prevailing_wage1==1),]
results1Remaining <- results[!(results$test_pred1==1),]
results1Remaining <- within(results1Remaining, {prevailing_wage1 = ifelse(prevailing_wage <90000,2,3)})
keep<-c("employer_name","employer_state","job_title","soc_code","prevailing_wage1")
dsTrain2=results1Remaining[keep]
dsTrain2$prevailing_wage1=as.factor(dsTrain2$prevailing_wage1)
classifier2 = rpart(formula=dsTrain2$prevailing_wage1~.,data=dsTrain2)
test_pred2 <-predict(classifier2,newdata =dsTrain2[-5],type='class')
cm2 = table(dsTrain2[, 5], test_pred2)
cm2
(accuracy <- sum(diag(cm2)) / sum(cm2))
results2 <-cbind(results1Remaining,test_pred2)
results2$test_pred1 <- NULL
library(plyr)
rename(results2,c("test_pred2"="test_pred1"))
names(results2)[names(results2)=="test_pred2"] <- "test_pred1"
final = rbind(result1,results2)
cm3 = table(final[, 7], final$test_pred1)
cm3
(accuracy <- sum(diag(cm3)) / sum(cm3))
























