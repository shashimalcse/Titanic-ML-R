train<-read.csv('train.csv',header = TRUE)
test<-read.csv('test.csv',header = TRUE)
test.Survived<-data.frame(Survived=rep("None",nrow(test)),test[,])

data.combined<-rbind(train, test.Survived)

str(data.combined)

data.combined$Survived<-as.factor(data.combined$Survived)
data.combined$Pclass<-as.factor(data.combined$Pclass)

table(data.combined$Survived)

table(data.combined$Pclass)

library(ggplot2)

train$Pclass<-as.factor(train$Pclass)
ggplot(train,aes(x=Pclass,fill=factor(Survived)))+
         geom_bar(width=0.5)+
         xlab("Pclass")+
         ylab("Total Count")+
         labs(fiil="Survived")
length(unique(as.character(data.combined$Name)))

dup.names<-as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

library(stringr)

misses<- data.combined[which(str_detect(data.combined$Name,"Miss.")),]
misses[1:5,]

mrses<- data.combined[which(str_detect(data.combined$Name,"Mrs.")),]
mrses[1:5,]

males<- data.combined[which(train$Sex=="male"),]
males[1:5,]

extractTitle<-function(name){
  name<-as.character(name)
  if(length(grep("Miss.",name))>0){
    return("Miss.")
  }
  else if(length(grep("Master.",name))>0){
    return("Master.")
  }
  else if(length(grep("Mrs.",name))>0){
    return("Mrs.")
  }
  else if(length(grep("Mr.",name))>0){
    return("Mr.")
  }
  else{
    return("Other")
  }
  
}
titles<-NULL
for(i in 1:nrow(data.combined)){
  titles<-c(titles,extractTitle(data.combined[i,"Name"]))
}
data.combined$Titles<-as.factor(titles)

ggplot(data.combined[1:891,],aes(x=Titles,fill=Survived))+
         geom_bar(width = 0.5)+
         facet_wrap(~Pclass)+
         ggtitle("Pclass")+
         xlab("Title")+
         ylab("Total Count")+
         labs(fill="Survived")

ggplot(data.combined[1:891,],aes(x=Sex,fill=Survived))+
        geom_bar(width=0.5)+
        facet_wrap(~Pclass)+
        ggtitle("Pclass")+
        xlab("Sex")+
        ylab("Total Count")+
        labs(fill="Survived")
        
 summary(data.combined[1:891,"Age"]) 

 ggplot(data.combined[1:891,],aes(x=Age,fill=Survived))+
   geom_histogram(binwidth=10)+
   facet_wrap(~Sex+Pclass)+
   xlab("Age")+
   ylab("Total Count")
   
         
boys<-data.combined[which(data.combined$Titles =="Master."),] 
summary(boys$Age)

ggplot(misses[misses$Survived != "None",],aes(x=Age,fill=Survived))+
  geom_histogram(binwidth=5)+
  facet_wrap(~Pclass)+
  ggtitle("Age for Miss by Pclass")
  xlab("Age")+
  ylab("Total Count")
 
misses.alone<-misses[which(misses$SibSp==0 & misses$Parch==0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <=14.5))

summary(data.combined$SibSp)
  
length(unique(data.combined$SibSp))

data.combined$SibSp<-as.factor(data.combined$SibSp)

ggplot(data.combined[1:891,],aes(x=SibSp,fill=Survived))+
  geom_bar(width = 1)+
  facet_wrap(~Pclass+Titles)+
  ggtitle("Pclass,Title")+
  xlab("Sibsp")+
  ylab("Total Count")+
  ylim(0,300)
  labs(fill="Survived")
  
data.combined$Parch<-as.factor(data.combined$Parch)
  
ggplot(data.combined[1:891,],aes(x=Parch,fill=Survived))+
    geom_bar(width = 1)+
    facet_wrap(~Pclass+Titles)+
    ggtitle("Pclass,Title")+
    xlab("Parch")+
    ylab("Total Count")+
    ylim(0,300)
  labs(fill="Survived")  

temp.Sibsp<-c(train$SibSp,test$SibSp)
temp.Parch<-c(train$Parch,test$Parch)

data.combined$Family.size<-as.factor(temp.Parch+temp.Sibsp+1)

ggplot(data.combined[1:891,],aes(x=Family.size,fill=Survived))+
  geom_bar(width = 1)+
  facet_wrap(~Pclass+Titles)+
  ggtitle("Pclass,Title")+
  xlab("Family.size")+
  ylab("Total Count")+
  ylim(0,300)
labs(fill="Survived")   

str(data.combined$Ticket)  
  
      
  
