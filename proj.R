print(getwd())
setwd("C:/Users/shashank/Downloads")
library(syuzhet)
library(ggplot2)
library(tm)
library(wordcloud)
library(compare)
texts=readLines("WhatsApp Chat with Abhishek Shukla(Bm).txt")
sentiment=get_nrc_sentiment(texts)
sentiment
text=cbind(texts,sentiment)
texts2=readLines("WhatsApp Chat with Sagar Kumar.txt")
sentiment2=get_nrc_sentiment(texts2)
text2=cbind(texts2,sentiment2)
Totalsentiment2=data.frame(colSums(text[,c(2:11)]),colSums(text2[,c(2:11)]))
colnames(Totalsentiment2)=c("chat1","chat2")
Totalsentiment2=cbind("sentiment"=rownames(Totalsentiment2),Totalsentiment2)
rownames(Totalsentiment2)=NULL
Totalsentiment2
values=c(Totalsentiment2[,2],Totalsentiment2[,3])
values
ggplot(data=Totalsentiment2,aes(x=sentiment,y=values))+geom_bar(stat = "identity",position=position_dodge())+xlab("sentiment")+ylab("Total count")+ggtitle("total sentiment score")
