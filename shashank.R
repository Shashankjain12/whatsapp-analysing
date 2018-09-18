#chat1

print(getwd())
setwd("C:/Users/shashank/Downloads")
library(syuzhet)
library(ggplot2)
library(tm)
library(wordcloud)
library(compare)
library(abind)
library(RColorBrewer)
texts=readLines("WhatsApp Chat with Abhishek Shukla(Bm).txt")
print(texts)
sentiment=get_nrc_sentiment(texts)
print(sentiment)
text=cbind(texts,sentiment)
Totalsentiment=data.frame(colSums(text[,c(2:11)]))
Totalsentiment
names(Totalsentiment)="count"
Totalsentiment=cbind("sentiment"=rownames(Totalsentiment),Totalsentiment)
rownames(Totalsentiment)=NULL
ggplot(data=Totalsentiment,aes(x=sentiment,y=count))+geom_bar(aes(fill=sentiment),stat = "identity")+xlab("sentiment1")+ylab("Total count")+ggtitle("total sentiment score")




#wordcloud of chat1
modi<-Corpus(VectorSource(texts))
modi_data<-tm_map(modi,stripWhitespace)
modi_data<-tm_map(modi_data,tolower)
modi_data<-tm_map(modi_data,removeNumbers)
modi_data<-tm_map(modi_data,removePunctuation)
modi_data<-tm_map(modi_data,removeWords, stopwords(kind = "en"))
words<-c(and,the,our,that,are,also,more,has,must,have,should,this,with)
modi_data<-tm_map(modi_data,removeWords,words)
tdm_modi<-TermDocumentMatrix (modi_data) #Creates a TDM
TDM1<-as.matrix(tdm_modi) #Convert this into a matrix format
v = sort(rowSums(TDM1), decreasing = TRUE) #Gives you the frequencies for every word
summary(v)
wordcloud (modi_data, scale=c(2.5,0.5), max.words=50, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Paired"))



#chat2
texts2=readLines("WhatsApp Chat with Sagar kumar.txt")
print(texts2)
sentiment2=get_nrc_sentiment(texts2)
print(sentiment2)
text2=cbind(texts2,sentiment2)
print(text2)
Totalsentiment2=data.frame(colSums(text2[,c(2:11)]))
Totalsentiment2
names(Totalsentiment2)="count"
Totalsentiment2
rownames(Totalsentiment2)
Totalsentiment2=cbind("sentiment2"=rownames(Totalsentiment2),Totalsentiment2)
Totalsentiment2
rownames(Totalsentiment2)=NULL
Totalsentiment2
ggplot(data=Totalsentiment2,aes(x=sentiment2,y=count))+geom_bar(aes(fill=sentiment2),stat = "identity")+xlab("sentiment2")+ylab("Total count")+ggtitle("total sentiment score")




#wordcloud of chat 2
modi<-Corpus(VectorSource(texts2))
modi_data<-tm_map(modi,stripWhitespace)
modi_data<-tm_map(modi_data,tolower)
modi_data<-tm_map(modi_data,removeNumbers)
modi_data<-tm_map(modi_data,removePunctuation)
modi_data<-tm_map(modi_data,removeWords, stopwords(kind = "en"))
words<-c(and,the,our,that,are,also,more,has,must,have,should,this,with)
modi_data<-tm_map(modi_data,removeWords,words)
tdm_modi<-TermDocumentMatrix (modi_data) #Creates a TDM
TDM1<-as.matrix(tdm_modi) #Convert this into a matrix format
v = sort(rowSums(TDM1), decreasing = TRUE) #Gives you the frequencies for every word
summary(v)
wordcloud (modi_data, scale=c(2,0.5), max.words=50, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))




#average of both chats
c4=c("A","B","C","D","E","F","G","H","I","J")
temp_array <- abind(Totalsentiment[2],Totalsentiment2[2],along=2)
temp_array
a=rowMeans(temp_array,dims = 1)
b=as.integer(a)
avgsentiments1=data.frame(b)
avgsentiments1=cbind("avgsentiments"=Totalsentiment2[1],b)
avgsentiments1
rownames(avgsentiments1)=NULL
if(avgsentiments1[9,2]<5){
ggplot(data=avgsentiments1,aes(x=sentiment2,y=b))+geom_bar(aes(fill=sentiment2),stat = "identity")+xlab("average sentiments")+ylab("Total count")+ggtitle("average sentiment score")
}else{
  ggplot(data=avgsentiments1,aes(x=sentiment2,y=b,fill=c4))+geom_bar(stat = "identity")+xlab("average sentiments")+ylab("Total count")+ggtitle("average sentiment score")+scale_fill_manual("legend", values = c("A" = "green", "B" = "orange", "C" = "purple","D"="blue","E"="yellow","F"="black","G"="magenta","H"="blue","I"="red","J"="pink"))
}
