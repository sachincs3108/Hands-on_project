raw_sms<-read.csv("D:/datascience/sms_spam.csv",stringsAsFactors= FALSE)
str(raw_sms)
View(raw_sms)
raw_sms$type

#make it factors so we can count and 
#classify it as an output
raw_sms$type <- factor(raw_sms$type)
raw_sms$type

str(raw_sms$type)
table(raw_sms$type)
install.packages("tm")
library(tm)
str(raw_sms)
typeof(raw_sms$text)
sms_corpus= Corpus(VectorSource((raw_sms$text)))
#corpus- to store set of text documents
#samedatatype therefore vectorsource
print(sms_corpus)
inspect(sms_corpus[1:3])
sms_corpus[1:3]
#cant use print it will print str here

sms_corpus=tm_map(sms_corpus,tolower)
inspect(sms_corpus[1:3])


sms_corpus=tm_map(sms_corpus,removeNumbers)
inspect(sms_corpus[1:3])

sms_corpus=tm_map(sms_corpus,removePunctuation)
inspect(sms_corpus[1:3])
sms_corpus=tm_map(sms_corpus,stripWhitespace)
inspect(sms_corpus[1:3])
stopwords()
sms_corpus=tm_map(sms_corpus,removeWords,stopwords())
inspect(sms_corpus[1:3])

sms_dtm = DocumentTermMatrix(sms_corpus)
View(sms_dtm)
sms_corpus_train1=sms_corpus[1:4169]
sms_corpus_test1=sms_corpus[4170:5547]
install.packages("wordcloud")
library(wordcloud)
wordcloud(sms_corpus_train1,min.freq =10,random.order = FALSE)
sms_dtm_train=sms_dtm[1:4180,]
sms_dtm_test=sms_dtm[4181:5574,]
findFreqTerms(sms_dtm_train,5)
convert_cnts= function(x){
  x=ifelse(x>0,1,0)
  x=factor(x,levels =c(0,1),labels = c("No","Yes"))
}

sms_dict=c(findFreqTerms(sms_dtm_train,5))
sms_dict

#to keep only 5 times occuring frequent terms in train dataset
sms_train=DocumentTermMatrix(sms_corpus_train1,list(dictionary=sms_dict))
View(sms_train)
#to replace count with yes or no for prediction,,
#apply function on matrix,, margin=2 ie. apply on columns, 1=rows
sms_train=apply(sms_train,MARGIN = 2,convert_cnts)
View(sms_train)


sms_test=DocumentTermMatrix(sms_corpus_test1,list(dictionary=sms_dict))
sms_test=apply(sms_test,MARGIN = 2,convert_cnts)
View(sms_test)

install.packages("e1071")
library(e1071)
raw_sms_train=raw_sms[1:4169, ]
raw_sms_test=raw_sms[4169:5547,]
sms_classifier=naiveBayes(sms_train,raw_sms_train$type)
sms_classifier

#now predict
sms_test_predict=predict(sms_classifier,sms_test)
install.packages("gmodels")
library(gmodels)
str(raw_sms_test)
str(sms_test)
CrossTable(sms_test_predict,raw_sms_test$type[1:1378],prop.chisq = FALSE,prop.t = FALSE,dnn=c('predicted','actual'))

