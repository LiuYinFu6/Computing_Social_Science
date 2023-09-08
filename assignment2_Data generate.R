##更改URL
Url <-"C:/Users/yinfu/Desktop/研一下/social data science/exam/project2/data/MrTrumpSpeeches.csv"
TrumpData <-read.csv(file=Url,header=T,sep='~')
TrumpData2<-TrumpData[-grep("music", TrumpData$subtitles),]
TrumpData3<-TrumpData2[-grep("Applause", TrumpData2$subtitles),]
TrumpData2<-data.frame(TrumpData2)
TrumpData3<-data.frame(TrumpData3)

# Splitting the dataset into the Training set and Test set 
library(caTools) 
set.seed(123) 
split = sample.split(TrumpData3, SplitRatio = 0.6) 
training_set = subset(TrumpData3, split == TRUE) 
#test_set = subset(dataset, split == FALSE) 
TrumpData1<- data.frame(training_set$title,training_set$upload_date,training_set$subtitles)
names(TrumpData1)[1] <-"title"
names(TrumpData1)[2] <-"date"
names(TrumpData1)[3]<-"content"
TrumpData1$Party<-"R"
library(readxl)

##更改URL
Url2 <-"C:/Users/yinfu/Desktop/研一下/social data science/exam/project2/data/obama_speeches_dataframe.xlsx"
ObamaData <-read_excel(Url2)
ObamaData <-ObamaData [,-1]
ObamaData$Party<-"D"

#更改URL
url3<-"C:/Users/yinfu/Desktop/研一下/social data science/exam/project2/data/BUSH_speech.csv"
BushData1 <-read.csv(url3)
BushData1<-BushData1[,-1]
BushData1<-BushData1[,-4]
BushData1<-BushData1[,-4]
BushData1$Party<-'R'
length(BushData1)
result<- c(1:97)

for(i in 1:97){
  BushData1$content[i] = substring(BushData1$content[i], 300)
}





FullDataSet1<-rbind(TrumpData1,ObamaData)
FullDataSet<-rbind(FullDataSet1,BushData1)
#更改URL
write.csv(x = FullDataSet,file  = "C:/Users/yinfu/Desktop/研一下/social data science/exam/project2/data/FullData1.csv")
url3<-"C:/Users/yinfu/Desktop/研一下/social data science/exam/project2/data/FullData.csv"
s1 <-read.csv(url3)
