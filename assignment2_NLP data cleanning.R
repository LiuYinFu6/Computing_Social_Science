# read in the libraries we're going to use
library(ldatuning)
library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions
library(SnowballC) # for stemming
library(reticulate)

url3<-"C:/Users/yinfu/Desktop/研一下/social data science/exam/project2/data/FullData1.csv"
FullData<-read.csv(url3)
names(FullData)[1]<-"id"


coupus<-FullData$content

coupus<- iconv(coupus,"WINDOWS-1252","UTF-8")
coupus <- Clean_Text(coupus)
# create a document term matrix to clean

text <- Corpus(VectorSource(coupus)) 
Content <- TermDocumentMatrix(text)
# convert the document term matrix to a tidytext corpus
Content_tidy<-tidy(Content)



#url_stop_words<-"C:/Users/yinfu/Desktop/研一下/social data science/exam/project2/gist_stopwords.txt"
#gist_stop_word<-read.table(url_stop_words, header=F,sep=',')
add_stop_words =c ('like','youre','ive','im','really','id','just','dont','didnt','thi','wa',
                  'say','know','make','people',"today","way","day","time","year",'tonight',
'say','like','just','dont','don','im','it','ve','re','we',
                'live','youll','youve','things','thing','youre','right','really','lot',
                'make','know','people','way','day',
                'little', 'maybe','men',"americans","america",
                'kind','heart', "american","president","united","states" ,"doesn","obama:well","Trump","obama:i","youtube"
,"thatâ","â","."
)
custom_stop_words <- tibble(add_stop_words)
names(custom_stop_words)[1]<-"word"


# remove stopwords
newsDTM_tidy_cleaned <-Content_tidy %>% 
  anti_join(stop_words, by = c("term" = "word")) %>% # remove English stopword
  anti_join(custom_stop_words, by = c("term" = "word")) # remove our own dictionary words

#newsDTM_tidy_cleaned <- Clean_Text(newsDTM_tidy_cleaned)

# reconstruct cleaned documents (so that each word shows up the correct number of times)
stops_documents <- newsDTM_tidy_cleaned %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()
head(stops_documents)
names(stops_documents)[1]<-"stop_document_id"
names(stops_documents)[2]<-"document_stop_word"

# stem the words (e.g. convert each word to its stem, where applicable)
newsDTM_tidy_cleaned <- newsDTM_tidy_cleaned %>% 
  mutate(stem = wordStem(term))

# reconstruct our documents
cleaned_documents <- newsDTM_tidy_cleaned %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(stem, count))) %>%
  select(document, terms) %>%
  unique()
names(cleaned_documents)[1]<-"stemmed_document_id"
names(cleaned_documents)[2]<-"document_stemmed"

FullData_cleaned<-cbind(FullData,cleaned_documents)
FullData_cleaned<-cbind(FullData_cleaned,stops_documents)
FullData_cleaned_R<-subset(FullData_cleaned, Party == "R")
FullData_cleaned_D<-subset(FullData_cleaned, Party == "D")













Clean_Text <- function(text){
  
  # Lowercase
  temp <- tolower(text)
  
  # Remove punctuation
  temp <- gsub("[[:punct:][:blank:]]+", " ",  temp)
  
  # Remove numbers
  temp <- gsub("[[:digit:]]", "",  temp)
  
  # Remove regular urls
  temp <- gsub("(s?)(f|ht)tp(s?)://\\S+\\b", "", temp)
  
  # Remove tiny urls
  temp <- gsub("[A-Za-z]{1,5}[.][A-Za-z]{2,3}/[A-Za-z0-9]+\\b", "", temp)
  
  # Remove the word "review"
  temp <- gsub("review", "", temp)
  
  # Remove the word "review"
  temp <- gsub("reviews", "", temp)
  
  # Remove "mm" symbol
  temp <- gsub(" mm " ,"", temp) 
  
  # Remove "pp" symbol
  temp <- gsub(" pp " ,"", temp) 
  
  # Remove "bb" symbol
  temp <- gsub(" bb " ,"", temp) 
  
  # Remove "q" symbol
  temp <- gsub(" q " ,"", temp) 
  
  # Remove "hn" symbol
  temp <- gsub(" hn " ,"", temp) 
  return(temp)
}
