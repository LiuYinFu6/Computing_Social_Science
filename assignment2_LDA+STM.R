#########################
#LDA with only stopwords#
########################
library(quanteda)
library(seededlda)
corp_party <- corpus(FullData_cleaned_D, text_field = "document_stop_word")
token_corp_party_no_stopsword_D <- tokens(corp_party)
dfmat_party_lda <- dfm(token_corp_party_no_stopsword) %>% 
  dfm_trim(min_docfreq = 0.01, max_docfreq = 0.8, docfreq_type = "prop")
party_lda <- textmodel_lda(dfmat_party_lda, k = 7)
terms(party_lda, 20)
job_topics <- topics(party_lda)

corp_party <- corpus(FullData_cleaned_R, text_field = "document_stop_word")
token_corp_party_no_stopsword_R <- tokens(corp_party)
dfmat_party_lda <- dfm(token_corp_party_no_stopsword) %>% 
  dfm_trim(min_docfreq = 0.01, max_docfreq = 0.8, docfreq_type = "prop")
party_lda <- textmodel_lda(dfmat_party_lda, k = 7)
terms(party_lda, 20)
job_topics <- topics(party_lda)


#########################
#LDA with tokennixed stopwords#
########################
#D
tokens = word_tokenizer(token_corp_party_no_stopsword_D)
it = itoken(tokens, progressbar = FALSE)
v = create_vocabulary(it)
v = prune_vocabulary(v, term_count_min = 30, doc_proportion_max = 0.2)

vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it, vectorizer, type = "dgTMatrix")

lda_model_D = LDA$new(n_topics = 10, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr_D = 
  lda_model_D$fit_transform(x = dtm, n_iter = 2500, 
                          convergence_tol = 0.001, n_check_convergence = 25, 
                          progressbar = FALSE)
lda_model_D$get_top_words(n = 20, lambda = 0.7)


#R
tokens = word_tokenizer(token_corp_party_no_stopsword_R )
it = itoken(tokens, progressbar = FALSE)
v = create_vocabulary(it)
v = prune_vocabulary(v, term_count_min = 30, doc_proportion_max = 0.2)

vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it, vectorizer, type = "dgTMatrix")

lda_model_R = LDA$new(n_topics = 10, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr_R = 
  lda_model_R$fit_transform(x = dtm, n_iter = 2500, 
                          convergence_tol = 0.001, n_check_convergence = 25, 
                          progressbar = FALSE)
lda_model_R$get_top_words(n = 20, lambda = 0.7)





#######################
#LDA with stemmed data#
#######################
# function to get & plot the most informative terms by a specificed number
# of topics, using LDA
top_terms_by_topic_LDA <- function(input_text, # should be a columm from a dataframe
                                   plot = T, # return a plot? TRUE by defult
                                   number_of_topics = 4) # number of topics (4 by default)
{    
  # create a corpus (type of object expected by tm) and document term matrix
  Corpus <- Corpus(VectorSource(input_text)) # make a corpus object
  DTM <- DocumentTermMatrix(Corpus) # get the count of words/document
  
  # remove any empty rows in our document term matrix (if there are any 
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  # preform LDA & get the words/topic in a tidy text format
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  # get the top ten terms for each topic
  top_terms <- topics  %>% # take the topics data frame and..
    group_by(topic) %>% # treat each topic as a different group
    top_n(25, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup
    arrange(topic, -beta) # arrange words in descending informativeness
  
  # if the user asks for a plot (TRUE by default)
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
      ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
      geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
      labs(x = NULL, y = "Beta") + # no x label, change y label 
      coord_flip() # turn bars sideways
  }else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
}
top_terms_by_topic_LDA(FullData_cleaned_D$terms, number_of_topics = 5)
top_terms_by_topic_LDA(FullData_cleaned_R$terms, number_of_topics = 5)

# function that takes in a dataframe and the name of the columns
# with the document texts and the topic labels. If plot is set to
# false it will return the tf-idf output rather than a plot.
top_terms_by_topic_tfidf <- function(text_df, text_column, group_column, plot = T){
  # name for the column we're going to unnest_tokens_ to
  # (you only need to worry about enquo stuff if you're
  # writing a function using using tidyverse packages)
  group_column <- enquo(group_column)
  text_column <- enquo(text_column)
  
  # get the count of each word in each review
  words <- text_df %>%
    unnest_tokens(word, !!text_column) %>%
    count(!!group_column, word) %>% 
    ungroup()
  
  # get the number of words per text
  total_words <- words %>% 
    group_by(!!group_column) %>% 
    summarize(total = sum(n))
  
  # combine the two dataframes we just made
  words <- left_join(words, total_words)
  
  # get the tf_idf & order the words by degree of relevence
  tf_idf <- words %>%
    bind_tf_idf(word, !!group_column, n) %>%
    select(-total) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))))
  
  if(plot == T){
    # convert "group" into a quote of a name
    # (this is due to funkiness with calling ggplot2
    # in functions)
    group_name <- quo_name(group_column)
    
    # plot the 10 most informative terms per topic
    tf_idf %>% 
      group_by(!!group_column) %>% 
      top_n(70) %>% 
      ungroup %>%
      ggplot(aes(word, tf_idf, fill = as.factor(group_name))) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(reformulate(group_name), scales = "free") +
      coord_flip()
  }else{
    # return the entire tf_idf dataframe
    return(tf_idf)
  }
}
# let's see what our most informative deceptive words are
top_terms_by_topic_tfidf(text_df = FullData_cleaned, # dataframe
                         text_column = terms, # column with text
                         group_column = Party, # column with topic label
                         plot = T) # return a plot
################
# STM 
###############
library(stm)
processed <- textProcessor(FullData_cleaned$content, metadata =FullData_cleaned)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta
First_STM <- stm(documents = out$documents, vocab = out$vocab,
                 K = 10, prevalence =~ Party ,
                 max.em.its = 75, data = out$meta,
                 init.type = "Spectral", verbose = FALSE)
plot(First_STM)
#findThoughts(First_STM, texts = FullData_cleaned$content,
          #   n = 2, topics = 3)
findingk <- searchK(out$documents, out$vocab, K = c(4:15),
                    prevalence =~ Party, data = meta, verbose=FALSE)

plot(findingk)

predict_topics<-estimateEffect(formula = 1:10 ~ Party, stmobj = First_STM, metadata = out$meta, uncertainty = "Global")
plot(predict_topics, covariate = "Party", topics = c(1, 5, 8),
     model = First_STM, method = "difference",
     cov.value1 = "L", cov.value2 = "R",
     xlab = "More Conservative ... More Liberal",
     main = "Effect of Liberal vs. Conservative",
     xlim = c(-.1, .1), labeltype = "custom",
     custom.labels = c('Topic 3', 'Topic 5','Topic 8'))

plot(predict_topics,covariate = "Party",model = First_STM,topics = c(1,2,6),
     cov.value1 = "L", cov.value2 = "R", main = "Effect of L vs. R",xlab = "More Republican ... More Demostic")




