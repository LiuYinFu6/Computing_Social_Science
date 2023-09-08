dfmat_toks_jobs_nostop <- dfm(token_corp_party_no_stopsword_R)
print(dfmat_toks_jobs_nostop)
dfmat_toks_jobs_nostop <- dfm(token_corp_party_no_stopsword_D)
print(dfmat_toks_jobs_nostop)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(quanteda.textmodels)
dfmat_toks_jobs_nostop %>% 
  textstat_frequency(n = 100) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()


#D
unigram_toks_jobs <- tokens_ngrams(token_corp_party_no_stopsword_D, n=1)
unigram_dfm_jobs <- dfm(unigram_toks_jobs)
unigram_freq_jobs <-textstat_frequency(unigram_dfm_jobs)

# Plot wordcloud to show most frequent words
textplot_wordcloud(unigram_dfm_jobs, max_words = 200,
                   ordered_color = TRUE)
#R
unigram_toks_jobs <- tokens_ngrams(token_corp_party_no_stopsword_R, n=1)
unigram_dfm_jobs <- dfm(unigram_toks_jobs)
unigram_freq_jobs <-textstat_frequency(unigram_dfm_jobs)

# Plot wordcloud to show most frequent words
textplot_wordcloud(unigram_dfm_jobs, max_words = 200,
                   ordered_color = TRUE)

