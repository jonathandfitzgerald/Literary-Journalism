bibWords = tokenize_words(allBib$title, lowercase = T, stopwords = stopwords("en")) %>% unlist()
bibWords = bibWords %>% as_data_frame()
bibWords = bibWords %>% group_by(value) %>% mutate(count = n()) %>% arrange(-count)
bibWords = bibWords %>% group_by(value) %>% slice(1) %>% arrange(-count)
wordcloud(bibWords$value, bibWords$count, min.freq = 5,colors = brewer.pal(8, "Spectral"),scale=c(5,.5))


# NEED TO DO A REGEX FOR JUST TITLES. UGH.
