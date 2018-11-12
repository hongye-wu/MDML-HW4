
library(tidyverse)
library(tm)
library(tidytext)

#A)

#Reading in the recent_opinions.tsv:
appeals.data <- read_tsv("recent_opinions.tsv")


#Adding opinion_id, a unique id column:
appeals.data$opinion_id <-  seq.int(nrow(appeals.data))

#Loading stop_words:
data(stop_words)

#Reading in custom_words.txt:
custom_words<- read.delim("custom_words.txt", header=F)

#Renaming column:
colnames(custom_words)<-"word"

#Adding a lexicon column with values as "custom":
custom_words$lexicon<- "custom"

custom_stop_words= rbind(stop_words, custom_words)


#B)a)

text_appeals.data <-appeals.data %>% unnest_tokens(word, text)


#Removing custom stop words:
text_appeals.data <- text_appeals.data %>% anti_join((custom_stop_words))

#10 most common words are :
text_appeals.data %>% count(word) %>% arrange(desc(n)) %>% slice(1:10)

#10 most common words by circuit are:
text_appeals.data %>% group_by (circuit) %>% count(word) %>% arrange(desc(n)) %>% slice(1:10)


#b) Finding the 100 most common words that are not stop words and saving it as a vector:
hundred.most.common=text_appeals.data %>% count(word) %>% arrange(desc(n)) %>% slice(1:100) 

#Creating a document term matrix:
w=text_appeals.data %>% count(opinion_id, word) %>% cast_dtm(opinion_id, word, n, weightTfIdf)

#Checking the dim- we have 16380 rows- not 16389?
dim(w)

#Subsetting for the top 100 words:
top_dtm= as.matrix(w[1:dim(w)[1], intersect(dimnames(w)[[2]], hundred.most.common$word)])

#Has the 16380 opinion_ids/docs and the 100 most common words
dim(top_dtm)

top_dtm<- as.data.frame(top_dtm)

#Adding in docs as "opinion_id" (the opinion_id) to the dataframe:
top_dtm$opinion_id=dimnames(top_dtm)[[1]]

#Creating the final tibble:
final= merge(top_dtm, appeals.data, by="opinion_id")
final= final %>% select(-year, -text)
dim(final)

#Not sure why we have 16379 rows instead of 16389. Issues with cast_dtm and the merge?




