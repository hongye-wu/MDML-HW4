
library(tidyverse)
library(tm)
library(tidytext)
library(ROCR)

#A)

#Reading in the recent_opinions.tsv:
appeals.data <- read_tsv("recent_opinions.tsv")


#Adding opinion_id, a unique id column:
appeals.data$opinion_id <-  seq.int(nrow(appeals.data))

#Loading stop_words:
data(stop_words)

#Reading in custom_words.txt:
custom_words<- read.delim("custom_words.txt", header=F, encoding="UTF-8")

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


# B)b) 

#Finding the 100 most common words that are not stop words and saving it as a vector:
hundred.most.common=text_appeals.data %>% count(word) %>% arrange(desc(n)) %>% slice(1:100) 

#Creating a document term matrix:
w=text_appeals.data %>% count(opinion_id, word) %>% cast_dtm(opinion_id, word, n)

#Checking the dim- we have 16389 rows
dim(w)

#Subsetting for the top 100 words:
top_dtm= as.matrix(w[1:dim(w)[1], intersect(dimnames(w)[[2]], hundred.most.common$word)])

#Has the 16389 opinion_ids/docs and the 100 most common words
dim(top_dtm)

top_dtm<- as_tibble(top_dtm)

#Adding in docs as "opinion_id" (the opinion_id) to the dataframe:
top_dtm$opinion_id=dimnames(top_dtm)[[1]] 

# adding in circuit variable and making it binary, and creating the final table 
final <- top_dtm %>% 
  mutate(opinion_id = as.integer(opinion_id)) %>% 
  left_join(appeals.data, by = "opinion_id") %>% 
  select(-year, -text) %>% 
  mutate(
    circuit = case_when(
      circuit == "fifth" ~1,
      circuit == "ninth" ~0
    )
  )

# shuffle the data, split training and test sets
final <- final %>% slice(sample(1:n()))
split_size = floor(nrow(final)/2)
train <- final %>% slice(1:split_size)
test <- final %>% slice(split_size+1:n())


# B)c)

# fit circuit as a function of all other predictors 
model <- glm(circuit ~., data = train, family = "binomial")
# Warning message:
# glm.fit: algorithm did not converge 
train$predicted.probability <- predict(model, train, type = "response")
pred <- prediction(train$predicted.probability, train$circuit)
# AUC
perf <- performance(pred, "auc")
cat("the auc score is", 100*perf@y.values[[1]], "\n")
# The AUC turns out to be 100, which means that at least one of the predictors is generated as a result of "circuit"
# When we created the opinion_id variable, we did not shuffle the data, which means that a particular opinion_id is the
# cut-off point where all the texts before that belong to one circuit and all following texts belong to another

# B)d)

# drop opinion_id
final <- final %>% select(-opinion_id) %>%  slice(sample(1:n()))
split_size = floor(nrow(final)/2)
train <- final %>% slice(1:split_size)
test <- final %>% slice(split_size+1:n())

# new model
new_model <- glm(circuit ~., data = train, family = "binomial")
train$predicted.probability <- predict(new_model, train, type = "response")
pred <- prediction(train$predicted.probability, train$circuit)
perf <- performance(pred, "auc")
cat("the auc score is", 100*perf@y.values[[1]], "\n")

# five smallest and five largest coefficients
coeff <- as_data_frame(c(new_model$xlevels, new_model$coefficients)) %>% 
  gather() %>% 
  arrange(desc(value))
head(coeff)
tail(coeff)
# The term "argues" is 26.8% more likely to appear in documents from the fifth circuit in comparison to documents 
# from the ninth circuit

## C)

# C)a)
bigram_data <-appeals.data %>% unnest_tokens(word, text, token = "ngrams", n =2)

# remove bigrams that contain stop words
data(stop_words)
stop_words <- stop_words %>% 
  select(-lexicon) %>% 
   pull(word)

bigram_clean <- bigram_data %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words, !word2 %in% stop_words) %>% 
  transmute(
    year, circuit, opinion_id,
    word = paste(word1, word2, sep = " ")
  )

#10 most common bigrams are :
bigram_clean %>% count(word) %>% arrange(desc(n)) %>% slice(1:10)

#10 most common bigrams by circuit are:
bigram_clean %>% group_by (circuit) %>% count(word) %>% arrange(desc(n)) %>% slice(1:10)

#C)b)

#Finding the 100 most common bigrams that are not stop words and saving it as a vector:
hundred.most.common=bigram_clean %>% count(word) %>% arrange(desc(n)) %>% slice(1:100) 

#Creating a document term matrix:
w=bigram_clean %>% count(opinion_id, word) %>% cast_dtm(opinion_id, word, n)

#Checking the dim
dim(w)

#Subsetting for the top 100 words:
top_dtm= as.matrix(w[1:dim(w)[1], intersect(dimnames(w)[[2]], hundred.most.common$word)]) #16388 rows??
dim(top_dtm)

top_dtm<- as_tibble(top_dtm)

#Adding in docs as "opinion_id" (the opinion_id) to the dataframe:
top_dtm$opinion_id=dimnames(top_dtm)[[1]] 

# adding in circuit variable and making it binary, and creating the final table 
final <- top_dtm %>% 
  mutate(opinion_id = as.integer(opinion_id)) %>% 
  left_join(appeals.data, by = "opinion_id") %>% 
  select(-year, -text) %>% 
  mutate(
    circuit = case_when(
      circuit == "fifth" ~1,
      circuit == "ninth" ~0
    )
  )

# C)d)

# drop opinion_id
final <- final %>% select(-opinion_id) %>%  slice(sample(1:n()))
split_size = floor(nrow(final)/2)
train <- final %>% slice(1:split_size)
test <- final %>% slice(split_size+1:n())

# new model
new_model <- glm(circuit ~., data = train, family = "binomial")
train$predicted.probability <- predict(new_model, train, type = "response")
pred <- prediction(train$predicted.probability, train$circuit)
perf <- performance(pred, "auc")
cat("the auc score is", 100*perf@y.values[[1]], "\n") # 99 AUC???

# five smallest and five largest coefficients
coeff <- as_data_frame(c(new_model$xlevels, new_model$coefficients)) %>% 
  gather() %>% 
  arrange(desc(value))
head(coeff)
tail(coeff)
# NEED TO ADD INTERPRETATION


## D)

# D)b)

#Finding the 100 most common bigrams by frequency
hundred.most.common=bigram_clean %>% count(word) %>% arrange(desc(n)) %>% slice(1:100)
w=bigram_clean %>% count(opinion_id, word) %>% cast_dtm(opinion_id, word, n)
# subsetting
top_dtm= as.matrix(w[1:dim(w)[1], intersect(dimnames(w)[[2]], hundred.most.common$word)])
top_dtm<- as_tibble(top_dtm)

#Adding in docs as "opinion_id" (the opinion_id) to the dataframe:
top_dtm$opinion_id=dimnames(top_dtm)[[1]] 

# adding in circuit variable and making it binary, and creating the final table 
final <- top_dtm %>% 
  mutate(opinion_id = as.integer(opinion_id)) %>% 
  left_join(appeals.data, by = "opinion_id") %>% 
  select(-year, -text) %>% 
  mutate(
    circuit = case_when(
      circuit == "fifth" ~1,
      circuit == "ninth" ~0
    )
  )

# D)d)

# compute tf-idf values for each of the top 100 bigrams
tfidf <- bigram_clean %>% 
  count(opinion_id, word) %>% 
  cast_dtm(opinion_id, word, n, weighting = weightTfIdf)
# subsetting
top_dtm= as.matrix(tfidf[1:dim(tfidf)[1], intersect(dimnames(tfidf)[[2]], hundred.most.common$word)])
top_dtm<- as_tibble(top_dtm)

#Adding in docs as "opinion_id" (the opinion_id) to the dataframe:
top_dtm$opinion_id=dimnames(top_dtm)[[1]] 

# adding in circuit variable and making it binary, and creating the final table 
final <- top_dtm %>% 
  mutate(opinion_id = as.integer(opinion_id)) %>% 
  left_join(appeals.data, by = "opinion_id") %>% 
  select(-year, -text) %>% 
  mutate(
    circuit = case_when(
      circuit == "fifth" ~1,
      circuit == "ninth" ~0
    )
  )

final <- final %>% select(-opinion_id) %>%  slice(sample(1:n()))
split_size = floor(nrow(final)/2)
train <- final %>% slice(1:split_size)
test <- final %>% slice(split_size+1:n())

# new model
new_model <- glm(circuit ~., data = train, family = "binomial")
train$predicted.probability <- predict(new_model, train, type = "response")
pred <- prediction(train$predicted.probability, train$circuit)
perf <- performance(pred, "auc")
cat("the auc score is", 100*perf@y.values[[1]], "\n") #100 AUC???



## E)



## F)

# generate trigram data
trigram_data <-appeals.data %>% unnest_tokens(word, text, token = "ngrams", n =3)

# remove bigrams that contain stop words
data(stop_words)
stop_words <- stop_words %>% 
  select(-lexicon) %>% 
  pull(word)

trigram_clean <- trigram_data %>% 
  separate(word, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words, !word2 %in% stop_words, !word3 %in% stop_words) %>% 
  transmute(
    year, circuit, opinion_id,
    word = paste(word1, word2, word3, sep = " ")
  )

#10 most common trigrams that contain "supreme" by circuit 
trigram_clean %>% 
  filter(
   stringr::str_detect(word, "supreme")
  ) %>% 
  group_by (circuit) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  slice(1:10)
# the top 10 trigrams containing the word "supreme" for each district are closely associated with other circuits in 
# proximity to their geographic locations. The fifth circuit is based in Louisiana, and their references related 
# to supreme courts center around nearby states like Mississippi and Texas.
# The ninth circuit is located in California, and it is not surprising that Arizona and Nevada appear in the top ten of 
# their supreme court mentions.

