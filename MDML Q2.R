
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

# C)
bigram_data <-appeals.data %>% unnest_tokens(word, text, token = "ngrams", n =2)
# remove bigrams that contain stop words
stop_words <- stop_words %>% 
  select(-lexicon) %>% 
  pull(word)

bigram_clean <- bigram_data %>% 
  select(-on())

#10 most common words are :
text_appeals.data %>% count(word) %>% arrange(desc(n)) %>% slice(1:10)

#10 most common words by circuit are:
text_appeals.data %>% group_by (circuit) %>% count(word) %>% arrange(desc(n)) %>% slice(1:10)