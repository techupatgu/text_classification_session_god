### Generic Script for structural Topic Modelling 

library(tidyverse)
library(haven)
library(pdftools)
library(ggwordcloud)
library(SnowballC)
library(stopwords)
library(tidytext)
library(quanteda)
library(stm)

### Retrieve the data frame with text
### This chunk is unique to my dataset, to get the same results, just replace the objects with your texts,
### but make sure that there is a doc_id variable, as well as a clearly indicated text variable
annotated_data_process <- readxl::read_xlsx("./annotations/class_out2x2_annotated.xlsx") %>% 
  filter(!specify %in% c("Just guessing based on the part of the sentence that is there",
                        "No proper sentence but guessing the categories based on the sentence I have",
                        "Not codable; Incomplete",
                        "Not codable; Mixed"),
         speech_id != "470_980_499_KNAPP") %>% 
  mutate(maori_affairs = ifelse(maori_affairs == 332, NA, maori_affairs)) %>% 
  select(-specify)

write.csv(annotated_data_process, "./annotations/classified_training_set.csv")


full_text_data <- readxl::read_xlsx("./annotations/class_out2x2.xlsx") %>% 
  select(doc_id = speech_id, text = Line) %>% 
  filter(doc_id != "470_980_499_KNAPP")


meta_data <- read_csv("./processed_data/questions_trimmed_full_data.csv")

full_text_data2 <- full_text_data %>% 
  left_join(meta_data, by = c("doc_id" = "speech_id"), multiple = "first")

## We retrieve the stopwords from a dictionary for this example, but other approaches can work, replace the "en" bit with another language if thats what you are working with

stoppers <- stopwords(language = "en")

#### Simplifying the text, remove some artefacts and punctuation.

simple_fr <- full_text_data2 %>% 
  select(doc_id, text) %>% 
  mutate(text = str_remove_all(text, "[:punct:]"), ## Regex for punctuation
         text = str_remove_all(text, " NEWPAGE "), ## This is a preprocessing artefact from my end, unlikely to exist in your data
         text = str_to_lower(text))  ## Reduce everything to lower case, this simplifies the processing a lot, so that certain words do not get separated based on starting a sentence

## Next steps create a document feature matrix, basicly making a matrix where each column is a word/token, and each row is a document,
## then we score each document simply based on how many times the word appears
## This is a bag-of-words approach, we are agnostic to the order in which the words appears. This has some huge downsides though. 

token_f <- simple_fr %>%        
  group_by(doc_id) %>% 
  unnest_tokens(input = text, 
                output = word,
                token = "words") %>% 
  count(word)

token_f2 <- token_f %>% 
  filter(!(word %in% stoppers))

token_f2 <- token_f2 %>% 
  mutate(stem = quanteda::char_wordstem(word, language = "en"))

token_f3 <- token_f2 %>% 
  group_by(doc_id, stem) %>% 
  summarise(count = sum(n))

cool_dfm <- token_f3 %>% 
  cast_dfm(doc_id, stem, count)


##### Here we apply the structural topic modelling function STM to our data. 

cool_mod <- stm(cool_dfm, K = 20, # 20 topics
                prevalence = ~ gender, # Checking prevalence against gender in particular, can be retrieved with estimateeffect function
                data = full_text_data2, # Giving the meta data
                init.type = "Spectral", max.em.its = 500, # Spectral initiation and maximum of 500 iterations, unless it converges
                emtol = 1e-05, verbose = TRUE, reportevery = 10) # Set a threshold for convergence, and ask it to report the status of the model often in the console output


### WE can retrieve the betas, ie the topic probability for each topic-document combo with the tidy function

topic_explores <- tidy(cool_mod, matrix = "beta")

## Ahead of labelling the data, you can explore which tokens affect the relevant data. This gives you an idea what the topics substantially are.
## Use this to label the topics.

tokenexploringframe <- topic_explores %>%
  group_by(topic) %>% # Getting the top term per topic, thus using group_by
  slice_max(beta, n = 10) %>% # Fetching the 10 terms with the highest beta
  ungroup() # Ungrouping to get the dataframe back to normal


tokenexploringframe %>% 
  ggplot(aes(x = term, y = beta)) + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  facet_wrap( ~ topic, # # Make different plots for each topic
              ncol = 3, # Arrange them in three columns
              scales = "free") 

### Assigner

topicassigner <- tidy(cool_mod, matrix = "gamma",
                      document_names = rownames(cool_dfm))


## full frame

topic_matrix <- topicassigner %>% 
  pivot_wider(names_from = topic, values_from = gamma,
              names_prefix = "topic_")

fun_frame <- full_text_data2 %>% 
  left_join(topic_matrix, by = c("doc_id" = "document"))


fun_frame %>% 
  group_by(gender) %>% 
  summarise(across(starts_with("topic"), ~ mean(.x))) %>% 
  pivot_longer(cols = starts_with("topic"),
               names_to = "topic", values_to = "mean_val") %>% 
  mutate(topic = str_remove(topic, "topic\\_")) %>% 
  ggplot(aes(x=topic, y = mean_val)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(vars(gender)) + 
  theme_bw()
