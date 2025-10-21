#### In house Classifier

library(tidyverse)
library(caret)

## Data loading, replace with what you have

zero_shot_classified <- read_csv("./processed_data/classified_results_second_it.csv")

annotated_data <- readxl::read_xlsx("./annotations/class_out2x2_annotated.xlsx") %>% 
  filter(!specify %in% c("Just guessing based on the part of the sentence that is there",
                         "No proper sentence but guessing the categories based on the sentence I have",
                         "Not codable; Incomplete",
                         "Not codable; Mixed"),
         speech_id != "470_980_499_KNAPP") %>% 
  mutate(maori_affairs = ifelse(maori_affairs == 332, NA, maori_affairs)) %>% 
  select(-specify, -maori_affairs, -women_narrowly) 

vars_col <- names(annotated_data)[c(-1,-2)]

annotated_data <- annotated_data %>% 
  mutate(across(all_of(vars_col), ~ ifelse(is.na(.x), 0, .x))) %>% 
  pivot_longer(cols = all_of(vars_col), names_to = "label", values_to = "entailment_t")

names(zero_shot_classified) <- str_replace_all(names(zero_shot_classified), "\\s", "\\.")

zero_shot_classified_bin <- zero_shot_classified %>% 
  mutate(across(all_of(vars_col), ~ ifelse(.x > .95, 1, 0))) %>% 
  pivot_longer(cols = all_of(vars_col), names_to = "label", values_to = "entailment_p")


### Join the two, create the confMatrix

full_set <- annotated_data %>% 
  left_join(zero_shot_classified_bin, by = c("speech_id", "Line", "label")) %>% 
  mutate(entailment_p = factor(entailment_p), 
         entailment_t = factor(entailment_t))

confusionMatrix(full_set$entailment_p,full_set$entailment_t)


## Subtopics

for(i in 1:length(vars_col)){
  
  test_var <- vars_col[i]
  
  delimited_set <- full_set[which(full_set$label == test_var),]

  confusion <- confusionMatrix(delimited_set$entailment_p, delimited_set$entailment_t)
  
  print(paste0("Table for topic: ", test_var))
  print(confusion[["table"]])
}

