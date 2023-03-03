
# Load libraries and data -------------------------------------------------



setwd("C:/Users/fsdha/OneDrive - 11593765 Canada Association/Fares Drive/Works in progress/RDA/RDA_Analytics/RDA_Pathways_mapping/Planaries")

library(tidyverse)
library(htm2txt)
library(rvest)
library(unpivotr)
library(stringr)        # text cleaning and regular expressions
library(tidytext) 
library(data.table)

data <- read.csv("p20_p19_clean.csv")
glimpse(data)

data <- data %>% select(-Plenary) %>% unique()
# write.csv(data, "p20_p19_clean_no_dup.csv")


# Automate text mining - Plenaries ----------------------------------------


results <- 
  data  %>% 
  rowwise() %>%
  mutate(content = read_html(Link_to_submission) %>%
           html_nodes("p") %>%
           html_text2() %>%
           list()
  ) %>%
  ## list content items ([1] ... [2] ... etc.) in
  ## separate rows:
  unnest_longer(content) ## list content items


group_words <- results %>%  
  unnest_tokens(Text, content) %>% 
  count(NID, Text, sort = TRUE)


# Calculate the frequency
total_words <- group_words %>% 
  group_by(NID) %>% 
  summarize(total = sum(n))

# Join the tables
group_words <- left_join(group_words, total_words)

# Group and sort the table
book_tf_idf <- group_words %>%
  bind_tf_idf(Text, NID, n)

# Create a list of nouns to be used as a filter
book_tf_idf1 <- book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

book_tf_idf1_stop_w <- book_tf_idf1 %>% 
  filter(!Text %in% stop_words$word)

#list of words
selected_words <- book_tf_idf1_stop_w %>% 
  filter(tf_idf > 0.001 & n >= 2) %>% 
  select(-tf, -idf)

glimpse(selected_words)


## Left join with the pathways using NID
pathways_words <- left_join(selected_words,
                            data,
                            by = "NID")

glimpse(pathways_words)

summrized_table <- pathways_words %>% group_by(Applicable_Pathways, Text) %>% 
  summarise(n)

DT <- data.table(summrized_table)
DT <- DT[, lapply(.SD, sum), by=list(Applicable_Pathways, Text)]
glimpse(DT)

top_5_words <- DT %>% arrange(desc(n)) %>%  group_by(Applicable_Pathways) %>% 
  slice(1:10)

# write.csv(top_5_words, "top_5_words.csv")

top_5_words <- read.csv("top_5_words.csv")
top_5_words_sep <- top_5_words %>% 
  separate(Applicable_Pathways, c("a", "b", "d"), sep = "/")

plan_top_5 <- top_5_words_sep %>% gather("x", "pathway", 1:3) %>% 
  select(-x)

# write.csv(plan_top_5, "plan_top_5.csv")
plan_top_5 <- read.csv("plan_top_5.csv")

DT2 <- data.table(plan_top_5)
DT2 <- DT2[, lapply(.SD, sum), by=list(pathway, Text)]
glimpse(DT2)

top_5_words_2 <- DT2 %>% arrange(desc(n)) %>%  group_by(pathway) %>% 
  slice(1:10)

# write.csv(top_5_words_2, "final_top_10_words.csv")


# Merge output with manual pathways words ---------------------------------


manual <- read.csv("pathways_tidy.csv")
automatic <- read.csv("Pathways_top_10_words_P20P19.csv")

join <- 
  automatic %>% 
  full_join(manual, by = c("pathway" = "category"))


join1 <- 
  join %>% 
  select(-n) %>% 
  gather("x1", "x2", 2:3) %>% 
  select(-x1) %>% 
  rename(word = x2) %>% 
  unique()

# write.csv(join1, "full_pathways_words.csv")



