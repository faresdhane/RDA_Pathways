setwd("C:/Users/fsdha/OneDrive - 11593765 Canada Association/Fares Drive/Works in progress/RDA/RDA Analytic/RDA_Pathways_mapping/Outputs_pdf")

# read data ---------------------------------------------------------------


#libraries
library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions
library(pdftools)
library(tm)

#read data
files <- list.files(pattern = "pdf$")#To select multiple pdf if

opinions <- lapply(files, pdf_text) #data as list

lapply(opinions, length)#Number of pages per dataframe

#text mining
titles <- as.character(files) # add names

series <- tibble()

for(i in seq_along(titles)) {
  
  clean <- tibble(chapter = seq_along(opinions[[i]]),
                  text = opinions[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean)
}

series$book <- factor(series$book, levels = rev(titles))


# text mining -------------------------------------------------------------

##option one
series1 <- series %>% select(-chapter)


group_words <- series1 %>%  
  unnest_tokens(Text, word) %>% 
  count(book, Text, sort = TRUE)

# Frequency
total_words <- group_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

group_words <- left_join(group_words, total_words)

book_tf_idf <- group_words %>%
  bind_tf_idf(Text, book, n)

# delete words with tf-idf near zero = words that occur in many of the groups
book_tf_idf1 <- book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))


book_tf_idf1 <- book_tf_idf1 %>% filter(tf_idf > 0.001)

book_tf_idf2 <- book_tf_idf1 %>% arrange(book, n) %>%
  group_by(book) %>% top_n(10)

#Option two

##Option 1 top 10 words = i prefer this one
output_top_10_words <-
  series %>%
  anti_join(stop_words) %>%
  group_by(book) %>%
  count(word, sort = TRUE) %>%
  top_n(10)


# read data for matching --------------------------------------------------


#read data
output_top_words <- read.csv("Output_data.csv") %>% 
  select(-X)# groups output words
pathways_tidy <- read.csv("full_pathways_words.csv") # rda pathways
groups_top_words <- read.csv("RDA_Groups_top5_words_Input1_V2.csv") #groups words
groups_names <- read.csv("Groups_outputs_csv_20230103_V2.csv") %>% 
  select(Output_title, Group_Title)


# Create join tables = matching -------------------------------------------


##join tables: only output names and groups names
output_groups <- 
  output_top_words %>% 
  select(book) %>% 
  unique() %>% 
  left_join(groups_names,
            by = c("book" = "Output_title")) %>% 
  arrange(Group_Title)


##Join tables: output + pathways
output_pathways <- 
  output_top_words %>% 
  inner_join(pathways_tidy, by = "word") %>% 
  select(-n)

##join tables: map = groups words and pathways
groups_pathways <- 
  groups_top_words %>% 
  inner_join(pathways_tidy, by = c("Text" = "word")) %>% 
  select(-n, tf_idf) %>% 
  rename(word = Text) %>% 
  unique()

##join tables: map groups + outputs
# grps_out_merge <- 
#   groups_top_words %>% 
#   select(-n, -tf_idf)  %>% 
#   left_join(output_groups,
#             by = c("Group" = "Group_Title")) %>% 
#   unique()
##
##
##
##Now I merge groups words and output words to keep only
##groups and words
# grps_out_words <- 
#   grps_out_merge %>% 
#   left_join(output_top_words,
#             by = "book") %>% 
#   select(Group, Text, word) %>% 
#   gather("Text", "word", 2:3) %>% 
#   select(-Text) %>% 
#   unique()

## Now I merge grps_out_words with the pathways
all_groups_words_path <- 
  groups_top_words %>% 
  inner_join(pathways_tidy, by = c("Text" = "word")) %>% 
  unique()

onlygroups_unique <- 
  all_groups_words_path %>% 
  select(Group, pathway) %>% 
  unique()

# write.csv(onlygroups_unique, "onlygroups_unique.csv")

onlygroups_unique %>% group_by(pathway) %>% count()
##
## icite w9eft
##
##Now I will do the same for the outputs = map them with the pathways
output_top_words <- 
  output_top_words %>% 
  select(-n)

##join tables: outputs + pathways
b1 <- output_top_words
b2 <- b1

b <- full_join(b1, b2, by = "book")
b <- b %>% rename(word1 = word.x , word2 = word.y)
df.b <- b %>% filter(!word1 == word2) #delete duplicated cells


p1 <- read.csv("full_pathways_words.csv")
p2 <- read.csv("full_pathways_words.csv")

p <- full_join(p1, p2, by = "pathway")
p <- p %>% rename(word1 = word.x , word2 = word.y) %>% select(-X.y)
df.p <- p %>% filter(!word1 == word2) #delete duplicated cells


path_output <- inner_join(df.p, df.b, by = c("word1", "word2")) %>% 
  select(-X.x, -word1, -word2) %>% unique()

data %>% group_by(pathway) %>% count()

# write.csv(path_output, "path_output.csv")

# Essai avec deux key words: ----------------------------------------------



p1 <- read.csv("full_pathways_words.csv")
p2 <- read.csv("full_pathways_words.csv")

p <- full_join(p1, p2, by = "pathway")
p <- p %>% rename(word1 = word.x , word2 = word.y) %>% select(-X.y)
df.p <- p %>% filter(!word1 == word2) #delete duplicated cells

g1 <- read.csv("RDA_Groups_top5_words_Input1_V2.csv")
g2 <- read.csv("RDA_Groups_top5_words_Input1_V2.csv")

g <- full_join(g1, g2, by = "Group")
g <- g %>% rename(word1 = Text.x, word2 = Text.y) %>% select(-n.x, -n.y, -tf_idf.x, -tf_idf.y)

df.g <- g %>% filter(!word1 == word2) #delete duplicated cells

#match groups with pathways based on two columns:

data <- inner_join(df.p, df.g, by = c("word1", "word2")) %>% 
  select(-X.x, -word1, -word2) %>% unique()

data %>% group_by(pathway) %>% count()

# write.csv(data, "groups_pathways_two_words.csv")

final_data <- read.csv("groups_pathways_two_words.csv")
final_data <- unique(final_data)

# write.csv(final_data, "findal_groups_path_data.csv")



# Accuracy assessment -----------------------------------------------------


plenaries <- read.csv("p20_p19_clean.csv") %>% 
  select(Applicable_Pathways, Group_name) %>% 
  rename(pathway = Applicable_Pathways, group = Group_name) %>% 
  unique() %>% 
  drop_na() %>% 
  separate(pathway, into = c("a", "b", "c"), sep = " / ") %>% 
  gather("x", "pathway", 1:3) %>% 
  select(-x)

plenaries$group[plenaries$group == ""] <- NA
plenaries <- drop_na(plenaries)
plenaries <- plenaries %>% 
  mutate(group = str_replace_all(plenaries$group, "[\r\n]" , ""))


# write.csv(plenaries, "plenaries.csv")

#select groups from the final output that are in "plenaries"
# 
gp_sel <- plenaries %>%
  inner_join(final_data, by = c("group" = "Group")) %>%
  rename(Classified = pathway.y, GrndTruth = pathway.x)

# write.csv(gp_sel, "ver_group_pathways.csv")


gp_sel1 <- gp_sel %>% 
  select(-Classified) %>% unique()


# write.csv(unique(dd), "dd.csv")


# Adding links to groups and outputs --------------------------------------


links <- read.csv("RDA_Groups_websites_V1.csv")
groups.path <- read.csv("groups_pathways_final_V1.csv")

groups <- groups.path %>% 
  select(Group) %>% 
  unique()

has.links <- 
  groups %>% 
  inner_join(links, by = "Group")

no.links <- 
  groups %>% 
  anti_join(links, by = "Group")

path.group.link <- 
  groups.path %>% 
  left_join(has.links, by = "Group")

# write.csv(path.group.link, "path.group.link.csv")
