
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

# dd <- read.csv("book1.csv")
# write.csv(unique(dd), "dd.csv")
