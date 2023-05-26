
library(tidyverse)

df <- readxl::read_excel('search_term_mappings/search_term_mapping_substance.xlsx')

#df <- read.csv('search_term_mappings/combined_search_terms_cg.csv', header = T, as.is = T)

df_main <- distinct(df, main)


df_for_skos_main <- df_main %>%
  mutate(URI = paste('lter:', main)) %>%
  mutate("rdf:type" = 'skos:Collection') %>%
  rename("skos:prefLabel" = main) %>%
  mutate("^skos:member" = '') %>%
  select(URI, "rdf:type", "skos:prefLabel", "^skos:member")
  

df_l1 <- distinct(df, level_1, .keep_all = T) %>%
  filter(!is.na(level_1))

df_for_skos_l1 <- df_l1 %>%
  mutate(URI = paste('lter:', level_1)) %>%
  mutate("rdf:type" = ifelse(is.na(level_2), '', 'skos:Collection')) %>%
  rename("skos:prefLabel" = level_1) %>%
  mutate("^skos:member" = paste('lter:', main)) %>%
  select(URI, "rdf:type", "skos:prefLabel", "^skos:member")

df_l2 <- distinct(df, level_2, .keep_all = T) %>%
  filter(!is.na(level_2))

df_for_skos_l2 <- df_l2 %>%
  mutate(URI = paste('lter:', level_2)) %>%
  mutate("rdf:type" = ifelse(is.na(level_3), '', 'skos:Collection')) %>%
  rename("skos:prefLabel" = level_2) %>%
  mutate("^skos:member" = paste('lter:', level_1)) %>%
  select(URI, "rdf:type", "skos:prefLabel", "^skos:member")

df_l3 <- distinct(df, level_3, .keep_all = T) %>%
  filter(!is.na(level_3))

df_for_skos_l3 <- df_l3 %>%
  mutate(URI = paste('lter:', level_3)) %>%
  mutate("rdf:type" = '') %>%
  rename("skos:prefLabel" = level_3) %>%
  mutate("^skos:member" = paste('lter:', level_2)) %>%
  select(URI, "rdf:type", "skos:prefLabel", "^skos:member")

df_for_skos <- rbind(df_for_skos_main, df_for_skos_l1, df_for_skos_l2, df_for_skos_l3)

write.csv(df_for_skos, file = 'search_term_mappings/skos/for_skos_substance.csv', row.names = F)

