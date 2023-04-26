

library(tidyverse)

input_path <- "parsed_eml"

output_path <- "parsed_eml"

df_datasets <- read.csv(file.path(input_path, 'datasetKeywords.csv'), as.is = T, header = T)

df_keywords <- df_datasets %>%
  select(packageid, keywords) %>%
  mutate(keywordlist = str_split(keywords, ',')) %>%
  select(-keywords) %>%
  unnest(c(packageid, keywordlist))

df_keyword_count <- df_keywords %>%
  mutate(keywordlist = tolower(keywordlist)) %>%
  group_by(keywordlist) %>%
  mutate(kw_count = n()) %>%
  select(-packageid) %>%
  distinct()

write.csv(df_keyword_count, file.path(output_path, 'keyword_count.csv'), row.names = F)
