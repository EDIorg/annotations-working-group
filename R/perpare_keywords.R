
# read EML files and parse into table with title, abstract, keywords

library(xml2)
library(tidyverse)

# folder with EML files, not in git

eml_path <- "eml_files"

output_path <- "parsed_eml"

file_names <- list.files(path = eml_path, pattern = '*.xml')

df <- data.frame(fileName = character(0),
                 title = character(0),
                 abstract = character(0),
                 keywords = character(0),
                 stringsAsFactors = FALSE) 

for (i in 1:length(file_names)) {
  file_name <- paste(eml_path, file_names[i], sep = "/")
  xml_file <- read_xml(file_name)
  
  eml_title <- xml_text(xml_find_first(xml_file, './/title'))
  
  eml_abstract <- xml_text(xml_find_first(xml_file, './/abstract'))

  eml_keywords <- xml_text(xml_find_all(xml_file, './/keyword'))
  all_keywords <- paste(eml_keywords, collapse=",")
  
  
  df_part <- data.frame(fileName = file_names[i],
                        title = eml_title,
                        abstract = eml_abstract,
                        keywords = all_keywords)
  
  df <- rbind(df, df_part)
}

df_metadata <- df %>%
  separate(fileName, into = c('scope', 'package_id', 'version', NA), sep = '\\.') %>%
  mutate(packageid = paste(scope, package_id, sep = '.')) %>%
  select(packageid, title, keywords, abstract)

write.csv(df_metadata, file = paste(output_path,'datasetKeywords.csv', sep = '/'), row.names = F)

# ---------------------------------------------------------------------------

# make keyword pairs and counts

df_keywords <- df_metadata %>%
  mutate(keywords = tolower(keywords))

df_kw_single <- data.frame(single_kw = character(0),
                           package_id = character(0))

for (i in 1:nrow(df_keywords)) {
  
  kw <- str_split(df_keywords$keywords[i], ',')
  package_id <- df_keywords$packageid[i]
  
  df_kw <- data.frame(single_kw = kw[[1]])
  df_kw <- mutate(df_kw, package_id = package_id)
  
  df_kw_single <- rbind(df_kw_single, df_kw)
  
}

# clean up some keywords

df_kw_edited <- df_kw_single %>%
  mutate(single_kw = str_replace_all(single_kw, "[_\\/\\-\\:\\&]", " ")) %>%
  mutate(single_kw = str_trim(single_kw)) %>%
  mutate(single_kw = str_replace(single_kw, '.*disturbance.*', 'disturbance')) %>%
  mutate(single_kw = str_replace(single_kw, '.*population.*', 'populations')) %>%
  mutate(single_kw = str_replace(single_kw, '.*inorganic nutrient.*', 'inorganic nutrients')) %>%
  mutate(single_kw = str_replace(single_kw, '.*product.*', 'primary productivity')) %>%
  mutate(single_kw = str_replace(single_kw, '.*organic matter.*', 'organic matter'))

# check out the keywords and what to remove to make more sense

df_kw_count_raw <- df_kw_edited %>%
  group_by(single_kw) %>%
  summarise(count = n())

write.csv(df_kw_count_raw, file = paste(output_path, 'keyword_count.csv', sep = '/'), row.names = F)


df_kw_nested <- df_kw_edited %>%
  group_by(package_id) %>%
  nest(keywords = single_kw)

df_kw_together <- data.frame(word_pairs=character())


for (i in 1:nrow(df_kw_nested)) {
  
  kw <- unlist(df_kw_nested$keywords[i])
  
  m <- outer(kw, kw, paste, sep = "|")
  
  u <- m[!lower.tri(m)]
  
  df_upper <- data.frame(u)
  
  df_kw_together <- rbind(df_upper, df_kw_together)
  
}


df_kw_pairs <- df_kw_together %>%
  separate(u, into = c('from', 'to'), sep = "\\|") %>%
  filter(from != to)


write.csv(df_kw_pairs, file = paste(output_path, 'keyword_pairs.csv', sep = '/'), row.names = F)
