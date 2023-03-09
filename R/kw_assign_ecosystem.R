
# Load libraries
library(tidyverse)
library(EDIutils)

# Define paths
input_path <- "parsed_eml"

search_term_path <- "search_term_mappings"

output_path <- "assigned_kw"

# Read in current keyword / search term path
df_datasets <- read.csv(file.path(input_path, 'datasetKeywords.csv'), as.is = T, header = T)

search_term <- read.csv(file.path(search_term_path,'search_term_mapping_ecosystem.csv'), header = T, as.is = T)

# Make an empty dataframe to write stuff into
df_ds_subset <- data.frame(packageid = character(0),
                           title = character(0),
                           ecosystem_term = character(0),
                           envo_id = character(0))

# Collect relevant information on each search term
for (j in 1:nrow(search_term)) {
  
  df_ds_subset_row <- df_datasets %>%
    filter(str_detect(tolower(keywords), regex(search_term[[1]][j]))) %>%
    mutate(ecosystem_term = search_term[[3]][j]) %>%
    mutate(envo_id = search_term[[2]][j]) %>%
    select(-keywords, -abstract)
  
  df_ds_subset <- rbind(df_ds_subset, df_ds_subset_row)

  df_ds_subset_row <- df_datasets %>%
    filter(str_detect(tolower(title), regex(search_term[[1]][j]))) %>%
    mutate(ecosystem_term = search_term[[3]][j]) %>%
    mutate(envo_id = search_term[[2]][j]) %>%
    select(-keywords, -abstract)
  
  df_ds_subset <- rbind(df_ds_subset, df_ds_subset_row)

  df_ds_subset_row <- df_datasets %>%
    filter(str_detect(tolower(abstract), regex(search_term[[1]][j]))) %>%
    mutate(ecosystem_term = search_term[[3]][j]) %>%
    mutate(envo_id = search_term[[2]][j]) %>%
    select(-keywords, -abstract)
  
  df_ds_subset <- rbind(df_ds_subset, df_ds_subset_row)
  
  # query_text <- paste('q=title:"',
  #                     str_replace_all(search_term[[1]][j], ' ', '+'),
  #                     '"+OR+abstract:"',
  #                     str_replace_all(search_term[[1]][j], ' ', '+'),
  #                     '"&fl=packageid,title&',
  #                     'fq=-scope:(ecotrends+lter-landsat+lter-landsat-ledaps)',
  #                     sep = '')
  # 
  # df_res <- search_data_packages(query = query_text)
  # 
  # df_res <- df_res %>%
  #   separate(packageid, c('scope', 'id', 'version'), sep = '\\.') %>%
  #   mutate(packageid = paste(scope, id, sep = '.' )) %>%
  #   mutate(ecosystem_term = search_term[[3]][j]) %>%
  #   mutate(envo_id = search_term[[2]][j]) %>%
  #   select(packageid, title, ecosystem_term, envo_id)
  # 
  # df_ds_subset <- rbind(df_ds_subset, df_res)
  # 
  
}

# Keep only distinct values of this
df_ds_sub_distinct <- df_ds_subset %>%
  distinct(packageid, ecosystem_term, envo_id)

# Attach the dataset info to these distinct values
df_ds_out <- left_join(df_ds_sub_distinct, df_datasets, by = c('packageid'))

# Retrieve the row names as a column
df_ds_out$record_id <- rownames(df_ds_out)

# Identify datasets to drop
df_ds_remove <- df_ds_out %>%
  filter(str_detect(packageid, 'and|hbr|sgs|nwt') & ecosystem_term == 'agricultural ecosystem')

# Use `anti_join` to exclude these
df_ds_out <- anti_join(df_ds_out, df_ds_remove, by = "record_id")

# Export output
write.csv(df_ds_out, file = file.path(output_path,'ecosystem.csv'), row.names = F)

# Wrangle this object into wide format
df_ds_out_wide <- df_ds_out %>%
  select(-envo_id, -record_id) %>%
  mutate(exist = '1') %>%
  spread(ecosystem_term, exist)

# Export the wide information
write.csv(df_ds_out_wide, file = file.path(output_path, 'ecosystem_wide.csv'), na = '', row.names = F)
# no ecosystem information

# Identify cases where the ecosystem is present
df_eco_pres <- distinct(df_ds_out, packageid)

# 
df_eco_scope <- separate(df_eco_pres, packageid, into = c('scope', 'dataset_id'), sep = '\\.')
df_scope <- distinct(df_eco_scope, scope)

# Remove them via `anti_join`
df_eco_abs <- anti_join(df_datasets, df_eco_pres, by = c('packageid'))

# Export the set of files that lack ecosystem information
write.csv(df_eco_abs, file = file.path(output_path, 'no_ecosystem.csv'), row.names = F)
