
# Load libraries
library(tidyverse)
library(EDIutils)
library(googledrive)

# Define paths
input_path <- "parsed_eml"

search_term_path <- "search_term_mappings"

output_path <- "assigned_kw/ecosystem"


#-----------------------------------------------
# set up googldrive according to https://nceas.github.io/scicomp.github.io/tutorials.html#using-the-googledrive-r-package

# then get the latest version of the search_term_mapping files
# set the folder url
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1bDyFKmGg6pL04fuQ9r5rtZvKwprfchty")

# read content of folder
drive_folder <- googledrive::drive_ls(path = drive_url)

# download files into local folder
for (j in 1:nrow(drive_folder)) {
  
  drive_download(file = drive_folder$id[j], 
                 path = file.path(search_term_path, drive_folder$name[j]),
                 overwrite = T)
  
}

# --------------------------------------------------

# Read in current keyword / search term path
df_datasets <- read.csv(file.path(input_path, 'datasetKeywords.csv'), as.is = T, header = T)

# df_datasets <- head(df_datasets, 100)
df_exclude_terms <- readxl::read_excel(file.path(search_term_path, 'exclude_terms.xlsx'))

exclude_terms <- unlist(df_exclude_terms$exclude_ecosystem)

remove_terms <- tolower(paste(exclude_terms, collapse = '|'))

df_datasets <- df_datasets %>%
  mutate(abstract = str_remove_all(abstract, '\\n')) %>%
  unite("text_combined", 2:4, sep = ' ', remove = F) %>%
  mutate(text_combined = str_trim(text_combined, side = c("both"))) %>%
  mutate(text_combined = str_replace_all(text_combined, '\\W', ' ')) %>%
  mutate(text_combined = str_squish(text_combined)) %>%
  mutate(text_combined = tolower(text_combined)) %>%
  mutate(text_combined = str_remove_all(text_combined, remove_terms)) %>%
  mutate(text_combined = str_squish(text_combined))

write.csv(df_datasets, file = file.path(input_path, "datasetKeywords_cleaned_ecosystems.csv"))

search_term <- readxl::read_excel(file.path(search_term_path,'search_term_mapping_ecosystem.xlsx'))

# Make an empty dataframe to write stuff into
df_ds_subset <- data.frame(packageid = character(0),
                           title = character(0),
                           ecosystem_level_1 = character(0),
                           ecosystem_level_2 = character(0),
                           ecosystem_level_3 = character(0))

# Collect relevant information on each search term
for (j in 1:nrow(search_term)) {
  
  df_ds_subset_row <- df_datasets %>%
    filter(str_detect(tolower(text_combined), regex(search_term[[1]][j]))) %>%
    mutate(ecosystem_level_1 = search_term[[8]][j]) %>%
    mutate(ecosystem_level_2 = search_term[[9]][j]) %>%
    mutate(ecosystem_level_3 = search_term[[10]][j])
  
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
  distinct(packageid, ecosystem_level_1, ecosystem_level_2, ecosystem_level_3)

# Attach the dataset info to these distinct values
df_ds_out <- left_join(df_ds_sub_distinct, df_datasets, by = c('packageid'))

# Retrieve the row names as a column
df_ds_out$record_id <- rownames(df_ds_out)

# -----------------------------------------
# custom rules to improve assignment
# this needs improvement, encode rules in a csv file or equivalent don't hardcode here

# Identify datasets to drop
# some sites are on abandoned agricultural land 

df_ds_out <- df_ds_out %>%
  mutate(del = if_else(str_detect(packageid, 'and|sgs|nwt') & ecosystem_level_2 == 'agricultural', 'rem', '')) %>%
  mutate(del = if_else(str_detect(packageid, 'and|arc') & ecosystem_level_1 == 'coastal', 'rem', del)) %>%
  mutate(del = if_else(str_detect(packageid, 'arc') & ecosystem_level_2 == 'marine', 'rem', del)) %>%
  mutate(del = if_else(str_detect(packageid, 'bes') & ecosystem_level_1 == 'polar', 'rem', del)) %>%
  mutate(del = if_else(str_detect(packageid, 'bes|bnz') & ecosystem_level_2 == 'island', 'rem', del)) %>%
  mutate(del = if_else(str_detect(packageid, 'hfr') & ecosystem_level_2 == 'intertidal', 'rem', del)) %>%
  mutate(del = if_else(str_detect(packageid, 'sbc') & ecosystem_level_1 == 'terrestrial' & ecosystem_level_2 == 'forest', 'rem', del))

df_ds_out <- df_ds_out %>%
  filter(del == '')
# -------------------------------------------

df_ds_out <- separate(df_ds_out, packageid, into = c('scope', 'dataset_id'), sep = '\\.', remove = F)
df_scope <- distinct(df_ds_out, scope)


# Export output
write.csv(df_ds_out, file = 'assigned_kw/combined/ecosystem.csv', row.names = F)

# Export by lter site

for (j in 1:nrow(df_scope)) {
  
  df_ds_site <- df_ds_out %>%
    filter(scope == df_scope[[1]][j])
  
  write.csv(df_ds_site, file = paste(output_path, '/', df_scope[[1]][j], '_ecosystem.csv', sep = ''), row.names = F)
}

# no ecosystem information

# Identify cases where the ecosystem is present
df_eco_pres <- distinct(df_ds_out, packageid)

# Remove them via `anti_join`
df_eco_abs <- anti_join(df_datasets, df_eco_pres, by = c('packageid'))

# 
df_eco_abs <- separate(df_eco_abs, packageid, into = c('scope', 'dataset_id'), sep = '\\.', remove = F)
df_scope <- distinct(df_eco_abs, scope)

# Export the set of files that lack ecosystem information
# write.csv(df_eco_abs, file = file.path(output_path, 'no_ecosystem.csv'), row.names = F)


for (j in 1:nrow(df_scope)) {
  
  df_ds_site <- df_eco_abs %>%
    filter(scope == df_scope[[1]][j])
  
  write.csv(df_ds_site, file = paste(output_path, '/', df_scope[[1]][j], '_no_ecosystem.csv', sep = ''), row.names = F)
}

# ----------------------------------
# write all files to google drive

upload_url <- googledrive::as_id("https://drive.google.com/drive/folders/1ddzwrXmMxQOJkYoIv7G5WcbAIoYaP_7W")

file_names <- dir(output_path)

for (j in 1:length(file_names)) {
  
  drive_upload(file.path(output_path, file_names[j]),
               path = upload_url,
               type = "spreadsheet",
               overwrite = TRUE)
  
  
  
}

