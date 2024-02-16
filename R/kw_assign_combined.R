
# Load libraries
library(tidyverse)
library(EDIutils)
library(googledrive)

# Define paths
input_path <- "parsed_eml"

search_term_path <- "search_term_mappings"

output_path <- "assigned_kw/combined"


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
# make a combined file

file_names <- dir(search_term_path, pattern = "search_term_mappin*")

df_terms <- readxl::read_excel(file.path(search_term_path, file_names[1]))

df_terms <- select(df_terms, 1:12)

for (j in 2:length(file_names)) {
  df_others <- readxl::read_excel(file.path(search_term_path, file_names[j]))
  
  df_others <- select(df_others, 1:12)
  
  df_terms <- rbind(df_terms, df_others)
  
}

df_terms <- df_terms %>%
  mutate(search_term_clean = str_remove_all(search_term, '\\WW'))

write.csv(df_terms, file = file.path(search_term_path, 'combined_search_terms.csv'), row.names = F)

# -------------------------------------------------------------------------------


search_term_all <- read.csv(file.path(search_term_path,'combined_search_terms.csv'))

# Read in current keyword / search term path
df_datasets_raw <- read.csv(file.path(input_path, 'datasetKeywords.csv'), as.is = T, header = T)

main_categories <- unlist(distinct(search_term_all, main))

# df_datasets <- head(df_datasets, 100)
df_exclude_terms <- readxl::read_excel(file.path(search_term_path, 'exclude_terms.xlsx'))

# Make an empty dataframe to write stuff into
df_ds_subset <- data.frame(packageid = character(0),
                           title = character(0),
                           main = character(0),
                           level_1 = character(0),
                           level_2 = character(0),
                           level_3 = character(0))

for (k in 1:length(main_categories)) {
  
  colheader <- paste('exclude', main_categories[k], sep = '_')
  exclude_terms <- unlist(select(df_exclude_terms, colheader))
  exclude_terms <- exclude_terms[!is.na(exclude_terms)]
  exclude_terms <- unique(exclude_terms)
  
  if(length(exclude_terms) == 0) {exclude_terms <- c("0")}
  
  remove_terms <- tolower(paste(exclude_terms, collapse = '|'))
  
  df_datasets <- df_datasets_raw %>%
    mutate(abstract = str_remove_all(abstract, '\\n')) %>%
    unite("text_combined", 2:4, sep = ' ', remove = F) %>%
    mutate(text_combined = str_trim(text_combined, side = c("both"))) %>%
    mutate(text_combined = str_replace_all(text_combined, '\\W', ' ')) %>%
    mutate(text_combined = str_squish(text_combined)) %>%
    mutate(text_combined = tolower(text_combined)) %>%
    mutate(text_combined = str_remove_all(text_combined, remove_terms)) %>%
    mutate(text_combined = str_squish(text_combined))
  
  out_file_name <- paste('datasetKeywords_cleaned_', main_categories[k], '.csv', sep = '')
  
  write.csv(df_datasets, file = file.path(input_path, out_file_name))
  
  search_term <- filter(search_term_all, main == main_categories[k])
  
  # Collect relevant information on each search term
  for (j in 1:nrow(search_term)) {
    
    df_ds_subset_row <- df_datasets %>%
      filter(str_detect(tolower(text_combined), regex(search_term[[1]][j]))) %>%
      mutate(main = search_term$main[j]) %>%
      mutate(level_1 = search_term$level_1[j]) %>%
      mutate(level_2 = search_term$level_2[j]) %>%
      mutate(level_3 = search_term$level_3[j])
    
    df_ds_subset <- rbind(df_ds_subset, df_ds_subset_row)
    
  }
  
}


df_ds_out <- df_ds_subset %>%
  distinct(packageid, main, level_1, level_2, level_3, .keep_all = T)


# -----------------------------------------
# custom rules to improve assignment
# this needs improvement, encode rules in a csv file or equivalent don't hardcode here

# Identify datasets to drop
# some sites are on abandoned agricultural land 

df_ds_out <- df_ds_out %>%
  mutate(del = if_else(str_detect(packageid, 'and|sgs|nwt') & level_2 == 'agricultural', 'rem', '')) %>%
  mutate(del = if_else(str_detect(packageid, 'and|arc') & level_1 == 'coastal', 'rem', del)) %>%
  mutate(del = if_else(str_detect(packageid, 'arc') & level_2 == 'marine', 'rem', del)) %>%
  mutate(del = if_else(str_detect(packageid, 'bes') & level_1 == 'polar', 'rem', del)) %>%
  mutate(del = if_else(str_detect(packageid, 'bes|bnz') & level_2 == 'island', 'rem', del)) %>%
  mutate(del = if_else(str_detect(packageid, 'hfr') & level_2 == 'intertidal', 'rem', del)) %>%
  mutate(del = if_else(str_detect(packageid, 'hbr') & level_1 == 'tropical', 'rem', del)) %>%
  mutate(del = if_else(str_detect(packageid, 'mcm') & level_2 == 'marine', 'rem', del)) %>%
  mutate(del = if_else(str_detect(packageid, 'mcm') & level_2 == 'island', 'rem', del)) %>%
  mutate(del = if_else(str_detect(packageid, 'sbc') & level_1 == 'terrestrial' & level_2 == 'forest', 'rem', del)) %>%
  mutate(del = if_else(str_detect(packageid, 'nes') & level_3 == 'stream', 'rem', del)) %>%
  mutate(del = if_else(str_detect(packageid, 'nes') & level_2 == 'hillslope', 'rem', del)) %>%
  mutate(del = if_else(str_detect(packageid, 'nes') & level_2 == 'wood', 'rem', del)) %>%
  mutate(del = if_else(str_detect(packageid, 'nes') & level_2 == 'sand', 'rem', del))

df_ds_out <- df_ds_out %>%
  filter(del == '')
# -------------------------------------------
# -------------------------------------------

df_ds_out <- separate(df_ds_out, packageid, into = c('scope', 'dataset_id'), sep = '\\.', remove = F)
df_scope <- distinct(df_ds_out, scope)


# Export output
# this file is used in reformatting and in generating data for the shiny app

write.csv(df_ds_out, file = 'assigned_kw/combined/combined.csv', row.names = F)

# Export by lter site
# I have not been using this for a while

for (j in 1:nrow(df_scope)) {
  
  df_ds_site <- df_ds_out %>%
    filter(scope == df_scope[[1]][j])
  
  write.csv(df_ds_site, file = paste(output_path, '/', df_scope[[1]][j], '_combined.csv', sep = ''), row.names = F)
}

# no substrate information

# Identify cases where the substrate is present
df_eco_pres <- distinct(df_ds_out, packageid)

# Remove them via `anti_join`
df_eco_abs <- anti_join(df_datasets_raw, df_eco_pres, by = c('packageid'))

# 
df_eco_abs <- separate(df_eco_abs, packageid, into = c('scope', 'dataset_id'), sep = '\\.', remove = F)
df_scope <- distinct(df_eco_abs, scope)

# Export the set of files that lack substrate information
# write.csv(df_eco_abs, file = file.path(output_path, 'no_substrate.csv'), row.names = F)


for (j in 1:nrow(df_scope)) {
  
  df_ds_site <- df_eco_abs %>%
    filter(scope == df_scope[[1]][j])
  
  write.csv(df_ds_site, file = paste(output_path, '/', df_scope[[1]][j], '_no_kw.csv', sep = ''), row.names = F)
}

# ----------------------------------
# write all files to google drive
# this takes forever

upload_url <- googledrive::as_id("https://drive.google.com/drive/folders/1eIGRe8jPoNNnlyyEWrBEXe--RNEBXJYo")

file_names <- dir(output_path)

for (j in 1:length(file_names)) {
  
  drive_upload(file.path(output_path, file_names[j]),
               path = upload_url,
               type = "spreadsheet",
               overwrite = TRUE)
  
  
  
}

