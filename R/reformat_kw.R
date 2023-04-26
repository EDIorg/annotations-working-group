
library(tidyverse)

output_path <- "assigned_kw/new_keywords"

df_ds_kw <- read.csv('assigned_kw/combined/combined.csv', header = T, as.is = T)
df_datasets <- read.csv('parsed_eml/datasetKeywords.csv', header = T, as.is = T)

df_ds_kw_long <- df_ds_kw %>%
  select(packageid, main, level_1, level_2, level_3) %>%
  gather(level, term, 3:5) %>%
  distinct()

df_ds_new_kw <- df_ds_kw_long %>%
  group_by(packageid, main) %>%
  summarise(new_keywords = paste(term, collapse = ', ')) %>%
  mutate(new_keywords = str_remove_all(new_keywords, 'NA,')) %>%
  mutate(new_keywords = str_remove_all(new_keywords, ', NA')) %>%
  mutate(new_keywords = str_replace_all(new_keywords, ',,', ',')) %>%
  ungroup()

df_ds_new_kw <- spread(df_ds_new_kw, main, new_keywords)

# Attach the dataset info to these distinct values
df_ds_out <- left_join(df_ds_new_kw, df_datasets, by = c('packageid'))

# Export output

write.csv(df_ds_out, file = 'assigned_kw/new_keywords/new_keywords.csv', row.names = F)

df_ds_out <- separate(df_ds_out, packageid, into = c('scope', 'dataset_id'), sep = '\\.', remove = F)
df_scope <- distinct(df_ds_out, scope)



# Export by lter site

for (j in 1:nrow(df_scope)) {
  
  df_ds_site <- df_ds_out %>%
    filter(scope == df_scope[[1]][j])
  
  write.csv(df_ds_site, file = paste(output_path, '/', df_scope[[1]][j], '_new_keywords.csv', sep = ''), row.names = F)
}

# no keywords

# Identify cases where keywords are present
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
  
  write.csv(df_ds_site, file = paste(output_path, '/', df_scope[[1]][j], '_no_keyword.csv', sep = ''), row.names = F)
}

# ----------------------------------
# write all files to google drive

upload_url <- googledrive::as_id("https://drive.google.com/drive/folders/1lhvcgktZV2QIYEBdtDvL4mElxf2WZZTc")

file_names <- dir(output_path)

for (j in 1:length(file_names)) {
  
  drive_upload(file.path(output_path, file_names[j]),
               path = upload_url,
               type = "spreadsheet",
               overwrite = TRUE)

}

