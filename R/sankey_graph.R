
library(networkD3)
library(tidyverse)
library(webshot)
library(googledrive)

output_path <- "assigned_kw/"

df_ds_kw <- read.csv('assigned_kw/combined/combined.csv', header = T, as.is = T)

df_ds_kw <- df_ds_kw %>%
  separate(packageid, into = c('scope', 'dataset_id'), sep = '\\.', remove = F)

scope <- unlist(distinct(df_ds_kw, scope))

for (j in 1:length(scope)) {
  
  ch_scope <- scope[j]
  
  df_by_scope <- df_ds_kw %>%
    filter(scope == ch_scope)
  
  main_kw <- unlist(distinct(df_by_scope, main))
  
  for (k in 1:length(main_kw)) {
    
    ch_main_kw <- main_kw[k]
    
    df_by_scope_main_kw <- df_by_scope %>%
      filter(main == ch_main_kw) %>%
      arrange(main, level_1, level_2, level_3)
    
    
    main <- distinct(df_by_scope_main_kw, main) %>%
      rename(name = main) %>%
      mutate(group = "m")
    
    level1 <- distinct(df_by_scope_main_kw, level_1) %>%
      rename(name = level_1) %>%
      mutate(group = "l1")
    
    level2 <- distinct(df_by_scope_main_kw, level_2) %>%
      rename(name = level_2) %>%
      mutate(group = "l2")
    
    level3 <- distinct(df_by_scope_main_kw, level_3) %>%
      rename(name = level_3) %>%
      mutate(group = "l3")
    
    nodes <- rbind(main, level1, level2, level3) %>%
      filter(!is.na(name))
    
    nodes$group <- as.factor(nodes$group)
    
    links1 <- df_by_scope_main_kw %>%
      select(packageid, main, level_1) %>%
      rename(source = main) %>%
      rename(target = level_1) %>%
      distinct() %>%
      select(source, target)
    
    links2 <- df_by_scope_main_kw %>%
      select(packageid, level_1, level_2) %>%
      rename(source = level_1) %>%
      rename(target = level_2) %>%
      filter(!is.na(target)) %>%
      distinct() %>%
      select(source, target)
    
    links3 <- df_by_scope_main_kw %>%
      select(packageid, level_2, level_3) %>%
      rename(source = level_2) %>%
      rename(target = level_3) %>%
      filter(!is.na(target)) %>%
      distinct() %>%
      select(source, target)
    
    links <- rbind(links1, links2, links3) %>%
      filter(!is.na(source))
    
    links$IDsource <- match(links$source, nodes$name)-1 
    links$IDtarget <- match(links$target, nodes$name)-1
    
    links <- links %>%
      group_by(source, target, ) %>%
      mutate(value = n()) %>%
      distinct()
    
    links <- as.data.frame(links)
    
    p <- sankeyNetwork(Links = links, Nodes = nodes,
                       Source = "IDsource", Target = "IDtarget",
                       Value = "value", NodeID = "name", sinksRight = FALSE,
                       NodeGroup = "group", fontSize = "12")
    p
    
    html_name <- paste(output_path, main_kw[k], "/", scope[j],'_', main_kw[k], '.', "html", sep = "")
    
    png_name <- paste(output_path, main_kw[k], "/", scope[j],'_', main_kw[k], '.', "png", sep = "")
    
    saveNetwork(p, html_name)
    
    webshot::webshot(html_name, png_name, vwidth = 1000, vheight = 900)
    
  }
}


# ----------------------------------
# write all files to google drive
main_kw <- c("discipline","ecosystem", "events", "methods",
             "org_unit","organism","process","substrate")

g_folder <- c(discipline = "https://drive.google.com/drive/folders/1zlnP0SHlDGCvme9Kick23Wx6g1CRz7lJ",
              ecosystem = "https://drive.google.com/drive/folders/1ddzwrXmMxQOJkYoIv7G5WcbAIoYaP_7W",
              events = "https://drive.google.com/drive/folders/1OngdxZVIbEtLpcBJHqgM-0eyqzeLmZhx",
              methods = "https://drive.google.com/drive/folders/1lQglEnwLLt9_iqtBigiP_ihQ_l8AxsJ1",
              org_unit = "https://drive.google.com/drive/folders/1VoKgxYYSPdEFenaBHx8k8chRYvW7uYHf",
              organism = "https://drive.google.com/drive/folders/1C1WT8SZ8spwHvNkUeKGCzPZTAnrg2mU7",
              process = "https://drive.google.com/drive/folders/1axGs4_aOzCPpZ8FNCnLJBOYkhRBaG7pI",
              substrate = "https://drive.google.com/drive/folders/1ECwnOrBbr8M55Yuq-fFICtJocMt4ntv4")

for (i in 2:length(main_kw)) {
  
  folder_url <- g_folder[main_kw[i]]
  
  upload_url <- googledrive::as_id(folder_url)
  
  dyn_path <- paste(output_path, main_kw[i], sep = "")
  
  file_names <- dir(dyn_path, "\\.png")
  
  
  for (j in 1:length(file_names)) {
    
    drive_upload(file.path(dyn_path, file_names[j]),
                 path = upload_url,
                 overwrite = TRUE)
    

  }
}
