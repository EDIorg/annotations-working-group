

library(tidyverse)

df <- read.csv("assigned_kw/combined/combined.csv", header = T, as.is = T)
main <- c("ecosystem", "discipline", "process", "org_unit",
          "organism", "methods", "events", "substrate", "substance")
level <- rep("level_1",9)
term <- rep("", 9)
df_terms_empty <- data.frame(main, level, term)

df_sub_long <- df %>%
  #filter(str_detect(packageid, "ntl|arc|sbc")) %>%
  select(packageid,main, level_1, level_2, level_3, title, abstract) %>%
  separate(packageid, into = c("siteid", "package_num"), sep = "\\.", remove = F) %>%
  mutate(siteid = str_remove(siteid, "knb-lter-")) %>%
  select(-package_num) %>%
  gather(level, term, 4:6) %>%
  filter(!is.na(term)) %>%
  distinct()


df_sub <- df_sub_long %>%
  group_by(packageid) %>%
  mutate(new_keywords = paste(term, collapse = ', '), ) %>%
  mutate(new_keywords = str_remove_all(new_keywords, 'NA,')) %>%
  mutate(new_keywords = str_remove_all(new_keywords, ', NA')) %>%
  mutate(new_keywords = str_replace_all(new_keywords, ',,', ',')) %>%
  select(-level,-term, -main) %>%
  slice(1)

#df_sub <- spread(df_sub_main, main, new_keywords)

df_terms <- df_sub_long %>%
  distinct(main, level, term) %>%
  filter(!is.na(term)) %>%
  filter(level != "level_3") %>%
  filter(level != "level_2") %>%
  rbind(df_terms_empty) %>%
  arrange(main, level, term) %>%
  distinct()

write.csv(df_sub, file = "shiny_ds_search/ds_search/data/df_sub.csv", row.names = F)
write.csv(df_sub_long, file = "shiny_ds_search/ds_search/data/df_sub_long.csv", row.names = F)
write.csv(df_terms, file = "shiny_ds_search/ds_search/data/df_terms.csv", row.names = F)
write.csv(df_terms_empty, file = "shiny_ds_search/ds_search/data/df_terms_empty.csv", row.names = F)

save(df_sub, df_terms, file = "shinyds_search/ds_search/data/edi_ecosystem.RData")

