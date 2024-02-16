
# download all publicly accessible EML files from the EDI repository

library(EDIutils)
library(xml2)
library(tidyverse)

# this function catches html return errors. EML files that are not public will be ignored
poss_get_eml <- possibly(.f = read_metadata, otherwise = NULL)

# directory to store EML files, not in git
eml_path <- "eml_files"

# Create a folder to download to if it doesn't exist locally (yet)
## This folder is not checked into git so won't be retrieved at clone
dir.create(path = eml_path, showWarnings = F)

# Identify any file(s) that have already been downloaded
downloaded_xmls <- list.files(path = eml_path, pattern = '*.xml')

# Define scopes

scopes <- c("edi", "knb-lter-and", "knb-lter-arc", "knb-lter-bes",
            "knb-lter-ble", "knb-lter-bnz", "knb-lter-cap",
            "knb-lter-cce", "knb-lter-cdr", "knb-lter-cwt",
            "knb-lter-fce", "knb-lter-gce", "knb-lter-hbr",
            "knb-lter-hfr", "knb-lter-jrn", "knb-lter-kbs",
            "knb-lter-knz", "knb-lter-luq", "knb-lter-mcm",
            "knb-lter-mcr", "knb-lter-nes", "knb-lter-nin",
            "knb-lter-ntl", "knb-lter-nwk", "knb-lter-nwt",
            "knb-lter-pal", "knb-lter-pie", "knb-lter-sbc",
            "knb-lter-sev", "knb-lter-sgs", "knb-lter-vcr",
            "msb-cap", "msb-paleon", "msb-tempbiodev")

# For each scope...
for (j in 1:length(scopes)) {
  
  #get all identifiers for each scope
  identifiers <- list_data_package_identifiers(scopes[j])
  
  # For each identifier (within each scope)...
  for (i in 1:length(identifiers)) {
    
    #get newest revision for each identifier
    revision <- list_data_package_revisions(scope = scopes[j],
                                            identifier = identifiers[i],
                                            filter = "newest")
    
    # Identify package ID
    package_id <- paste(scopes[j], identifiers[i], revision, sep = ".")
    
    # Identify file name
    file_name <- paste0(package_id, '.xml')
    
    # If the file has already been downloaded, skip it with a message
    if(file_name %in% downloaded_xmls){
      message("XML ", i, " of ", length(identifiers), " already downloaded")
      
      # If the file *hasn't* already been downloaded...
    } else {
      
      # Get EML file using above function
      eml_xml <- purrr::map(.x = package_id, .f = poss_get_eml)
      
      # If a file is returned, save it (if not, then ends this iteration of loop and continues)
      if (!is.null(eml_xml[[1]])){
        write_xml(eml_xml[[1]], file = file.path(eml_path,  file_name))
        
        # Print success message per downloaded XML
        message("Downloaded XML ", i, " of ", length(identifiers))
      }
    }
    
    #slow down the the requests to avoid running into PASTA's rate limitations
    Sys.sleep(1)
  }
  
  # Print finished message for scope too
  message("Finished downloading identifiers in scope: '", scopes[j], "'")
}

# remove the old files when a new one was downloaded

# read the new list of files after download
updated_file_list <- list.files(path = eml_path, pattern = '*.xml')

# make into data frame
df_files <- data_frame(file_name = updated_file_list)

# separate file name into parts
df_files <- df_files %>%
  separate(file_name, c("scope", "id", "version", "extension"), sep = "\\.") %>%
  mutate(package = paste(scope, id, sep = "."))

# find the older duplicate 
df_dups <- duplicated(df_files$package, fromLast = T)

# add column of true false for being the older duplicate
df_files$dups <- df_dups

# put the file name back together
df_dup_files <- df_files %>%
  filter(dups == TRUE) %>%
  mutate(file_name = paste(scope, id, version, extension, sep = "."))

# remove the older EML version when a new on was downloaded
for (j in 1:nrow(df_dup_files)) {
  
  print(df_dup_files$file_name[j])
  
  file.remove(paste(eml_path, df_dup_files$file_name[j], sep = "/"))
  
  
}
  



