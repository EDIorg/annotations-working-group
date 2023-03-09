
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
dir.create(path = "eml_files", showWarnings = F)

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


for (j in 1:length(scopes)) {
  
  #get all identifiers for each scope
  
  identifiers <- list_data_package_identifiers(scopes[j])
  
  for (i in 1:length(identifiers)) {
    
    #get newest revision for each identifier
    
    revision <- list_data_package_revisions(scope = scopes[j],
                                            identifier = identifiers[i],
                                            filter = "newest")
    
    package_id <- paste(scopes[j],identifiers[i],revision, sep = ".")
    
    #get EML file using above function
    
    eml_xml <- map(.x = package_id, .f = poss_get_eml)
    
    # catch the case when no file is returned or save what is returned
    
    if (!is.null(eml_xml[[1]])){
      
      file_name <- paste(package_id, 'xml', sep = '.')
      
      write_xml(eml_xml[[1]], file = paste(eml_path,  file_name, sep = "/"))
    }
    
  }
  
}

