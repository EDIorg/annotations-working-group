# Annotations Working Group

A development space for the EDI Annotations Working Group

## Script Explanation
### workflow for setting up the shiny faceted search app

- "get_eml_files": Downloads publicly-available EMLs locally for subsequent keyword parsing. Puts them in a "eml_files" folder that is not tracked by Git

- "prepare_keywords.R": Ingests the EMLs downloaded above and processes them for their keywords.

- "kw_assign_combined.R": Searches the csv file with dataset metadata and assignes mapped new standard keyword hierarchy

- "prepare_data_shiny.R": formats newly assigned keywords for use in shiny faceted search app. Writes csv file needed in the 'data' folder for shiny app.

- "reformat_kw.R": produces spreadsheets for each LTER site with new keywords in their respective main categories for each dataset. Easier to read and manually evaluate.

- "shiny_ds_search/app.R": the shiny application for faceted search. Beware: the data in the folder "shiny_ds_search/data" need to be updated before publishing the app.
