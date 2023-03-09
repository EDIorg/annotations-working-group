# Annotations Working Group

A development space for the EDI Annotations Working Group

## Script Explanation

- "get_eml_files": Downloads publicly-available EMLs locally for subsequent keyword parsing. Puts them in a "eml_files" folder that is not tracked by Git

- "prepare_keywords.R": Ingests the EMLs downloaded above and processes them for their keywords. If run, overwrites the contents of the "parsed_eml" folder (this folder *is* tracked by git)

