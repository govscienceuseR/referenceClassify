# citationClassify

The citationClassify tool from govscienceuseR is designed to take tagged citation data produced by anystyle.io and improve accuracy through disambiguating the publishing journal or agency, and classifying citations into general groupings. See [citationClassify vignette](http://htmlpreview.github.io/?https://github.com/govscienceuseR/citationClassify/blob/master/vignettes/sgma.html) for an example using California Groundwater Sustainability Plan documents.

## Installation  

Run the following line in your console to install the package from GitHub:  
```
devtools::install_github("govscienceuseR/citationClassify")
```

## Overview  

This package expects users to begin with data table of (potential) citations that they would like to classify. This package provides the following functionality. 

### 1. Check for exact matches by publishing journal or agency  

Three functions, `journal_match()`, `agency_match()`, and `conference_match()`, look for exact matches to indices of journal, agency, and conferences names. Each function outputs a logical vector for whether there is an exact match to an index per potential citation. The default indices are journal and conference lists from [Scimago](https://rdrr.io/github/ikashnitsky/sjrdata/man/sjr_journals.html), and the agency list was constructed by the package authors with US federal agencies and partially comprehensive state-level agencies. Each function has the option to append additional data to the indices for greater personalization of the tool. For example, if the documents are from the EU, or specific to country-level agencies in a state, a new index of these agencies can be added. 

### 2. Disambiguate the journals based on common abbreviations   

The `journal_disambig()` function takes one argument: ref_dir. The function transforms the JSON files in the reference directory (ref_dir) to tabular data and compiles them all into one data frame, adding the file name as an identifier.    

### 3. Clean and filter the citation data frame    

The `citation_clean` takes one argument: dt, which is the data table output by the `citation_compile()` function. The function goes through a series of steps to try to improve Anystyle's citation output. For each column the function unlists the data and filters out unlikely candidates. For instance, if a number listed in the date column does not match any reasonable date format or expectation, it is removed. If a string in the URL column actually resembles a DOI, it is moved to that column. And so on.  

