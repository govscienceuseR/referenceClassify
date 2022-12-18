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

### 2. Reallocate authorship based on "prepared by" text  

This should maybe go into the `citation_clean()` function instead?

### 3. Disambiguate the journals based on common abbreviations   

The `journal_disambig()` function takes one argument: a column containing (potential) journal names. If working through the govscienceuseR workflow, this column name is 'container'. The column is first matched against an official index of journal abbreviations, and then remaining, unmatched potential journal names are run through a series of regular expressions for detecting typical or observed journal abbreviations. The output is a new vector of journal names that have been changed to their complete journal or agency name is matched to an abbreviation. After running the `journal_disambig()` function it can be nice to check back in on the reference matching functions (Step 1) to see how many exact matches have been gained from the disambiguation process.  

### 4. Classify the citations into general groupings using regular expressions  

The `regex_classify()` function uses regular expressions to sort matched items into classes...

### 5. Classify the citations into general groupings using Keras neural network    

The `keras_classify()` function feeds the data into a keras model, trained using data from environmental impact assessments. The input is a data.table with the following columns: author, title, publisher, and journal. The columns are used as inputs for a single feature, multi-class model to predict the likelihood that a reference is to an agency, a journal, a conference, or not a citation.  




