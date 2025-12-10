# LSR6 - Cognitive Bias Modification (CBM) for social anxiety, a living systematic review and meta-analysis

- This project is licensed under the terms of the Creative Commons Attribution 4.0 International license (CC-BY 4.0) (https://creativecommons.org/licenses/by/4.0/). 
- Detailed methods can be found in the protocol: (https://doi.org/10.12688/wellcomeopenres.23278.2)
- If your working directory is set to LSR6_CBM_H you will be able to run the analysis and knit the R markdown.
- /data contains raw data from EPPI-reviewer for cleaning and analysis and a number of helper .xlsx files to aid the analysis
- /result contains the raw results from each NMA/pairwise MA, generated automatically by the analysis scripts
- /util contains combined_cleaning.R which cleans the EPPI-reviewer export and two analysis scripts, one for NMA and one for pairwise MA. N.B: the NMA script sources the pairwise and cleaning scripts and cannot be run independently. This folder also contains two helper .R scripts which aid the data cleaning process.
- To avoid constant re-generation of the /results file, code that creates them has been suppressed with a '#'. 
 