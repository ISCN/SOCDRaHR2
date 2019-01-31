# How to contribute

I'm really glad you're reading this, because we need volunteers to help this project come to fruition. Thank you!

**The purpose** of this project is to collect haromonization scripts for data sets related to soil carbon.

There are three main ways to contribute to this project:
1) *Identify* datasets of interest
2) *Code* data harmonization scripts
3) *Everything else* including
  - code review
  - curation of dataset lists
  - development of control vocabularies and best practices
  - development of QA/QC scripts and software features
  - community moderators
  - documentation edits (Fixing spelling and grammer mistakes are a great first contribution!)

Please see the [README](README.md) for more details on how and when contributors are cited.

This is a passion project started by a collection of busy scientists.
We will try to respond to pull requests, issues, and general project chatter within 48 hours.
However, we may be delayed by holidays, proposal deadlines, conventions, start/end of semesters, or other insanities of academia.
Please be understanding, and if you don't hear back from someone in one week please email iscncoordinator at gmailcom.

## Indentify datasets of interest

We love data and believe that if someone took the time to record something that data has value.
That being said not every data is appropreate for this project.

For incorporation into this project a dataset needs to have:
 1) Soil carbon stocks \[mass volume-1\] quantified, preferably bulk density \[mass volume-1\] and organic carbon percentage \[mass mass-1\]. Any gap-filling needs to be clearly identified.
 2) A public archive-level DOI or permanent URL guarantee for at least 10 years
 
 Datasets will be prioritized based on:
  1) Large number of sites (latitude-longitude-depth-time) characterized
  2) Data that are currently of interest to contributors working on meta-analysis
  3) Data reuse policy
  
  If you are interested in contributing a dataset please for the repository, make a new row entry in the [data sources](DATASOURCE.md) table, and submit a pull request.
 
 We currently have trouble with high dimensional data (e.g. high time resolution, FTICR-MS, NMR, genomics, transcriptomics, etc).
 If the dataset is primarily high dimensional but also has bulk data then it may be considered for an incomplete read in.
 We hope to change this in the [future](roadmap.md).
 
 ## Code contributions
 
 We need a better guide writen for code contributions.
 
 There is a generalized [key-template script](https://github.com/ktoddbrown/soilDataR/blob/master/R/processData_Templet.R) which we suggest you use when possible.
 Place any dataset keys in the dataset_keys folder of this project; variables should be keyed to ISCN standard names when possible (see the `var` column in [ISCNKey](https://github.com/ktoddbrown/soils-long-tail-recovery/blob/master/dataset_keys/ISCNKey.xlsx)), new variables will be considered as standardized candidates for future ISCN data products.
 For more details on the expected input file formats and structure of the data key please see the [wiki](https://github.com/ktoddbrown/soils-long-tail-recovery/wiki).
 
 We will also accept unique data ingestions scripts.
 
 ## Everything else
 
 Community projects like this are not just coding and there are several things that you can do to contribute!
 If you have an idea of how to contribute to this project but do not see it listed here please email iscncoordinator at gmailcom or start an Issue in the discussion forum of this repository.
 
 # Project conventions
 
 We will make every effort to key the bulk of the files in this project flat text (ASCII) files, both to reduce the size of the repository and ensure readability of the files going forward.
 Note that there are some legacy excel files that are slated for transfer to csv and removal as of December 2017.
 Final presentations and reports may be committed to the repository as pdf documents.
 
*Documentation and non-code files* should use [Markdown](http://commonmark.org/help/) with a .md extention.
New sentences should start on a new line.
Spelling conventions should adhere to American standards (Sorry Brits! We had to pick one!).
The language must be consistant with our [Code of Conduct](CONTRIBUTING.md) and strive to be inclusive and approachable.

*Tables* intended to be machine readable (keys primarily) should be stored as csv files.
CSV files are common ',' seperate and have hard returns at the end of each row '\n'.
Headers for csv files are strongly encouraged.
CSV file headers should only contain standard alphanumerics [A-Za-z0-1] or underscores '_'.

*Code* contributions must be in [R](https://www.r-project.org/) and are encouraged to use the [Tidyverse](https://www.tidyverse.org/) libraries.
