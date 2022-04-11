# Purpose

This is an R package with utility functions to assist in harmonizing digital soil repositories.
The purpose of the Soil Organic Carbon Data Rescue and Harmonization Repository (SOC-DRaHR) is to collect harmonization scripts for data sets related to soil carbon to enable scientific research.
This project identifies soil carbon datasets that are publicly available, provides data harmonization scripts to integrate those data sets, and provides output scripts for a harmonized data product.
Data can come from field surveys, field manipulation studies, and laboratory experiments.

This project is not a _data_ repository or archive but instead a code repository.
End users are responsible for complying with ALL data use policies of the orginal data providers, please check with the orginal archives and repositories to ensure you are complying with use policies.

# Project links

Here are some important resources to get you started:
1) [International Soil Carbon Network](http://iscn.fluxdata.org/) is our parent organization.
2) [Our code of conduct](CONTRIBUTING.md) is based on the [The Contributor Covenant](https://www.contributor-covenant.org/).
3) [How to contribute](CONTRIBUTING.md)
4) [Our mailing list](https://groups.google.com/forum/#!forum/soc-drahr)

# Quick start

To quickly access ISCN3 data:
```R
library(devtools)
install_github("ISCN/SOCDRaHR2")
library(SOCDRaH2)
ISCN3 <- ISCN3(dataDir='YourDataDir')
```

# Harmonization principles

1) Data is downloaded from an archived url link.
2) Meta data is encoded as a set of data table, and is ideally generated from the associated metadata on the archiving repository.
3) Data columns that share common `variables` are directly comparable with trivial unit conversions.

## Data download

Data should be archived on a TRUST-ed repository (Lin et al, 2020) with an associated download url.
SOC-DRaR2 scripts will then fetch the data from the identified url and may transform the data depending on the original structure.
In general, `R/read*.R` scripts return a set of relational data tables that is wide or un-normalized where each row is a unique sample and each column an associated measurement or information about a measurement.
The distinction between data and metadata is blurry and often varies between each contributing data set.

## Meta-data as data tables

While we acknowledge that meta-data is often encoded as some XML tree structure, for this project we have opted to go with a data tables.
The metadata has the following structure:

header | description
------------|-------------
 `data_id`  | the name of the data product
 `table_id` | the name of the data table
 `column_id`| the name of the column header OR `NA` if the information is in the `entry` column
 `variable` | a valid ISCN measurement name that the column is associated with (see more on variables below)
 `data_type` | what kind of is is either in the column OR encoded in the data_entry
 `entry` | `NA` if this describes the values of a column OR the value of the data_type

For example, in ISC3:

data_id  |  table_id | column_id | variable | data_type | entry
---------|---------|-----------|----------|-----------|------
  ISCN3  |   `NA`  | `NA`      |    `NA`  | download_url | `ftp:[...]xlsx`
  ISCN3  |  layer  | 13c (‰)   |  13c     | value_number | `NA`
  ISCN3  |  layer  |   `NA`    |  13c     | unit      | permille

Note that this format is easy to derive from a wide table format

data_id  |  table_id | column_id | variable | data_type |  value_string |   unit 
---------|----------|-----------|-----------|-----------|---------------|------------
  ISCN3  |   `NA`   | `NA`  | download_url | `NA`       |  `ftp:[...]xlsx` |  `NA`   
  ISCN3  |  layer  | 13c (‰)   |  13c     | value_number |  `NA`         |  permille
  
Valid `data_type` entries include: `value_number`, `value_string`, `unit`, `sigma`, `measurement_method`, `observation_note`, and `id`.

The proposed vocabulary list from the ISCN 2016 data template is in `data/vocabularyList.csv`.
ISCN 2016 has 295 terms grouped across 9 tables. 
This has been reduced to 123 terms across 5 tables for ISCN3 and many of the 2016 template terms were never used.
In addition 58 new terms were added by ISCN3, mainly from the NRCS-Kellog Soil Science Lab.
This is currently being document and cleaned up.

An example of the head of this vocabulary is:

data_table|data_column|variable|unit
----------|-----------|-------------------|----
site|site_name|Site Name|NA
site|site_note|Site Notes|NA
site|country|Country|NA
site|province|Province|NA
site|state|State|(us_states)
site|county|County|NA
site|lat|Latitude|dec. deg
site|long|Longitude|dec. deg

## Comparable data share `variable`

Data columns that are comparable with trivial conversions (generally unit conversion) share a `variable`.
In some cases this requires unit conversions (eg grams to kilograms) but conversion factors should be coded as separate variables (eg loss on ignition times a conversion factor equals organic carbon fraction).

A current draft of the variable table is here `data/vocabularyList.csv`, it is under active development.


# How to contribute

Please see the [CONTRIBUTING](CONTRIBUTING.md) document for more details on how to contribute, including how to *identify* datasets, contribute to the *code*, and *everything else* that is needed to run an open source community project.
Contributors will be offered co-authorship for repository DOI but can not be guaranteed co-authorship on manuscripts, proposals or studies that utilize this repository.
[This repository is licensed here](LICENSE).

# Citations
Repository versions will be assigned DOIs as needed.
Any manuscripts which use these data harmonization scripts are asked to cite the appropreate repository version but are not required to list contributors as co-authors.
The aggregation scripts here are licensed under BSD 2-clause. See [LICENSE](LICENSE.txt) for details.

# References

Lin, D., Crabtree, J., Dillo, I. et al. The TRUST Principles for digital repositories. Sci Data 7, 144 (2020). https://doi.org/10.1038/s41597-020-0486-7

# Outside links of interest

[International Soil Radiocarbon Database (ISRaD)](https://soilradiocarbon.org/) a data curation project around soil radiocarbon and fractionation measurements.

[Environmental Data Initiative](https://environmentaldatainitiative.org/) has a [GitRepo](https://github.com/EDIorg/ecocomDP) that has a similar approach to aggregating survey data.

[Earth Sciences Information Partners](http://www.esipfed.org/)

[DataOne](https://www.dataone.org/) is a go-to resource for finding environmental data in general and searches many different established repositories. They have an R package for interacting with their search engine [here](https://jsta.github.io/2017/03/28/dataone.html)

[rOpenSci Project](https://ropensci.github.io/)

Five ways consortia can catalyse open science, [Nature, 2017](http://www.nature.com/news/five-ways-consortia-can-catalyse-open-science-1.21706)

[National Data Service](http://www.nationaldataservice.org/)

[NCSS database description](https://ncss-tech.github.io/lab-data-delivery/SDA.html)

