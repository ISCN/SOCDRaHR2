# Purpose

This is an R package with untility functions to assist in harmonizing digitial soil repositories.
The purpose of the Soil Organic Carbon Data Rescue and Harmonization Repository (SOC-DRaHR) is to collect haromonization scripts for data sets related to soil carbon to enable scientific research.
This project identifies soil carbon datasets that are publicly avaiable, provides data harmonization scripts to integrate those data sets, and provides output scripts for a harmonized data product.
Data can come from field surveys, field manipulation studies, and laboratory experiments.

This project is not a _data_ repository or archive but instead a code repository.
End users are responsible for complying with ALL data use policies of the orginal data providers, please check with the orginal archives and reposities to ensure you are complying with use policies.

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
install_github("ISCN/SOCDRaH2")
library(SOCDRaH2)
ISCN3 <- ISCN3(dataDir='YourDataDir')
```

# How to contribute
Please see the [CONTRIBUTING](CONTRIBUTING.md) document for more details on how to contribute, including how to *identify* datasets, contribute to the *code*, and *everything else* that is needed to run an open source community project.
Contributers will be offered co-authorship for repository DOI but can not be guaranteed co-authorship on manuscripts, proposals or studies that utilize this repository.
[This repository is licensed here](LICENSE).

# Citations
Repository versions will be assigned DOIs as needed.
Any manuscripts which use these data harmonization scripts are asked to cite the appropreate repository version but are not required to list contributers as co-authors.
The aggregation scripts here are licensed under BSD 2-clause. See [LICENSE](LICENSE.txt) for details.

# Outside links of interest

[International Soil Radiocarbon Database (ISRaD)](https://soilradiocarbon.org/) a data curation project around soil radiocarbon and fractionation measurements.

[Environmental Data Initiative](https://environmentaldatainitiative.org/) has a [GitRepo](https://github.com/EDIorg/ecocomDP) that has a similar approach to aggregating survey data.

[Earth Sciences Information Partners](http://www.esipfed.org/)

[DataOne](https://www.dataone.org/) is a go-to resource for finding environmental data in general and searches many different established repositories. They have an R package for interacting with their search engine [here](https://jsta.github.io/2017/03/28/dataone.html)

[rOpenSci Project](https://ropensci.github.io/)

Five ways consortia can catalyse open science, [Nature, 2017](http://www.nature.com/news/five-ways-consortia-can-catalyse-open-science-1.21706)

[National Data Service](http://www.nationaldataservice.org/)


