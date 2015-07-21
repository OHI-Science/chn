README.md

gl_spp_all.csv data is from IUCN. 

Download from the IUCN API, which just has the risk category, no trend info. We have a script for SPP in the global model that iterates over all those to download the species-specific info (e.g. http://www.iucnredlist.org/details/6494/0) and scrapes that for habitat and trend (and for ICO, scrapes the countries and subpop info).
