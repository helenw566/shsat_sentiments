## Examining SHSAT Sentiments

Helen Wang (@helenw566, email: hw566@georgetown.edu)

#### BLUF

> The project examines headlines and abstracts/snippets across NYC-based newspapers to examining how semantic associations and stances towards the Specialized High School Admissions Test (SHSAT) has changed over time, specifically pre- and post-2018. The project was created for PPOL 6801: Text as Data - Computational Linguistics and deliverables include a project proposal, presentation, and final report. For this project, the following objectives were accomplished:
> 1.	Diachronic Semantic Analysis of SHSAT headlines, specifically in the context of DEI and Merit.
> 2.	Stance Detection using off-the-shelf Classifers and manual hand valdidation.


#### Reposiory Contents

This repository contains the following folders: 

- `data`
- `scripts`
- `proposal`
- `report`
- `presentation`
- `validation`
- `graphs`

Descriptions of folder contents can be found below.

#### Data

This folder contains results from scraping 10 different NYC based news outlets (see `scripts\01_data_collection.ipynb` for replication). The merged and processed results of this scraping are available in `scraping_results.csv`. In addition, we have the raw results from the stance detection conducted by the off-the-shelf classifer in `stance_detection.json` and the preprocessed version in `stance_detection_df.csv`.

#### Scripts

This folder contains all the .ipynb and .rmd files used to scrap data, preprocess, and run analyses along with a .R file containing the functions used. Here are the descriptions of the purposes and outputs of each script:

- `01_data_collection.ipynb`: This script collects headlines and related metadata from NYC-local news outlets on articles that cover the SHSAT. It collects data via either API calls or webscraping, wrangles data into a uniform structure, and saves the results.
- `02_preprocessing.ipynb`: This script (1) merges data to create the final dataset and (2) runs some exploratory data analysis of variables.
- `03_semantic analysis.rmd`: This script runs the diachronic semantic analysis, including the exploration of two different approaches, implementation of bootstrap resampling, and graph production.
- `04_stance_detection.ipynb`: This script calls the off-the-shelf classifer for stance detection, including aggregation of stance detection for multiple target words.
- `05_stance_analysis.rmd`: This scripts run rudimentary models and EDA for the stance detection, computes kappa for hand validation, and compares model performance to human performance.
- `functions.R`: This contains all the self-made functions used for `03_structural_topic_modeling.rmd` and `04_networks.rmd`.

#### Proposal

This folder contains a .docx files containing the original project proposal.

#### Report

This folder contains a .pdf of the final report.

#### Presentation

This folder contains a .pdf of the final presentation, given on December 8th, 2025. The results presented are not up to date (missing ~20 documents) but are essentially identical.

#### Validation

This folder contains .csv of the hand coded validation results of 200 randomly sampled documents.

#### Graphs

This folder contains .png of the final graphs used in the report.

