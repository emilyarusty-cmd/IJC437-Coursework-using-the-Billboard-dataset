# IJC437-Coursework-Billboard-Success

This repository has the R code and outputs for my IJC437 Introduction to Data Science coursework. My project as shown below, analyses the Billboard Hot 100 data from the years 2000 to 2023 to investigate whether Spotify audio features are associated with chart success and whether the Top 10 success can be predicted using such features.

# The research questions

1. Which Spotify audio features are most strongly associated with peak Billboard chart position?
2. How have the audio characteristics of Top 10 songs changed over time?
3. Can Top 10 chart success be predicted using Spotify audio features?

# What is in the repository

- `billboard_analysis.R` – main R script (data import, cleaning, analysis, visualisation, modelling)
- `billboard_24years_lyrics_spotify.csv` – dataset used in the analysis
- `tables/` – CSV tables generated for my results section
- `figures/` – PNG figures generated for my results section
- `docs/` – GitHub Pages site presenting the project findings

## How to run the analysis

1. Download this repository.
2. Open RStudio and set the working directory to the repository folder.
3. Install required packages (first time only and then use 'library' any of the other times after this): install.packages(c("tidyverse", "rpart"))
