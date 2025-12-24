# Billboard Chart Success and Spotify Audio Features

This project explores whether measurable Spotify audio features can help explain and predict chart success in the Billboard Hot 100 between the years of 2000 to 2023.

This if for the IJC437 Introduction to Data Science assessment

Research questions

1. Which Spotify audio features are associated with higher chart success?
2. How have the characteristics of highly successful songs changed over time?
3. Can Top 10 chart success be predicted using audio features alone?

The data and methods

The dataset combines weekly Billboard Hot 100 chart data with Spotify audio features.
These include danceability, energy, valence, tempo, loudness and acousticness.

The analysis is exploratory but also uses modelling in the workflow
What was done is listen below:

-Data cleaning and preprocessing to have one observation per song
-Exploratory analysis using correlations and scatterplots
-Trend analysis comparing the Top 10 and non Top 10 songs over time
-Predictive modelling using logistic regression and a decision tree

All analysis was conducted in R using functions and techniques 

The key findings

-Individual audio features show only weak associations with the peak chart position
-Trends over time indicate changes in the sound profile of highly successful songs, particularly in loudness and emotional tone
-Predictive models have limited accuracy, suggesting that audio features alone cannot fully explain the chart success

Code and outputs

All code/tables/figures used in the coursework report are available in themain GitHub repository:

https://github.com/emilyarusty-cmd/IJC437-Coursework-using-the-Billboard-dataset

Running the analysis script reproduces all results presented in the report

