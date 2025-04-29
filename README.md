# Repository Information

This repository is for the final ML (Machine Learning) project for "CRSS 8030 - Data Science and Statistical Programming Applied to Agriculture"" class of Dr. Leo Bastos.


# Collaborators

Collaborators are listed below in alphabetical order:

- Md Shakir Moazzem (PhD student in Food Science)
- Umar Munir (PhD student in Crop and Soil Science)

# Goal of the project

We have been provided with a **corn variety trial** data set.  
This data contains over **164,000 rows** spanning **10 years** (2014-2023), **45 sites** across the USA, for a total of **270** site-years, where over **5,000** corn hybrids were evaluated.  

We have been provided with the following **training data** for years 2014-2023:   

  - Trait information (including site, year, hybrid, yield, and grain moisture) [the yield that we see in this "trait information" is for whatever moisture it is being reported right next to it. We will need to transform this to 15.5% moisture, which is the default that we report the corn grain yield] 
  - Meta information (including site, year, previous crop, planting date, harvest date, longitude, and latitude)  
  - Soil information (including site, year, soil pH, soil organic matter, soil P, soil K)  

We are also being provided with the following **testing data** for year 2024:  
  - Submission information (site, year, hybrid, **no yield**)  
  - Meta information (same as training)  
  - Soil information (same as training)  

We are tasked in training machine learning models using the training data to predict yield on the test data.  

One difference from class is that **we are not provided with the test data yield (predicted variable)**, only the predictor variables.  
Therefore, using the training data wisely to create predictive models will be important. 

> The goal of the project is to train machine learning models with XGboost and shiny app to predict the yields for the year 2024 with year, site, and hybrid combinations as listed in the provided "testing_submission.csv" file.

