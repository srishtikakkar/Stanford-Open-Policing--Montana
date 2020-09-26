## Describing the best set of features recognising the over-speeding problem of the Montana State

### Contents

1. Chapter 1: Introduction
1. Chapter 2: Data
   1. Explanatory and Outcome Variables
      1. Traffic Stop Analysis by Statewide Racial Composition
      1. Driver Gender
      1. Reason for Stop
      1. Outcome and Hour of the Day
1. Chapter 3: Methodologies
   1. Generalised Linear Models
      1. Linear Regression (Guassian Family)
      1. Poisson Regression
      1. Negative Binomial Regression
      1. Model Comparison
1. Chapter 4: Results
   1. Linear Regression (Guassian Family)
   1. Poisson Regression
   1. Negative Binomial Regression
   1. Model Comparison
1. Chapter 5: Discussion
1. Chapter 6: Conclusion

# Chapter 1: Introduction

The Stanford Open Policing Project (https://openpolicing.stanford.edu/) is collaboration between the Stanford Computational Journalism Lab and the Stanford Computational Policy Lab, and it has been the lead entity in dealing informally with the Montana Highway Patrol (MHP) for the past two years. This contact is mainly the result of general discussions related to the traffic stop data collection and statistical analysis and how this data is applied to the various issues like racial profiling (biased police), gender biasing and road rule violations. The project was formalized in late 2015 as a research contract between MHP and Stanford Computational Policy Lab. The development of this resource has been marked by challenges.

Immediately following the execution of the contract and the approval through MHP, the Open Policing Project Team began working closely with MHP project advisor and data managers on issues related to collection and analysis of traffic stop data (Gaffney, et al., 2013). Post transformations, the data were made available for analysis; extract significant disparities in policing to attempt to understand the relationship between police tactics and public behaviour. Montana was the first and only state to abolish the numerical daytime speed limit from its interstate highways.

This report is thus based on the analysis performed using the data provided through the Open Policing Project which contain traffic stop data from the period January 2009 to through December 2016. The dataset contains more than 0.7 million stop records and the demographics related with it. Through the study of relevant facts and statistics, the report proposes to detect the factors influencing the speeding violation for the state based on the attributes like subject race, gender, type of the vehicle and time. It will also break down the analysis to look at the 3 groups for the Outcome of each stop made separately, to explore how drivers’ demographic factors influence the decision-making of the police on speeding stops for the arrest-made, ticket issued or left with a warning. As the dataset contains the geographic coordinates for the stops conducted, it is expected to provide more useful insights to the results.
