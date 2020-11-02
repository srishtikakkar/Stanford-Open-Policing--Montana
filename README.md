# MSc Thesis - Doing Without Speed Limits: Report on Montana Highway Patrol

This is a thesis project completed as part of the ST606 - Msc in Data Science and Analytics- Project & Disseration at Maynooth University, Ireland.

## Abstract 

The project presents an analysis and modelling techniques, an attempt to deal with the over-speeding problem that Montana state is facing since the government passed the rule that abolished the day-time speeding limit. The dataset contains various attributes and through the analysis performed we find the best set of variables that associate with the state’s speeding violation problem. We look for modelling strategies to fit data variations and are able to achieve the fit capturing most of the variation exhibited by our dependent variable (number of speeding stops for each outcome group: warning, citation and arrest) . The dataset used for analysis is over-dispersed and the results of Poisson Model will lead to make incorrect inferences. Alternative method of dealing with over-dispersed count data like negative binomial is used. After comparing each fitted model against their AICs, negative binomial model is found to best fit the data. Also, the full model with negative binomial fit displayed better results than the pruned negative binomial models.

 For more information, please refer to the [main thesis report](https://github.com/srishtikakkar/Stanford-Open-Policing--Montana/blob/master/Report/DoingWithoutSpeedLimits.pdf).

### Author
Srishti Kakkar, MSc Data Science and Analytics candidate

### Supervisor
[Dr. Niamh Cahill](https://www.maynoothuniversity.ie/people/niamh-cahill) , Lecturer / Assistant Professor at Maynooth University

## Repository contents
code/ - source code written for the analysis of the the traffic stop data for the Montana State, US
analysis/ - main thesis report, supplementary material and research data management plan
images/- various .png files representing the visualisation and modeling results
data/ - CSV file used for the project
