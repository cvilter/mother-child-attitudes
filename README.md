# Final Project: Mathematical Foundations for Machine Learning

## Research Question
We proposed to predict adolescents' attitudes towards gender roles by applying machine learning methods to the detailed information available in the U.S. National Longitudinal Survey of Youth (NLSY). The longstanding survey program asks both mothers and children about their views on gender roles along with a wide range of other demographic, economic, and social questions. Though a subjective concept like "gender attitudes" presents descriptive and predictive challenges, the NLSY's depth and breadth is able to provide an exceptional level of information about children's lives and, by extension, some insight into the origins of their perspective on gender.

## Methods

For a detailed review of our data, methods, and findings, see our [final report](https://github.com/cvilter/mother-child-attitudes/blob/main/Math_for_ML_Final_Report.pdf).

## Data Preparation
Run the data preparation R scripts in the following order:
+ `requirements.R`
+ `child_vars_2.R`
+ `children_xvars.R`
+ `mother_vars.R` 
+ `mother_demographics.R`
+ `mother_vars_merging.R`

This will produce a file called `all_data.csv`.
