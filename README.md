# STAT378TermPaper

## Abstract 
It is self-evident that the effective handling of crime is paramount to the well-being of any community. As such, the practice of “crime analysis” has proved to be an enduring field of statistical analysis, and recent developments in the world of police management have increasingly shifted the crime analysis focus towards predictive metrics at a rapid pace. In this paper, we explore statistics pertaining to major metropolitan areas in the US during the 1970s and aim to identify relationships between total serious crime and several other statistics. To achieve this goal, we use multiple linear regression analysis in R and ultimately fit a final model using Ridge regression. We split the dataset into a prediction and estimation set using the DUPLEX algorithm. The initial model that we fit did not satisfy the assumptions of linear regression, so we had to consider various transformations to satisfy the assumptions. In addition, we added interaction terms to the model. We eventually used forward stepwise regression to conclude the final model. For our results, we will first check the validity of the model using our prediction data to see if the model is valid, then we will fit our model to the full data set and check to see if we pass our assumptions. Afterwards, we will check for influential and leverage points in order to see if we require robust regression. Additionally, we will check if our model has a multicollinearity problem and if we do we will find our final model by doing penalized regression. Our final model features the variables land area, total population, percentage of population 65 or older, number of active physicians, percentage of high school graduates, civilian labour force, total person income, a set of indicator variables corresponding to the broader region, and several interaction terms.

## Repository Directory
### code
- methodology: code used in the methodology section of the paper
  - [`fitting_the_regression_model.R`](https://github.com/Jacob-Winch/STAT378TermPaper/blob/main/code/methodology/fitting_the_regression_model.R): R script to fit the initial linear regression model.
  - [`possible_interaction_between_regressors.R`](https://github.com/Jacob-Winch/STAT378TermPaper/blob/main/code/methodology/possible_interaction_between_regressors.R): R script to find possible interaction terms between regressors.
  - [`residual_analysis`](https://github.com/Jacob-Winch/STAT378TermPaper/blob/main/code/methodology/residual_analysis.R): R script to graphical analyse the residuals to analysis assumptions of the model.
  - [`splitting_the_data_via_duplex.R`](https://github.com/Jacob-Winch/STAT378TermPaper/blob/main/code/methodology/splitting_the_data_via_duplex.R): R script to split the data into a prediction and estimation set via the DUPLEX algorithm.
  - [`transformations.R`](https://github.com/Jacob-Winch/STAT378TermPaper/blob/main/code/methodology/transformations.R): R script to determine transformations to satisfy linear regression assumptions.
  - [`variable_selection.R`](https://github.com/Jacob-Winch/STAT378TermPaper/blob/main/code/methodology/variable_selection.R): R script to select the best model.

- results: code used in the results section of the paper
    - [`All_data_final_model_residual_analysis.R`](https://github.com/Jacob-Winch/STAT378TermPaper/blob/main/code/results/All_data_final_model_residual_analysis.R): R script to fit the best model to all the data.
    - [`fitting_the_model_to_the_full_data.R`](https://github.com/Jacob-Winch/STAT378TermPaper/blob/main/code/results/fitting_the_model_to_the_full_data.R): R script to fit the best model to all the data.
    - [`model_full_model_selection_press_and_influential_points.R`](https://github.com/Jacob-Winch/STAT378TermPaper/blob/main/code/results/model_full_model_selection_press_and_influential_points.R): R script to determine PRESS statistics, and influential points for the full model.
    - [`model_validation.R`](https://github.com/Jacob-Winch/STAT378TermPaper/blob/main/code/results/model_validation.R): R script to find R<sup>2</sup><sub>prediction</sub>.
    - [`multicollinearity.R`](https://github.com/Jacob-Winch/STAT378TermPaper/blob/main/code/results/multicollinearity.R): R script to determine multicollinearity for the final model.
    - [`penalized_regression_model.R`](https://github.com/Jacob-Winch/STAT378TermPaper/blob/main/code/results/penalized_regression_model.R): R script to fit a penalized regression model to deal with the multicollineartity problem.

### data
- [`Term Paper - Fall 2023 - Data.txt`](https://github.com/Jacob-Winch/STAT378TermPaper/blob/main/data/Term%20Paper%20-%20%20Fall%202023%20-%20Data.txt): Full dataset.
- [`data_estimation_duplex.txt`](https://github.com/Jacob-Winch/STAT378TermPaper/blob/main/data/data_estimation_duplex.txt): Estimation set from DUPLEX algorithm. 
- [`data_estimatin_random.txt`](https://github.com/Jacob-Winch/STAT378TermPaper/blob/main/data/data_estimation_random.txt): Estimation set from random selection.
- [`data_prediction_duplex.txt`](https://github.com/Jacob-Winch/STAT378TermPaper/blob/main/data/data_prediction_duplex.txt): Prediction set from DUPLEX algorithm.
- [`data_prediction_random.txt](https://github.com/Jacob-Winch/STAT378TermPaper/blob/main/data/data_prediction_random.txt): Prediction set from random selection.

### plots
- Some plots used in the report, not all the plots.

### [`Using Regression to Predict Total Crime.pdf`](https://github.com/Jacob-Winch/STAT378TermPaper/blob/main/Using%20Regression%20to%20Predict%20Total%20Crime.pdf)
