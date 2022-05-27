# End Customer Grouping Prediction
- Business Problem: There are roughly 10K of new end customers appear in sales report per month. My job, as the data specialist, is to make sure those new end customers with high budget flags are assigned to the correct groups. However, messy data can make the cleaning and grouping process time consuming. 
  - Sample of the new end customers exported

    | New End Customers Exported| Budget USD (M)| 
    | ------------- |:-------------------------:| 
    | CREATION TECHNOLOGIES WISCONSIN INC.     | $1600 | 
    | APPLIED TECH NO      | $100  |  
    | ASTROCAST SA | $1    |    
- Current Solution: Before implementing the model, we group those manually. We identify those customers with high budget flags and assign the correct groupings to them.
- Goal: The model is designed to efficiently group similar customers in a large dataset with less working time and higher accuracy.
- Tech used: R-Studio

## Project Details
A Naive Bayes classifer will be used in this machine learning modeling. The process can be broken down into below pieces:
  1. Load Data
  2. Data Preparation
  3. Perform Down Sampling
  4. Text Mining on both training and testing dataset
  5. Visualization
  6. Naive Bayes Model Preparation
  7. Train model on the data
  8. Predict and Evaluate

## Why chosing Naive Bayes Model?
 - Please refer to another repository regarding text mining.

