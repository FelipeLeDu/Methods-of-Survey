# Model-Based Survey Adjustment with MRP

This project applies a model-based approach to survey adjustment using Multilevel Regression and Poststratification, commonly known as MRP.

In many real-world surveys, the final collected sample may not be fully representative of the target population. This can happen because of nonresponse, fieldwork constraints, coverage problems, or the impossibility of drawing a perfectly random sample.

MRP is used as a model-based correction method for this type of problem.

---

## Project Overview

The goal of this project is to estimate population-level outcomes from a survey sample that may not perfectly match the target population.

The method combines two steps:

1. **Multilevel regression**

   A statistical model is estimated using the survey data. The model predicts the outcome of interest based on some variables from the 
   dataset.

2. **Poststratification**

   The model predictions are then aggregated using known population counts for each category or cell. This step adjusts the estimates so that they reflect the structure of the target population rather than only the structure of the observed sample.

In simplified terms, MRP first estimates how different groups behave in the sample and then reweights those predictions according to how frequent those groups are in the real population.
