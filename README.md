# Titanic Survival Prediction | Classification Modeling & ROC Analysis in R

## Project Overview

This project applies statistical machine learning techniques to predict passenger survival in the historical Titanic dataset. The objective was to build, evaluate, and compare multiple classification models while interpreting key survival drivers.

The analysis focuses on both predictive performance and model interpretability.

---

## Objectives

- Predict survival outcome (0 = No, 1 = Yes)
- Compare multiple classification algorithms
- Evaluate performance using ROC-AUC and confusion matrix metrics
- Interpret model coefficients using odds ratios
- Identify the strongest survival predictors

---

## Dataset

The dataset contains passenger-level features:

- **Pclass** – Passenger class (1, 2, 3)
- **Sex** – Male / Female
- **Age** – Passenger age
- **SibSp** – Number of siblings/spouse aboard
- **Fare** – Ticket fare
- **Survived** – Target variable (0/1)

### Data Preparation
- Removed missing values using `na.omit()`
- 70/30 train-test split
- Set seed for reproducibility

---

## Models Implemented

- Logistic Regression
- Linear Discriminant Analysis (LDA)
- Quadratic Discriminant Analysis (QDA)
- Naive Bayes
- k-Nearest Neighbors (k = 3, 5)

---

## Model Evaluation

Models were evaluated using:

- Accuracy
- Precision
- Recall
- F1-Score
- ROC Curve
- AUC (Area Under Curve)

### Best Performing Model: Logistic Regression

- **Accuracy:** ~79%
- **AUC:** ~0.88
- Lowest error rate among all models

---

## Key Insights

- Gender was the strongest survival predictor.
- Passenger class significantly influenced survival probability.
- Age showed a negative relationship with survival.
- Logistic Regression provided the best balance between performance and interpretability.

---

## Tech Stack

- R
- ggplot2
- dplyr
- MASS
- e1071
- class
- ROCR

---

## Repository Structure

```
Titanic-Survival-Prediction/
│
├── Titanic.csv
├── analysis.R
├── ROC_Titanic_1200x800.png
└── README.md
```

---

## Learning Outcomes

- End-to-end classification modeling
- Comparative model evaluation
- ROC-AUC analysis
- Odds ratio interpretation
- Translating statistical output into business insights

---

If you found this project interesting, feel free to connect or explore the code.
