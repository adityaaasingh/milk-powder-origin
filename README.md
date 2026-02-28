# milk-powder-origin

Milk Powder Origin Classification

Overview
This project investigates whether trace elemental composition (ppm) can be used to classify the regional origin of milk powder samples. Using a Random Forest classifier and feature selection techniques, a reduced model was developed and validated.
The final model achieved ~96% accuracy under repeated 10-fold cross-validation, demonstrating strong geographic signal in elemental profiles.

Methodology
- Replaced <LOQ values using element-specific LOQ/2 (to reduce bias from zero substitution)
- Trained a Random Forest classifier (1000 trees)
- Ranked predictors using Mean Decrease Accuracy (MDA)
- Selected the top 10 most informative elements
- Validated using repeated 10-fold cross-validation (5 repeats)
- Visualised structure using PCA and boxplots

Key Results
- Accuracy: ~96%
- Kappa: ~0.94
- Reduced model outperformed the full model, indicating signal concentration in a small subset of elements.
- PCA confirmed clear regional clustering.
