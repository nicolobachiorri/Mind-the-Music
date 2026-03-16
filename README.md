# Statistical Analysis on Music Listening and Mental Health

This project, carried out as part of an undergraduate university Data Mining course, analyzes the relationship between music listening habits and mental well-being variables, using linear and logistic statistical models.

## Repository Contents

- `Bachiorri_Barrow_Mezzanzanica_Saccardo_1MOD copia.r`: R script for linear modeling of music listening (target: listening hours).
- `Bachiorri_Barrow_Mezzanzanica_Saccardo_2MOD copia.R`: R script for logistic modeling (target: perceived effects of music).
- `Bachiorri_Barrow_Mezzanzanica_Saccardo_1MOD.pdf` & `2MOD.pdf`: complete model outputs with plots and diagnostic tests.
- `Bachiorri_Barrow_Mezzanzanica_Saccardo.Rdata`: dataset containing preprocessed observations ready for modeling.

## Objective

To study the impact of variables such as preferred music genre, listening hours, and activities performed while listening, on mental health indicators (anxiety, depression, insomnia, OCD), through:

- **Linear regression** (with Box-Cox transformations and multicollinearity reduction)
- **Logistic regression** (with separation handling and variable selection via AIC and VIF)

## Main Packages Used

- `mice`, `VIM` – missing value handling and visualization
- `car`, `mctest` – diagnostics and VIF
- `lmtest`, `gam`, `factorMerger` – diagnostic tests and categorical variable transformation
