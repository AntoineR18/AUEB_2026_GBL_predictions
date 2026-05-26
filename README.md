# Predictive models for Greek Basketball League (GBL)

## Objective
In the context of my internship at the Athens University of Economics and
Business (AUEB), I have been asked to predict the results of the GBL playoffs.

## Packages used
- tidyverse (data analysis and visualization)
- patchwork (plot composition)
- xtable (table export)

## Project structure
```
AUEB_2026_GBL_predictions/
├── raw_data/
│   ├── po/
│   ├── reg/
│   └── standings/
├── outputs/
│   ├── final_predictions/
│   ├── SF_predictions/
│   ├── SF_predictions_with25/
│   └── validation/
└── scripts/
```

## Workflow
1. Data collection from Basketball Reference
2. Data cleaning and preprocessing
3. Model training
4. Prediction generation
5. Validation and output export

## Data source
[Basketball Reference](https://www.basketball-reference.com/)

## Related work
[EuroLeague predictions](https://github.com/AntoineR18/AUEB_2026_euroleague_predictions.git)
[LNB Elite predictions](https://github.com/AntoineR18/AUEB_2026_LNB_predictions.git)

## Author
Antoine Rustenholz
