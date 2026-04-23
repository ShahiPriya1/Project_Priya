# Whitefly Host Preference on Cotton and Aromatic Herbs

## Data Source and Description 
The dataset for this project comes from a dual-choice bioassay designed to evaluate the host plant preference of Bemisia tabaci (sweetpotato whitefly) against three aromatic herbs — Mint, Sage, and Basil — each paired with Cotton. The data were collected in a controlled laboratory experiment at Auburn University. In each trial, 15 pairs of adult whiteflies were exposed for 48 hours to two detached leaves (one Cotton, one herb) placed in an experiment box with the lower leaf surface accessible. Egg and nymph counts on each leaf were recorded at 48 hours and on day 10 using a microscope. Each herb-Cotton pair was replicated 30 times, giving 90 trials and 180 total observations.

## Data Analysis Plan
- Data reshaping and visualization using the tidyverse (e.g., dplyr, ggplot2)

- Exploratory Data Analysis (EDA) including summary statistics and host preference visualizations across arenas

- Preference Index (PI) calculation per replication to quantify relative attractiveness of each herb compared to Cotton

- Non-parametric statistical testing using the Wilcoxon signed-rank test (wilcox.test) to compare egg and nymph counts between Cotton and each aromatic herb within each arena

- Preference Index values tested against zero (no preference) using a one-sample Wilcoxon test to determine if whitefly preference for Cotton was statistically significant

*All figures are generated to be manuscript-ready using ggplot2.*

## Reproducible Workflow

- Version control using Git and GitHub to track all code and file changes

- R Markdown used to document code, analysis, and interpretation in an executable format

- Relative paths used throughout to ensure consistent file referencing

- Scripted data pipeline to automate data cleaning, analysis, and figure generation

- README documentation and final scripts made public in this repository for reproducibility and reuse


## File tree
```
├── Project_Priya.R
├── Project_Priya.Rmd
├── Project_Priya.Rproj
├── Project_Priya.docx
├── Project_Priya.html
├── Project_Priya.md
├── Project_Priya_files
│   └── figure-gfm
│       ├── fig-eggs-1.png
│       ├── fig-nymphs-1.png
│       └── fig-pi-1.png
├── Project_data.csv
└── README.md
```

##  Software Requirements

- R version 2026.01.0+392.
- RStudio
- Required libraries:`tidyverse`, `lme4`, `emmeans`, `multcomp`, `multcompView`, `ggplot2`,`fs`

