# Fiscal Equalization Research

This repository contains the code and data used to study fiscal equalization and the distribution of federal transfers across Mexican states. The project was developed as part of the research assistance for Alejandro Chanes's undergraduate thesis.

All scripts are written in **R** and follow a typical data analysis workflow consisting of four stages:

1. **Data Cleaning** (`scripts/data.R`)
   - Reads the raw Excel files with GDP, population, federal transfers, and deflator series.
   - Converts them to a tidy long format.
   - Harmonizes state names and computes derived variables such as real transfers per capita and GDP per capita.
   - The cleaned table is saved as `participaciones-federales.csv`.

2. **Exploratory Analysis** (`scripts/summary.R`)
   - Generates descriptive statistics and diagnostic plots for the main variables.
   - Includes boxplots, histograms, scatter plots and simple regression checks.

3. **Visualization** (`scripts/summary.R` and `scripts/random-effects.R`)
   - Produces figures summarizing correlations over time and relationships between variables.
   - Creates forest plots and predicted value plots for mixed models.
   - Resulting images are stored in the repository in PNG format.

4. **Modeling** (`scripts/regressions.R`, `scripts/random-effects.R`, `scripts/bootstrap.R`)
   - Fits fixed‑effects regressions as well as hierarchical (mixed) models with random slopes.
   - Includes bootstrap routines to compute robust standard errors for random effects.
   - Outputs tables and plots summarizing the estimated relationships.

The repository also contains the final cleaned data and several generated graphics. Original raw Excel files and a PDF document of the thesis are provided for reference.

## Usage

Open each script in the `scripts/` directory and execute them sequentially in R. The scripts assume that the Excel files are present in the repository root. Generated outputs will be written to the same folder.

## License

This work is distributed under the MIT License as stated in the `LICENSE` file.

