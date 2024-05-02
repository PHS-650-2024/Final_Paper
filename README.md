# Olympics Secondary Analysis

This repository contains the analysis code, datasets, and results for our study focused on the impact of demographic factors on Olympic athletes' medal winnings among those competing in the Games from 1994 onward. The analysis is structured around two specific aims, each examining different aspects of the athletes' data (i.e., biological sex and age).

## Repository Structure

- **/R/**: This folder contains R scripts used for data scoring and analysis.
  - `0.Scoring.R`: Script for preparing and scoring the dataset.
  - `1.Aim1.R`: Analysis script for Aim 1, focusing on athlete sex and gold medal winnings.
  - `2.Aim2.R`: Analysis script for Aim 2, focusing on age and gold medal winnings.
  
- **/figs/**: Directory where all figures generated by the analysis scripts are saved as PNG files.
  
- **/results/**: Contains the output of analyses, including CSV and RDS files with the summarized results.
  
- **/data/**: Holds the raw data (`olympics.csv`) and the scored dataset (`oly_scored.csv`).

- **Final_paper.qmd**: A Quarto markdown file that includes detailed sections outlining the: 
  - **Introduction**: Overview of the research goals and significance.
  - **Method**: Details on the dataset, variable scoring, and analytical methods used.
  - **Results**: Presentation of findings with tables for each specific aim and associated figures.
  - **Discussion**: Summary and potential future research directions.
  

## Manuscript

The manuscript derived from this project has been published on [GitHub Pages](<https://phs-650-2024.github.io/Final_Paper/>), providing an accessible version of the complete study.

- **/docs/**: Folder containing the manuscript in both a MS word docx (`final_paper.docx`) and HTML file (`final_paper.html`).  

## Preregistrations

This study was preregistered to facilitate transparency and rigor in research methods:
- [Preregistration for Aim 1](<https://doi.org/10.17605/OSF.IO/E8QYF>)
- [Preregistration for Aim 2](<https://doi.org/10.17605/OSF.IO/KV6WP>)

## Usage

To replicate the analysis:
1. Clone the repository.
2. Run the R scripts in numerical order starting from `0.Scoring.R`.
3. View the results in the `results` folder or the compiled `.qmd` file.

## Dependencies

- Ensure you have R installed along with the necessary libraries mentioned in the scripts (see below).
  - library(tidyverse)
  - library(knitr)
  - library(dplyr)
  - library(janitor)
  - library(kableExtra)
  - library(broom.mixed)
  - library(lme4)
  - library(car)
  - library(ggplot2)
  - library(ggeffects)
  - library(pwr)
  - library(lmerTest)

## License

This project is available under the GNU General Public License (GPL) 3.0. 
