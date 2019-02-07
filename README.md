# STAT 628 Module 1: Body Fat Calculator

### Authors

* Yunhui Qi (qi44@wisc.edu)
* Sam Waterbury (waterbury@wisc.edu)
* Junxia Zhu (jzhu334@wisc.edu)

_(We belong to group #6 in the Thursday lecture.)_

### Introduction

The goal of this project is to develop a simple rule of thumb technique for estimating body fat percentage using only measurements which can be easily obtained. After data cleaning, variable selection, model evaluation, and diagnostics, we obtained the following estimator:

`
BODYFAT (%) = 0.91 * ABDOMEN (cm) - 0.14 * WEIGHT (lbs) - 40
`

This estimator is easy to understand and can be quickly computed since it only uses two variables.

### Contents of this Repository

**Files:**
* `Executive-Summary.ipynb` is the Jupyter notebook containing a summary of our analysis and results.
* `Shiny-App.R` contains the code necessary for running the Shiny application. Running this file will open the app in a web browser.
* `Presentation Slides.pdf` contains our in-class presentation slides.

**Folders:**
* `data/` contains the raw dataset and the "clean" dataset for which several values were imputed.
* `image/` contains the all of the plots and figures we produced.
* `R/` contains all of the code we wrote to perform our analysis and plots for this project.

