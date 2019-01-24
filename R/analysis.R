#' This file contains the code used to clean, analyze, and model the data.

# Packages
suppressPackageStartupMessages(library(tidyverse))
library(shiny)

# Data loading & cleaning
dat <- suppressMessages(read_csv('../data/BodyFat.csv'))
head(dat)
