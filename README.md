
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Predicting Salary of Major League Baseball Players

This repository consists of an R project for MAST90111 - Advanced
Statistical Modelling, holding the source files for reproducing the
deliverable, including the code files and their version history.

## Requirements

To reproduce the deliverables of the project, you will need to install
the following packages and their dependencies as follows:

``` r
install.packages("tidyverse")
install.packages("xaringan")
install.packages("xaringanthemer")
install.packages("np")
install.packages("mgcv")
install.packages("tinytex")
tinytex::install_tinytex()
```

## Reproducible research

The main deliverable of the project consists of a PDF report rendered by
R Markdown.

``` r
rmarkdown::render("report/report.Rmd")
```

The presentation of the project consists of an HTML slide rendered by R
Markdown with the [xaringan](https://github.com/yihui/xaringan) package.

``` r
rmarkdown::render("slide/slide.Rmd")
```
