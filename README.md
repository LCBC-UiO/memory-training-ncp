# Supplementary code

This repository contains supplementary code to the manuscript "Cognitive and hippocampal changes from memory training weeks and years after training". 

To get an overview of the code, please see the file `overview.Rmd`. All R code is available as functions in the `code` directory, and these functions are called from `overview.Rmd`. Each function has its own file, so a function `foo()` will be placed in file `foo.R`.

The file `output_with_actual_data.pdf` shows the result of running the rmarkdown document `overview.Rmd` with the real data. These data are not publicly available, so instead we have created simulated datasets which allow the code to be run. Note that the results when running the code on simulated data will not be comparable to the actual analysis results. Instead, they are intended to be a useful tool for exploring the code.

If you use [RStudio](https://www.rstudio.com/), please start by opening the project file `memory-training-ncp.Rproj`. Otherwise, make sure that the top directory is the working directory.
