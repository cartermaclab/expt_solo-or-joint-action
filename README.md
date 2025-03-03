# Preferences for costly cooperation are highly individualized

This repository contains the necessary files and code to reproduce the analyses, figures, and the manuscript using R and quarto.

## Usage
You will need to have R (https://cran.r-project.org/) and RStudio (https://www.rstudio.com/products/rstudio/download/#download) or Positron (https://github.com/posit-dev/positron/releases) installed on your computer.

This project makes use of the `renv` R package (see https://rstudio.github.io/renv/articles/renv.html). The `renv` package helps with reproducibility as it keeps track of the required R packages (including version), and (if known) the external source from which packages were retrieved (e.g., CRAN, Github). With `renv`, packages are installed to a project specific library rather than your user or system library. The `renv` package must be installed on your machine before being able to benefit from its features. The package can be installed using the following command:

``` r
install.packages("renv")
```

Once you have `renv` installed, you can get a copy of this repository on your machine by clicking the green Code button then choose Download zip. Save to your machine and extract.

### If using RStudio

 After extraction, you can double click the `expt_solo-or-joint-action.Rproj` file in the root directory. This will automatically open the R Project in RStudio. This will ensure all paths work on your system as the working directory will be set to the location of the `.Rproj` file. Upon opening, RStudio will recognize the `renv` files and you will be informed that the project library is out of sync with the lockfile. As shown in the console pane of RStudio, running `renv::restore()` will install the packages recorded in the lockfile. This could take some time depending on your machine and internet connection.

 ### If using Positron

 With Positron opened, you can click Open Folder and choose the extracted directory. This will ensure all paths work on your system as the root level of this directory will be set as the working directory. Once opened, you will be informed that the project library is out of sync with the lockfile. You will need to run `renv::restore()` to install the packages recorded in the lockfile. This could take some time depending on your machine and internet connection.
 