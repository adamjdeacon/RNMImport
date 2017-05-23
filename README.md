# RNMImport
Tools for importing and manipulating NONMEM data.

NONMEM is a FORTRAN application for fitting mixed effects models. 
NONMEM licenses are available from ICON Development Solutions.
Models are specified as plain text files called control files 
with blocks corresponding to components of the model.
Model results are presented as plain text files. These input 
and output files can be imported into R by **RNMImport** to 
facilitate model modification, model diagnostics, model interpretation
and reporting.

```{r}
# install devtools for devtools::install_github
install.packages("devtools")
library(devtools)
# install RNMImport
install_github("MangoTheCat/RNMImport")
library(RNMImport)
```

A NONMEM execution including both input files and results files can
be read using `importNm`.
