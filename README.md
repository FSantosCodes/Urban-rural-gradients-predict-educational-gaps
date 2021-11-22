# Urban-rural-gradients-predict-educational-gaps

This is the repository for the code and data used in the publicacion "". Here, we demostrate how to reproduce this methodology for the BeBachelor survey data. 

## R ENVIRONMENT

Required R libraries are installed or load into environment below:

```{r eval=FALSE}
#install or read libraries
list.of.packages <- c("rgdal","raster", "openxlsx","stringi","parallel","data.table","rfUtilities","randomForest",
                      "doParallel","foreach","Hmisc","Boruta","gtools","GWmodel","caret")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){install.packages(new.packages,dependencies = T)}
lapply(list.of.packages, require, character.only = T)
```

Also for reproducibility seed.seed is required

```{r eval=FALSE}
#favorite random number
set.seed(666)
```
