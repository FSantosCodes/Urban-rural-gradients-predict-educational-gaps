# Urban-rural-gradients-predict-educational-gaps

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
