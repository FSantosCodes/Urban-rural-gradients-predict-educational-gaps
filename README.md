# Urban–rural gradients predict educational gaps: Evidence from a machine learning approach, involving academic performance and impervious surfaces in Ecuador

Fabián Santos, Phd.

11/22/2021

## 1. INTRODUCTION

This report describes the algorithm implementation described in the publication: "Urban–rural gradients predict educational gaps: Evidence from a machine learning approach, involving academic performance and impervious surfaces in Ecuador". In this research, a feature analysis combining the Boruta algorithm [(Kursa & Rudnicki, 2010)](https://doi.org/10.18637/jss.v036.i11) and backward elimination are used to identify the best predictors and optimize the Random Forest algorithm [(Breiman, 2001)](https://doi.org/10.1023/A:1010933404324). This is done for explain academic performance (AP) scores, using high schools locations and their students´ answers to a questionnaire-based survey (See: [INEVAL](http://evaluaciones.evaluacion.gob.ec/BI/bases-de-datos-ser-bachiller/)). Handling them as individual cases, this enabled  to map prediction probabilities and conduct a correlation analysis with an impervious surfaces map [(Gong et al., 2020)](https://doi.org/10.1016/j.rse.2019.111510) to derive our conclusions.

To organize this report, we first describe where to get input and output files, as well as the folders contained in this repository for store the R scripts and the shiny apps: 

* **data inputs**: this [link](https://indoamerica-my.sharepoint.com/:f:/g/personal/ernestosantos_uti_edu_ec/EiuLcuHXu9JNg_kiPRIeIMoBZsCIi6rX7lcHcH4n90RhNQ?e=faBYdd) allows to download data input files. These includes the HSCH, MCRO and AFAC databases;

* **data outputs**: this [link](https://indoamerica-my.sharepoint.com/:f:/g/personal/ernestosantos_uti_edu_ec/Elu3MiDn9Y1MlC2ZSf2sUoMB0VCDxWrewJBE4S43Jiy0Zg?e=2Xs7K8) allows to download data output files, organized by theme groups described in the publication (AE: academic environment; SC: socio economic and cultural; and CS: cognitive skills). These files include the resulting R environment (.Rdata), a sample of the iteration files (.csv) and shapefiles (.shp) produced during processing;

* **scripts**: located in this repository, this folder contains the scripts used for processing; and

* **shinyApp**: located in this repository, this folder contains the source code of the web app used for visualize outputs.

The next sections describes with detail the structure and relevant sections of the scripts, using as example the Academic Environment (AE) theme group.

## 2. R ENVIROMENT

Required R libraries are installed or load into environment below:

```{r eval=F}
#install or read libraries
list.of.packages <- c("rgdal","raster","openxlsx","stringi","parallel","data.table","rfUtilities","randomForest","doParallel","foreach","Hmisc","Boruta","gtools","GWmodel","caret")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){install.packages(new.packages,dependencies = T)}
lapply(list.of.packages, require, character.only = T)
```

Also for reproducibility seed.seed is required

```{r eval=F}
#favorite random number
set.seed(666)
```

## 3. DATA INPUTS

Specific files are required before RF+Boruta algorithm is ran. These files are:

* Micro (MCRO) and associated factors (AFAC) databases. Both are expected as excel files; and
* Associated factors dictionary, which constitutes an excel file describing codification of AFAC database.
* High schools spatial points shapefile (HSCH), which should include the ID column of high schools (amie) to link them with databases MCRO and AFAC;

At the time of this research (January 2020), we downloaded freely all of them at: 

1. http://evaluaciones.evaluacion.gob.ec/BI/bases-de-datos-ser-bachiller/

but lastly HSCH is now located at:

2. https://iedg.sni.gob.ec/geoportal-iedg/documentos/Establecimientos%20Educativos.zip

Note that updates of these datasets may affect the script as it is not prepared for changes in their format and structure. 

To configure these data inputs in the algorithm, they should be specified as:

```{r eval=F}
#input high schools (HSCH) shapefile
ins_base <- "E:/DATA/Ineval/HSCH/amieactivas_ciclo15_16.shp"
#input micro (MCRO) and associated (AFAC) excel databases
micro_base <- "E:/DATA/Ineval/MCRO/SBAC17_micro_627960_20180426_XLS.xlsx"
fa_base <- "E:/DATA/Ineval/AFAC/SBAC17_faestudiantes_627960_20180307_XLS.xlsx"
#input fa (AFAC) excel dictionary
fa_dic <- "E:/DATA/Ineval/AFAC/SBAC17_diccionariofaestudiantes_1188_20180307_DIC.xlsx"
```

An empty output folder is also required but note that here is specified as "AE" to refer to the academic environment. This is one of the three theme groups examined in our research for alleviate data processing:

```{r eval=F}
#output folder
output_folder <- "E:/DATA/Ineval/outputs/AE"
```

Additional parameters are required. First are those needed for define high and low academic performance (AP):

```{r eval=F}
#AP score column name (it should be numeric)
AP_var <- "Notaexamendegrado"
#since two classes are examined, this is the score for define them 
AP_cut <- 7
#these are their labels
AP_labels <- c("Low","High")
```

Then, some parameters are required for configure sample size (here, 20 students as in the paper), Boruta and backward elimination steps:

```{r eval=F}
#sample size considered in the spatial sampling procedure
sample_size <- 20
#Boruta maximal number of importance source runs
max_runs <- 100
#Number of backward recursive elimination steps
max_iterations <- 10 
```

A final set of parameters are used to control and restart processing from a specific spatial point (or high school location). To identify which point, users should explore the output folder and identify the number of the csv file saved. Advice that AFAC database may consume ~25 GB of RAM just to load into the environment, then the number of cores should be decided wisely to avoid overflow:

```{r eval=F}
#cores to use
ncores <- 8
#do processing? if false, the script will only restitute csv files at the output folder 
processing <- F
#start point (to restart from a specific spatial point if computer is turned off)
start_point <- 1
```

## 4. DATA PREPARATION

This section describes some data wrangling operations before the algorithm is ready to run. 

Fist, the output folders are created:

```{r eval=F}
#start time
start.time <- proc.time()
#this is the main output folder
dir.create(output_folder,showWarnings=F,recursive=T)
#this subfolder is defined for save the csv files
iter.folder <- paste0(output_folder,"/iter") 
dir.create(iter.folder,showWarnings=F)
#this subfolder save a description of data transformation in AFAC, MCRO and HSCH inputs
dic.folder <- paste0(output_folder,"/dic")
dir.create(dic.folder,showWarnings=F)
```

Second, the HSCH is read and modified for processing. This happens if the parameter **processing** is defined as TRUE (see above), othewise resulting csv files from processing are read (see last section). It is important to note that the high schools IDs column name "amie" is renamed as "ID_ins" to facilitate joining operations with other databases. Furthermore, also advice that some variables are removed as they were not relevant for our investigation.

```{r eval=F}
if(processing){
  #read shapefile + rename join columns
  ins.base <- shapefile(ins_base)
  names(ins.base)[grep("amie",names(ins.base))] <- "ID_ins"
  #clean spurious observations (ID_ins)
  ins.base <- ins.base[!is.na(ins.base@data$ID_ins),]
  ins.base <- ins.base[!duplicated(ins.base@data$ID_ins),]
  #remove not informative variables
  rem.vars <- c("codigo","id_zona","id_dist","id_circ","t1_inst","t2_inst",
                "t3_inst","em_inst","ins_nmrec","ins_tfrec","ins_emarec","estado_c16",
                "ubi_utmx","ubi_utmy","di_inst")
  ins.base@data <- ins.base@data[,!names(ins.base@data) %in% rem.vars]
  #reserve variables as CHARACTER
  fac.vars <- sapply(ins.base@data,class)=="character"
  fac.vars[1:5] <- F
  #variables to consider as FACTOR
  ins.base@data[,fac.vars] <- lapply(ins.base@data[,fac.vars,drop=F],tolower)
  ins.base@data[,fac.vars] <- lapply(ins.base@data[,fac.vars,drop=F],factor)
  #dictionary
  ins.dic <- data.frame(varName=names(ins.base), dataType=sapply(ins.base@data,class),stringsAsFactors=F)
  rownames(ins.dic) <- NULL
  ins.dic$operation <- "none"
  ins.dic$levels <- "none"
  for(i in 1:nrow(ins.dic)){
    if(ins.dic[i,2]=="factor"){
      i.levels <- levels(ins.base@data[,i])
      i.levels <- paste(i.levels,collapse=" | ")
      ins.dic[i,4] <- i.levels
    }
  }
  #end if-processing
}
```

Third, we now describe processing of MCRO. This dataset is an excel file, which is stored in the first sheet. Note that it is read from thee second row, due it contains the column names. As in this database contains irrelevant variables, we removed those repetitive or not interesting for our investigation. After database reading, it is filtered to the surveyed student population, as not all records are complete or refer to our focus group. Advice that last line of this code chunk is used to classify AP scores into the "low" and "high" AP classes.


```{r eval=F}
if(processing){
  #variables to eliminate
  rem.vars <- c("Institucion","Region","Zonadeplanificacion","Zonadeplanificacion","Anodenacimiento","Distrito","Circuito","Codigodelaprovincia","Provincia","Codigodelcanton","Canton","Codigodelaparroquia","Parroquia","Tipodesostenimiento","Area","Tipodepoblacion","Estado","Puntajeparapostularalaeducacionsuperior","Notaexamendegradoajustado","Indicesocioeconomico","Etnia","DominioMatematico","DominioLinguistico","DominioCientifico","DominioSocial","Regimendeevaluacion","DominioMatematico_cat","DominioLinguistico_cat","DominioCientifico_cat","DominioSocial_cat")
  #read MICRO + clean names
  micro.base <- read.xlsx(micro_base,sheet = 1, startRow = 2, colNames = T)
  names(micro.base) <- stri_trans_general(names(micro.base),id = "Latin-ASCII")
  names(micro.base) <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", names(micro.base))
  #identify 'dominio' columns with categorical values + rename
  dom.cols <- grep("Dominio",names(micro.base))
  dom.cols <- dom.cols[sapply(micro.base[,dom.cols],class)=="character"]
  names(micro.base)[dom.cols] <- paste0(names(micro.base)[dom.cols],"_cat")
  #identify codigos for institucion and students + rename
  dom.cols <- grep("Codigodelainstitucion",names(micro.base))
  names(micro.base)[dom.cols] <- "ID_ins"
  dom.cols <- grep("Codigodelsustentante",names(micro.base))
  names(micro.base)[dom.cols] <- "ID_est"
  #filter to 'ESCOLAR' (solo estudiantes)
  micro.base <- micro.base[micro.base$Tipodepoblacion=="ESCOLAR",]
  #filter to 'EVALUADO' (respondio factores asociados)
  micro.base <- micro.base[micro.base$Estado=="EVALUADO",]
  #filter to 'ID_ins'
  micro.base <- micro.base[micro.base$ID_ins %in% unique(ins.base@data$ID_ins),]
  #apply removal
  micro.base <- micro.base[,!names(micro.base) %in% rem.vars]
  #reserve ID_ins & ID_stu as CHARACTER
  id.ins <- as.character(micro.base$ID_ins)
  id.est <- as.character(micro.base$ID_est)
  #variables to consider as FACTOR
  fac.vars <- sapply(micro.base,class)=="character"
  micro.base[,fac.vars] <- lapply(micro.base[,fac.vars,drop=F],tolower)
  micro.base[,fac.vars] <- lapply(micro.base[,fac.vars,drop=F],factor)
  #remove variables with an unique factor level (linear combo)
  fac.vars <- sapply(micro.base,class)=="factor"
  rem.vars <- names(which(sapply(lapply(micro.base[,fac.vars],levels),length)==1))
  micro.base <- micro.base[,!names(micro.base) %in% rem.vars]
  #restore ID_ins & ID_est
  micro.base$ID_ins <- id.ins
  micro.base$ID_est <- id.est;rm(id.ins,id.est)
  #prepare micro dictionary & put levels as numbers in micro.base
  micro.dic <- data.frame(varName=names(micro.base), dataType=sapply(micro.base,class),stringsAsFactors=F)
  rownames(micro.dic) <- NULL
  micro.dic$operation <- "none"
  micro.dic$levels <- "none"
  #get score min and max
  AP.cut <- c(min(na.omit(micro.base[,grep(AP_var,names(micro.base))])),
              AP_cut,
              max(na.omit(micro.base[,grep(AP_var,names(micro.base))]))
  )
  #end if-processing
}
```

Fourth, we now describe the processing and preparison of the AFAC database. As this database is large, it takes some minutes to open it (~15-20 min.). As we aimed variables of the AE theme group, all the rest are removed in the first lines of code. Observe that variables "etnibbe" and "ncelcbe" are preserved since they were used for some descriptive statistics in a later section. After reading, a similar filtering procedure to MCRO dabase is applied to select surveyed student population. Finally, as some answer to survey questions were uninformative (e.g. I dont´t know), they were identified to exclude from our analysis in a later step.

```{r eval=F}

if(processing){
  #eliminate variables
  rem.vars <- c("es_regeva","poblacion","estado","isec","sexoabe","diskabe","disfbbe","fartbbe","fbibdbe","fcinbbe","fdepbbe","fleebbe","fnotbbe","fsocbbe","fvisbbe","hrarabe","hrdpabe","hrvlabe","isocbbe","prculbe","cboxabe","cboxbbe","comhdbe","comjcbe","comoabe","comrcbe","comvcbe","hfpcbbe","hfpccbe","hrtvabe","ncelbbe","ncelcbe","rredabe","tvstabe","ucorabe","caslbbe","etnibbe","etnicbe","inacabe","lengabe","momoabe","movhabe","movhbbe","movhcbe","movhdbe","movhebe","movhfbe","movhgbe","movhhbe","refuabe","tmovabe","actmabe","actpabe","niembbe","niemfbe","niepbbe","niepfbe","ocjhabe","ocumebe","ocupebe","aguaabe","basuabe","clavabe","cocqbbe","cradabe","crefbbe","cuarpbe","desaabe","luzeabe","nbanabe","ncarabe","nducabe","nmicabe","nmtvabe","nvcaabe","pardabe","piscabe","telfabe","tpviabe","tshgabe","bdhuabe","ctracbe","intrbbe","niessbe","nsegabe","patrbbe","patrcbe","veccabe","jhogabe","nhijabe","nhijbbe","vivfcbe","vivfdbe","vivfebe","vivffbe","vivfgbe","vivfhbe","vivfjbe","vivfsbe","vivfybe","vivsabe","preuabe","psbcabe","repmabe","reppabe","repsabe","bcnnabe","bmatabe","cdudabe","cdudbbe","cdudcbe","cduddbe","cdudebe","cdudfbe","cdudgbe","cdudhbe","cdudibe","mdudabe","mdudbbe","mdudcbe","mduddbe","mdudebe","mdudfbe","mdudgbe","mdudhbe","mdudibe","amabbbe","binfbbe","cncpbbe","cuidbbe","cunoabe","decibbe","dgpibbe","flclabe","htardbe","iadiabe","inasabe","inedbbe","oppsabe","oreiabe","plesbbe","plmebbe","posiabe","provbbe","rmatbbe","rpalabe","rsitbbe","rtxtbbe","tdurbbe","tmucbbe","trsfabe","alecbbe","clecbbe","diccabe","dllrcbe","entlbbe","hrlacbe","hrpeabe","lartabe","lcliabe","lecebbe","leobabe","leobbbe","liabbbe","lpoeabe","mledbbe","nlibcbe","paslabe","ptilabe","ptilbbe","tleebbe","bessabe","blylabe","edudabe","edudbbe","edudcbe","eduddbe","edudebe","edudfbe","edudgbe","edudhbe","edudibe","ldudabe","ldudbbe","ldudcbe","lduddbe","ldudebe","ldudfbe","ldudgbe","ldudhbe","ldudibe","afutbbe","carrbbe","dccarbe","eticabe","matgabe","nedufbe","nedugbe","ptecabe","soceabe","sticabe","tsupabe","tuniabe")
  #reserved vars
  rem.vars <- rem.vars[!rem.vars %in% c("etnibbe","ncelcbe")]
  #read FA
  fa.base <-  read.xlsx(fa_base,sheet = 1, startRow = 1, colNames = T) 
  names(fa.base)[grep("amie",names(fa.base))] <- "ID_ins"
  #identify codigos for institucion and sudents + rename
  dom.cols <- grep("amie",names(fa.base))
  names(fa.base)[dom.cols] <- "ID_ins"
  dom.cols <- grep("codigo_estudiante",names(fa.base))
  names(fa.base)[dom.cols] <- "ID_est"
  #filter to 'ESCOLAR' (solo estudiantes)
  fa.base <- fa.base[fa.base$poblacion=="ESCOLAR",]
  #filter to 'EVALUADO' (respondio factores asociados)
  fa.base <- fa.base[fa.base$estado=="EVALUADO",]
  #filter to 'ID_ins'
  fa.base <- fa.base[fa.base$ID_ins %in% unique(ins.base@data$ID_ins),]
  #apply removal
  fa.base <- fa.base[,!names(fa.base) %in% rem.vars]
  #reserve ID_ins & ID_stu as CHARACTER
  id.ins <- as.character(fa.base$ID_ins)
  id.est <- as.character(fa.base$ID_est)
  #variables to consider as NUMERIC
  num.vars <- sapply(fa.base,class)=="numeric"
  fa.base[,num.vars] <- lapply(fa.base[,num.vars,drop=F],as.numeric)
  #variables to consider as FACTOR
  fac.vars <- sapply(fa.base,class)=="character"
  fa.base[,fac.vars] <- lapply(fa.base[,fac.vars,drop=F],tolower)
  fa.base[,fac.vars] <- lapply(fa.base[,fac.vars,drop=F],factor)
  #remove variables with an unique factor level (linear combo)
  fac.vars <- sapply(fa.base,class)=="factor"
  rem.vars <- names(which(sapply(lapply(fa.base[,fac.vars],levels),length)==1))
  fa.base <- fa.base[,!names(fa.base) %in% rem.vars]
  #restore ID_ins & ID_est
  fa.base$ID_ins <- id.ins
  fa.base$ID_est <- id.est;rm(id.ins,id.est)
  #prepare xls FA dictionary
  dic.lev <- openxlsx::read.xlsx(fa_dic,sheet = 1, startRow = 2, colNames = T)
  names(dic.lev) <- stri_trans_general(names(dic.lev),id = "Latin-ASCII")
  names(dic.lev) <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", names(dic.lev))
  dic.lev <- dic.lev[!is.na(dic.lev$Etiqueta),]
  dic.lev <- split(dic.lev,dic.lev$Codigo,drop=F)
  #prepare FA dictionary
  fa.dic <- data.frame(varName=names(fa.base), dataType=sapply(fa.base,class),stringsAsFactors=F)
  rownames(fa.dic) <- NULL
  fa.dic$operation <- "none"
  fa.dic$operation[fa.dic$dataType=="factor"] <- "dummyVars"
  fa.dic$question <- "none"
  fa.dic$levels <- "none"
  for(i in 1:nrow(fa.dic)){
    if(fa.dic[i,2]=="factor"){
      i.levels <- dic.lev[[grep(fa.dic[i,1],fixed=T,names(dic.lev))]]
      fa.dic[i,4] <- i.levels$Item[1]
      i.levels <- paste(paste0(1:nrow(i.levels),":",i.levels$Etiqueta),collapse=" | ")
      fa.dic[i,5] <- i.levels
    }
  }
  #identify "No sé" or other uninformative answers 
  fa.noSe <- fa.dic[grep("No sé",fa.dic$levels, fixed=T),]
  if(nrow(fa.noSe)>0){
    fa.noSe <- sapply(strsplit(fa.noSe$levels,"[|]"),grep,pattern="No sé",fixed=T,value=T)
    fa.noSe <- sapply(strsplit(fa.noSe,"[:]"),head,1)
    fa.noSe <- paste0(fa.dic[grep("No sé",fa.dic$levels, fixed=T),][,1],".",fa.noSe)
    fa.noSe <- data.frame(fa.noSe,stringsAsFactors=F)
  }
  #clean
  rm(dic.lev,i,i.levels,rem.vars)
  #end if-processing
}
```

Finally, HSCH, MCRO and AFAC databases are linked through the "ID_ins" identifier. This operation filters high school cases occurring in all the three databases.

```{r eval=F}
if(processing){
  #filter to existing 'ID_ins' in micro.base, fa.base & ins.base
  id.ins <- intersect(micro.base$ID_ins,fa.base$ID_ins)
  ins.base <- ins.base[ins.base@data$ID_ins %in% id.ins,]
  micro.base <- micro.base[micro.base$ID_ins %in% id.ins,]
  fa.base <- fa.base[fa.base$ID_ins %in% id.ins,]
  #save ins.base points for later use
  out.name <- paste0(output_folder,"/highSchools_des.shp")
  shapefile(ins.base[,"ID_ins"],out.name,overwrite=T)
  #end if-processing
}
```

## 5. PROCESSING FUNCTIONS

Since the algorithm operates high school in a casewise fashion, the next set of functions are implemented to complete their pre-processing. The first one, ingest AFAC data from a high school case to binarize (or convert to dummy variables) its survey questions according to their possible responces. 

```{r eval=F}
dummyVars.extract <- function(x){
  #select cols
  x.class <- sapply(sapply(x,class),function(y){y <- any(y=="factor")})
  x <- x[,x.class,drop=F]
  #extract classes as variables
  x.vars.ls <- list()
  for(j in 1:ncol(x)){
    x.name <- names(x)[j]
    x.name <- paste0(x.name,".",levels(x[,j]))
    x.vars <- x[,j]
    na.rows <- sum(is.na(x.vars))
    x.vars <- data.frame(model.matrix(~x.vars + 0))
    if(na.rows!=0){
      na.rows <- data.frame(matrix(rep(NA,na.rows*ncol(x.vars)),na.rows,ncol(x.vars)))
      names(na.rows) <- names(x.vars)
      x.vars <- rbind(x.vars,na.rows) 
    }
    names(x.vars) <- x.name
    x.vars.ls[[j]] <- x.vars
  }
  #end
  return(as.data.frame(x.vars.ls))
}
```

The next function is designed to split data features from MCRO and AFAC databases, since some from the first case are used for descriptive statistics, while the second are used for feature analysis. 

```{r eval=F}
prepare.data<- function(x,collect=T){
  
  ##### 1) MICROBASE
  
  #prepare
  micro.i <- micro.base[grepl(x,micro.base$ID_ins,fixed=TRUE),,drop=F]
  rownames(micro.i) <- NULL
  #data extraction and removal
  qui.i <- micro.i$Quintil; micro.i$Quintil <- NULL; micro.dic <<- micro.dic[!micro.dic$varName=="Quintil",]
  sex.i <- micro.i$Sexo; micro.i$Sexo <- NULL; micro.dic <<- micro.dic[!micro.dic$varName=="Sexo",]
  han.i <- micro.i$Discapacidad; micro.i$Discapacidad <- NULL; micro.dic <<- micro.dic[!micro.dic$varName=="Discapacidad",]
  fin.i <- micro.i$Tipodefinanciamiento; micro.i$Tipodefinanciamiento <- NULL; micro.dic <<- micro.dic[!micro.dic$varName=="Tipodefinanciamiento",]
  #get score and then replace with classification
  AP.i <- micro.i[,grep(AP_var,names(micro.i))]
  micro.i[,grep(AP_var,names(micro.i))] <- Hmisc::cut2(micro.i[,grep(AP_var,names(micro.i))],AP.cut)
  levels(micro.i[,grep(AP_var,names(micro.i))]) <- AP_labels
  #dummy vars
  if(any(micro.dic$operation=="dummyVars")){
    micro.i <- cbind(micro.i[,micro.dic$operation=="none"],dummyVars.extract(micro.i))
  }
  micro.i$ID_ins <- NULL
  
  ##### 2) FA BASE
  
  #prepare
  fa.i <- fa.base[grepl(x,fa.base$ID_ins,fixed=TRUE),,drop=F]
  rownames(fa.i) <- NULL
  #data extract and removal
  etn.i <- fa.i$etnibbe; fa.i$etnibbe <- NULL; fa.dic <<- fa.dic[!fa.dic$varName=="etnibbe",]
  int.i <- fa.i$ncelcbe; fa.i$ncelcbe <- NULL; fa.dic <<- fa.dic[!fa.dic$varName=="ncelcbe",]
  #dummy vars
  if(any(fa.dic$operation=="dummyVars")){
    fa.i <- cbind(fa.i[,fa.dic$operation=="none"],dummyVars.extract(fa.i))
  }
  fa.i$ID_ins <- NULL
  #remove "no se"
  fa.i <- fa.i[,!names(fa.i) %in% fa.noSe[,1]]
  
  ##### 4) join data frames 
  
  #merge bases
  merged.i <- base::merge(micro.i,fa.i,by="ID_est",all=F)
  merged.i$ID_est <- NULL
  
  ##### 5) return data
  
  if(collect){
    collected.data <- list(AP=AP.i,qui=qui.i,sex=sex.i,han=han.i,etn=etn.i,int=int.i,class=nrow(merged.i),fin=fin.i)
    return(list(merged.i,collected.data))
  }else{
    return(merged.i)
  }
}
```

Now, we describe the main function for feature analysis. This includes the execution of Boruta (multiple times if the first try do not give an answer), accompained by the **TentativeRoughFix** to unjam it in complex cases. The latter simplifies feature selection significance test when Boruta cannot give a proper solution. After these steps, the random forest (RF) algorithm is executed. This is done thought the ranger library, which implements an efficient version of RF. Note that it is executed with the "confirmed" important features identified by Boruta and variables importance scores is derived in this step (via permutation). After that, class probabilities are also calculated and extracted from the RF model to end the pipeline collecting accuracy metrics and cross validation test resultas. In exceptional cases Boruta, neither TentativeRoughFix cannot solve feature selection (~5% of all models in our experiments) and function assigns NA (not available) to these observations. Observe that all these steps are nested in a while loop in order to iterated for a number of times (defined at the beginning of this script by the parameter **max_iterations**). At each iteration, the selected features by Boruta are eliminated in order to integrate the backward recursive elimination procedure into our procedure. 

```{r eval=F}
boruta.wrapper <- function(x,max.runs=100,max_iterations=10){
  #initial variables
  iteration <- 1
  output.ls <- list()
  x.run <- T
  #evaluate
  while(x.run){
    #first BORUTA run
    x.formula <- as.formula(paste0(AP_var,"~."))
    x.boruta <- Boruta::Boruta(x.formula,x,maxRuns=max.runs)
    #resolve tentative features
    if(any(x.boruta$finalDecision=="Tentative")){
      x.boruta <- Boruta::TentativeRoughFix(x.boruta)
    }
    #if NONE is confirmed run BORUTA for second time
    if(all(x.boruta$finalDecision=="Rejected")){
      x.boruta <- Boruta::Boruta(x.formula,x,maxRuns=max.runs)
      if(any(x.boruta$finalDecision=="Tentative")){
        x.boruta <- Boruta::TentativeRoughFix(x.boruta)
      }
    }
    #run ranger
    if(!all(x.boruta$finalDecision=="Rejected")){
      #train RF
      x.formula <- Boruta::getNonRejectedFormula(x.boruta)
      x.model <- ranger::ranger(formula=x.formula,
                                data=x,
                                scale.permutation.importance=T,
                                probability=T,
                                importance="permutation")
      #Boruta results:importances
      x.bor <- Boruta::attStats(x.boruta)
      x.bor$var <- rownames(x.bor);rownames(x.bor) <- NULL
      x.bor$iter <- iteration
      x.bor <- x.bor[x.bor$decision=="Confirmed",]
      x.bor <- x.bor[,c(8,7,1:5)] #reorder column names
      rownames(x.bor) <- NULL
      #evaluate RF class probabilities
      x.acc <- as.data.frame(x.model$predictions)
      x.acc$pred <- factor(apply(x.acc,1,function(x)names(x)[which.max(x)]),levels=AP_labels)
      x.acc <- c(round(caret::confusionMatrix(x[,1],x.acc$pred)$overall,6)[1:2],
                 names(which.max(table(x.acc$pred))))
      x.acc <- data.frame(iter=iteration,
                          acc=x.acc[1],
                          kappa=x.acc[2],
                          brier=round(x.model$prediction.error,3),
                          pred=x.acc[3])
      #cross validation
      if(length(x.bor$var)>=2){
        x.rfcv <- rfcv(trainx=x[,x.bor$var],trainy=x[,1])
        x.cross <- data.frame(iter=iteration,cvAll=x.rfcv$error.cv[1],nvarAll=x.rfcv$n.var[1])
        x.rfcv <- x.rfcv$error.cv[-1]
        x.rfcv <- x.rfcv[which.min(x.rfcv)]
        x.cross$cvMin <- as.numeric(x.rfcv)
        x.cross$nvarMin <- as.numeric(names(x.rfcv))
      }else{
        x.cross <- data.frame(iter=iteration,
                              cvAll=NA,
                              nvarAll=length(x.bor$var),
                              cvMin=NA,
                              nvarMin=NA)
      }
      #ranger results: probabilities
      x.prob <- round(as.data.frame(t(apply(x.model$predictions,2,mean))),3)
      x.acc <- cbind(x.acc,x.prob)
      rownames(x.acc) <- NULL
      #store valid outputs
      if(x.run){
        output.ls[[iteration]] <- list(acc=x.acc,bor=x.bor,cross=x.cross)
      }
      #update iteration and evaluate
      x <- x[,!names(x) %in% x.bor$var]
      iteration <- iteration + 1
      if(iteration >= max_iterations){
        x.run <- F
      }
    }else{
      #if definitely NONE is confirmed, return NA
      x.acc <- data.frame(iter=iteration,acc=NA,kappa=NA,brier=NA,pred=NA,Low=NA,High=NA)
      x.bor <- data.frame(iter=iteration,var=NA,meanImp=NA,medianImp=NA,minImp=NA,maxImp=NA,normHits=NA)
      x.cross <- data.frame(iter=iteration,cvAll=NA,nvarAll=NA,cvMin=NA,nvarMin=NA)
      output.ls[[iteration]] <- list(acc=x.acc,bor=x.bor,cross=x.cross)
      #update iteration and evaluate
      x <- x[,!names(x) %in% x.bor$var]
      iteration <- iteration + 1
      if(iteration >= max_iterations){
        x.run <- F
      }
    }
  }
  #close
  return(output.ls)
}
```

Another important function is the spatial sampling based in distances. This allows to complete samples in a high schools case, when it do not complete the required **sample_size** number. This number is an equivalent to the number of students for each AP class and high school case (i.e. 20 for low and 20 for high AP). TO associate a student to the higgh school case, it simply sort the neighboring high schools to the examined case and return their data to extract the missing samples required.

```{r}
#find nearest high-schools (x is the position index at ins.base)
find.nearest <- function(x){
  x.data <- ins.base@data
  x.data$dist <- gw.dist(dp.locat=coordinates(ins.base),rp.locat=coordinates(ins.base[i,]))[,1]
  x.data <- x.data[order(x.data$dist),c("ID_ins","dist"),drop=F]
  return(x.data)
}
```

A final simply but needed function was coordinates extraction. It was created to derive them easily in other funcitons.

```{r}
codes.coordinates <- function(x){
  x <- coordinates(ins.base[ins.base$ID_ins==x,])
}
```

## 6. EXECUTION OF THE ALGORITHM

With all the data preparation and definition of processing function, here we explain our implementation. First, users should advice that it is programmed for parallel computing using a loop structure to process each high school case. This is defined in the first lines, where the parameter **ncores** defines how many CPU cores are used. This number should be managed with care as it duplicates the R environment and this can overload RAM (if it is not enought) In our experiments, we used 8 cores in a Windows 10 workstation with a i7-8700 Intel processor and 32 GB of RAM. As each high school case correspond an iteration number, observe the parameter **start_point** allows to restart processing from a specific point. Finally, observe that some R libraries are called again, while the random number generator is defined, as well as the clock for measure processing time. This is due parallel processing requires to construct new R environments and each of them need to load specific libraries and parameters to run the algorithm. Note that this code chuck is controled by a **if** structure which is activated when the parameter **processing** is set as TRUE at the beggining of the script.

```{r eval=F}
if(processing){
  #start cluster
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  #start point
  iter.seq <- start_point:length(id.ins)
  #required libraries
  par.libraries <- c("raster","Hmisc","ranger","Boruta","GWmodel","caret","rfUtilities","randomForest")

  #go processing #   
  boruta.res <- foreach(i=iter.seq,.packages=par.libraries,.errorhandling="stop") %dopar% {
    #to be sure that the core is using the favorite random number
    set.seed(666)
    #processing time
    start.time.i <- proc.time()
    
  #---CODE CONTINUES BELOW---#
```

The next section of the algorithm, applies the prepare.data processing function. It return the databases merged into one.

```{r eval=F}
    ##### 1) prepare target observation   ##### 
    
    #preprocess data + get inputs
    merged.i <- prepare.data(id.ins[i],collect = T)
    collected.i <- merged.i[[2]]
    #update
    merged.i <- merged.i[[1]]
    
    #---CODE CONTINUES BELOW---#
```

Then it comes with a sequence of operations, where most relevant are tabulation of AP classes to measure how much samples are required/exceed the **sample_size** number. If this number is not meet then the function **find.nearest** is applied to obtain data from neighboring high schools. If the number of samples of an AP class exceed, then it is downsampled to meet the sample_size number. Observe that some statistics are collected, e.g. number and distances of high schools used in the spatial sampling (linkObs, linkDist). Finally, it is also calculated the percentage of missing data in the AFAC database.

```{r eval=F}
    ##### 2) balance samples   ##### 
    
    #calculate sizes
    class.size <- table(merged.i[,1])
    low.classes <- names(class.size)[class.size < sample_size]
    #activate if any class is not balanced
    if(length(low.classes)>=1){
      #get near high-schools
      near.i <- find.nearest(i)
      near.i <- near.i[2:nrow(near.i),]
      #calculate needed samples
      required.samples <- sample_size - class.size[names(class.size) %in% low.classes]
      #initiate
      samples.data <- list()
      joined.schools <- list()
      for(j in 1:nrow(near.i)){
        #get samples
        samples.j <- prepare.data(near.i[j,1],collect = F)
        samples.num <- table(samples.j[,grep(AP_var,names(samples.j))])
        samples.num <- samples.num[names(samples.num) %in% low.classes]
        #collect samples
        samples.data[[j]] <- samples.j
        joined.schools[[j]] <- near.i[j,]
        #update required samples
        required.samples <- required.samples-samples.num
        required.samples[required.samples < 0] <- 0
        if(all(required.samples==0)){
          break
        }
      }
      #merge 
      samples.data <- do.call("rbind.data.frame",samples.data)
      rm(samples.num,samples.j)
      #get required samples by distance order
      samples.class <- samples.data[,grep(AP_var,names(samples.data))]
      required.samples <- sample_size - class.size[names(class.size) %in% low.classes]
      samples.add <- list()
      for(j in 1:length(required.samples)){
        samples.j <- required.samples[j]
        samples.j <- samples.data[samples.class==names(samples.j),]
        samples.j <- samples.j[1:required.samples[j],]
        samples.add[[j]] <- samples.j
      }
      samples.add <- do.call("rbind.data.frame",samples.add)
      #update target data
      merged.i <- rbind(merged.i,samples.add)
      #update joined schools + store
      joined.schools <- do.call("rbind.data.frame",joined.schools)
      joined.schools <- rbind(find.nearest(i)[1,],joined.schools)
      collected.i$linkObs <- nrow(joined.schools)
      collected.i$linkDist <- round(max(joined.schools[,2]),2)
      #get links
      collected.i$linkCoor <- do.call("rbind.data.frame",lapply(joined.schools[,1],codes.coordinates))
    }else{
      joined.schools <- find.nearest(i)[1,]
      collected.i$linkObs <- nrow(joined.schools)
      collected.i$linkDist <- round(max(joined.schools[,2]),2)
      collected.i$linkCoor <- NA
    }
    merged.i <- merged.i[!is.na(merged.i[,1]),]
    #remove offsets by downsampling or minor gaps by duplicating
    merged.i <- base::split(merged.i,merged.i[,1])
    for(j in 1:length(merged.i)){
      if(nrow(merged.i[[j]]) > sample_size){
        #remove (keep short distances)
        merged.i[[j]] <- merged.i[[j]][1:sample_size,]
      }else if(nrow(merged.i[[j]]) < sample_size){
        #get random sample
        ran.sample <- merged.i[[j]][sample(1:nrow(merged.i[[j]]),sample_size-nrow(merged.i[[j]])),,drop=F]
        merged.i[[j]] <- rbind(merged.i[[j]],ran.sample)
      }
    }
    merged.i <- do.call("rbind.data.frame",merged.i)
    #calculate data gaps
    merged.na <- table(is.na(merged.i))
    if(length(names(merged.na))==1){
      if(grepl("TRUE",names(merged.na))){
        merged.na <- 100
      }else{
        merged.na <- 0
      }
    }else{
      merged.na <- (merged.na[2]*100) / sum(merged.na)
    }
    collected.i$naPerc <- merged.na
    
    #---CODE CONTINUES BELOW---#
```

As these samples are balanced, they are still noisy. To clean them, we first removed features (or columns) with all samples as null, while those with some missing cases are filled with the median value. If the feature shows zero-variance, they are also removed since they are noisy and not recommended for train machine learning models (See: [Kuhn, 2019](https://topepo.github.io/caret/))  

```{r eval=F}
    ##### 3) clean samples   ##### 
    
    #remove all-NA columns
    merged.i <- merged.i[, !apply(is.na(merged.i), 2, all)]
    #impute other gaps with the median
    merged.i <- data.frame(lapply(merged.i,impute,median),stringsAsFactors=F)
    #find and remove zero-variance 
    merged.i <- merged.i[, -nearZeroVar(merged.i)]
    
    #---CODE CONTINUES BELOW---#
```

After data cleaning, Boruta and RF are executed through the **boruta.warapper** function. It is tried two times if result are null. Results of accuracy metrics, variables importance, cross vlaidation, among at the end collected.

```{r eval=F}
    ##### 4) run BORUTA + RF  ##### 
    
    #fist run
    boruta.i <- boruta.wrapper(merged.i,max.runs=max_runs,max_iterations=10)
    #second try 
    if(length(boruta.i)==0){
      boruta.i <- boruta.wrapper(merged.i,max.runs=max_runs,max_iterations=10)
    }
    #if none happens
    if(length(boruta.i)==0){
      boruta.i <- NA
    }
    #merge
    if(length(boruta.i)!=0){
      boruta.i <- list(acc=do.call("rbind.data.frame",lapply(boruta.i,"[[",1)),
                       bor=do.call("rbind.data.frame",lapply(boruta.i,"[[",2)),
                       cross=do.call("rbind.data.frame",lapply(boruta.i,"[[",3)))
    }else{
      boruta.i <- boruta.i[[1]]
    }
```

As in the research paper it was included a statistical summary of high schools´ students populations, the next lines of code shows some calculations made with MCRO and HSCH databases

```{r eval=F}
    ##### 5) derive additional respect high schools   #####
    
    #socio-economic quintiles
    qui.i <- as.data.frame(table(collected.i$qui))
    qui.i$per <- (qui.i$Freq * 100) / sum(qui.i$Freq)
    qui.i <- data.frame(qui.i=c("Low-Medium","High"),per=c(sum(qui.i$per[1:3]),sum(qui.i$per[4:5])),stringsAsFactors=F)
    if(any(qui.i$per >=60)){
      qui.i <- qui.i[qui.i$per >=60,1]
    }else{
      qui.i <- "Mixed"
    }
    #sex predominance
    sex.i <- collected.i$sex
    levels(sex.i) <- c("Males","Females")
    sex.i <- as.data.frame(table(sex.i))
    sex.i$per <- (sex.i$Freq * 100) / sum(sex.i$Freq)
    if(any(sex.i$per >=60)){
      sex.i <- sex.i[sex.i$per >=60,1]
    }else{
      sex.i <- "Mixed"
    }
    #handicap presence
    han.i <- collected.i$han
    han.i <- as.data.frame(table(han.i))
    if(han.i[2,]$Freq!=0){
      han.i <- "Present"
    }else{
      han.i <- "Non present"
    }
    #etnicity composition
    etn.i <- collected.i$etn
    if(all(is.na(etn.i))){
      etn.i <- NA
    }else{
      levels(etn.i) <- c("Afro-Ecuatorian","Montubio","Indigenous","White/Mestizo","Other")
      etn.i <- as.data.frame(table(etn.i))
      etn.i$per <- (etn.i$Freq * 100) / sum(etn.i$Freq)
      if(any(etn.i$per >=60)){
        etn.i <- as.character(etn.i[etn.i$per >=60,1])
      }else{
        etn.i <- "Mixed"
      }
    }
    #cell phone with internet
    int.i <- collected.i$int
    levels(int.i) <- c("None","One","Two","Three or more")
    int.i <- as.data.frame(table(int.i))
    int.i$per <- (int.i$Freq * 100) / sum(int.i$Freq)
    int.i <- as.character(int.i[which.max(int.i$per),1])
    #classroom size
    class.i <- collected.i$class
    if(class.i>=75){
      class.i <- "Large (>75 ind.)"
    }else if(class.i>=25 & class.i<=74){
      class.i <- "Medium (25-75 ind.)"
    }else{
      class.i <- "Small (<25 ind.)"
    }
    #AP classes
    ap.i <- Hmisc::cut2(collected.i$AP,AP.cut)
    levels(ap.i) <- AP_labels
    ap.i <- names(which.max(table(ap.i)))
    #Financing  sources
    fin.i <- collected.i$fin
    fin.i <- names(which.max(table(fin.i))[1])
    if(fin.i=="publico"){
      fin.i <- "Public"
    }else if(fin.i=="privado"){
      fin.i <- "Private"
    }else{
      fin.i <- "Mixed"
    }
    #store these summaries
    boruta.i$des <- data.frame(ID_ins=id.ins[i],
                               AP=mean(collected.i$AP),
                               APclass=ap.i,
                               size=collected.i$class,
                               qui=qui.i,
                               sex=sex.i,
                               han=han.i,
                               etn=etn.i,
                               int=int.i,
                               fin=fin.i,
                               class=class.i,
                               stringsAsFactors=F)
```

To close the output collection, some format is gave to outputs to finalize structuration of results and facilitate their management and storage.  

```{r eval=F}
    ##### 6) complete tables with IDs   #####
    
    #add to boruta.i accuracy metrics, failed predictions, and spatial sampling settings
    boruta.i$acc <- cbind(ID_ins=id.ins[i],
                          boruta.i$acc[,1:4],
                          naPerc=as.numeric(collected.i$naPerc),
                          linkObs=collected.i$linkObs,
                          linkDist=collected.i$linkDist,
                          pred=boruta.i$acc$pred,
                          fail=ifelse(boruta.i$acc[,5]==boruta.i$des$APclass,"no","yes"),
                          boruta.i$acc[,6:ncol(boruta.i$acc)])
    rownames(boruta.i$acc) <- NULL
    #complete boruta.i with ID_ins
    boruta.i$bor$ID_ins <- id.ins[i]
    boruta.i$bor <- split(boruta.i$bor,boruta.i$bor$iter)
    for(j in 1:length(boruta.i$bor)){
      boruta.i$bor[[j]]$kappa <- as.numeric(as.character(boruta.i$acc$kappa[j]))
    }
    boruta.i$bor <- do.call("rbind.data.frame",boruta.i$bor)
    boruta.i$bor <- boruta.i$bor[,c("ID_ins","iter","kappa","var","meanImp","medianImp","minImp","maxImp","normHits")]
    #finally, add additional features respect spatial sampling
    boruta.i$lkn <- data.frame(ID_ins=id.ins[i],
                               collected.i$linkCoor,
                               linkObs=collected.i$linkObs,
                               linkDist=collected.i$linkDist)
```

The last steps constitutes the storage of outputs. First, clock is stopped to measure computing time applied in the iteration, while a series of of 5 .csv files are saved. Each of them describes:

* bor_(iteration number).csv: refers to the output of Boruta feature selection analysis. Only the most important variable is maintained from the model;
* acc_(iteration number).csv: refers to accuracy metrics and prediction probabilities for each AP class;
* des_(iteration number).csv: refers to socio-economic among other statistics calculated for each high school;
* lkn_(iteration number).csv: refers to some statistics derived from spatial sampling; and
* cross_(iteration number).csv: refers to the cross validation test results. For more information please see: [rfcv](http://127.0.0.1:21781/library/randomForest/html/rfcv.html)

```{r eval=F}
    ##### 7) save iteration results   #####
    
    #close: end time + progress file
    end.time.i <- proc.time() - start.time.i
    cat(paste0(as.character(i)," of ",length(id.ins)," | time used: ",round(end.time.i[3],1),"\n"),
        file=paste0(output_folder,"/data_progress.txt"), append=TRUE)
    #boruta
    out.name <- paste0(iter.folder,"/bor_",i,".csv")
    write.csv(boruta.i$bor,out.name,row.names=F)
    #accuracy
    out.name <- paste0(iter.folder,"/acc_",i,".csv")
    write.csv(boruta.i$acc,out.name,row.names=F)
    #description
    out.name <- paste0(iter.folder,"/des_",i,".csv")
    write.csv(boruta.i$des,out.name,row.names=F)
    #links
    out.name <- paste0(iter.folder,"/lkn_",i,".csv")
    write.csv(boruta.i$lkn,out.name,row.names=F)
    #cross validation
    out.name <- paste0(iter.folder,"/cross_",i,".csv")
    write.csv(boruta.i$cross,out.name,row.names=F)
  }
```

To end parallel processing when all the high schools´cases are computed, the next lines of code stop cores from further working but also calculates the overall processing time. Finally, a series of .xlsx files are saved to describe what has been done to features on each database case (ins refers to HSCH, micro to MCRO, and fa to AFAC). The resulting R workspace is also stored for future exploration into a .RData file, which can be easily read directly into RStudio. Advice that the **if** structure is closed here when the parameter **processing** is set TRUE at the beginning of the script.

```{r eval=F}
  #end cluster
  stopCluster(cl)
  end.time <- proc.time() - start.time
  cat(paste0("*** Total time used: ",end.time[3]," ***"),
      file=paste0(output_folder,"/data_progress.txt"), append=TRUE)
  #save dictionaries
  write.xlsx(ins.dic,paste0(dic.folder,"/ins_dictionary.xlsx"),overwrite=T)
  write.xlsx(micro.dic,paste0(dic.folder,"/micro_dictionary.xlsx"),overwrite=T)
  write.xlsx(fa.dic,paste0(dic.folder,"/fa_dictionary.xlsx"),overwrite=T)
  #save R enviroment
  out.name <- paste0(output_folder,"/workspace_boruta.RData")
  save.image(out.name)
  #end if-processing
}
```


## 7. COMPILATION OF RESULTS - END OF THE SCRIPT

When all results are computed, the next final code chunk is applied. It first open the /iter folder at the **output_folder** which is defined at the begining of the script to compile csv files into a unique database. This is used to distribute data among high school cases but as spatial points to facilitate mapping and analysis in a geographical information systems, such as [QGIS](https://qgis.org/en/site/). Note that if the parameter **processing** is set FALSE, this part of the script will be executed directly avoiding the processing, which can take around ~9.5 for 3321 high school cases.

```{r eval=F}
#read high schools shapefile
ins.base <- shapefile(paste0(output_folder,"/highSchools_des.shp"))
ins.base@data <- ins.base@data[,1,drop=F]
#read csv files
csv.files <- list()
csv.files$acc <- mixedsort(list.files(iter.folder,full.names=T,pattern="^acc_"))
csv.files$bor <- mixedsort(list.files(iter.folder,full.names=T,pattern="^bor_"))
csv.files$des <- mixedsort(list.files(iter.folder,full.names=T,pattern="^des_"))
csv.files$lkn <- mixedsort(list.files(iter.folder,full.names=T,pattern="^lkn_"))
csv.files$cross <- mixedsort(list.files(iter.folder,full.names=T,pattern="^cross_"))
#save description shapefile
csv.des <- lapply(csv.files$des,fread,data.table=F)
csv.des <- do.call("rbind.data.frame",csv.des)
ins.des <- ins.base[ins.base$ID_ins %in% csv.des$ID_ins,]
ins.des@data <- merge(ins.des@data,csv.des,by="ID_ins")
out.name <- paste0(output_folder,"/highSchools_des.shp")
shapefile(ins.des,out.name,overwrite=T)
#save accuracy shapefile
csv.acc <- lapply(csv.files$acc,fread,data.table=F)
csv.acc <- do.call("rbind.data.frame",csv.acc)
csv.acc <- split(csv.acc,csv.acc[,1])
csv.acc <- lapply(csv.acc,function(x){
  x <- x[which.max(x$kappa),]
})
csv.acc <- do.call("rbind.data.frame",csv.acc)
rownames(csv.acc) <- NULL
ins.acc <- ins.base[ins.base@data[,1] %in% csv.acc[,1],]
ins.acc@data <- merge(ins.acc@data,csv.acc,by="ID_ins")
out.name <- paste0(output_folder,"/highSchools_acc.shp")
shapefile(ins.acc,out.name,overwrite=T)
#save links shapefile
csv.lkn <- lapply(csv.files$lkn,fread,data.table=F)
csv.lkn <- csv.lkn[sapply(csv.lkn,nrow)!=1]
csv.lkn <- lapply(csv.lkn,function(x){
  x.sp <- spLines(as.matrix(x[,2:3]),crs=ins.base@proj4string@projargs)
  x <- SpatialLinesDataFrame(x.sp,x[,c(1,4,5)])
})
csv.lkn <- do.call("rbind",csv.lkn)
out.name <- paste0(output_folder,"/highSchools_lkn.shp")
shapefile(csv.lkn,out.name,overwrite=T)
#save boruta shapefile
csv.bor <- lapply(csv.files$bor,fread,data.table=F)
csv.bor <- do.call("rbind.data.frame",csv.bor)
csv.bor <- split(csv.bor,csv.bor[,1])
csv.bor <- lapply(csv.bor,function(x){
  if(!is.na(x$kappa[1])){
    x <- x[x$kappa==max(x$kappa,na.rm=T),] #choose best kappa
    x <- x[!is.na(x$iter),] #clean
    if(length(x$iter)>=2){
      x$iter <- max(x$iter)
    }
    x <- x[order(x$meanImp,decreasing=T)[1:3],] #order
    x <- x[complete.cases(x),] #if NA
    x.var <- data.frame(matrix(x$var,1,nrow(x)))
    if(ncol(x.var)<3){
      x.add <- as.data.frame(matrix(rep(NA,3-ncol(x.var)),1,3-ncol(x.var)))
      x.var <- cbind(x.var,x.add)
    }
    names(x.var) <- c("VarImp1","VarImp2","VarImp3")
    x.bor <- data.frame(ID_ins=unique(x[,1]),
                        iter=unique(x[,2]),
                        kappa=unique(x[,3]),
                        meanImp=mean(x$meanImp),
                        normHits=mean(x$normHits))
    x.bor <- cbind(x.bor,x.var)
    x.bor <- cbind(x.bor[,1:2],AP_class=csv.des[csv.des[,1] %in% x.bor[,1],3],x.bor[,3:ncol(x.bor)])
  }else{
    x.bor <- data.frame(ID_ins=unique(x$ID_ins),AP_class=csv.des[csv.des[,1] %in% x$ID_ins,3],
                        iter=NA,kappa=NA,meanImp=NA,normHits=NA,VarImp1=NA,VarImp2=NA,VarImp3=NA)
  }
})
csv.bor <- do.call("rbind.data.frame",csv.bor)
rownames(csv.bor) <- NULL
csv.bor <- csv.bor[!is.na(csv.bor$ID_ins),] #remove NA (not processed by boruta)
ins.bor <- ins.base[ins.base@data[,1] %in% csv.bor[,1],]
ins.bor@data <- merge(ins.bor@data,csv.bor,by="ID_ins")
out.name <- paste0(output_folder,"/highSchools_bor.shp")
shapefile(ins.bor,out.name,overwrite=T)
#save cross shapefile
csv.cross <- lapply(csv.files$cross,fread,data.table=F)
csv.bor <- lapply(csv.files$bor,fread,data.table=F)
for(i in 1:length(csv.cross)){
  test.na <- !all(is.na(csv.cross[[i]]$cvAll))
  if(test.na){
    csv.cross[[i]]$ID_ins <- unique(csv.bor[[i]]$ID_ins)
    csv.cross[[i]]$kappa <- sapply(lapply(split(csv.bor[[i]],csv.bor[[i]]$iter),"[[",3),unique)
    csv.cross[[i]] <- csv.cross[[i]][,c(6,7,1:5)]
    #remove NA (not cross-validated or with boruta output) 
    csv.cross[[i]] <- csv.cross[[i]][!is.na(csv.cross[[i]]$cvAll),]
    #extract best kappa
    csv.cross[[i]] <- csv.cross[[i]][which.max(csv.cross[[i]]$kappa)[1],]
  }else{
    csv.cross[[i]] <- NA
  }
}
csv.cross <- csv.cross[!is.na(csv.cross)]
csv.cross <- do.call("rbind.data.frame",csv.cross)
ins.cross <- ins.base[ins.base@data[,1] %in% csv.cross[,1],]
ins.cross@data <- merge(ins.cross@data,csv.cross,by="ID_ins")
out.name <- paste0(output_folder,"/highSchools_cross.shp")
shapefile(ins.cross,out.name,overwrite=T)
#end routine
```

## 8. CLOSING REMARKS

The algorithm processing is demanding and users should be proceed with caution since large computations can fail (we experience a recursive interruption after ~2500 iterations without an explanation but it is suspicious a RAM overflow or a CPU temperature breakpoint). Therefore, it is important to monitor processed .csv files to advice the last processing point and restart processing. If users that require an update or help using this routine can email  **fabian_santos_@hotmail.com** to discuss possibilities.
