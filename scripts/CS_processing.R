######### R ENVIRONMENT ##############

#install or read lilbraries
list.of.packages <- c("rgdal","raster", "openxlsx","stringi","parallel","data.table","rfUtilities","randomForest",
                      "doParallel","foreach","Hmisc","Boruta","gtools","GWmodel","caret")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){install.packages(new.packages,dependencies = T)}
lapply(list.of.packages, require, character.only = T)
#favorite random number
set.seed(666)

######### PROCESS INEVAL HSCH, MCRO & AFAC DATABASES 2016-2017 ######### 

#input high schools (HSCH) shapefile
ins_base <- "./amieactivas_ciclo15_16.shp"
#input micro (MCRO) anf fa (AFAC) excel databases. Check below "PREPARE..." for specific variable removed due duplication or uninformative data.
micro_base <- "./SBAC17_micro_627960_20180426_XLS.xlsx"
fa_base <- "./SBAC17_faestudiantes_627960_20180307_XLS.xlsx"
#input fa (AFAC) excel dictionary
fa_dic <- "./SBAC17_diccionariofaestudiantes_1188_20180307_DIC.xlsx"
#save folder
output_folder <- "./CS"
#AP score column name, cutpoints and labels (+1)
AP_var <- "Notaexamendegrado"
AP_cut <- 7
AP_labels <- c("Low","High")
#boruta parameters
sample_size <- 20
max_runs <- 100
#backward recursive elimination steps
max_iterations <- 10 
#cores to use
ncores <- 8
#do processing? if false, the script will only read processed csv files at the output folder 
processing <- F
#start point (to restart from a specific spatial point if computer is turned off)
start_point <- 1

######### OUTPUT FOLDERS ##############

#start time
start.time <- proc.time()
#create folders
dir.create(output_folder,showWarnings=F,recursive=T)
iter.folder <- paste0(output_folder,"/iter")
dir.create(iter.folder,showWarnings=F)
dic.folder <- paste0(output_folder,"/dic")
dir.create(dic.folder,showWarnings=F)

######### PREPARE ins.base (HSCH) ##############

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

######### PREPARE micro.base (MCRO) ##############

if(processing){
  #variables to eliminate
  rem.vars <- c("Institucion","Region","Zonadeplanificacion","Zonadeplanificacion","Anodenacimiento",
                "Distrito","Circuito","Codigodelaprovincia","Provincia","Codigodelcanton","Canton","Codigodelaparroquia",
                "Parroquia","Tipodesostenimiento","Area","Tipodepoblacion","Estado",
                "Puntajeparapostularalaeducacionsuperior","Notaexamendegradoajustado","Indicesocioeconomico","Etnia",
                "DominioMatematico","DominioLinguistico","DominioCientifico","DominioSocial","Regimendeevaluacion",
                "DominioMatematico_cat","DominioLinguistico_cat","DominioCientifico_cat","DominioSocial_cat")
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
  AP.cut <- c(min(na.omit(micro.base[,grep(AP_var,names(micro.base))])),AP_cut,max(na.omit(micro.base[,grep(AP_var,names(micro.base))])))
  #end if-processing
}

######### PREPARE fa.base (AFAC) ##############

if(processing){
  #eliminate variables
  rem.vars <- c("es_regeva","poblacion","estado","isec","sexoabe","diskabe")
  rem.vars <- c(rem.vars,
                "cescabe",
                "cescbbe",
                "clapabe",
                "clapbbe",
                "comicbe",
                "escrabe",
                "hupcbbe",
                "imprbbe",
                "inteabe",
                "prejbbe",
                "uintabe",
                "aytacbe",
                "capacbe",
                "clueabe",
                "factabe",
                "flcaabe",
                "hambabe",
                "hvecabe",
                "ppeebbe",
                "prdsabe",
                "prdscbe",
                "agrsabe",
                "bpelcbe",
                "brobabe",
                "bvancbe",
                "roalbbe",
                "segebbe",
                "artiabe",
                "atrsabe",
                "connabe",
                "cultabe",
                "declcbe",
                "depiabe",
                "expeabe",
                "faltabe",
                "inciabe",
                "rabuabe",
                "rabubbe",
                "rabucbe",
                "rabudbe",
                "rabuebe",
                "rabufbe",
                "rabugbe",
                "rabuhbe",
                "rabuibe",
                "rabujbe",
                "rabukbe",
                "rabulbe",
                "rabumbe",
                "rabunbe",
                "rabuobe",
                "rabupbe",
                "rperabe",
                "scamabe",
                "segeabe",
                "sociabe",
                "tescbbe",
                "tesccbe",
                "trviabe",
                "trvicbe",
                "acopabe",
                "allcabe",
                "amclabe",
                "amclbbe",
                "amocbbe",
                "atecabe",
                "ayuoabe",
                "buclcbe",
                "buesabe",
                "buesbbe",
                "dicsabe",
                "didsabe",
                "diecabe",
                "dietabe",
                "dirfabe",
                "emptabe",
                "estrabe",
                "extrabe",
                "extrcbe",
                "hramabe",
                "perdbbe",
                "srecabe",
                "amdoabe",
                "aprebbe",
                "atemabe",
                "ayudbbe",
                "cliadbe",
                "enclbbe",
                "feldabe",
                "inapbbe",
                "inmeabe",
                "inmebbe",
                "llmaabe",
                "llmabbe",
                "maanbbe",
                "mahcbbe",
                "maprbbe",
                "men1bbe",
                "men2bbe",
                "mensbbe",
                "mticabe",
                "ovocabe",
                "prdibbe",
                "prexbbe",
                "prmobbe",
                "proebbe",
                "propbbe",
                "prrebbe",
                "relmabe",
                "relmcbe",
                "solcbbe",
                "tclabbe",
                "vamaabe",
                "vamabbe",
                "disfbbe",
                "fartbbe",
                "fbibdbe",
                "fcinbbe",
                "fdepbbe",
                "fleebbe",
                "fnotbbe",
                "fsocbbe",
                "fvisbbe",
                "hrarabe",
                "hrdpabe",
                "hrvlabe",
                "isocbbe",
                "prculbe",
                "cboxabe",
                "cboxbbe",
                "comhdbe",
                "comjcbe",
                "comoabe",
                "comrcbe",
                "comvcbe",
                "hfpcbbe",
                "hfpccbe",
                "hrtvabe",
                "ncelbbe",
                "ncelcbe",
                "rredabe",
                "tvstabe",
                "ucorabe",
                "caslbbe",
                "etnibbe",
                "etnicbe",
                "inacabe",
                "lengabe",
                "momoabe",
                "movhabe",
                "movhbbe",
                "movhcbe",
                "movhdbe",
                "movhebe",
                "movhfbe",
                "movhgbe",
                "movhhbe",
                "refuabe",
                "tmovabe",
                "actmabe",
                "actpabe",
                "niembbe",
                "niemfbe",
                "niepbbe",
                "niepfbe",
                "ocjhabe",
                "ocumebe",
                "ocupebe",
                "aguaabe",
                "basuabe",
                "clavabe",
                "cocqbbe",
                "cradabe",
                "crefbbe",
                "cuarpbe",
                "desaabe",
                "luzeabe",
                "nbanabe",
                "ncarabe",
                "nducabe",
                "nmicabe",
                "nmtvabe",
                "nvcaabe",
                "pardabe",
                "piscabe",
                "telfabe",
                "tpviabe",
                "tshgabe",
                "bdhuabe",
                "ctracbe",
                "intrbbe",
                "niessbe",
                "nsegabe",
                "patrbbe",
                "patrcbe",
                "veccabe",
                "jhogabe",
                "nhijabe",
                "nhijbbe",
                "vivfcbe",
                "vivfdbe",
                "vivfebe",
                "vivffbe",
                "vivfgbe",
                "vivfhbe",
                "vivfjbe",
                "vivfsbe",
                "vivfybe",
                "vivsabe")
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
  #identify "No sé" irrelevant answers 
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

######### JOIN DATABASES ##############

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

######### BORUTA FUNCTIONS ##############

#dummy vars function
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

#prepare high-school data (x is the institution code)
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

#boruta wrapper function
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

#find nearest high-schools (x is the position index at ins.base)
find.nearest <- function(x){
  x.data <- ins.base@data
  x.data$dist <- gw.dist(dp.locat=coordinates(ins.base),rp.locat=coordinates(ins.base[i,]))[,1]
  x.data <- x.data[order(x.data$dist),c("ID_ins","dist"),drop=F]
  return(x.data)
}

#derive coordinates from schools codes (x is the school code)
codes.coordinates <- function(x){
  x <- coordinates(ins.base[ins.base$ID_ins==x,])
}

######### BORUTA IMPLEMENTATION (algorithm 1) ##############

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
    
    ##### 1) prepare target observation   ##### 
    
    #preprocess data + get inputs
    merged.i <- prepare.data(id.ins[i],collect = T)
    collected.i <- merged.i[[2]]
    #update
    merged.i <- merged.i[[1]]
    
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
    
    ##### 3) clean samples   ##### 
    
    #remove all-NA columns
    merged.i <- merged.i[, !apply(is.na(merged.i), 2, all)]
    #impute other gaps with the median
    merged.i <- data.frame(lapply(merged.i,impute,median),stringsAsFactors=F)
    #find and remove zero-variance 
    merged.i <- merged.i[, -nearZeroVar(merged.i)]
    
    ##### 4) run BORUTA + RF  ##### 
    
    #fist run
    boruta.i <- boruta.wrapper(merged.i,max.runs=max_runs,max_iterations=10)
    #second try 
    if(length(boruta.i)==0){
      boruta.i <- boruta.wrapper(merged.i,max.runs=max_runs)
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

######### RESTITUTE RESULTS & SAVE ##############

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
