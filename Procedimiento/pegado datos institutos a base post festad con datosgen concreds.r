rm(list=ls())
gc()
directorio<-"C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Institutos Secundaria"
setwd(directorio)

gen2012<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Creditos\\gen2012_sindup_confest_compat_condatosgen_concreds.csv")
gen2013<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Creditos\\gen2013_sindup_confest_compat_condatosgen_concreds.csv")
gen2014<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Creditos\\gen2014_sindup_confest_compat_condatosgen_concreds.csv")
gen2015<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Creditos\\gen2015_sindup_confest_compat_condatosgen_concreds.csv")

institutos<-read.csv("institutos_sec.csv",sep=";")
institutos<-institutos[which(institutos$BEDELIA=="CCEE"),]

gen2012$COD_INSTITUTO<-gen2012$INSTITUTO
gen2013$COD_INSTITUTO<-gen2013$INSTITUTO
gen2014$COD_INSTITUTO<-gen2014$INSTITUTO
gen2015$COD_INSTITUTO<-gen2015$INSTITUTO

institutos$COD_INSTITUTO<-institutos$CODIGO
institutos$NOM_INSTITUTO<-institutos$NOMINST
institutos$LUG_INSTITUTO<-institutos$LUGAR
institutos$NOMLUG_INSTITUTO<-institutos$NOMLUGAR
institutos$TIPO_INSTITUTO<-institutos$TIPOINST


total12<-join(gen2012,institutos[,c("COD_INSTITUTO","NOM_INSTITUTO","LUG_INSTITUTO","NOMLUG_INSTITUTO","TIPO_INSTITUTO")],by=c("COD_INSTITUTO"))
total13<-join(gen2013,institutos[,c("COD_INSTITUTO","NOM_INSTITUTO","LUG_INSTITUTO","NOMLUG_INSTITUTO","TIPO_INSTITUTO")],by=c("COD_INSTITUTO"))
total14<-join(gen2014,institutos[,c("COD_INSTITUTO","NOM_INSTITUTO","LUG_INSTITUTO","NOMLUG_INSTITUTO","TIPO_INSTITUTO")],by=c("COD_INSTITUTO"))
total15<-join(gen2015,institutos[,c("COD_INSTITUTO","NOM_INSTITUTO","LUG_INSTITUTO","NOMLUG_INSTITUTO","TIPO_INSTITUTO")],by=c("COD_INSTITUTO"))

write.csv(total12,"gen2012_sindup_confest_compat_condatosgen_concreds_coninst.csv")
write.csv(total13,"gen2013_sindup_confest_compat_condatosgen_concreds_coninst.csv")
write.csv(total14,"gen2014_sindup_confest_compat_condatosgen_concreds_coninst.csv")
write.csv(total15,"gen2015_sindup_confest_compat_condatosgen_concreds_coninst.csv")

