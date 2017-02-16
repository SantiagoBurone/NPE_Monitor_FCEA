rm(list=ls())
gc()
directorio<-"C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Egresos"
setwd(directorio)
library(plyr)

egresos<-read.csv("g_egre_anio_2012_2017_conv.csv")

gen2012<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Institutos Secundaria\\gen2012_sindup_confest_compat_condatosgen_concreds_coninst.csv")
gen2013<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Institutos Secundaria\\gen2013_sindup_confest_compat_condatosgen_concreds_coninst.csv")
gen2014<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Institutos Secundaria\\gen2014_sindup_confest_compat_condatosgen_concreds_coninst.csv")
gen2015<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Institutos Secundaria\\gen2015_sindup_confest_compat_condatosgen_concreds_coninst.csv")

egresos$CARR_EGRESO<-egresos$CARRERA

gen2012<-join(gen2012,egresos[,c("NOMBRE","FECHA_EGRESO","ANO_EGRESO","CARR_EGRESO")],by=c("NOMBRE"))
gen2013<-join(gen2013,egresos[,c("NOMBRE","FECHA_EGRESO","ANO_EGRESO","CARR_EGRESO")],by=c("NOMBRE"))
gen2014<-join(gen2014,egresos[,c("NOMBRE","FECHA_EGRESO","ANO_EGRESO","CARR_EGRESO")],by=c("NOMBRE"))
gen2015<-join(gen2015,egresos[,c("NOMBRE","FECHA_EGRESO","ANO_EGRESO","CARR_EGRESO")],by=c("NOMBRE"))

gen2012$COD_CARR_EGRESO<-rep(0,nrow(gen2012))
gen2012$COD_CARR_EGRESO[which(gen2012$CARR_EGRESO=="CONTADOR PÚBLICO")]<-rep(11,length(gen2012$COD_CARR_EGRESO[which(gen2012$CARR_EGRESO=="CONTADOR PÚBLICO")]))
gen2012$COD_CARR_EGRESO[which(gen2012$CARR_EGRESO=="LICENCIATURA EN ECONOMÍA")]<-rep(12,length(gen2012$COD_CARR_EGRESO[which(gen2012$CARR_EGRESO=="LICENCIATURA EN ECONOMÍA")]))
gen2012$COD_CARR_EGRESO[which(gen2012$CARR_EGRESO=="LICENCIATURA EN ADMINISTRACIÓN")]<-rep(14,length(gen2012$COD_CARR_EGRESO[which(gen2012$CARR_EGRESO=="LICENCIATURA EN ADMINISTRACIÓN")]))
gen2012$COD_CARR_EGRESO[which(gen2012$CARR_EGRESO=="TÉCNICO EN ADMINISTRACIÓN")]<-rep(2,length(gen2012$COD_CARR_EGRESO[which(gen2012$CARR_EGRESO=="TÉCNICO EN ADMINISTRACIÓN")]))

gen2013$COD_CARR_EGRESO<-rep(0,nrow(gen2013))
gen2013$COD_CARR_EGRESO[which(gen2013$CARR_EGRESO=="CONTADOR PÚBLICO")]<-rep(11,length(gen2013$COD_CARR_EGRESO[which(gen2013$CARR_EGRESO=="CONTADOR PÚBLICO")]))
gen2013$COD_CARR_EGRESO[which(gen2013$CARR_EGRESO=="LICENCIATURA EN ECONOMÍA")]<-rep(12,length(gen2013$COD_CARR_EGRESO[which(gen2013$CARR_EGRESO=="LICENCIATURA EN ECONOMÍA")]))
gen2013$COD_CARR_EGRESO[which(gen2013$CARR_EGRESO=="LICENCIATURA EN ADMINISTRACIÓN")]<-rep(14,length(gen2013$COD_CARR_EGRESO[which(gen2013$CARR_EGRESO=="LICENCIATURA EN ADMINISTRACIÓN")]))
gen2013$COD_CARR_EGRESO[which(gen2013$CARR_EGRESO=="TÉCNICO EN ADMINISTRACIÓN")]<-rep(2,length(gen2013$COD_CARR_EGRESO[which(gen2013$CARR_EGRESO=="TÉCNICO EN ADMINISTRACIÓN")]))

gen2014$COD_CARR_EGRESO<-rep(0,nrow(gen2014))
gen2014$COD_CARR_EGRESO[which(gen2014$CARR_EGRESO=="CONTADOR PÚBLICO")]<-rep(11,length(gen2014$COD_CARR_EGRESO[which(gen2014$CARR_EGRESO=="CONTADOR PÚBLICO")]))
gen2014$COD_CARR_EGRESO[which(gen2014$CARR_EGRESO=="LICENCIATURA EN ECONOMÍA")]<-rep(12,length(gen2014$COD_CARR_EGRESO[which(gen2014$CARR_EGRESO=="LICENCIATURA EN ECONOMÍA")]))
gen2014$COD_CARR_EGRESO[which(gen2014$CARR_EGRESO=="LICENCIATURA EN ADMINISTRACIÓN")]<-rep(14,length(gen2014$COD_CARR_EGRESO[which(gen2014$CARR_EGRESO=="LICENCIATURA EN ADMINISTRACIÓN")]))
gen2014$COD_CARR_EGRESO[which(gen2014$CARR_EGRESO=="TÉCNICO EN ADMINISTRACIÓN")]<-rep(2,length(gen2014$COD_CARR_EGRESO[which(gen2014$CARR_EGRESO=="TÉCNICO EN ADMINISTRACIÓN")]))

gen2015$COD_CARR_EGRESO<-rep(0,nrow(gen2015))
gen2015$COD_CARR_EGRESO[which(gen2015$CARR_EGRESO=="CONTADOR PÚBLICO")]<-rep(11,length(gen2015$COD_CARR_EGRESO[which(gen2015$CARR_EGRESO=="CONTADOR PÚBLICO")]))
gen2015$COD_CARR_EGRESO[which(gen2015$CARR_EGRESO=="LICENCIATURA EN ECONOMÍA")]<-rep(12,length(gen2015$COD_CARR_EGRESO[which(gen2015$CARR_EGRESO=="LICENCIATURA EN ECONOMÍA")]))
gen2015$COD_CARR_EGRESO[which(gen2015$CARR_EGRESO=="LICENCIATURA EN ADMINISTRACIÓN")]<-rep(14,length(gen2015$COD_CARR_EGRESO[which(gen2015$CARR_EGRESO=="LICENCIATURA EN ADMINISTRACIÓN")]))
gen2015$COD_CARR_EGRESO[which(gen2015$CARR_EGRESO=="TÉCNICO EN ADMINISTRACIÓN")]<-rep(2,length(gen2015$COD_CARR_EGRESO[which(gen2015$CARR_EGRESO=="TÉCNICO EN ADMINISTRACIÓN")]))

write.csv(gen2012,"gen2012_sindup_confest_compat_condatosgen_concreds_coninst_conegre.csv")
write.csv(gen2013,"gen2013_sindup_confest_compat_condatosgen_concreds_coninst_conegre.csv")
write.csv(gen2014,"gen2014_sindup_confest_compat_condatosgen_concreds_coninst_conegre.csv")
write.csv(gen2015,"gen2015_sindup_confest_compat_condatosgen_concreds_coninst_conegre.csv")