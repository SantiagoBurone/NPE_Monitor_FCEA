rm(list=ls())
gc()
library(plyr)
library(zoo)
directorio<-"C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Creditos"
setwd(directorio)

#Primer Paso: Abrir el archivo .LST, eliminar la última línea (que dice el numero de filas seleccionadas). Guardar como .txt.
cell.widths<-c(35,5,9,1,9,1,9,9,9)
base<-read.fwf(file = "g_materias a 3_10_2016.txt",widths = cell.widths+1,header = F)
base<-base[which(base$V5!=base$V5[2] & base$V1!=base$V1[3] & base$V1!=base$V1[4]),]
func1<-function(x) {ifelse(is.na(x)|trimws(x)=="",T,F)}
base<-base[which(apply(apply(base,1,func1),2,sum)!=ncol(base)),]
colnames(base)<-c("NOMBRE","MAT","TIPOCURSO","T","VALIDEZ","M","CARR","CICLO","CREDITOS")
for(i in 1:ncol(base)){
  base[,i]<-sapply(as.character(base[,i]),trimws)
}
base<-base[,c("NOMBRE","MAT","TIPOCURSO","T","VALIDEZ","M","CARR","CICLO","CREDITOS")]
write.csv(base,"base_creditos.csv")
rm(base)
gc()
base<-read.csv("base_creditos.csv", header=T, na.strings=c("","NA"))
base<-na.locf(base,na.rm=T,fromLast=F)
base<-base[,c("NOMBRE","MAT","TIPOCURSO","T","VALIDEZ","M","CARR","CICLO","CREDITOS")]
write.csv(base,"base_creditos_conv.csv")



