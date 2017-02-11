directorio<-"C:\\Users\\areasocial\\Documents\\Mesareasocial\\Mesareasocial\\Consultas SGB\\CCEE\\g_carcic"
setwd(directorio)
library(foreign)
library(memisc)
#Primer Paso: Abrir el archivo .LST, eliminar la última línea y las primeras hasta los nombres de las variables y -- exclusive. Guardar como .txt.
#Segundo Paso: Idem al paso 1 pero eliminar tambien los nombres de las variables y los --- divisorios. Guardar como .csv#
base<-read.table("g_carcic.txt", sep=";", quote="\"", na.strings="")
cell.widths<-c(nchar(strsplit(as.character(base[2,]),split = " ")[[1]][1]),nchar(strsplit(as.character(base[2,]),split = " ")[[1]][2]),nchar(strsplit(as.character(base[2,]),split = " ")[[1]][3]),nchar(strsplit(as.character(base[2,]),split = " ")[[1]][4]))
rm(base)
base<-read.fwf(file = "g_carcic.csv",widths = cell.widths+1,header = F)
func1<-function(x) {ifelse(is.na(x)|trimws(x)=="",T,F)}
base<-base[which(apply(apply(base,1,func1),2,sum)!=ncol(base)),]
colnames(base)<-c("CARR","NOMCAR","CICLO","NOMCIC")
for(i in 1:ncol(base)){
  base[,i]<-sapply(as.character(base[,i]),trimws)
}
write.csv(base,"g_carcic_conv.csv")


