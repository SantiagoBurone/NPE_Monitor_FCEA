directorio<-"D:\\Facultad\\Mesareasocial\\Consultas SGB\\CCEE\\g_icarr4"
setwd(directorio)
library(foreign)
library(memisc)
#Primer Paso: Abrir el archivo .LST, eliminar la última línea y las primeras hasta los nombres de las variables y -- exclusive. Guardar como .txt.
#Segundo Paso: Idem al paso 1 pero eliminar tambien los nombres de las variables y los --- divisorios. Guardar como .csv#
base<-read.table("g_icarr4_12_2012.txt", sep=";", quote="\"", na.strings="")
cell.widths<-c(nchar(strsplit(as.character(base[2,]),split = " ")[[1]][1]),nchar(strsplit(as.character(base[2,]),split = " ")[[1]][2]),nchar(strsplit(as.character(base[2,]),split = " ")[[1]][3]),nchar(strsplit(as.character(base[2,]),split = " ")[[1]][4]),nchar(strsplit(as.character(base[2,]),split = " ")[[1]][5]),nchar(strsplit(as.character(base[2,]),split = " ")[[1]][6]),nchar(strsplit(as.character(base[2,]),split = " ")[[1]][7]))
rm(base)
base<-read.fwf(file = "g_icarr4_12_2012.csv",widths = cell.widths+1,header = F)
base<-base[-which(apply(apply(base,1,is.na),2,sum)==ncol(base)),]
colnames(base)<-c("CI","NOMBRE","DIR","TEL","MAIL","LUGAR_NAC","INSTITUTO_SEC")
for(i in 2:ncol(base)){
base[,i]<-sapply(as.character(base[,i]),trimws)
}