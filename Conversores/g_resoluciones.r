directorio<-"C:\\Users\\areasocial\\Documents\\Mesareasocial\\Mesareasocial\\Consultas SGB\\Consultas\\FIC\\g_resoluciones"
setwd(directorio)
library(foreign)
library(memisc)
#Primer Paso: Abrir el archivo .LST, eliminar la última línea y las primeras hasta los nombres de las variables y -- exclusive. Guardar como .txt.
#Segundo Paso: Idem al paso 1 pero eliminar tambien los nombres de las variables y los --- divisorios. Guardar como .csv#
#NOTA: Caracteres como numeral (#) generan problemas al cargado de la base. Eliminarlos del archivo fuente antes de hacer la conversion
base<-read.table("g_resoluciones.txt", sep=";", quote="\"", na.strings="",header=F)
cell.widths<-c(nchar(strsplit(as.character(base[2,]),split = " ")[[1]][1]),nchar(strsplit(as.character(base[2,]),split = " ")[[1]][2]),nchar(strsplit(as.character(base[2,]),split = " ")[[1]][3]),nchar(strsplit(as.character(base[2,]),split = " ")[[1]][4]),nchar(strsplit(as.character(base[2,]),split = " ")[[1]][5]),nchar(strsplit(as.character(base[2,]),split = " ")[[1]][6]))
rm(base)
base<-read.fwf(file = "g_resoluciones.csv",widths = cell.widths+1,header = F)
func1<-function(x) {ifelse(is.na(x)|trimws(x)=="",T,F)}
base<-base[which(apply(apply(base,1,func1),2,sum)!=ncol(base)),]
colnames(base)<-c("CODREV","FECRES","VALIRES","TEXTO1","TEXTO2","TEXTO3")
for(i in 1:ncol(base)){
  base[,i]<-sapply(as.character(base[,i]),trimws)
}
write.csv(base,"g_resoluciones_conv.csv")
