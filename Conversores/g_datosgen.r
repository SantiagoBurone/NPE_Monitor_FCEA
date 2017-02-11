directorio<-"C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Datos filiatorios"
setwd(directorio)
#Primer Paso: Abrir el archivo .LST, eliminar la última línea y la primera hasta los guiones exclusive. Guardar como .txt.
#Segundo Paso: Idem al paso 1 pero eliminar tambien los --- divisorios. Guardar como .csv#
base<-read.table("g_datosgen_2016.txt", sep=";", quote="\"", na.strings="",header=F)
cell.widths<-c(nchar(strsplit(as.character(base[1,]),split = " ")[[1]][1]),nchar(strsplit(as.character(base[1,]),split = " ")[[1]][2]),nchar(strsplit(as.character(base[1,]),split = " ")[[1]][3]),nchar(strsplit(as.character(base[1,]),split = " ")[[1]][4]),nchar(strsplit(as.character(base[1,]),split = " ")[[1]][5]),nchar(strsplit(as.character(base[1,]),split = " ")[[1]][6]),nchar(strsplit(as.character(base[1,]),split = " ")[[1]][7]),nchar(strsplit(as.character(base[1,]),split = " ")[[1]][8]),nchar(strsplit(as.character(base[1,]),split = " ")[[1]][9]))
rm(base)
base<-read.fwf(file = "g_datosgen_2016.csv",widths = cell.widths+1,header = F)
func1<-function(x) {ifelse(is.na(x)|trimws(x)=="",T,F)}
base<-base[which(apply(apply(base,1,func1),2,sum)!=ncol(base)),]
colnames(base)<-c("CEDULA","DIGITO","NOMBRE","S","FNACIMIENTO","TELEFONO","LUGAR","AÑOFINESTUDIOS","INSTITUTO")
for(i in 1:ncol(base)){
  base[,i]<-sapply(as.character(base[,i]),trimws)
}
write.csv(base,"g_datosgen_2016_conv.csv")