directorio<-"C:\\Users\\areasocial\\Documents\\Mesareasocial\\Mesareasocial\\Consultas SGB\\Consultas\\Derecho\\g_genactual"
setwd(directorio)
#Primer Paso: Abrir el archivo .LST, eliminar la última línea y la primera hasta el primer número de cédula. Llenar con --- un renglon dejando solo un espacio en blanco entre variable y variable. Guardar como .txt.
#Segundo Paso: Idem al paso 1 pero eliminar tambien los --- divisorios. Guardar como .csv#
base<-read.csv("g_gen1985.csv",header=F)
base<-read.table("g_gen1985.txt", sep=";", quote="\"", na.strings="",header=F)
cell.widths<-c(nchar(strsplit(as.character(base[1,]),split = " ")[[1]][1]),nchar(strsplit(as.character(base[1,]),split = " ")[[1]][2]),nchar(strsplit(as.character(base[1,]),split = " ")[[1]][3]),nchar(strsplit(as.character(base[1,]),split = " ")[[1]][4]),nchar(strsplit(as.character(base[1,]),split = " ")[[1]][5]))
rm(base)
base<-read.fwf(file = "g_gen1985.csv",widths = cell.widths+1,header = F)
func1<-function(x) {ifelse(is.na(x)|trimws(x)=="",T,F)}
base<-base[which(apply(apply(base,1,func1),2,sum)!=ncol(base)),]
colnames(base)<-c("ESTCI","CI_DIGITO","NOMBRE","DIR","TEL")
for(i in 1:ncol(base)){
  base[,i]<-sapply(as.character(base[,i]),trimws)
}
write.csv(base,"g_gen1985_conv.csv")








#Forma loop para varias bases: ejemplo#
ind1<-seq(1985,2015,1)
for (j in 1:length(ind1)){
base<-read.csv(paste("g_gen",ind1[j],".csv",sep=""),sep=";",header=F)
base<-read.table(paste("g_gen",ind1[j],".txt",sep=""), sep=";", quote="\"", na.strings="",header=F)
cell.widths<-c(nchar(strsplit(as.character(base[1,]),split = " ")[[1]][1]),nchar(strsplit(as.character(base[1,]),split = " ")[[1]][2]),nchar(strsplit(as.character(base[1,]),split = " ")[[1]][3]),nchar(strsplit(as.character(base[1,]),split = " ")[[1]][4]),nchar(strsplit(as.character(base[1,]),split = " ")[[1]][5]))
rm(base)
base<-read.fwf(file = paste("g_gen",ind1[j],".csv",sep=""),widths = cell.widths+1,header = F,sep=";",fill=T)
func1<-function(x) {ifelse(is.na(x)|trimws(x)=="",T,F)}
base<-base[which(apply(apply(base,1,func1),2,sum)!=ncol(base)),]
colnames(base)<-c("ESTCI","CI_DIGITO","NOMBRE","DIR","TEL")
for(i in 1:ncol(base)){
  base[,i]<-sapply(as.character(base[,i]),trimws)
}
write.csv(base,as.character(paste("g_gen",ind1[j],"_conv.csv",sep="")))
}