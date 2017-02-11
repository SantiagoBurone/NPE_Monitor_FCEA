directorio<-"C:\\Users\\areasocial\\Documents\\Mesareasocial\\Mesareasocial\\Consultas SGB\\Consultas\\CCEE\\g_inscriptos"
setwd(directorio)
library(foreign)
library(memisc)
#La consulta devuelve dos salidas en un mismo archivo.#
#Primer Paso: Abrir el archivo .LST, eliminar la segúnda consulta, la última línea de la primera (que dice el numero de filas seleccionadas), y las primeras hasta la primer fila de fecha exclusive. Guardar como (nombre del archivo)_consulta1.txt.
base<-read.table("g_inscriptos_19570101_20150101_consulta1.txt", sep=";", quote="\"")
p.head1<-rep(0,nrow(base))
for(i in 1:length(p.head1)){
  p.head1[i]<-grepl("página",strsplit(as.character(base[i,])," "))
}
base<-base[-which(p.head1==T),]
p.head2<-rep(0,length(base))
for(i in 1:length(p.head2)){
  p.head2[i]<-grepl("INSCRIPTOS",strsplit(as.character(base[i])," "))
}
base<-base[-which(p.head2==T)]
cell.widths<-c(nchar(strsplit(as.character(base[2]),split = " ")[[1]][1]),nchar(strsplit(as.character(base[2]),split = " ")[[1]][2]),nchar(strsplit(as.character(base[2]),split = " ")[[1]][3]),nchar(strsplit(as.character(base[2]),split = " ")[[1]][4]),nchar(strsplit(as.character(base[2]),split = " ")[[1]][5]),nchar(strsplit(as.character(base[2]),split = " ")[[1]][6]))
p.head3<-rep(0,length(base))
for(i in 1:length(p.head3)){
  p.head3[i]<-grepl("ESTCI",strsplit(as.character(base[i])," "))
}
base<-base[-which(p.head3==T)]
p.head4<-rep(0,length(base))
for(i in 1:length(p.head4)){
  p.head4[i]<-grepl("--",strsplit(as.character(base[i])," "))
}
base<-base[-which(p.head4==T)]
write.csv(base,"base.csv",row.names=F)
rm(base)
base<-read.fwf(file = "base.csv",widths = cell.widths+1,header = F)
base<-base[-1,]
base[,1]<-as.character(base[,1])
for(i in 1:nrow(base)){
  base[i,1]<-strsplit(base[i,1],"\"")[[1]][2]
}
func1<-function(x) {ifelse(is.na(x)|trimws(x)=="",T,F)}
base<-base[which(apply(apply(base,1,func1),2,sum)!=ncol(base)),]
colnames(base)<-c("ESTCI","NOMBRE","CARR","CICLO","FECHA","OBS")
for(i in 1:ncol(base)){
  base[,i]<-sapply(as.character(base[,i]),trimws)
}
write.csv(base,"g_inscriptos_19570101_20150101_consulta1_conv.csv")

