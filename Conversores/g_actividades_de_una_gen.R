directorio<-"C:\\Users\\areasocial\\Documents\\Mesareasocial\\Mesareasocial\\Consultas SGB\\Consultas\\Derecho\\g_actividades_de_una_gen"
setwd(directorio)
#Paso 1: Abrir el .LST en notepad, eliminar todo lo que aparece antes del primer numero de cedula y la ultima fila (i.e., nro de filas seleccionadas), y guardar como archivo .csv
g_actividades_de_una_gen<-function(masterfile,file,ext){
  #masterfile: nombre del archivo a convertir (entre comillas y con la extension (p.e. "g_actividades_de_una_gen.csv"))
  #file: nombre del archivo a generar (entre comillas y sin la extension (p.e. "salida"))
  #ext: extensión del archivo a generar entre comillas y con punto (p.e. ".csv")
  #extensiones posibles para la salida: arff, csv, csv2, dbf, dcf, dta)
  require(foreign)
  require(memisc)
base <- read.csv(masterfile, header=FALSE)
base <- apply(base,2,trimws)
colnames(base)<-c("ci","carr","ciclo","mat", "tipo_act", "nota", "fecha")
while(sum(is.na(as.integer(base[-which(base[,"ci"]==""),"ci"])))>=1){
  base<-base[-which(base[,"ci"]==base[which(base[,"ci"]!=""),][which(is.na(sapply(base[which(base[,"ci"]!=""),"ci"],as.integer))),1]),]
  }
#base<-base[-which(base[,"ci"]==base[which(base[,"ci"]!=""),][which(is.na(sapply(base[which(base[,"ci"]!=""),"ci"],as.integer))),1]),]
func1<-function(x) {ifelse(is.na(x)==T | as.character(x)=="",T,F)}
base<-base[which(apply(apply(base[1:nrow(base),],1,func1),2,sum)!=ncol(base)),]
ind2<-rep(0,length(levels(as.factor(base[,"ci"]))[which(levels(as.factor(base[,"ci"]))!="")]))
vec1<-sort(as.integer(levels(as.factor(base[,"ci"]))[which(levels(as.factor(base[,"ci"]))!="")]))
for(i in 1:length(ind2)){
  ind2[i]<-which(base[,"ci"]==vec1[i])
}
vec2<-sort(as.integer(levels(as.factor(base[,"ci"]))[which(levels(as.factor(base[,"ci"]))!="")]))
nrb<-nrow(base)
lind2<-length(ind2)
lbase<-seq(1,nrow(base))
base<-cbind.data.frame(lbase,base)
for(j in 1:(lind2-1)){
  base[ind2[j]:(ind2[j+1]-1),"ci"]<-vec2[j]
  print(c(j))
}
base[(max(ind2):nrow(base)),"ci"]<-rep(vec2[length(vec2)],length((max(ind2):nrow(base))))
base<-base[which(apply(apply(base[1:nrow(base),-2],1,func1),2,sum)!=ncol(base)-2),]
fechas<-base[,8]
base<-cbind.data.frame(factor(base[,2]),factor(base[,3]),factor(base[,4]),factor(base[,5]),factor(base[,6]),factor(base[,7]))
base<-cbind.data.frame(base,fechas)
colnames(base)<-c("ci","carr","ciclo","mat", "tipo_act", "nota", "fecha")
if (ext==".dta"){
write.dta(base,file = paste(file,ext,sep=""),convert.factors = "string")
}
if (ext==".csv"){
write.csv(base,file=paste(file,ext,sep=""))
}
}
masterfile<-"g_actividades_de_una_gen_2012.csv"
file<-"g_actividades_de_una_gen_2012_conv"
ext<-".csv"
g_actividades_de_una_gen(masterfile,file,ext)
