rm(list=ls())
gc()
directorio<-"C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Actividades actualizadas\\Nuevas para actualizar"
setwd(directorio)
load("cohortes2.RData")
library(plyr)

#Ejemplo: Estudiante economista (puro o parcial) de gen2012#
#Cargo la base de actividades de la gen2012#
gen2012<-listadobases[[1]]
gen2012<-gen2012[which(gen2012$ecopuro12==1 | gen2012$ecocont12==1 | gen2012$ecoadm12==1 | gen2012$todas12==1 | gen2012$contpuro12==1 | gen2012$admpuro12==1 | gen2012$contadm12==1),]

#Fecha de fin semestre (sin incluir revalidas sin nota):
#Fecha del fin del 1er semestre: (20120804)
sort(levels(as.factor(gen2012$fecha[which(gen2012$fecha<20130101 & gen2012$nota!=20)])))
#Fecha del fin del 2do semestre (incluyendo verano 2013): (20130309)
sort(levels(as.factor(gen2012$fecha[which(gen2012$fecha<20130601 & gen2012$fecha>20120804 & gen2012$nota!=20)])))
#Fecha del fin del 3er semestre: (20130803)
sort(levels(as.factor(gen2012$fecha[which(gen2012$fecha<20131001 & gen2012$fecha>20130309 & gen2012$nota!=20)])))
#Fecha del fin del 4to semestre (incluyendo verano 2014): (20140620)
sort(levels(as.factor(gen2012$fecha[which(gen2012$fecha<20140701 & gen2012$fecha>20130803 & gen2012$nota!=20)])))
#Fecha del fin del 5to semestre: (20140804)
sort(levels(as.factor(gen2012$fecha[which(gen2012$fecha<20140931 & gen2012$fecha>20140620 & gen2012$nota!=20)])))
#Fecha del fin del 6to semestre (incluyendo verano 2015): (20150523)
sort(levels(as.factor(gen2012$fecha[which(gen2012$fecha<20150701 & gen2012$fecha>20140804 & gen2012$nota!=20)])))
#Fecha del fin del 7mo semestre: (20150808)
sort(levels(as.factor(gen2012$fecha[which(gen2012$fecha<20151001 & gen2012$fecha>20150523 & gen2012$nota!=20)])))
#Fecha del fin del 8vo semestre (incluyendo verano 2016): (20160528)
sort(levels(as.factor(gen2012$fecha[which(gen2012$fecha<20160806 & gen2012$fecha>20150808 & gen2012$nota!=20)])))
#Fecha del fin del 9no semestre: (20160806)

#Identificador de semestre:
gen2012$sem_gen2012<-ifelse(gen2012$fecha<=20120804,1,
						ifelse(gen2012$fecha<=20130309 & gen2012$fecha>20120804,2,
							ifelse(gen2012$fecha<=20130803 & gen2012$fecha>20130309,3,
								ifelse(gen2012$fecha<20140620 & gen2012$fecha>20130803,4,
									ifelse(gen2012$fecha<=20140804 & gen2012$fecha>20140620,5,
										ifelse(gen2012$fecha<=20150523 & gen2012$fecha>20140804,6,
											ifelse(gen2012$fecha<=20150808 & gen2012$fecha>20150523,7,
												ifelse(gen2012$fecha<=20160528 & gen2012$fecha>20150808 ,8,
													ifelse(gen2012$fecha<=20160806 & gen2012$fecha>20160528 ,9,10)))))))))
										
#Hay que no tener en cuenta las actividades en posgrados
#Elimino las actividades en posgrados (restringir la base a actividades relacionadas a alguna de las 4 licenciaturas o la TUA)
#table(gen2012$carr)#
asig<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\CCEE\\g_asig\\g_asig_conv.csv")
ind1<-levels(as.factor(asig$MAT))
restr<-rep(0,length(ind1))
for(i in 1:length(ind1)){
restr[i]<-ifelse(11%in%asig$CARR[which(asig$MAT==ind1[i])] | 12%in%asig$CARR[which(asig$MAT==ind1[i])] | 14%in%asig$CARR[which(asig$MAT==ind1[i])] | 2%in%asig$CARR[which(asig$MAT==ind1[i])] | 3%in%asig$CARR[which(asig$MAT==ind1[i])],1,0)
}
for(i in 1:nrow(asig)){
asig$r[i]<-ifelse(restr[which(asig$MAT[i]==ind1)]==1,1,0)
}
colnames(asig)[2]<-"mat"
gen2012<-join_all(list(gen2012,asig),by=c("mat"))
gen2012<-gen2012[which(gen2012$r==1),c("ci","NOMBRE","carr","ciclo","mat","tipo_act","nota","fecha","sem_gen2012","anio_act","ecopuro12","contpuro12","admpuro12","ecocont12","ecoadm12","contadm12","todas12","ident1")]

#Identificador de cursos exonerados, no exonerados, y examenes dados
aux1<-function(x){
out<-ifelse("C"%in%x$tipo_act==F & "E"%in%x$tipo_act==T & x$nota>=3,"examen aprobado",
																										ifelse("C"%in%x$tipo_act==F & "E"%in%x$tipo_act==T & x$nota<3,"examen perdido",
																											ifelse("C"%in%x$tipo_act==T & "E"%in%x$tipo_act==T & x$nota!=0,"curso exonerado",
																												ifelse("C"%in%x$tipo_act==T & "E"%in%x$tipo_act==F & x$nota==0,"curso no exonerado",0)
)
)
)
}
out1<-dlply(.data=gen2012,.variables=c("ci","mat","fecha"),.fun=aux1,.progress="text",.inform=T)
dout1<-as.data.frame(unlist(out1))
dout1$nombre<-row.names(dout1)
daux1<-as.data.frame(do.call(rbind,strsplit(as.character(dout1$nombre),".",fixed=T)))
daux1[,3]<-as.integer(substring(as.character(daux1[,3]), 1, 8))
daux1[,4]<-as.character(dout1[,1])
colnames(daux1)<-c("ci","mat","fecha","result_act")
gen2012$ci<-as.character(gen2012$ci)
gen2012$mat<-as.character(gen2012$mat)
gen2012$fecha<-as.character(gen2012$fecha)
daux1$ci<-as.character(daux1$ci)
daux1$mat<-as.character(daux1$mat)
daux1$fecha<-as.character(daux1$fecha)
daux1$ident2<-duplicated(daux1,by=c("ci","mat","fecha"))
daux1<-daux1[which(daux1$ident2==F),-ncol(daux1)]
gen2012<-join(gen2012,daux1)

#Identificar las revalidas con nota 20
gen2012$result_act[which(gen2012$nota==20)]<-rep("Revalida sin nota",length(gen2012$result_act[which(gen2012$nota==20)]))
#Identificar los trabajos finales de grado plan 2012, que llevan solo C
gen2012$result_act[which(gen2012$mat=="ITF12")]<-rep("Trabajo Final",length(gen2012$result_act[which(gen2012$mat=="ITF12")]))
#Identificar cursos con examen obligatorio, donde la nota del C es la de aprobacion 3, y la nota de aprobacion es la del examen.

#Todos los que tienen result_act=0 son cursos no exonerados tambien:
#table(gen2012$nota[which(duplicated(gen2012[,c("ci","mat","fecha")])==F & gen2012$result_act==0)],gen2012$tipo_act[which(duplicated(gen2012[,c("ci","mat","fecha")])==F & gen2012$result_act==0)])
gen2012$result_act[which(gen2012$result_act==0)]<-rep("curso no exonerado",length(gen2012$result_act[which(gen2012$result_act==0)]))
#Identificar todas las UPC
gen2012$result_act[which(gen2012$mat=="UPC" | gen2012$mat=="UPC10" | gen2012$mat=="UPC15" | gen2012$mat=="UPC20")]<-rep("Practica Curricular",length(gen2012$result_act[which(gen2012$mat=="UPC" | gen2012$mat=="UPC10" | gen2012$mat=="UPC15" | gen2012$mat=="UPC20")]))
#View(gen2012[which(duplicated(gen2012[,c("ci","mat","fecha")])==F),])
gen2012$n_estud<-as.factor(gen2012$ci)
levels(gen2012$n_estud)<-seq(1:length(levels(gen2012$n_estud)))
write.csv(gen2012[which(duplicated(gen2012[,c("ci","mat","fecha","result_act")])==F),],"gen2012_sindup.csv")

##########################################
rm(list=ls())
gc()
directorio<-"C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Actividades actualizadas\\Nuevas para actualizar"
setwd(directorio)
load("cohortes2.RData")
library(plyr)

#Ejemplo: Estudiante economista (puro o parcial) de gen2013#
#Cargo la base de actividades de la gen2013#
gen2013<-listadobases[[2]]
gen2013<-gen2013[which(gen2013$ecopuro13==1 | gen2013$ecocont13==1 | gen2013$ecoadm13==1 | gen2013$todas13==1 | gen2013$contpuro13==1 | gen2013$admpuro13==1 | gen2013$contadm13==1),]

#Fecha de fin semestre (sin incluir revalidas sin nota):
#Util: gen2013[which(gen2013$fecha==20160305),]
#Fecha del fin del 1er semestre: (20130803)
sort(levels(as.factor(gen2013$fecha[which(gen2013$fecha<20130803 & gen2013$nota!=20)])))
#Fecha del fin del 2do semestre (incluyendo verano 2014): (20140308)
sort(levels(as.factor(gen2013$fecha[which(gen2013$fecha<20140308 & gen2013$fecha>20130803 & gen2013$nota!=20)])))
#Fecha del fin del 3er semestre: (20140804)
sort(levels(as.factor(gen2013$fecha[which(gen2013$fecha<20141001 & gen2013$fecha>20140308 & gen2013$nota!=20)])))
#Fecha del fin del 4to semestre (incluyendo verano 2015): (20150424)
sort(levels(as.factor(gen2013$fecha[which(gen2013$fecha<20150424 & gen2013$fecha>20140804 & gen2013$nota!=20)])))
#Fecha del fin del 5to semestre: (20150808)
sort(levels(as.factor(gen2013$fecha[which(gen2013$fecha<20151031 & gen2013$fecha>20150424 & gen2013$nota!=20)])))
#Fecha del fin del 6to semestre (incluyendo verano 2016): (20160305)
sort(levels(as.factor(gen2013$fecha[which(gen2013$fecha<20160305 & gen2013$fecha>20150808 & gen2013$nota!=20)])))
#Fecha del fin del 7mo semestre: (20160806)


#Identificador de semestre:
gen2013$sem_gen2013<-ifelse(gen2013$fecha<=20130803,1,
						ifelse(gen2013$fecha<=20140308 & gen2013$fecha>20130803,2,
							ifelse(gen2013$fecha<=20140804 & gen2013$fecha>20130308,3,
								ifelse(gen2013$fecha<=20150424 & gen2013$fecha>20140804,4,
									ifelse(gen2013$fecha<=20150808 & gen2013$fecha>20150424,5,
										ifelse(gen2013$fecha<=20160305 & gen2013$fecha>20150808,6,
											ifelse(gen2013$fecha<=20160806 & gen2013$fecha>20160305,7,8)))))))

										
#Hay que no tener en cuenta las actividades en posgrados
#Elimino las actividades en posgrados (restringir la base a actividades relacionadas a alguna de las 4 licenciaturas o la TUA)
#table(gen2013$carr)#
asig<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\CCEE\\g_asig\\g_asig_conv.csv")
ind1<-levels(as.factor(asig$MAT))
restr<-rep(0,length(ind1))
for(i in 1:length(ind1)){
restr[i]<-ifelse(11%in%asig$CARR[which(asig$MAT==ind1[i])] | 12%in%asig$CARR[which(asig$MAT==ind1[i])] | 14%in%asig$CARR[which(asig$MAT==ind1[i])] | 2%in%asig$CARR[which(asig$MAT==ind1[i])] | 3%in%asig$CARR[which(asig$MAT==ind1[i])],1,0)
}
for(i in 1:nrow(asig)){
asig$r[i]<-ifelse(restr[which(asig$MAT[i]==ind1)]==1,1,0)
}
colnames(asig)[2]<-"mat"
gen2013<-join_all(list(gen2013,asig),by=c("mat"))
gen2013<-gen2013[which(gen2013$r==1),c("ci","NOMBRE","carr","ciclo","mat","tipo_act","nota","fecha","sem_gen2013","anio_act","ecopuro13","contpuro13","admpuro13","ecocont13","ecoadm13","contadm13","todas13","ident1")]

#Identificador de cursos exonerados, no exonerados, y examenes dados
aux1<-function(x){
out<-ifelse("C"%in%x$tipo_act==F & "E"%in%x$tipo_act==T & x$nota>=3,"examen aprobado",
																										ifelse("C"%in%x$tipo_act==F & "E"%in%x$tipo_act==T & x$nota<3,"examen perdido",
																											ifelse("C"%in%x$tipo_act==T & "E"%in%x$tipo_act==T & x$nota!=0,"curso exonerado",
																												ifelse("C"%in%x$tipo_act==T & "E"%in%x$tipo_act==F & x$nota==0,"curso no exonerado",0)
)
)
)
}
out1<-dlply(.data=gen2013,.variables=c("ci","mat","fecha"),.fun=aux1,.progress="text",.inform=T)
dout1<-as.data.frame(unlist(out1))
dout1$nombre<-row.names(dout1)
daux1<-as.data.frame(do.call(rbind,strsplit(as.character(dout1$nombre),".",fixed=T)))
daux1[,3]<-as.integer(substring(as.character(daux1[,3]), 1, 8))
daux1[,4]<-as.character(dout1[,1])
colnames(daux1)<-c("ci","mat","fecha","result_act")
gen2013$ci<-as.character(gen2013$ci)
gen2013$mat<-as.character(gen2013$mat)
gen2013$fecha<-as.character(gen2013$fecha)
daux1$ci<-as.character(daux1$ci)
daux1$mat<-as.character(daux1$mat)
daux1$fecha<-as.character(daux1$fecha)
daux1$ident2<-duplicated(daux1,by=c("ci","mat","fecha"))
daux1<-daux1[which(daux1$ident2==F),-ncol(daux1)]
gen2013<-join(gen2013,daux1)

#Identificar las revalidas con nota 20
gen2013$result_act[which(gen2013$nota==20)]<-rep("Revalida sin nota",length(gen2013$result_act[which(gen2013$nota==20)]))
#Identificar los trabajos finales de grado plan 2013, que llevan solo C
gen2013$result_act[which(gen2013$mat=="ITF12")]<-rep("Trabajo Final",length(gen2013$result_act[which(gen2013$mat=="ITF12")]))
#Identificar cursos con examen obligatorio, donde la nota del C es la de aprobacion 3, y la nota de aprobacion es la del examen.

#Todos los que tienen result_act=0 son cursos no exonerados tambien:
#table(gen2013$nota[which(duplicated(gen2013[,c("ci","mat","fecha")])==F & gen2013$result_act==0)],gen2013$tipo_act[which(duplicated(gen2013[,c("ci","mat","fecha")])==F & gen2013$result_act==0)])
gen2013$result_act[which(gen2013$result_act==0)]<-rep("curso no exonerado",length(gen2013$result_act[which(gen2013$result_act==0)]))
#Identificar todas las UPC
gen2013$result_act[which(gen2013$mat=="UPC" | gen2013$mat=="UPC10" | gen2013$mat=="UPC15" | gen2013$mat=="UPC20")]<-rep("Practica Curricular",length(gen2013$result_act[which(gen2013$mat=="UPC" | gen2013$mat=="UPC10" | gen2013$mat=="UPC15" | gen2013$mat=="UPC20")]))
#View(gen2013[which(duplicated(gen2013[,c("ci","mat","fecha")])==F),])
gen2013$n_estud<-as.factor(gen2013$ci)
levels(gen2013$n_estud)<-seq(1:length(levels(gen2013$n_estud)))
write.csv(gen2013[which(duplicated(gen2013[,c("ci","mat","fecha","result_act")])==F),],"gen2013_sindup.csv")
##########################################
rm(list=ls())
gc()
directorio<-"C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Actividades actualizadas\\Nuevas para actualizar"
setwd(directorio)
load("cohortes2.RData")
library(plyr)

#Ejemplo: Estudiante economista (puro o parcial) de gen2014#
#Cargo la base de actividades de la gen2014#
gen2014<-listadobases[[3]]
gen2014<-gen2014[which(gen2014$ecopuro14==1 | gen2014$ecocont14==1 | gen2014$ecoadm14==1 | gen2014$todas14==1 | gen2014$contpuro14==1 | gen2014$admpuro14==1 | gen2014$contadm14==1),]

#Fecha de fin semestre (sin incluir revalidas sin nota):
#Util: gen2014[which(gen2014$fecha==20160520),]
#Fecha del fin del 1er semestre: (20140802)
sort(levels(as.factor(gen2014$fecha[which(gen2014$fecha<20141003 & gen2014$nota!=20)])))
#Fecha del fin del 2do semestre (incluyendo verano 2015): (20150307)
sort(levels(as.factor(gen2014$fecha[which(gen2014$fecha<20150608 & gen2014$fecha>20140802 & gen2014$nota!=20)])))
#Fecha del fin del 3er semestre: (20150808)
sort(levels(as.factor(gen2014$fecha[which(gen2014$fecha<20151001 & gen2014$fecha>20150307 & gen2014$nota!=20)])))
#Fecha del fin del 4to semestre (incluyendo verano 2016): (20160305)
sort(levels(as.factor(gen2014$fecha[which(gen2014$fecha<20160524 & gen2014$fecha>20150808 & gen2014$nota!=20)])))
#Fecha del fin del 5to semestre: (20160806)

#Identificador de semestre:
gen2014$sem_gen2014<-ifelse(gen2014$fecha<=20140802,1,
						ifelse(gen2014$fecha<=20150307 & gen2014$fecha>20140802,2,
							ifelse(gen2014$fecha<=20150808 & gen2014$fecha>20150307,3,
								ifelse(gen2014$fecha<=20160305 & gen2014$fecha>20150808,4,
									ifelse(gen2014$fecha<=20160806 & gen2014$fecha>20160305,5,6)))))
#Hay que no tener en cuenta las actividades en posgrados
#Elimino las actividades en posgrados (restringir la base a actividades relacionadas a alguna de las 4 licenciaturas o la TUA)
#table(gen2014$carr)#
asig<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\CCEE\\g_asig\\g_asig_conv.csv")
ind1<-levels(as.factor(asig$MAT))
restr<-rep(0,length(ind1))
for(i in 1:length(ind1)){
restr[i]<-ifelse(11%in%asig$CARR[which(asig$MAT==ind1[i])] | 12%in%asig$CARR[which(asig$MAT==ind1[i])] | 14%in%asig$CARR[which(asig$MAT==ind1[i])] | 2%in%asig$CARR[which(asig$MAT==ind1[i])] | 3%in%asig$CARR[which(asig$MAT==ind1[i])],1,0)
}
for(i in 1:nrow(asig)){
asig$r[i]<-ifelse(restr[which(asig$MAT[i]==ind1)]==1,1,0)
}
colnames(asig)[2]<-"mat"
gen2014<-join_all(list(gen2014,asig),by=c("mat"))
gen2014<-gen2014[which(gen2014$r==1),c("ci","NOMBRE","carr","ciclo","mat","tipo_act","nota","fecha","sem_gen2014","anio_act","ecopuro14","contpuro14","admpuro14","ecocont14","ecoadm14","contadm14","todas14","ident1")]

#Identificador de cursos exonerados, no exonerados, y examenes dados
aux1<-function(x){
out<-ifelse("C"%in%x$tipo_act==F & "E"%in%x$tipo_act==T & x$nota>=3,"examen aprobado",
																										ifelse("C"%in%x$tipo_act==F & "E"%in%x$tipo_act==T & x$nota<3,"examen perdido",
																											ifelse("C"%in%x$tipo_act==T & "E"%in%x$tipo_act==T & x$nota!=0,"curso exonerado",
																												ifelse("C"%in%x$tipo_act==T & "E"%in%x$tipo_act==F & x$nota==0,"curso no exonerado",0)
)
)
)
}
out1<-dlply(.data=gen2014,.variables=c("ci","mat","fecha"),.fun=aux1,.progress="text",.inform=T)
dout1<-as.data.frame(unlist(out1))
dout1$nombre<-row.names(dout1)
daux1<-as.data.frame(do.call(rbind,strsplit(as.character(dout1$nombre),".",fixed=T)))
daux1[,3]<-as.integer(substring(as.character(daux1[,3]), 1, 8))
daux1[,4]<-as.character(dout1[,1])
colnames(daux1)<-c("ci","mat","fecha","result_act")
gen2014$ci<-as.character(gen2014$ci)
gen2014$mat<-as.character(gen2014$mat)
gen2014$fecha<-as.character(gen2014$fecha)
daux1$ci<-as.character(daux1$ci)
daux1$mat<-as.character(daux1$mat)
daux1$fecha<-as.character(daux1$fecha)
daux1$ident2<-duplicated(daux1,by=c("ci","mat","fecha"))
daux1<-daux1[which(daux1$ident2==F),-ncol(daux1)]
gen2014<-join(gen2014,daux1)

#Identificar las revalidas con nota 20
gen2014$result_act[which(gen2014$nota==20)]<-rep("Revalida sin nota",length(gen2014$result_act[which(gen2014$nota==20)]))
#Identificar los trabajos finales de grado plan 2014, que llevan solo C
gen2014$result_act[which(gen2014$mat=="ITF12")]<-rep("Trabajo Final",length(gen2014$result_act[which(gen2014$mat=="ITF12")]))
#Identificar cursos con examen obligatorio, donde la nota del C es la de aprobacion 3, y la nota de aprobacion es la del examen.

#Todos los que tienen result_act=0 son cursos no exonerados tambien:
#table(gen2014$nota[which(duplicated(gen2014[,c("ci","mat","fecha")])==F & gen2014$result_act==0)],gen2014$tipo_act[which(duplicated(gen2014[,c("ci","mat","fecha")])==F & gen2014$result_act==0)])
gen2014$result_act[which(gen2014$result_act==0)]<-rep("curso no exonerado",length(gen2014$result_act[which(gen2014$result_act==0)]))
#Identificar todas las UPC
gen2014$result_act[which(gen2014$mat=="UPC" | gen2014$mat=="UPC10" | gen2014$mat=="UPC15" | gen2014$mat=="UPC20")]<-rep("Practica Curricular",length(gen2014$result_act[which(gen2014$mat=="UPC" | gen2014$mat=="UPC10" | gen2014$mat=="UPC15" | gen2014$mat=="UPC20")]))
#View(gen2014[which(duplicated(gen2014[,c("ci","mat","fecha")])==F),])
gen2014$n_estud<-as.factor(gen2014$ci)
levels(gen2014$n_estud)<-seq(1:length(levels(gen2014$n_estud)))
write.csv(gen2014[which(duplicated(gen2014[,c("ci","mat","fecha","result_act")])==F),],"gen2014_sindup.csv")
##########################################
rm(list=ls())
gc()
directorio<-"C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Actividades actualizadas\\Nuevas para actualizar"
setwd(directorio)
load("cohortes2.RData")
library(plyr)

#Ejemplo: Estudiante economista (puro o parcial) de gen2015#
#Cargo la base de actividades de la gen2015#
gen2015<-listadobases[[4]]
gen2015<-gen2015[which(gen2015$ecopuro15==1 | gen2015$ecocont15==1 | gen2015$ecoadm15==1 | gen2015$todas15==1 | gen2015$contpuro15==1 | gen2015$admpuro15==1 | gen2015$contadm15==1),]

#Fecha de fin semestre (sin incluir revalidas sin nota):
#Util: gen2015[which(gen2015$fecha==20150817),]
#Fecha del fin del 1er semestre: (20150817)
sort(levels(as.factor(gen2015$fecha[which(gen2015$fecha<20150817 & gen2015$nota!=20)])))
#Fecha del fin del 2do semestre (incluyendo verano 2016): (20160305)
sort(levels(as.factor(gen2015$fecha[which(gen2015$fecha<20150608 & gen2015$fecha>20150817 & gen2015$nota!=20)])))
#Fecha del fin del 3er semestre: (20160806)


table(gen2015$fecha)
gen2015[which(gen2015$fecha==20160921),]
#Identificador de semestre:
gen2015$sem_gen2015<-ifelse(gen2015$fecha<=20150817,1,
						ifelse(gen2015$fecha<=20160305 & gen2015$fecha>20150817,2,
							ifelse(gen2015$fecha<=20160806 & gen2015$fecha>20160305 ,3,4)))

										
#Hay que no tener en cuenta las actividades en posgrados
#Elimino las actividades en posgrados (restringir la base a actividades relacionadas a alguna de las 4 licenciaturas o la TUA)
#table(gen2015$carr)#
asig<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\CCEE\\g_asig\\g_asig_conv.csv")
ind1<-levels(as.factor(asig$MAT))
restr<-rep(0,length(ind1))
for(i in 1:length(ind1)){
restr[i]<-ifelse(11%in%asig$CARR[which(asig$MAT==ind1[i])] | 12%in%asig$CARR[which(asig$MAT==ind1[i])] | 14%in%asig$CARR[which(asig$MAT==ind1[i])] | 2%in%asig$CARR[which(asig$MAT==ind1[i])] | 3%in%asig$CARR[which(asig$MAT==ind1[i])],1,0)
}
for(i in 1:nrow(asig)){
asig$r[i]<-ifelse(restr[which(asig$MAT[i]==ind1)]==1,1,0)
}
colnames(asig)[2]<-"mat"
gen2015<-join_all(list(gen2015,asig),by=c("mat"))
gen2015<-gen2015[which(gen2015$r==1),c("ci","NOMBRE","carr","ciclo","mat","tipo_act","nota","fecha","sem_gen2015","anio_act","ecopuro15","contpuro15","admpuro15","ecocont15","ecoadm15","contadm15","todas15","ident1")]

#Identificador de cursos exonerados, no exonerados, y examenes dados
aux1<-function(x){
out<-ifelse("C"%in%x$tipo_act==F & "E"%in%x$tipo_act==T & x$nota>=3,"examen aprobado",
																										ifelse("C"%in%x$tipo_act==F & "E"%in%x$tipo_act==T & x$nota<3,"examen perdido",
																											ifelse("C"%in%x$tipo_act==T & "E"%in%x$tipo_act==T & x$nota!=0,"curso exonerado",
																												ifelse("C"%in%x$tipo_act==T & "E"%in%x$tipo_act==F & x$nota==0,"curso no exonerado",0)
)
)
)
}
out1<-dlply(.data=gen2015,.variables=c("ci","mat","fecha"),.fun=aux1,.progress="text",.inform=T)
dout1<-as.data.frame(unlist(out1))
dout1$nombre<-row.names(dout1)
daux1<-as.data.frame(do.call(rbind,strsplit(as.character(dout1$nombre),".",fixed=T)))
daux1[,3]<-as.integer(substring(as.character(daux1[,3]), 1, 8))
daux1[,4]<-as.character(dout1[,1])
colnames(daux1)<-c("ci","mat","fecha","result_act")
gen2015$ci<-as.character(gen2015$ci)
gen2015$mat<-as.character(gen2015$mat)
gen2015$fecha<-as.character(gen2015$fecha)
daux1$ci<-as.character(daux1$ci)
daux1$mat<-as.character(daux1$mat)
daux1$fecha<-as.character(daux1$fecha)
daux1$ident2<-duplicated(daux1,by=c("ci","mat","fecha"))
daux1<-daux1[which(daux1$ident2==F),-ncol(daux1)]
gen2015<-join(gen2015,daux1)

#Identificar las revalidas con nota 20
gen2015$result_act[which(gen2015$nota==20)]<-rep("Revalida sin nota",length(gen2015$result_act[which(gen2015$nota==20)]))
#Identificar los trabajos finales de grado plan 2015, que llevan solo C
gen2015$result_act[which(gen2015$mat=="ITF12")]<-rep("Trabajo Final",length(gen2015$result_act[which(gen2015$mat=="ITF12")]))
#Identificar cursos con examen obligatorio, donde la nota del C es la de aprobacion 3, y la nota de aprobacion es la del examen.

#Todos los que tienen result_act=0 son cursos no exonerados tambien:
#table(gen2015$nota[which(duplicated(gen2015[,c("ci","mat","fecha")])==F & gen2015$result_act==0)],gen2015$tipo_act[which(duplicated(gen2015[,c("ci","mat","fecha")])==F & gen2015$result_act==0)])
gen2015$result_act[which(gen2015$result_act==0)]<-rep("curso no exonerado",length(gen2015$result_act[which(gen2015$result_act==0)]))
#Identificar todas las UPC
gen2015$result_act[which(gen2015$mat=="UPC" | gen2015$mat=="UPC10" | gen2015$mat=="UPC15" | gen2015$mat=="UPC20")]<-rep("Practica Curricular",length(gen2015$result_act[which(gen2015$mat=="UPC" | gen2015$mat=="UPC10" | gen2015$mat=="UPC15" | gen2015$mat=="UPC20")]))
#View(gen2015[which(duplicated(gen2015[,c("ci","mat","fecha")])==F),])
gen2015$n_estud<-as.factor(gen2015$ci)
levels(gen2015$n_estud)<-seq(1:length(levels(gen2015$n_estud)))
write.csv(gen2015[which(duplicated(gen2015[,c("ci","mat","fecha","result_act")])==F),],"gen2015_sindup.csv")






