rm(list=ls())
gc()
directorio<-"C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Actividades actualizadas\\Nuevas para actualizar"
setwd(directorio)
library(plyr)
gen2012<-read.csv("gen2012_sindup.csv")
gen2013<-read.csv("gen2013_sindup.csv")
gen2014<-read.csv("gen2014_sindup.csv")
gen2015<-read.csv("gen2015_sindup.csv")
######################################Retirar info de ci y nombre
#gen2012<-gen2012[,which(colnames(gen2012)!="ci" & colnames(gen2012)!="NOMBRE")][,-1]
#gen2013<-gen2013[,which(colnames(gen2013)!="ci" & colnames(gen2013)!="NOMBRE")][,-1]
#gen2014<-gen2014[,which(colnames(gen2014)!="ci" & colnames(gen2014)!="NOMBRE")][,-1]
#gen2015<-gen2015[,which(colnames(gen2015)!="ci" & colnames(gen2015)!="NOMBRE")][,-1]
#Genero variable de generacion para el append
gen2012$gen<-rep(2012,nrow(gen2012))
gen2013$gen<-rep(2013,nrow(gen2013))
gen2014$gen<-rep(2014,nrow(gen2014))
gen2015$gen<-rep(2015,nrow(gen2015))
##### ACA FALTA:
###Identificar periodos por separados###
######Periodos Generacion 2012####
#Buscar a ojo a partir de gen2012[which(duplicated(gen2012[,c("mat","fecha")])==F & gen2012$result_act!="Revalida sin nota" & gen2012$fecha>20131221 & gen2012$fecha<20140620),c("mat","fecha","result_act")][order(gen2012[which(duplicated(gen2012[,c("mat","fecha")])==F & gen2012$result_act!="Revalida sin nota" & gen2012$fecha>20131221 & gen2012$fecha<20140620),c("mat","fecha")]),]
#O se pueden usar las fechas de calendarios de examenes de bedelia por otro lado (lo que se hizo aca)

gen2012$per_gen2012<-ifelse(gen2012$fecha<=20120804,"Julio",
						ifelse(gen2012$fecha<=20121222 & gen2012$fecha>20120803,"Diciembre",
							ifelse(gen2012$fecha<20130222 & gen2012$fecha>20121222,"Febrero",
								ifelse(gen2012$fecha<=20130309 & gen2012$fecha>20130221,"Marzo",
									ifelse(gen2012$fecha<=20130803 & gen2012$fecha>20130309,"Julio",
										ifelse(gen2012$fecha<=20131221 & gen2012$fecha>20130803,"Diciembre",
											ifelse(gen2012$fecha<=20140213 & gen2012$fecha>20131221,"Febrero",
												ifelse(gen2012$fecha<=20140308 & gen2012$fecha>20140213,"Marzo",
													ifelse(gen2012$fecha<=20140804 & gen2012$fecha>20140308,"Julio",
														ifelse(gen2012$fecha<=20141222 & gen2012$fecha>20140804,"Diciembre",
															ifelse(gen2012$fecha<=20150214 & gen2012$fecha>20141222,"Febrero",
																ifelse(gen2012$fecha<=20150307 & gen2012$fecha>20150214,"Marzo",
																	ifelse(gen2012$fecha<=20150808 & gen2012$fecha>20150307,"Julio",
																		ifelse(gen2012$fecha<=20151223 & gen2012$fecha>20150808,"Diciembre",
																			ifelse(gen2012$fecha<=20160213 & gen2012$fecha>20151223,"Febrero",
																				ifelse(gen2012$fecha<=20160305 & gen2012$fecha>20160213,"Marzo",
																					ifelse(gen2012$fecha<=20160806 & gen2012$fecha>20160305, "Julio",
																						ifelse(gen2012$fecha<=20161222 & gen2012$fecha>20160806,"Diciembre","NC"))))))))))))))))))
#Casos especiales (mesas especiales, reconocimiento de creditos, revalidas, todo actividades no dentro de un periodo de examenes y revisiones):
#table(gen2012$sem_gen2012,gen2012$per_gen2012)
gen2012[which(gen2012$sem_gen2012==4 & gen2012$per_gen2012=="Julio" & gen2012$nota!=20),"per_gen2012"]<-rep("Especial",nrow(gen2012[which(gen2012$sem_gen2012==4 & gen2012$per_gen2012=="Julio" & gen2012$nota!=20),]))
gen2012[which(gen2012$sem_gen2012==6 & gen2012$per_gen2012=="Julio" & gen2012$nota!=20),"per_gen2012"]<-rep("Especial",nrow(gen2012[which(gen2012$sem_gen2012==6 & gen2012$per_gen2012=="Julio" & gen2012$nota!=20),]))																		
gen2012[which(gen2012$result_act=="Revalida sin nota" | gen2012$result_act=="Trabajo Final" | gen2012$result_act=="Practica Curricular"),"per_gen2012"]<-rep("Especial",nrow(gen2012[which(gen2012$result_act=="Revalida sin nota" | gen2012$result_act=="Trabajo Final" | gen2012$result_act=="Practica Curricular"),]))
######Periodos Generacion 2013####
#Usando las mismas fechas que para 2012
gen2013$per_gen2013<-ifelse(gen2013$fecha<=20130803 & gen2013$fecha>20130309,"Julio",
										ifelse(gen2013$fecha<=20131221 & gen2013$fecha>20130803,"Diciembre",
											ifelse(gen2013$fecha<=20140213 & gen2013$fecha>20131221,"Febrero",
												ifelse(gen2013$fecha<=20140308 & gen2013$fecha>20140213,"Marzo",
													ifelse(gen2013$fecha<=20140804 & gen2013$fecha>20140308,"Julio",
														ifelse(gen2013$fecha<=20141222 & gen2013$fecha>20140804,"Diciembre",
															ifelse(gen2013$fecha<=20150214 & gen2013$fecha>20141222,"Febrero",
																ifelse(gen2013$fecha<=20150307 & gen2013$fecha>20150214,"Marzo",
																	ifelse(gen2013$fecha<=20150808 & gen2013$fecha>20150307,"Julio",
																		ifelse(gen2013$fecha<=20151223 & gen2013$fecha>20150808,"Diciembre",
																			ifelse(gen2013$fecha<=20160213 & gen2013$fecha>20151223,"Febrero",
																				ifelse(gen2013$fecha<=20160305 & gen2013$fecha>20160213,"Marzo",
																					ifelse(gen2013$fecha<=20160806 & gen2013$fecha>20160305, "Julio",
																						ifelse(gen2013$fecha<=20161222 & gen2013$fecha>20160806,"Diciembre","NC"))))))))))))))
#Casos especiales (mesas especiales, reconocimiento de creditos, revalidas, todo actividades no dentro de un periodo de examenes y revisiones):
#table(gen2012$sem_gen2012,gen2012$per_gen2012)
gen2013[which(gen2013$sem_gen2013==4 & gen2013$per_gen2013=="Julio" & gen2013$nota!=20),"per_gen2013"]<-rep("Especial",nrow(gen2013[which(gen2013$sem_gen2013==4 & gen2013$per_gen2013=="Julio" & gen2013$nota!=20),]))
gen2013[which(gen2013$sem_gen2013==6 & gen2013$per_gen2013=="Julio" & gen2013$nota!=20),"per_gen2013"]<-rep("Especial",nrow(gen2013[which(gen2013$sem_gen2013==6 & gen2013$per_gen2013=="Julio" & gen2013$nota!=20),]))																		
gen2013[which(gen2013$result_act=="Revalida sin nota" | gen2013$result_act=="Trabajo Final" | gen2013$result_act=="Practica Curricular"),"per_gen2013"]<-rep("Especial",nrow(gen2013[which(gen2013$result_act=="Revalida sin nota" | gen2013$result_act=="Trabajo Final" | gen2013$result_act=="Practica Curricular"),]))
######Periodos Generacion 2014####
#Usando las mismas fechas que para 2012
gen2014$per_gen2014<-ifelse(gen2014$fecha<=20140804 & gen2014$fecha>20140308,"Julio",
														ifelse(gen2014$fecha<=20141222 & gen2014$fecha>20140804,"Diciembre",
															ifelse(gen2014$fecha<=20150214 & gen2014$fecha>20141222,"Febrero",
																ifelse(gen2014$fecha<=20150307 & gen2014$fecha>20150214,"Marzo",
																	ifelse(gen2014$fecha<=20150808 & gen2014$fecha>20150307,"Julio",
																		ifelse(gen2014$fecha<=20151223 & gen2014$fecha>20150808,"Diciembre",
																			ifelse(gen2014$fecha<=20160213 & gen2014$fecha>20151223,"Febrero",
																				ifelse(gen2014$fecha<=20160305 & gen2014$fecha>20160213,"Marzo",
																					ifelse(gen2014$fecha<=20160806 & gen2014$fecha>20160305, "Julio",
																						ifelse(gen2014$fecha<=20161222 & gen2014$fecha>20160806,"Diciembre","NC"))))))))))
#Casos especiales (mesas especiales, reconocimiento de creditos, revalidas, todo actividades no dentro de un periodo de examenes y revisiones):
#table(gen2012$sem_gen2012,gen2012$per_gen2012)
gen2014[which(gen2014$sem_gen2014==4 & gen2014$per_gen2014=="Julio" & gen2014$nota!=20),"per_gen2014"]<-rep("Especial",nrow(gen2014[which(gen2014$sem_gen2014==4 & gen2014$per_gen2014=="Julio" & gen2014$nota!=20),]))
gen2014[which(gen2014$sem_gen2014==6 & gen2014$per_gen2014=="Julio" & gen2014$nota!=20),"per_gen2014"]<-rep("Especial",nrow(gen2014[which(gen2014$sem_gen2014==6 & gen2014$per_gen2014=="Julio" & gen2014$nota!=20),]))																		
gen2014[which(gen2014$result_act=="Revalida sin nota" | gen2014$result_act=="Trabajo Final" | gen2014$result_act=="Practica Curricular"),"per_gen2014"]<-rep("Especial",nrow(gen2014[which(gen2014$result_act=="Revalida sin nota" | gen2014$result_act=="Trabajo Final" | gen2014$result_act=="Practica Curricular"),]))
######Periodos Generacion 2015####
#Usando las mismas fechas que para 2012
gen2015$per_gen2015<-ifelse(gen2015$fecha<=20150808 & gen2015$fecha>20150307,"Julio",
																		ifelse(gen2015$fecha<=20151223 & gen2015$fecha>20150808,"Diciembre",
																			ifelse(gen2015$fecha<=20160213 & gen2015$fecha>20151223,"Febrero",
																				ifelse(gen2015$fecha<=20160305 & gen2015$fecha>20160213,"Marzo",
																					ifelse(gen2015$fecha<=20160806 & gen2015$fecha>20160305, "Julio",
																						ifelse(gen2015$fecha<=20161222 & gen2015$fecha>20160806,"Diciembre","NC"))))))
#Casos especiales (mesas especiales, reconocimiento de creditos, revalidas, todo actividades no dentro de un periodo de examenes y revisiones)
#table(gen2012$sem_gen2012,gen2012$per_gen2012)
gen2015[which(gen2015$sem_gen2015==4 & gen2015$per_gen2015=="Julio" & gen2015$nota!=20),"per_gen2015"]<-rep("Especial",nrow(gen2015[which(gen2015$sem_gen2015==4 & gen2015$per_gen2015=="Julio" & gen2015$nota!=20),]))
gen2015[which(gen2015$sem_gen2015==6 & gen2015$per_gen2015=="Julio" & gen2015$nota!=20),"per_gen2015"]<-rep("Especial",nrow(gen2015[which(gen2015$sem_gen2015==6 & gen2015$per_gen2015=="Julio" & gen2015$nota!=20),]))																		
gen2015[which(gen2015$result_act=="Revalida sin nota" | gen2015$result_act=="Trabajo Final" | gen2015$result_act=="Practica Curricular"),"per_gen2015"]<-rep("Especial",nrow(gen2015[which(gen2015$result_act=="Revalida sin nota" | gen2015$result_act=="Trabajo Final" | gen2015$result_act=="Practica Curricular"),]))
###Identificador unico por UC, variable con nombre de UC
materias<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Actividades actualizadas\\materias.csv",sep=";")
colnames(materias)[which(colnames(materias)=="MAT")]<-"mat"
gen2012<-join(gen2012,materias[,c("MATST","mat","NOMMAT")],c("mat"))
gen2013<-join(gen2013,materias[,c("MATST","mat","NOMMAT")],c("mat"))
gen2014<-join(gen2014,materias[,c("MATST","mat","NOMMAT")],c("mat"))
gen2015<-join(gen2015,materias[,c("MATST","mat","NOMMAT")],c("mat"))
write.csv(gen2012,"gen2012_sindup.csv")
write.csv(gen2013,"gen2013_sindup.csv")
write.csv(gen2014,"gen2014_sindup.csv")
write.csv(gen2015,"gen2015_sindup.csv")
######
#Resumen de result_act por UC-fecha
#aux1<-function(x){
#return(c(as.integer(table(x$result_act)[1]),as.integer(table(x$result_act)[2]),as.integer(table(x$result_act)[3]),as.integer(table(x$result_act)[4]),as.integer(table(x$result_act)[5]),as.integer(table(x$result_act)[6]),as.integer(table(x$result_act)[7])))
#}
#out1<-ddply(.data=gen2012,.variables=c("mat","fecha"),.fun=aux1,.progress="text",.inform=T)
#colnames(out1)<-c("mat","fecha","curso exonerado","curso no exonerado","examen aprobado","examen perdido","Practica Curricular","Revalida sin nota","Trabajo Final")
#gen2012<-join(gen2012,out1,c("mat","fecha"))
#out2<-ddply(.data=gen2013,.variables=c("mat","fecha"),.fun=aux1,.progress="text",.inform=T)
#colnames(out2)<-c("mat","fecha","curso exonerado","curso no exonerado","examen aprobado","examen perdido","Practica Curricular","Revalida sin nota","Trabajo Final")
#gen2013<-join(gen2013,out1,c("mat","fecha"))
#out3<-ddply(.data=gen2014,.variables=c("mat","fecha"),.fun=aux1,.progress="text",.inform=T)
#colnames(out3)<-c("mat","fecha","curso exonerado","curso no exonerado","examen aprobado","examen perdido","Practica Curricular","Revalida sin nota","Trabajo Final")
#gen2014<-join(gen2014,out1,c("mat","fecha"))
#out4<-ddply(.data=gen2015,.variables=c("mat","fecha"),.fun=aux1,.progress="text",.inform=T)
#colnames(out4)<-c("mat","fecha","curso exonerado","curso no exonerado","examen aprobado","examen perdido","Practica Curricular","Revalida sin nota","Trabajo Final")
#gen2015<-join(gen2015,out1,c("mat","fecha"))