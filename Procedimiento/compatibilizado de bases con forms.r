rm(list=ls())
setwd("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Actividades actualizadas\\Nuevas para actualizar")
gen2012<-read.csv("gen2012_sindup_confest.csv")
gen2013<-read.csv("gen2013_sindup_confest.csv")
gen2014<-read.csv("gen2014_sindup_confest.csv")
gen2015<-read.csv("gen2015_sindup_confest.csv")

variables<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\DGPlan\\Ingresos 2010_2015\\Ingresos 2010_2015\\Compatibilizador form est ingreso.csv",sep=";")
###################################
######Compatibilizado de variables#
###################################

#############barrio#############
gen2012$barrio<-as.factor(gen2012$barrio)
gen2013$barrio<-as.factor(gen2013$barrio)
gen2014$barrio<-as.factor(gen2014$barrio)
gen2015$barrio<-tolower(as.factor(gen2015$barrio))

#############lugar de residencia (residen)#############
gen2012$residen<-as.factor(gen2012$res_lug)
gen2015$residen<-tolower(as.factor(gen2015$residen))

#############sexo (mujer)#############
gen2012$mujer<-ifelse(gen2012$sexo=="mujer",1,0)
gen2013$mujer<-ifelse(gen2013$sexo=="f",1,0)
gen2014$mujer<-ifelse(gen2014$sexo=="f",1,0)
gen2015$mujer<-ifelse(gen2015$sexo=="Mujer",1,0)

#############fecha de nacimiento (fecha_nac)#############

#############lugar de nacimiento (lug_nac)#############
gen2012$lug_nac<-gen2012$nac_lug
gen2015$lug_nac<-tolower(gen2015$nac_lug)

#############lugar de residencia en marzo del año previo al ingreso (res_marzo)#############
gen2012$res_marzo<-ifelse(gen2012$res_marzo==1,"montevideo",
	ifelse(gen2012$res_marzo==2, "artigas",
		ifelse(gen2012$res_marzo==3, "canelones",
			ifelse(gen2012$res_marzo==4, "cerro largo",
				ifelse(gen2012$res_marzo==5, "colonia",
					ifelse(gen2012$res_marzo==6, "durazno",
						ifelse(gen2012$res_marzo==7, "flores",
							ifelse(gen2012$res_marzo==8, "florida",
								ifelse(gen2012$res_marzo==9, "lavalleja",
									ifelse(gen2012$res_marzo==10, "maldonado",
										ifelse(gen2012$res_marzo==11, "paysandú",
											ifelse(gen2012$res_marzo==12, "río negro",
												ifelse(gen2012$res_marzo==13, "rivera",
													ifelse(gen2012$res_marzo==14, "rocha",
														ifelse(gen2012$res_marzo==15, "salto",
															ifelse(gen2012$res_marzo==16, "san josé",
																ifelse(gen2012$res_marzo==17, "soriano",
																	ifelse(gen2012$res_marzo==18, "tacuarembó",
																		ifelse(gen2012$res_marzo==19, "treinta y tres",
																			ifelse(gen2012$res_marzo==20, "argentina",
																				ifelse(gen2012$res_marzo==21, "brasil",
																					ifelse(gen2012$res_marzo==22, "chile",
																						ifelse(gen2012$res_marzo==23, "paraguay",
																							ifelse(gen2012$res_marzo==24, "venezuela",
																								ifelse(gen2012$res_marzo==25, "perú",
																									ifelse(gen2012$res_marzo==26, "colombia",
																										ifelse(gen2012$res_marzo==27, "bolivia",
																											ifelse(gen2012$res_marzo==28, "ecuador",
																												ifelse(gen2012$res_marzo==29, "estados unidos",
																													ifelse(gen2012$res_marzo==30, "europa",
																														ifelse(gen2012$res_marzo==31, "otros", NA)))))))))))))))))))))))))))))))
																														
gen2015$res_marzo<-tolower(gen2015$res_marzo)

#############tipo de vivienda actual (tip_viv)#############
levels(gen2012$vive_en)<-levels(gen2013$tip_viv)
gen2012$tip_viv<-gen2012$vive_en
levels(gen2015$tip_viv)<-levels(gen2013$tip_viv)

#############numero de hijos (hij_num)#############
gen2013$hij_num<-gen2013$hijos
gen2014$hij_num<-gen2014$hijos
gen2015$hij_num<-gen2015$hijos

#############vive solo (viv_solo)#############
gen2012$viv_solo<-as.integer(gen2012$viv_solo)-1
gen2013$viv_solo<-as.integer(gen2013$viv_solo)-1

#############cantidad de personas que viven con ud (ocup)#############
gen2012$ocup<-gen2012$pers_num

#############cantidad de personas que viven con ud y perciben ingresos(ocup_ing)#############
gen2012$ocup_ing<-gen2012$pers_y

#############estado conyugal (est_cony)#############
levels(gen2015$est_cony)<-c(levels(gen2014$est_cony),"Viudo")

#############cantidad de padres que viven con ud (padres)#############

#############cantidad de padres que viven con ud y perciben ingresos (pad_ing)#############
gen2012$pad_ing<-gen2012$y_padres
gen2015$pad_ing<-gen2015$Pad_ing

#############cantidad de conyuges que viven con ud (conyug)#############

#############cantidad de conyuges que viven con ud  y perciben ingresos (cony_ing)#############
gen2012$cony_ing<-gen2012$y_conyug
gen2015$cony_ing<-gen2015$Cony_ing

#############cantidad de conyuges de padres que viven con ud (cony_pad)#############
gen2012$cony_pad<-gen2012$cony_p_m

#############cantidad de conyuges de padres que viven con ud  y perciben ingresos (cony_pad_ing)#############
gen2012$cony_pad_ing<-gen2012$y_cony_p_m
gen2015$cony_pad_ing<-gen2015$Cony_pad_ing

#############cantidad de hijos que viven con ud (hijo_viv)#############
gen2012$hijo_viv<-gen2012$hijos

#############cantidad de hijos que viven con ud  y perciben ingresos (hijo_ing)#############
gen2012$hijo_ing<-gen2012$y_hijos
gen2015$hijo_ing<-gen2015$Hijo_ing

#############cantidad de hermanos que viven con ud (herman)#############

#############cantidad de hermanos que viven con ud  y perciben ingresos (herm_ing)#############
gen2012$herm_ing<-gen2012$y_herman
gen2015$herm_ing<-gen2015$Herm_ing

#############cantidad de abuelos que viven con ud (abuelo)#############

#############cantidad de abuelos que viven con ud  y perciben ingresos (abu_ing)#############
gen2012$abu_ing<-gen2012$y_abuelo
gen2015$abu_ing<-gen2015$Abu_ing

#############cantidad de suegros que viven con ud (suegro)#############
gen2012$suegro<-gen2012$suegros

#############cantidad de suegros que viven con ud  y perciben ingresos (sueg_ing)#############
gen2012$sueg_ing<-gen2012$y_suegros
gen2015$sueg_ing<-gen2015$Sueg_ing

#############cantidad de otros familiares que viven con ud (fam_otro)#############

#############cantidad de otros familiares que viven con ud  y perciben ingresos (fam_ing)#############
gen2012$fam_ing<-gen2012$y_fam_otro
gen2015$fam_ing<-gen2015$Fam_ing

#############cantidad de otros estudiantes que viven con ud (est_otro)#############
gen2012$est_otro<-gen2012$estud

#############cantidad de otros estudiantes que viven con ud  y perciben ingresos (est_ing)#############
gen2012$est_ing<-gen2012$y_estud
gen2015$est_ing<-gen2015$Est_ing

#############cantidad de otras personas que viven con ud (per_otro)#############
gen2012$per_otro<-gen2012$otros

#############cantidad de otras personas que viven con ud  y perciben ingresos (per_ing)#############
gen2012$per_ing<-gen2012$y_otros
gen2015$per_ing<-gen2015$Per_ing

#############region donde curso educacion primaria (edu_prim)#############
gen2012$edu_prim<-gen2012$primaria_region
gen2015$edu_prim<-tolower(gen2015$edu_prim)

#############sector en que curso educacion primaria (tip_inst_pri)#############
gen2012$tip_inst_pri<-gen2012$primaria_sector
gen2015$tip_inst_pri<-tolower(gen2015$tip_inst_pri)

#############region donde curso primeros 5 anos de secundaria (sec_1_5)#############
gen2012$sec_1_5<-gen2012$secund_region
gen2015$sec_1_5<-tolower(gen2015$sec_1_5)

#############sector en que curso primeros 5 anos de secundaria (tip_inst_sec)#############
gen2012$tip_inst_sec<-gen2012$secund_sector
gen2015$tip_inst_sec<-tolower(gen2015$tip_inst_sec)

#############region donde curso 6 de secundaria (sec_6)#############
gen2012$sec_6<-gen2012$sexto_region
gen2015$sec_6<-tolower(gen2015$sec_6)

#############sector en que curso 6 de secundaria (tip_inst_sexto)#############
gen2012$tip_inst_sexto<-gen2012$sexto_sector
gen2015$tip_inst_sexto<-tolower(gen2015$tip_inst_sexto)

#############ano de egreso de educacion media (egre_sec)#############
gen2012$egre_sec<-gen2012$egre_sexto

#############educacion del padre(ed_padre)#############
#Niveles: 1 "S/inst-Prim.Incomp" 2 "Prim.Comp." 3 "Ed.Media.Incp" 4 "Ed.Media.Cmpl" 5 "Terc.No.Univ.Incmp" 6 "Terc.No.Univ.Cmpl" 7 "Univ.Incmpl" 8 "Univ.Cmpl" 99 "Residual"
gen2012$ed_padre<-rep(0,nrow(gen2012))
#levels(gen2012$pad_edu_seciu)
levels(gen2012$pad_edu_seciu)<-c(6, 5, 8, 7, 10, 9, 14, 2, 4, 3, 1, 13, 12, 11)
#levels(gen2012$pad_edu_2014)
levels(gen2012$pad_edu_2014)<-c(5, 4, 99, 3, 13)

gen2012$pad_edu_seciu[which(gen2012$pad_edu_seciu==7)]<-rep(3,length(gen2012$pad_edu_seciu[which(gen2012$pad_edu_seciu==7)]))
gen2012$pad_edu_seciu[which(gen2012$pad_edu_seciu==8)]<-rep(4,length(gen2012$pad_edu_seciu[which(gen2012$pad_edu_seciu==8)]))
gen2012$pad_edu_seciu[which(gen2012$pad_edu_seciu==9)]<-rep(5,length(gen2012$pad_edu_seciu[which(gen2012$pad_edu_seciu==9)]))
gen2012$pad_edu_seciu[which(gen2012$pad_edu_seciu==10 | gen2012$pad_edu_seciu==13)]<-rep(6,length(gen2012$pad_edu_seciu[which(gen2012$pad_edu_seciu==10 | gen2012$pad_edu_seciu==13)]))
gen2012$pad_edu_seciu[which(gen2012$pad_edu_seciu==11)]<-rep(7,length(gen2012$pad_edu_seciu[which(gen2012$pad_edu_seciu==11)]))
gen2012$pad_edu_seciu[which(gen2012$pad_edu_seciu==12)]<-rep(8,length(gen2012$pad_edu_seciu[which(gen2012$pad_edu_seciu==12)]))
gen2012$pad_edu_seciu[which(gen2012$pad_edu_seciu==14)]<-rep(99,length(gen2012$pad_edu_seciu[which(gen2012$pad_edu_seciu==14)]))

gen2012$pad_edu_2014[which(gen2012$pad_edu_2014==3)]<-rep(2,length(gen2012$pad_edu_2014[which(gen2012$pad_edu_2014==3)]))
gen2012$pad_edu_2014[which(gen2012$pad_edu_2014==4)]<-rep(3,length(gen2012$pad_edu_2014[which(gen2012$pad_edu_2014==4)]))
gen2012$pad_edu_2014[which(gen2012$pad_edu_2014==5)]<-rep(4,length(gen2012$pad_edu_2014[which(gen2012$pad_edu_2014==5)]))
gen2012$pad_edu_2014[which(gen2012$pad_edu_2014==13)]<-rep(8,length(gen2012$pad_edu_2014[which(gen2012$pad_edu_2014==13)]))

gen2012$ed_padre<-gen2012$pad_edu_seciu
gen2012[which(is.na(gen2012$pad_edu_seciu)==T & is.na(gen2012$pad_edu_2014)==F),"ed_padre"]<-gen2012[which(is.na(gen2012$pad_edu_seciu)==T & is.na(gen2012$pad_edu_2014)==F),"pad_edu_2014"]

levels(gen2013$ed_padre)<-c(6, 5, 6, 5, 4, 3, 2, 1, 4, 3, 99, 1, 8, 7)
gen2014$ed_padre<-as.factor(gen2014$ed_padre)
levels(gen2014$ed_padre)<-c(1, 1, 2, 3, 4, 5, 6, 7, 8, 99)
levels(gen2015$ed_padre)<-c(6, 5, 2, 1, 4, 3, 99, 1, 8, 7)

#############educacion de la madre(ed_madre)#############
#Niveles: 1 "S/inst-Prim.Incomp" 2 "Prim.Comp." 3 "Ed.Media.Incp" 4 "Ed.Media.Cmpl" 5 "Terc.No.Univ.Incmp" 6 "Terc.No.Univ.Cmpl" 7 "Univ.Incmpl" 8 "Univ.Cmpl" 99 "Residual"
gen2012$ed_madre<-rep(0,nrow(gen2012))
#levels(gen2012$mad_edu_seciu)
levels(gen2012$mad_edu_seciu)<-c(6, 5, 7, 10, 9, 14, 2, 4, 3, 1, 13, 12, 11)
#levels(gen2012$mad_edu_2014)
levels(gen2012$mad_edu_2014)<-c(11, 5, 4, 3, 2, 13)

gen2012$mad_edu_seciu[which(gen2012$mad_edu_seciu==7)]<-rep(3,length(gen2012$mad_edu_seciu[which(gen2012$mad_edu_seciu==7)]))
gen2012$mad_edu_seciu[which(gen2012$mad_edu_seciu==8)]<-rep(4,length(gen2012$mad_edu_seciu[which(gen2012$mad_edu_seciu==8)]))
gen2012$mad_edu_seciu[which(gen2012$mad_edu_seciu==9)]<-rep(5,length(gen2012$mad_edu_seciu[which(gen2012$mad_edu_seciu==9)]))
gen2012$mad_edu_seciu[which(gen2012$mad_edu_seciu==10 | gen2012$mad_edu_seciu==13)]<-rep(6,length(gen2012$mad_edu_seciu[which(gen2012$mad_edu_seciu==10 | gen2012$mad_edu_seciu==13)]))
gen2012$mad_edu_seciu[which(gen2012$mad_edu_seciu==11)]<-rep(7,length(gen2012$mad_edu_seciu[which(gen2012$mad_edu_seciu==11)]))
gen2012$mad_edu_seciu[which(gen2012$mad_edu_seciu==12)]<-rep(8,length(gen2012$mad_edu_seciu[which(gen2012$mad_edu_seciu==12)]))
gen2012$mad_edu_seciu[which(gen2012$mad_edu_seciu==14)]<-rep(99,length(gen2012$mad_edu_seciu[which(gen2012$mad_edu_seciu==14)]))

gen2012$mad_edu_2014[which(gen2012$mad_edu_2014==2)]<-rep(1,length(gen2012$mad_edu_2014[which(gen2012$mad_edu_2014==2)]))
gen2012$mad_edu_2014[which(gen2012$mad_edu_2014==3)]<-rep(2,length(gen2012$mad_edu_2014[which(gen2012$mad_edu_2014==3)]))
gen2012$mad_edu_2014[which(gen2012$mad_edu_2014==4)]<-rep(3,length(gen2012$mad_edu_2014[which(gen2012$mad_edu_2014==4)]))
gen2012$mad_edu_2014[which(gen2012$mad_edu_2014==5)]<-rep(4,length(gen2012$mad_edu_2014[which(gen2012$mad_edu_2014==5)]))
gen2012$mad_edu_2014[which(gen2012$mad_edu_2014==11)]<-rep(6,length(gen2012$mad_edu_2014[which(gen2012$mad_edu_2014==11)]))
gen2012$mad_edu_2014[which(gen2012$mad_edu_2014==13)]<-rep(8,length(gen2012$mad_edu_2014[which(gen2012$mad_edu_2014==13)]))

gen2012$ed_madre<-gen2012$mad_edu_seciu
gen2012[which(is.na(gen2012$mad_edu_seciu)==T & is.na(gen2012$mad_edu_2014)==F),"ed_madre"]<-gen2012[which(is.na(gen2012$mad_edu_seciu)==T & is.na(gen2012$mad_edu_2014)==F),"mad_edu_2014"]

levels(gen2013$ed_madre)<-c(6, 5, 6, 5, 4, 3, 2, 1, 4, 3, 99, 1, 8, 7)
gen2014$ed_madre<-as.factor(gen2014$ed_madre)
levels(gen2014$ed_madre)<-c(1, 1, 2, 3, 4, 5, 6, 7, 8, 99, 99)
levels(gen2015$ed_madre)<-c(6, 5, 2, 1, 4, 3, 99, 1, 8, 7)

#############OTROS ESTUDIOS TERCIARIOS : OMITIDO#############

#############horas promedio de trabajo semanal (hora_tra)#############
gen2012$hora_tra<-gen2012$hor_trab
gen2015$hora_tra<-as.factor(tolower(gen2015$hora_tra))
levels(gen2015$hora_tra[which(gen2015$hora_tra=="no corresponde")])<-rep(NA,length(gen2015$hora_tra[which(gen2015$hora_tra=="no corresponde")]))

#############anio en que comenzo su actividad laboral (inic_tra)#############
gen2012$inic_tra<-gen2012$an_com

#############relacion de la ocupacion con la carrera (rel_trab)#############
gen2015$rel_trab<-as.factor(tolower(gen2015$rel_trab))

#############categoria ocupacional del estudiante (cat_oc_est)#############
gen2013$cat_oc_est<-as.factor(gen2013$cat_oc_est)
levels(gen2013$cat_oc_est)<-c("asalariado privado","asalariado público","pasante-becario")
table(gen2013$cat_oc_est)

gen2015$cat_oc_est<-as.factor(tolower(gen2015$cat_oc_est))

#############ocupacion del estudiante (ocup_est)#############
levels(gen2015$ocup_est)
levels(gen2012$enc_trab)<-c("Trabajadores de los servicios y comercios/Vendedores","Directivo/Gerente/Rentas","Directivo/Gerente/Rentas","Profesionales/Técnicos/Docentes" ,"Profesionales/Técnicos/Docentes" ,"Trabajadores de los servicios y comercios/Vendedores","Trabajadores de los servicios y comercios/Vendedores","Fuerzas Armadas/Policía","No corresponde","Oficial/Operario/Artesano/Obrero","Oficial/Operario/Artesano/Obrero","NA","NA","Directivo/Gerente/Rentas","Directivo/Gerente/Rentas","Trabajadores calificados del Agro/Agricultores/Ganadero","NA","NA","NA","NA")
levels(gen2013$ocup_est)<-c("Directivo/Gerente/Rentas","Fuerzas Armadas/Policía","No corresponde","Oficial/Operario/Artesano/Obrero","Oficial/Operario/Artesano/Obrero","NA","NA","Directivo/Gerente/Rentas","Empleados de Oficina/Administrativos","Profesionales/Técnicos/Docentes","Profesionales/Técnicos/Docentes","Trabajadores de los servicios y comercios/Vendedores","Trabajadores de los servicios y comercios/Vendedores","NA","NA","NA","NA","Trabajadores calificados del Agro/Agricultores/Ganadero","Trabajadores de los servicios y comercios/Vendedores","Trabajadores de los servicios y comercios/Vendedores") 
levels(gen2014$ocup_est)<-c("Directivo/Gerente/Rentas", "Empleados de Oficina/Administrativos","Fuerzas Armadas/Policía","Oficial/Operario/Artesano/Obrero","Operador de maquinaria/Conductor" ,"Profesionales/Técnicos/Docentes","Trabajadores calificados del Agro/Agricultores/Ganadero","Trabajadores de los servicios y comercios/Vendedores","Trabajador no calificado" )
levels(gen2015$ocup_est)<-c("Directivo/Gerente/Rentas","Empleados de Oficina/Administrativos","Fuerzas Armadas/Policía","No corresponde","Oficial/Operario/Artesano/Obrero","Operador de maquinaria/Conductor","Profesionales/Técnicos/Docentes","Trabajador no calificado" , "Trabajadores calificados del Agro/Agricultores/Ganadero", "Trabajadores de los servicios y comercios/Vendedores")
gen2012$ocup_est<-gen2012$enc_trab

#############categoria ocupacional del padre (cat_oc_pad)#############
gen2013$cat_oc_pad<-as.factor(gen2013$cat_oc_pad)
levels(gen2013$cat_oc_pad)<-c("NA","asalariado privado","asalariado público","patrón","cuenta propia")
gen2015$cat_oc_pad<-as.factor(tolower(gen2015$cat_oc_pad))

#############ocupacion del padre (ocup_pad)#############
gen2012$ocup_pad<-gen2012$pad_trab
levels(gen2012$ocup_pad)<-c("Trabajadores de los servicios y comercios/Vendedores","Trabajadores de los servicios y comercios/Vendedores","Directivo/Gerente/Rentas","Directivo/Gerente/Rentas","Profesionales/Técnicos/Docentes","Profesionales/Técnicos/Docentes","Trabajadores de los servicios y comercios/Vendedores","Trabajadores de los servicios y comercios/Vendedores","Fuerzas Armadas/Policía","NA","No corresponde","Oficial/Operario/Artesano/Obrero","Oficial/Operario/Artesano/Obrero","NA","NA","Directivo/Gerente/Rentas","Directivo/Gerente/Rentas","Trabajadores calificados del Agro/Agricultores/Ganadero","Trabajadores calificados del Agro/Agricultores/Ganadero","Directivo/Gerente/Rentas","Directivo/Gerente/Rentas","NA","NA","NA","NA")
levels(gen2013$ocup_pad)<-c("Directivo/Gerente/Rentas","Directivo/Gerente/Rentas","Fuerzas Armadas/Policía","No corresponde","Oficial/Operario/Artesano/Obrero","Oficial/Operario/Artesano/Obrero","NA","NA","Directivo/Gerente/Rentas","Directivo/Gerente/Rentas","Empleados de Oficina/Administrativos","Profesionales/Técnicos/Docentes" ,"Profesionales/Técnicos/Docentes" ,"NA", "Directivo/Gerente/Rentas","Directivo/Gerente/Rentas","Directivo/Gerente/Rentas","Directivo/Gerente/Rentas","NA","NA","NA","NA","Trabajadores calificados del Agro/Agricultores/Ganadero","Trabajadores calificados del Agro/Agricultores/Ganadero","Trabajadores de los servicios y comercios/Vendedores","Trabajadores de los servicios y comercios/Vendedores")
levels(gen2014$ocup_pad)<-c("Directivo/Gerente/Rentas" , "Empleados de Oficina/Administrativos", "Fuerzas Armadas/Policía","NA","Oficial/Operario/Artesano/Obrero", "Operador de maquinaria/Conductor", "Profesionales/Técnicos/Docentes", "Trabajadores calificados del Agro/Agricultores/Ganadero", "Trabajadores de los servicios y comercios/Vendedores", "Trabajador no calificado")
levels(gen2015$ocup_pad)<-c("Directivo/Gerente/Rentas" , "Empleados de Oficina/Administrativos", "Fuerzas Armadas/Policía","No corresponde","Oficial/Operario/Artesano/Obrero", "Operador de maquinaria/Conductor", "Profesionales/Técnicos/Docentes", "Trabajador no calificado", "Trabajadores calificados del Agro/Agricultores/Ganadero", "Trabajadores de los servicios y comercios/Vendedores")


#############categoria ocupacional de la madre (cat_oc_mad)#############
gen2013$cat_oc_mad<-as.factor(gen2013$cat_oc_mad)
levels(gen2013$cat_oc_mad)<-c("asalariado privado","asalariado público","miembro de cooperativa de producción ","patrón","cuenta propia","NA")
gen2015$cat_oc_mad<-as.factor(tolower(gen2015$cat_oc_mad))

#############ocupacion de la madre (ocup_mad)#############
gen2012$ocup_mad<-gen2012$mad_trab
levels(gen2012$ocup_mad)<-c("Trabajadores de los servicios y comercios/Vendedores","Trabajadores de los servicios y comercios/Vendedores","Directivo/Gerente/Rentas","Directivo/Gerente/Rentas","Profesionales/Técnicos/Docentes","Profesionales/Técnicos/Docentes","Trabajadores de los servicios y comercios/Vendedores","Trabajadores de los servicios y comercios/Vendedores","Fuerzas Armadas/Policía","NA","No corresponde","Oficial/Operario/Artesano/Obrero","Oficial/Operario/Artesano/Obrero","NA","NA","Directivo/Gerente/Rentas","Directivo/Gerente/Rentas","Trabajadores calificados del Agro/Agricultores/Ganadero","Trabajadores calificados del Agro/Agricultores/Ganadero","NA","NA","NA","NA")
levels(gen2013$ocup_mad)<-c("Directivo/Gerente/Rentas","Directivo/Gerente/Rentas","Fuerzas Armadas/Policía","No corresponde","Oficial/Operario/Artesano/Obrero","Oficial/Operario/Artesano/Obrero","NA","NA","Directivo/Gerente/Rentas" ,"Directivo/Gerente/Rentas" ,"Empleados de Oficina/Administrativos", "Profesionales/Técnicos/Docentes","Profesionales/Técnicos/Docentes","NA","Directivo/Gerente/Rentas","Directivo/Gerente/Rentas","Directivo/Gerente/Rentas","Directivo/Gerente/Rentas","NA","NA","NA","NA","Trabajadores calificados del Agro/Agricultores/Ganadero","Trabajadores calificados del Agro/Agricultores/Ganadero","Trabajadores de los servicios y comercios/Vendedores","Trabajadores de los servicios y comercios/Vendedores")
levels(gen2014$ocup_mad)<-c("Directivo/Gerente/Rentas" , "Empleados de Oficina/Administrativos", "Fuerzas Armadas/Policía","NA","Oficial/Operario/Artesano/Obrero", "Operador de maquinaria/Conductor", "Profesionales/Técnicos/Docentes", "Trabajadores calificados del Agro/Agricultores/Ganadero", "Trabajadores de los servicios y comercios/Vendedores", "Trabajador no calificado")
levels(gen2015$ocup_mad)<-c("Directivo/Gerente/Rentas" , "Empleados de Oficina/Administrativos", "Fuerzas Armadas/Policía","No corresponde","Oficial/Operario/Artesano/Obrero", "Operador de maquinaria/Conductor", "Profesionales/Técnicos/Docentes", "Trabajador no calificado", "Trabajadores calificados del Agro/Agricultores/Ganadero", "Trabajadores de los servicios y comercios/Vendedores")

#############inscribio a algun programa de becas para iniciar sus estudios (beca)#############
gen2012$beca<-gen2012$becas
gen2015$beca<-as.factor(tolower(gen2015$beca))

#############solicito la beca del fondo de solidaridad (beca_fondo)#############
gen2012$beca_fondo<-gen2012$aplica_fs
gen2015$beca_fondo<-as.factor(tolower(gen2015$beca_fondo))

#############solicito la beca de bienestar universitario (beca_scbu)#############
gen2012$beca_scbu<-gen2012$aplica_bu
gen2015$beca_scbu<-as.factor(tolower(gen2015$beca_scbu))

#############solicito otras becas (beca_otra)#############
gen2012$beca_otra<-gen2012$aplica_otro
gen2015$beca_otra<-as.factor(tolower(gen2015$beca_otra))



###################################
######Ensamblado de bases##########
###################################

gen2012<-gen2012[,c("ci","NOMBRE","gen","carr","ciclo","mat","MATST","NOMMAT","tipo_act","nota","result_act","fecha","sem_gen2012","per_gen2012","anio_act","ecopuro12","contpuro12","admpuro12","ecocont12","ecoadm12","contadm12","todas12","ident1","barrio","residen","mujer","fecha_nac","lug_nac","res_marzo","tip_viv","hij_num","viv_solo","ocup","ocup_ing","est_cony","padres","pad_ing","conyug","cony_ing","cony_pad","cony_pad_ing","hijo_viv","hijo_ing","herman","herm_ing","abuelo","abu_ing","suegro","sueg_ing", "fam_otro","fam_ing","est_otro","est_ing","per_otro","per_ing","edu_prim","tip_inst_pri","sec_1_5","tip_inst_sec","sec_6","tip_inst_sexto","egre_sec","ed_padre","ed_madre","hora_tra","inic_tra","rel_trab","cat_oc_est","ocup_est","cat_oc_pad","ocup_pad","cat_oc_mad","ocup_mad","beca","beca_fondo","beca_scbu","beca_otra")]
gen2013<-gen2013[,c("ci","NOMBRE","gen","carr","ciclo","mat","MATST","NOMMAT","tipo_act","nota","result_act","fecha","sem_gen2013","per_gen2013","anio_act","ecopuro13","contpuro13","admpuro13","ecocont13","ecoadm13","contadm13","todas13","ident1","barrio","residen","mujer","fecha_nac","lug_nac","res_marzo","tip_viv","hij_num","viv_solo","ocup","ocup_ing","est_cony","padres","pad_ing","conyug","cony_ing","cony_pad","cony_pad_ing","hijo_viv","hijo_ing","herman","herm_ing","abuelo","abu_ing","suegro","sueg_ing", "fam_otro","fam_ing","est_otro","est_ing","per_otro","per_ing","edu_prim","tip_inst_pri","sec_1_5","tip_inst_sec","sec_6","tip_inst_sexto","egre_sec","ed_padre","ed_madre","hora_tra","inic_tra","rel_trab","cat_oc_est","ocup_est","cat_oc_pad","ocup_pad","cat_oc_mad","ocup_mad","beca","beca_fondo","beca_scbu","beca_otra")]
gen2014<-gen2014[,c("ci","NOMBRE","gen","carr","ciclo","mat","MATST","NOMMAT","tipo_act","nota","result_act","fecha","sem_gen2014","per_gen2014","anio_act","ecopuro14","contpuro14","admpuro14","ecocont14","ecoadm14","contadm14","todas14","ident1","barrio","residen","mujer","fecha_nac","lug_nac","res_marzo","tip_viv","hij_num","viv_solo","ocup","ocup_ing","est_cony","padres","pad_ing","conyug","cony_ing","cony_pad","cony_pad_ing","hijo_viv","hijo_ing","herman","herm_ing","abuelo","abu_ing","suegro","sueg_ing", "fam_otro","fam_ing","est_otro","est_ing","per_otro","per_ing","edu_prim","tip_inst_pri","sec_1_5","tip_inst_sec","sec_6","tip_inst_sexto","egre_sec","ed_padre","ed_madre","hora_tra","inic_tra","rel_trab","cat_oc_est","ocup_est","cat_oc_pad","ocup_pad","cat_oc_mad","ocup_mad","beca","beca_fondo","beca_scbu","beca_otra")]
gen2015<-gen2015[,c("ci","NOMBRE","gen","carr","ciclo","mat","MATST","NOMMAT","tipo_act","nota","result_act","fecha","sem_gen2015","per_gen2015","anio_act","ecopuro15","contpuro15","admpuro15","ecocont15","ecoadm15","contadm15","todas15","ident1","barrio","residen","mujer","fecha_nac","lug_nac","res_marzo","tip_viv","hij_num","viv_solo","ocup","ocup_ing","est_cony","padres","pad_ing","conyug","cony_ing","cony_pad","cony_pad_ing","hijo_viv","hijo_ing","herman","herm_ing","abuelo","abu_ing","suegro","sueg_ing", "fam_otro","fam_ing","est_otro","est_ing","per_otro","per_ing","edu_prim","tip_inst_pri","sec_1_5","tip_inst_sec","sec_6","tip_inst_sexto","egre_sec","ed_padre","ed_madre","hora_tra","inic_tra","rel_trab","cat_oc_est","ocup_est","cat_oc_pad","ocup_pad","cat_oc_mad","ocup_mad","beca","beca_fondo","beca_scbu","beca_otra")]

write.csv(gen2012,"gen2012_sindup_confest_compat.csv")
write.csv(gen2013,"gen2013_sindup_confest_compat.csv")
write.csv(gen2014,"gen2014_sindup_confest_compat.csv")
write.csv(gen2015,"gen2015_sindup_confest_compat.csv")