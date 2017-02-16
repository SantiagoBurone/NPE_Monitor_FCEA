rm(list=ls())
gc()
library(plyr)
directorio<-"C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Creditos"
setwd(directorio)

creditos<-read.csv("base_creditos_conv.csv")
creditos$MATST<-creditos$MAT
creditos$mat<-creditos$MAT
creditos$carr<-creditos$CARR
creditos$ciclo<-creditos$CICLO
creditos<-creditos[which(creditos$carr==11 | creditos$carr==12 | creditos$carr==14 | creditos$carr==2 | creditos$carr==3 | creditos$carr==24 | creditos$carr==33),]

gen2012<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Datos filiatorios\\gen2012_sindup_confest_compat_condatosgen.csv")
gen2013<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Datos filiatorios\\gen2013_sindup_confest_compat_condatosgen.csv")
gen2014<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Datos filiatorios\\gen2014_sindup_confest_compat_condatosgen.csv")
gen2015<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Datos filiatorios\\gen2015_sindup_confest_compat_condatosgen.csv")

#Materias en la base que no estan en la referencia de creditos
#gen2012$NOMMAT[gen2012$MATST==names(table(as.character(gen2012$MATST[which(gen2012$MATST%in%creditos$MATST==F)])))]
#gen2013$NOMMAT[gen2013$MATST==names(table(as.character(gen2013$MATST[which(gen2013$MATST%in%creditos$MATST==F)])))]
#gen2014$NOMMAT[gen2014$MATST==names(table(as.character(gen2014$MATST[which(gen2014$MATST%in%creditos$MATST==F)])))]
#gen2015$NOMMAT[gen2015$MATST==names(table(as.character(gen2015$MATST[which(gen2015$MATST%in%creditos$MATST==F)])))]

#No se puede pegar de forma que admita diferencia en creditos para un mismo curso segun carrera (porque estan mal registradas en g_actividades_de_una_gen del sgb)
total12<-join(gen2012,creditos[which(creditos$CREDITOS!=0),c("mat","CREDITOS")],by=c("mat"),match="first")
total13<-join(gen2013,creditos[which(creditos$CREDITOS!=0),c("mat","CREDITOS")],by=c("mat"),match="first")
total14<-join(gen2014,creditos[which(creditos$CREDITOS!=0),c("mat","CREDITOS")],by=c("mat"),match="first")
total15<-join(gen2015,creditos[which(creditos$CREDITOS!=0),c("mat","CREDITOS")],by=c("mat"),match="first")

total12$CREDITOS_ACT<-total12$CREDITOS
total13$CREDITOS_ACT<-total13$CREDITOS
total14$CREDITOS_ACT<-total14$CREDITOS
total15$CREDITOS_ACT<-total15$CREDITOS

creditos<-read.csv("base_creditos_conv.csv")
creditos$MATST<-creditos$MAT
creditos$mat<-creditos$MAT
creditos$carr<-creditos$CARR
creditos$ciclo<-creditos$CICLO


aux_econ<-function(x){
out<-rep(0,nrow(x))
for(i in 1:nrow(x)){
out[i]<-ifelse(as.character(x[i,"mat"])%in%creditos$mat,ifelse(12%in%creditos[which(as.character(creditos$mat)==as.character(x[i,"mat"])),"carr"],1,0),NA)
}
return(out)
}

aux_cont<-function(x){
out<-rep(0,nrow(x))
for(i in 1:nrow(x)){
out[i]<-ifelse(as.character(x[i,"mat"])%in%creditos$mat,ifelse(11%in%creditos[which(as.character(creditos$mat)==as.character(x[i,"mat"])),"carr"],1,0),NA)
}
return(out)
}


aux_adm<-function(x){
out<-rep(0,nrow(x))
for(i in 1:nrow(x)){
out[i]<-ifelse(as.character(x[i,"mat"])%in%creditos$mat,ifelse(14%in%creditos[which(as.character(creditos$mat)==as.character(x[i,"mat"])),"carr"],1,0),NA)
}
return(out)
}


ltotal12<-split(total12, f=total12$ci)
total12$tray_econ<-rep(0,nrow(total12))
total12$tray_cont<-rep(0,nrow(total12))
total12$tray_adm<-rep(0,nrow(total12))
total12$tray_econ<-unlist(lapply(ltotal12,aux_econ)) 
total12$tray_cont<-unlist(lapply(ltotal12,aux_cont))
total12$tray_adm<-unlist(lapply(ltotal12,aux_adm))

ltotal13<-split(total13, f=total13$ci)
total13$tray_econ<-rep(0,nrow(total13))
total13$tray_cont<-rep(0,nrow(total13))
total13$tray_adm<-rep(0,nrow(total13))
total13$tray_econ<-unlist(lapply(ltotal13,aux_econ)) 
total13$tray_cont<-unlist(lapply(ltotal13,aux_cont))
total13$tray_adm<-unlist(lapply(ltotal13,aux_adm))

ltotal14<-split(total14, f=total14$ci)
total14$tray_econ<-rep(0,nrow(total14))
total14$tray_cont<-rep(0,nrow(total14))
total14$tray_adm<-rep(0,nrow(total14))
total14$tray_econ<-unlist(lapply(ltotal14,aux_econ)) 
total14$tray_cont<-unlist(lapply(ltotal14,aux_cont))
total14$tray_adm<-unlist(lapply(ltotal14,aux_adm))

ltotal15<-split(total15, f=total15$ci)
total15$tray_econ<-rep(0,nrow(total15))
total15$tray_cont<-rep(0,nrow(total15))
total15$tray_adm<-rep(0,nrow(total15))
total15$tray_econ<-unlist(lapply(ltotal15,aux_econ)) 
total15$tray_cont<-unlist(lapply(ltotal15,aux_cont))
total15$tray_adm<-unlist(lapply(ltotal15,aux_adm))



total12<-total12[,c("ci","NOMBRE","gen","carr","ciclo","mat","MATST","NOMMAT","CREDITOS_ACT","tipo_act","nota","result_act","fecha","sem_gen2012","per_gen2012","anio_act","ecopuro12","contpuro12","admpuro12","ecocont12","ecoadm12","contadm12","todas12","tray_econ","tray_cont","tray_adm","ident1","barrio","residen","mujer","fecha_nac","lug_nac","res_marzo","tip_viv","hij_num","viv_solo","ocup","ocup_ing","est_cony","padres","pad_ing","conyug","cony_ing","cony_pad","cony_pad_ing","hijo_viv","hijo_ing","herman","herm_ing","abuelo","abu_ing","suegro","sueg_ing", "fam_otro","fam_ing","est_otro","est_ing","per_otro","per_ing","edu_prim","tip_inst_pri","sec_1_5","tip_inst_sec","sec_6","tip_inst_sexto","egre_sec","ed_padre","ed_madre","hora_tra","inic_tra","rel_trab","cat_oc_est","ocup_est","cat_oc_pad","ocup_pad","cat_oc_mad","ocup_mad","beca","beca_fondo","beca_scbu","beca_otra","S","FNACIMIENTO","LUGAR","AÑOFINESTUDIOS","INSTITUTO")]
total13<-total13[,c("ci","NOMBRE","gen","carr","ciclo","mat","MATST","NOMMAT","CREDITOS_ACT","tipo_act","nota","result_act","fecha","sem_gen2013","per_gen2013","anio_act","ecopuro13","contpuro13","admpuro13","ecocont13","ecoadm13","contadm13","todas13","tray_econ","tray_cont","tray_adm","ident1","barrio","residen","mujer","fecha_nac","lug_nac","res_marzo","tip_viv","hij_num","viv_solo","ocup","ocup_ing","est_cony","padres","pad_ing","conyug","cony_ing","cony_pad","cony_pad_ing","hijo_viv","hijo_ing","herman","herm_ing","abuelo","abu_ing","suegro","sueg_ing", "fam_otro","fam_ing","est_otro","est_ing","per_otro","per_ing","edu_prim","tip_inst_pri","sec_1_5","tip_inst_sec","sec_6","tip_inst_sexto","egre_sec","ed_padre","ed_madre","hora_tra","inic_tra","rel_trab","cat_oc_est","ocup_est","cat_oc_pad","ocup_pad","cat_oc_mad","ocup_mad","beca","beca_fondo","beca_scbu","beca_otra","S","FNACIMIENTO","LUGAR","AÑOFINESTUDIOS","INSTITUTO")]
total14<-total14[,c("ci","NOMBRE","gen","carr","ciclo","mat","MATST","NOMMAT","CREDITOS_ACT","tipo_act","nota","result_act","fecha","sem_gen2014","per_gen2014","anio_act","ecopuro14","contpuro14","admpuro14","ecocont14","ecoadm14","contadm14","todas14","tray_econ","tray_cont","tray_adm","ident1","barrio","residen","mujer","fecha_nac","lug_nac","res_marzo","tip_viv","hij_num","viv_solo","ocup","ocup_ing","est_cony","padres","pad_ing","conyug","cony_ing","cony_pad","cony_pad_ing","hijo_viv","hijo_ing","herman","herm_ing","abuelo","abu_ing","suegro","sueg_ing", "fam_otro","fam_ing","est_otro","est_ing","per_otro","per_ing","edu_prim","tip_inst_pri","sec_1_5","tip_inst_sec","sec_6","tip_inst_sexto","egre_sec","ed_padre","ed_madre","hora_tra","inic_tra","rel_trab","cat_oc_est","ocup_est","cat_oc_pad","ocup_pad","cat_oc_mad","ocup_mad","beca","beca_fondo","beca_scbu","beca_otra","S","FNACIMIENTO","LUGAR","AÑOFINESTUDIOS","INSTITUTO")]
total15<-total15[,c("ci","NOMBRE","gen","carr","ciclo","mat","MATST","NOMMAT","CREDITOS_ACT","tipo_act","nota","result_act","fecha","sem_gen2015","per_gen2015","anio_act","ecopuro15","contpuro15","admpuro15","ecocont15","ecoadm15","contadm15","todas15","tray_econ","tray_cont","tray_adm","ident1","barrio","residen","mujer","fecha_nac","lug_nac","res_marzo","tip_viv","hij_num","viv_solo","ocup","ocup_ing","est_cony","padres","pad_ing","conyug","cony_ing","cony_pad","cony_pad_ing","hijo_viv","hijo_ing","herman","herm_ing","abuelo","abu_ing","suegro","sueg_ing", "fam_otro","fam_ing","est_otro","est_ing","per_otro","per_ing","edu_prim","tip_inst_pri","sec_1_5","tip_inst_sec","sec_6","tip_inst_sexto","egre_sec","ed_padre","ed_madre","hora_tra","inic_tra","rel_trab","cat_oc_est","ocup_est","cat_oc_pad","ocup_pad","cat_oc_mad","ocup_mad","beca","beca_fondo","beca_scbu","beca_otra","S","FNACIMIENTO","LUGAR","AÑOFINESTUDIOS","INSTITUTO")]

write.csv(total12,"gen2012_sindup_confest_compat_condatosgen_concreds.csv")
write.csv(total13,"gen2013_sindup_confest_compat_condatosgen_concreds.csv")
write.csv(total14,"gen2014_sindup_confest_compat_condatosgen_concreds.csv")
write.csv(total15,"gen2015_sindup_confest_compat_condatosgen_concreds.csv")





#Comando para calcular suma de creditos obtenido para un estudiante
#sum(total14[which(total14$ci=="4711200" & total14$result_act%in%c("curso exonerado","examen aprobado","Trabajo Final","Revalida sin nota")),"CREDITOS_ACT"],na.rm=T)