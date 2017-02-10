rm(list=ls())
gc()
directorio<-"C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Egresos"
setwd(directorio)
library(foreign)

gen2012<-read.csv("gen2012_sindup_confest_compat_condatosgen_concreds_coninst_conegre.csv")
gen2013<-read.csv("gen2013_sindup_confest_compat_condatosgen_concreds_coninst_conegre.csv")
gen2014<-read.csv("gen2014_sindup_confest_compat_condatosgen_concreds_coninst_conegre.csv")
gen2015<-read.csv("gen2015_sindup_confest_compat_condatosgen_concreds_coninst_conegre.csv")

gen2012<-gen2012[which(duplicated(gen2012[,c("ci","fecha","result_act")])==F),]
gen2013<-gen2013[which(duplicated(gen2013[,c("ci","fecha","result_act")])==F),]
gen2014<-gen2014[which(duplicated(gen2014[,c("ci","fecha","result_act")])==F),]
gen2015<-gen2015[which(duplicated(gen2015[,c("ci","fecha","result_act")])==F),]

gen2012$ident1[which(gen2012$ident1==0 & duplicated(gen2012$ci)==F)]<-rep(1,length(gen2012$ident1[which(gen2012$ident1==0 & duplicated(gen2012$ci)==F)]))
gen2013$ident1[which(gen2013$ident1==0 & duplicated(gen2013$ci)==F)]<-rep(1,length(gen2013$ident1[which(gen2013$ident1==0 & duplicated(gen2013$ci)==F)]))
gen2014$ident1[which(gen2014$ident1==0 & duplicated(gen2014$ci)==F)]<-rep(1,length(gen2014$ident1[which(gen2014$ident1==0 & duplicated(gen2014$ci)==F)]))
gen2015$ident1[which(gen2015$ident1==0 & duplicated(gen2015$ci)==F)]<-rep(1,length(gen2015$ident1[which(gen2015$ident1==0 & duplicated(gen2015$ci)==F)]))



gen2012$sem_gen<-gen2012$sem_gen2012
gen2013$sem_gen<-gen2013$sem_gen2013
gen2014$sem_gen<-gen2014$sem_gen2014
gen2015$sem_gen<-gen2015$sem_gen2015

gen2012$per_gen<-gen2012$per_gen2012
gen2013$per_gen<-gen2013$per_gen2013
gen2014$per_gen<-gen2014$per_gen2014
gen2015$per_gen<-gen2015$per_gen2015

gen2012$ecopuro<-gen2012$ecopuro12
gen2013$ecopuro<-gen2013$ecopuro13
gen2014$ecopuro<-gen2014$ecopuro14
gen2015$ecopuro<-gen2015$ecopuro15

gen2012$contpuro<-gen2012$contpuro12
gen2013$contpuro<-gen2013$contpuro13
gen2014$contpuro<-gen2014$contpuro14
gen2015$contpuro<-gen2015$contpuro15

gen2012$admpuro<-gen2012$admpuro12
gen2013$admpuro<-gen2013$admpuro13
gen2014$admpuro<-gen2014$admpuro14
gen2015$admpuro<-gen2015$admpuro15

gen2012$ecocont<-gen2012$ecocont12
gen2013$ecocont<-gen2013$ecocont13
gen2014$ecocont<-gen2014$ecocont14
gen2015$ecocont<-gen2015$ecocont15

gen2012$ecoadm<-gen2012$ecoadm12
gen2013$ecoadm<-gen2013$ecoadm13
gen2014$ecoadm<-gen2014$ecoadm14
gen2015$ecoadm<-gen2015$ecoadm15

gen2012$contadm<-gen2012$contadm12
gen2013$contadm<-gen2013$contadm13
gen2014$contadm<-gen2014$contadm14
gen2015$contadm<-gen2015$contadm15

gen2012$todas<-gen2012$todas12
gen2013$todas<-gen2013$todas13
gen2014$todas<-gen2014$todas14
gen2015$todas<-gen2015$todas15

gen2012<-gen2012[,c("ci","NOMBRE","gen","carr","ciclo","mat", "MATST","NOMMAT",
"CREDITOS_ACT","tipo_act","nota","result_act","fecha","sem_gen",     
"per_gen","anio_act","ecopuro","contpuro","admpuro","ecocont",       
"ecoadm","contadm","todas","tray_econ","tray_cont","tray_adm","ident1","barrio","residen",         
"mujer","fecha_nac","lug_nac","res_marzo","tip_viv","hij_num","viv_solo",
"ocup","ocup_ing","est_cony","padres","pad_ing","conyug","cony_ing","cony_pad",        
"cony_pad_ing","hijo_viv","hijo_ing","herman","herm_ing","abuelo","abu_ing","suegro",          
"sueg_ing","fam_otro","fam_ing","est_otro","est_ing","per_otro",        
"per_ing","edu_prim","tip_inst_pri","sec_1_5","tip_inst_sec",
"sec_6","tip_inst_sexto","egre_sec","ed_padre","ed_madre","hora_tra",
"inic_tra","rel_trab","cat_oc_est","ocup_est","cat_oc_pad","ocup_pad",
"cat_oc_mad","ocup_mad","beca","beca_fondo","beca_scbu","beca_otra","S",               
"FNACIMIENTO","LUGAR","AÑOFINESTUDIOS","INSTITUTO","COD_INSTITUTO","NOM_INSTITUTO",   
"LUG_INSTITUTO","NOMLUG_INSTITUTO","TIPO_INSTITUTO","FECHA_EGRESO",
"ANO_EGRESO","CARR_EGRESO","COD_CARR_EGRESO")] 

gen2013<-gen2013[,c("ci","NOMBRE","gen","carr","ciclo","mat", "MATST","NOMMAT",
"CREDITOS_ACT","tipo_act","nota","result_act","fecha","sem_gen",     
"per_gen","anio_act","ecopuro","contpuro","admpuro","ecocont",       
"ecoadm","contadm","todas","tray_econ","tray_cont","tray_adm","ident1","barrio","residen",         
"mujer","fecha_nac","lug_nac","res_marzo","tip_viv","hij_num","viv_solo",
"ocup","ocup_ing","est_cony","padres","pad_ing","conyug","cony_ing","cony_pad",        
"cony_pad_ing","hijo_viv","hijo_ing","herman","herm_ing","abuelo","abu_ing","suegro",          
"sueg_ing","fam_otro","fam_ing","est_otro","est_ing","per_otro",        
"per_ing","edu_prim","tip_inst_pri","sec_1_5","tip_inst_sec",
"sec_6","tip_inst_sexto","egre_sec","ed_padre","ed_madre","hora_tra",
"inic_tra","rel_trab","cat_oc_est","ocup_est","cat_oc_pad","ocup_pad",
"cat_oc_mad","ocup_mad","beca","beca_fondo","beca_scbu","beca_otra","S",               
"FNACIMIENTO","LUGAR","AÑOFINESTUDIOS","INSTITUTO","COD_INSTITUTO","NOM_INSTITUTO",   
"LUG_INSTITUTO","NOMLUG_INSTITUTO","TIPO_INSTITUTO","FECHA_EGRESO",
"ANO_EGRESO","CARR_EGRESO","COD_CARR_EGRESO")]  

gen2014<-gen2014[,c("ci","NOMBRE","gen","carr","ciclo","mat", "MATST","NOMMAT",
"CREDITOS_ACT","tipo_act","nota","result_act","fecha","sem_gen",     
"per_gen","anio_act","ecopuro","contpuro","admpuro","ecocont",       
"ecoadm","contadm","todas","tray_econ","tray_cont","tray_adm","ident1","barrio","residen",         
"mujer","fecha_nac","lug_nac","res_marzo","tip_viv","hij_num","viv_solo",
"ocup","ocup_ing","est_cony","padres","pad_ing","conyug","cony_ing","cony_pad",        
"cony_pad_ing","hijo_viv","hijo_ing","herman","herm_ing","abuelo","abu_ing","suegro",          
"sueg_ing","fam_otro","fam_ing","est_otro","est_ing","per_otro",        
"per_ing","edu_prim","tip_inst_pri","sec_1_5","tip_inst_sec",
"sec_6","tip_inst_sexto","egre_sec","ed_padre","ed_madre","hora_tra",
"inic_tra","rel_trab","cat_oc_est","ocup_est","cat_oc_pad","ocup_pad",
"cat_oc_mad","ocup_mad","beca","beca_fondo","beca_scbu","beca_otra","S",               
"FNACIMIENTO","LUGAR","AÑOFINESTUDIOS","INSTITUTO","COD_INSTITUTO","NOM_INSTITUTO",   
"LUG_INSTITUTO","NOMLUG_INSTITUTO","TIPO_INSTITUTO","FECHA_EGRESO",
"ANO_EGRESO","CARR_EGRESO","COD_CARR_EGRESO")]  

gen2015<-gen2015[,c("ci","NOMBRE","gen","carr","ciclo","mat", "MATST","NOMMAT",
"CREDITOS_ACT","tipo_act","nota","result_act","fecha","sem_gen",     
"per_gen","anio_act","ecopuro","contpuro","admpuro","ecocont",       
"ecoadm","contadm","todas","tray_econ","tray_cont","tray_adm","ident1","barrio","residen",         
"mujer","fecha_nac","lug_nac","res_marzo","tip_viv","hij_num","viv_solo",
"ocup","ocup_ing","est_cony","padres","pad_ing","conyug","cony_ing","cony_pad",        
"cony_pad_ing","hijo_viv","hijo_ing","herman","herm_ing","abuelo","abu_ing","suegro",          
"sueg_ing","fam_otro","fam_ing","est_otro","est_ing","per_otro",        
"per_ing","edu_prim","tip_inst_pri","sec_1_5","tip_inst_sec",
"sec_6","tip_inst_sexto","egre_sec","ed_padre","ed_madre","hora_tra",
"inic_tra","rel_trab","cat_oc_est","ocup_est","cat_oc_pad","ocup_pad",
"cat_oc_mad","ocup_mad","beca","beca_fondo","beca_scbu","beca_otra","S",               
"FNACIMIENTO","LUGAR","AÑOFINESTUDIOS","INSTITUTO","COD_INSTITUTO","NOM_INSTITUTO",   
"LUG_INSTITUTO","NOMLUG_INSTITUTO","TIPO_INSTITUTO","FECHA_EGRESO",
"ANO_EGRESO","CARR_EGRESO","COD_CARR_EGRESO")]  

fusionadas<-rbind.data.frame(gen2012,gen2013,gen2014,gen2015)
fusionadas$n_estud<-as.factor(fusionadas$ci)
levels(fusionadas$n_estud)<-seq(1:length(levels(fusionadas$n_estud)))
fusionadas<-fusionadas[,c("n_estud","ci","NOMBRE","gen","carr","ciclo","mat", "MATST","NOMMAT",
"CREDITOS_ACT","tipo_act","nota","result_act","fecha","sem_gen",     
"per_gen","anio_act","ecopuro","contpuro","admpuro","ecocont",       
"ecoadm","contadm","todas","tray_econ","tray_cont","tray_adm","ident1","barrio","residen",         
"mujer","fecha_nac","lug_nac","res_marzo","tip_viv","hij_num","viv_solo",
"ocup","ocup_ing","est_cony","padres","pad_ing","conyug","cony_ing","cony_pad",        
"cony_pad_ing","hijo_viv","hijo_ing","herman","herm_ing","abuelo","abu_ing","suegro",          
"sueg_ing","fam_otro","fam_ing","est_otro","est_ing","per_otro",        
"per_ing","edu_prim","tip_inst_pri","sec_1_5","tip_inst_sec",
"sec_6","tip_inst_sexto","egre_sec","ed_padre","ed_madre","hora_tra",
"inic_tra","rel_trab","cat_oc_est","ocup_est","cat_oc_pad","ocup_pad",
"cat_oc_mad","ocup_mad","beca","beca_fondo","beca_scbu","beca_otra","S",               
"FNACIMIENTO","LUGAR","AÑOFINESTUDIOS","INSTITUTO","COD_INSTITUTO","NOM_INSTITUTO",   
"LUG_INSTITUTO","NOMLUG_INSTITUTO","TIPO_INSTITUTO","FECHA_EGRESO",
"ANO_EGRESO","CARR_EGRESO","COD_CARR_EGRESO")]  

write.csv(fusionadas,"fusionadas.csv")
write.dta(fusionadas,"fusionadas.dta")