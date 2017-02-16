rm(list=ls())
gc()
library(plyr)
directorio<-"C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Datos filiatorios"
setwd(directorio)
gen2012<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Actividades actualizadas\\Nuevas para actualizar\\gen2012_sindup_confest_compat.csv")
gen2013<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Actividades actualizadas\\Nuevas para actualizar\\gen2013_sindup_confest_compat.csv")
gen2014<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Actividades actualizadas\\Nuevas para actualizar\\gen2014_sindup_confest_compat.csv")
gen2015<-read.csv("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Actividades actualizadas\\Nuevas para actualizar\\gen2015_sindup_confest_compat.csv")

datos12<-read.csv("g_datosgen_2012_conv.csv")
datos13<-read.csv("g_datosgen_2013_conv.csv")
datos14<-read.csv("g_datosgen_2014_conv.csv")
datos15<-read.csv("g_datosgen_2015_conv.csv")

datos12$ci<-datos12$CEDULA
datos13$ci<-datos13$CEDULA
datos14$ci<-datos14$CEDULA
datos15$ci<-datos15$CEDULA


total12<-join_all(list(gen2012,datos12[,c("ci","S","FNACIMIENTO","LUGAR","AÑOFINESTUDIOS","INSTITUTO")]),by="ci")
total13<-join_all(list(gen2013,datos13[,c("ci","S","FNACIMIENTO","LUGAR","AÑOFINESTUDIOS","INSTITUTO")]),by="ci")
total14<-join_all(list(gen2014,datos14[,c("ci","S","FNACIMIENTO","LUGAR","AÑOFINESTUDIOS","INSTITUTO")]),by="ci")
total15<-join_all(list(gen2015,datos15[,c("ci","S","FNACIMIENTO","LUGAR","AÑOFINESTUDIOS","INSTITUTO")]),by="ci")

write.csv(total12,"gen2012_sindup_confest_compat_condatosgen.csv")
write.csv(total13,"gen2013_sindup_confest_compat_condatosgen.csv")
write.csv(total14,"gen2014_sindup_confest_compat_condatosgen.csv")
write.csv(total15,"gen2015_sindup_confest_compat_condatosgen.csv")