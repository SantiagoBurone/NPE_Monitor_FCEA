rm(list=ls())
gc()
directorio<-"C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\FCEA\\Actividades actualizadas\\Nuevas para actualizar"
setwd(directorio)
#Gen 2012
gen2012<-read.csv("gen2012_sindup.csv",sep=",")
library(foreign)
ing2012<-read.dta("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\DGPlan\\Ingresos 2010_2015\\Ingresos 2010_2015\\ing_social_y_art_stica_2012_final.dta")
#Quienes no completaron el formulario:
#View(gen2012[which(as.integer(gen2012$ci)%in%as.integer(ing2012$c_i)==F),])
#Quienes completaron el formulario en esta facultad:
#table(as.integer(gen2012$ci)%in%as.integer(ing2012$c_i[which(ing2012$fac==levels(ing2012$fac)[3])]))
#unique(gen2012$ci[which(as.integer(gen2012$ci)%in%as.integer(ing2012$c_i[which(ing2012$fac==levels(ing2012$fac)[3])]))])
#aux1<-unique(gen2012$ci[which(as.integer(gen2012$ci)%in%as.integer(ing2012$c_i[which(ing2012$fac==levels(ing2012$fac)[3])]))])
library(plyr)
colnames(ing2012)[which(colnames(ing2012)=="c_i")]<-"ci"
total12<-join(gen2012,ing2012,by="ci")
write.csv(total12,"gen2012_sindup_confest.csv")
#Gen 2013
gen2013<-read.csv("gen2013_sindup.csv",sep=",")
ing2013<-read.dta("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\DGPlan\\Ingresos 2010_2015\\Ingresos 2010_2015\\ing_social_y_art_stica_2013_final.dta")
#Quienes no completaron el formulario:
#View(gen2013[which(as.integer(gen2013$ci)%in%as.integer(ing2013$c_i)==F),])
colnames(ing2013)[which(colnames(ing2013)=="c_i")]<-"ci"
total13<-join(gen2013,ing2013,by="ci")
write.csv(total13,"gen2013_sindup_confest.csv")
#Gen 2014
gen2014<-read.csv("gen2014_sindup.csv",sep=",")
ing2014<-read.dta("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\DGPlan\\Ingresos 2010_2015\\Ingresos 2010_2015\\ing_social_y_art_stica_2014.dta")
#Quienes no completaron el formulario:
#View(gen2014[which(as.integer(gen2014$ci)%in%as.integer(ing2014$c_i)==F),])
colnames(ing2014)[which(colnames(ing2014)=="c_i")]<-"ci"
total14<-join(gen2014,ing2014,by="ci")
write.csv(total14,"gen2014_sindup_confest.csv")
#Gen 2015
gen2015<-read.csv("gen2015_sindup.csv",sep=",")
ing2015<-read.spss("C:\\Users\\msilva\\Documents\\Mathias\\Bases\\Bases\\DGPlan\\Ingresos 2010_2015\\Ingresos 2010_2015\\2015\\Ingresos 2015 área soc y art.sav",to.data.frame=T)
#Quienes no completaron el formulario:
#View(gen2015[which(as.integer(gen2015$ci)%in%as.integer(ing2015$c_i)==F),])
colnames(ing2015)[which(colnames(ing2015)=="CI")]<-"ci"
total15<-join(gen2015,ing2015,by="ci")
write.csv(total15,"gen2015_sindup_confest.csv")

