#Bring in mixing data
basecatmix<-read.csv("Mixing.csv",header=TRUE)

#Rename headings to be R friendly
newname<-c("sample_name","Site","date","Depth_cm","Al_mgL","Ca_mgL","Fe_mgL","K_mgL","Mg_mgL","Mn_mgL","Na_mgL")
colnames(basecatmix)<-newname

#convert date
library(lubridate)
basecatmix$newdate<-mdy(basecatmix$date)
basecatmix$doy<-yday(basecatmix$newdate)

#Transform mg/L to mmol/L

basecatmix$Al_mmolL<-(basecatmix$Al_mgL/26.982)
basecatmix$Ca_mmolL<-(basecatmix$Ca_mgL/40.08)
basecatmix$Fe_mmolL<-(basecatmix$Fe_mgL/55.845)
basecatmix$K_mmolL<-(basecatmix$K_mgL/39.099)
basecatmix$Mg_mmolL<-(basecatmix$Mg_mgL/24.305)
basecatmix$Mn_mmolL<-(basecatmix$Mn_mgL/54.938)
basecatmix$Na_mmolL<-(basecatmix$Na_mgL/22.99)

#Divide Ca by Mg and Na by K
basecatmix$Ca_Mg<-(basecatmix$Ca_mmolL/basecatmix$Mg_mmolL)
basecatmix$Na_K<-(basecatmix$Na_mmolL/basecatmix$K_mmolL)

#Plots base cation mixing diagram
library(ggplot2)
pal<-c("#ca0020","#f4a582","#0571b0","#c2a5cf")
shape1<-c(21, 22, 23, 24)
Basecatmixing<-ggplot(basecatmix, aes(basecatmix$Na_K,basecatmix$Ca_Mg,fill=as.factor(Site),shape=as.factor(Site)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nNa/K")+
  ylab("Ca/Mg")
#Call the graph
Basecatmixing