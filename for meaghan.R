names(StormEvents)<- c("sample", "Date_time", "discharge", "Al","Ca","Fe","K","Mg","Mn","Na",
                       "SO42","pH","EC")
library(ggplot2)
library(lubridate)
StormEvents$newdate<-mdy_hm(StormEvents$Date_time)

yscale<-1000 #fiddle with scaling here

two_ys<-ggplot(StormEvents, aes(x=newdate))+
  geom_line(aes(y=discharge), colour="blue", size=1.2)+
  geom_point(aes(y=Fe/yscale, fill="orange"), colour="black", pch=21, size=2)+
  geom_point(aes(y=Mn/yscale, fill="green"), pch=21, colour="black", size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale, name="Concentration"))+
  scale_fill_manual(guide="legend", name="ions", 
                     values = c("orange", "green"), labels=c("Fe","Mn"))+
  labs(y = "Discharge",
       x = "Date")+
  theme_bw()

two_ys