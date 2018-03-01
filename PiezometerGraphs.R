# Rename columns to be R friendly. 
#Piezocat is cation data from piezometers, BDP, and DP
newname<-c("sample_name","Site","date","Depth_cm","Al_mgL","Ca_mgL","Fe_mgL","K_mgL","Mg_mgL","Mn_mgL","Na_mgL")
piezocat<-read.csv(file="PiezometerData.csv",header=TRUE)
colnames(piezocat)<-newname
#Piezoan is anion data from piezometers, BDP, and DP
icnewname<-c("sample_name","Site","date","Depth_cm","SO42_mgL","Cl_mgL")
piezoan<-read.csv(file="ICPiezometer.csv",header=TRUE)
colnames(piezoan)<-icnewname

#Transform mg/L to mmol/L

piezocat$Al_mmolL<-(piezocat$Al_mgL/26.982)
piezocat$Ca_mmolL<-(piezocat$Ca_mgL/40.08)
piezocat$Fe_mmolL<-(piezocat$Fe_mgL/55.845)
piezocat$K_mmolL<-(piezocat$K_mgL/39.099)
piezocat$Mg_mmolL<-(piezocat$Mg_mgL/24.305)
piezocat$Mn_mmolL<-(piezocat$Mn_mgL/54.938)
piezocat$Na_mmolL<-(piezocat$Na_mgL/22.99)

piezoan$SO42_mmolL<-(piezoan$SO42_mgL/96.06)
piezoan$Cl_mmolL<-(piezoan$Cl_mgL/35.453)

#Access BDP subset of piezocat data

#BDPpiezocat<-piezocat[piezocat$sample_name == "BDP" | piezocat$sample_name == "P1"
                     # | piezocat$sample_name == "P2" | piezocat$sample_name == "P3"
                     # | piezocat$sample_name == "P4"]
#Why didn't this work? Mer.


#box and whisker plot of piezometer data and SO42-
plot(piezoan$sample_name,piezoan$SO42_mmolL)

#box and whisker plot of piezometer data and Ca
plot(piezocat$sample_name,piezocat$Ca_mmolL)


