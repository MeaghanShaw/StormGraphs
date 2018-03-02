# Rename columns to be R friendly

newname<-c("sample_name","date_and_time","Q_m3s","Al_mgL","Ca_mgL","Fe_mgL","K_mgL","Mg_mgL","Mn_mgL","Na_mgL","SO42_mgL","pH","EC")
CQ<-read.csv(file="CQ.csv",header=TRUE)
colnames(CQ)<-newname

#Transform mg/L to mmol/L

CQ$Al_mmolL<-(CQ$Al_mgL/26.982)
CQ$Ca_mmolL<-(CQ$Ca_mgL/40.08)
CQ$Fe_mmolL<-(CQ$Fe_mgL/55.845)
CQ$K_mmolL<-(CQ$K_mgL/39.099)
CQ$Mg_mmolL<-(CQ$Mg_mgL/24.305)
CQ$Mn_mmolL<-(CQ$Mn_mgL/54.938)
CQ$Na_mmolL<-(CQ$Na_mgL/22.99)
CQ$SO42_mmolL<-(CQ$SO42_mgL/96.06)

#Transform pH to H+ mg/L

CQ$H_mgL<-(10^(-1*CQ$pH))

#Take the log of everything

CQ$logQ<-(log10(CQ$Q_m3s))
CQ$logAl<-(log10(CQ$Al_mmolL))
CQ$logCa<-(log10(CQ$Ca_mmolL))
CQ$logFe<-(log10(CQ$Fe_mmolL))
CQ$logK<-(log10(CQ$K_mmolL))
CQ$logMg<-(log10(CQ$Mg_mmolL))
CQ$logMn<-(log10(CQ$Mn_mmolL))
CQ$logNa<-(log10(CQ$Na_mmolL))
CQ$logSO42<-(log10(CQ$SO42_mmolL))
CQlogH<-(log10(CQ$H_mgL))
CQlogEC<-(log10(CQ$EC))


