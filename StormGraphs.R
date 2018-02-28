# Rename columns to be R friendly

newname<-c("sample_name","date_and_time","Q_m3s","Al_mgL","Ca_mgL","Fe_mgL","K_mgL","Mg_mgL","Mn_mgL","Na_mgL","SO42_mgL","pH","EC")
stormevents<-read.csv(file="StormEvents.csv",header=TRUE)
colnames(stormevents)<-newname

# Pull out date and time