#SET UP FOR SOUNDTRAPS!!
#change parameters for SM3M and SM2

setwd("C:/Users/emily.griffiths/Documents/PASCAL/BW_binaryoutput/clicks/")
#Get all your packages
library(seewave)
library(RSQLite)
library(plyr)
library(tuneR)
library(signal)
library(gam)
unittype="SoundTrap"

etemp=list.files("S:/1650_PASCAL_2016/Data/DASBR_Pamguard_Post_Processing/Database/Final_BWDataset_w_MesID_20170906", pattern ="Events.csv", full.names = TRUE)
#Test folder
#etemp=list.files("../../test_clicks/", pattern="Events.csv", full.names = TRUE)


all_events=NULL
for (i in 1:length(etemp)) {
  a=read.csv(etemp[i], stringsAsFactors = FALSE)
  b=a[,c(2:9,14)]
  b$station=gsub( "_MAS.*$", "", basename(etemp[i]))
  b$Drift_EvtID=paste0(b$station,"_",b$Id)
  all_events=rbind(all_events,b)

}

#From PG.
binsize=4000
NumChs=2

#ST= 288000, SM3M= 256000, SM2= 192000
f=288000
#FFT selection is based so our final, filtered clip will be the same length used by Baumann-Pickering.
FFTsize=736 #ST=736, SM3M= 654, SM2= 456
nyq= FFTsize/2


sc_rds=list.files(paste0("./StrongClicks/",unittype,"/"), pattern="RDS",full.names = TRUE)
#sc_rds=list.files(paste0("./StrongClicks/old/",unittype,"2/"), pattern="Station-1_",full.names = TRUE)

#Drifts which had a HTI-92wb in Ch0 rather than an HTI-96min
HTI92=c("Station-1","Station-3","Station-5","Station-6","Station-8","Station-9","Station-10","Station-12","Station-15","Station-16","Station-19","Station-22","Station-23","Station-24","Station-27-30")

HP_96_calibration= read.csv("C:/Users/emily.griffiths/Documents/PASCAL/Frequency response of HTI-96min hydrophone from SM3M manual.csv")
HP_92_calibration= read.csv("C:/Users/emily.griffiths/Documents/PASCAL/Frequency response of HTI-92 hydrophone from SM3M manual.csv")

# fit GAM to hydrophone HTI96 calibration data
GamOutput96= gam(Sensitivity~s(Freq,50),data=HP_96_calibration)
plot(GamOutput96)
# fit GAM to HTI 92 calibration data
GamOutput92= gam(Sensitivity~s(Freq,50),data=HP_92_calibration)
plot(GamOutput92)


Descriptive_stats=NULL
for (k in 1:length(sc_rds)) {
  #Bring in the RDS data that stores the individual click binary wave info.
  b=basename(sc_rds)[k]
  station=gsub( ".RDS.*$", "", b)
  foo=readRDS(sc_rds[k])

  if (length(foo)==0) next
  #Shorten the station name to match the HTI92 vector above.
  HTI92ch0=gsub("_Sound.*$","",station)

  DSalmost=NULL
  #dir.create(paste0("./TKenergyplots/",station))
  #Isolate each click to process separately.
  for(i in 1:length(foo)) {
    if (is.na(lengths(foo)[i])) next
    if (lengths(foo)[i]==0) next
    BinaryFile=names(foo[i])
    DStemp=NULL
    for (j in 1:length(foo[[i]])) {
      ClickNo=names(foo[[i]])[j]
      if (is.na(ClickNo)) next
      #Find event data information
      thisevent=all_events[(all_events$Drift_EvtID==station),]
      com=thisevent[,c("eventType","comment","Drift_EvtID")]
      DSc=NULL
      FFF=foo[[i]][[j]]


      TKfunc.left= TKEO(FFF@left,f=f,M=1,plot = FALSE)
      # ignore first 0.001 sec to avoid start-up artifacts
      TKenergy= TKfunc.left[(0.001*f):length(TKfunc.left[,2]),2]   #second column in output is energy
      n= length(TKenergy)
      BinCount= 1:n # not used
      # convert energy to dB scale (note multiplier is 10 for energy, 20 for sound pressure)
      TKenergyDB= 10*log10(TKenergy-min(TKenergy,na.rm=TRUE))
      # normalize to 0 dB max
      TKenergyDB= TKenergyDB - max(TKenergyDB,na.rm=TRUE)
      TKenergyDB[!is.finite(TKenergyDB)] <- NA

      noiselevel=median(TKenergyDB,na.rm=TRUE)
      if (noiselevel > -20) next

      #Duration

      #Find noise threshold by multiplying all energy above the 40% threshold by 100.
      noisethreshold=quantile(TKfunc.left[,2],probs = .40, na.rm = TRUE)*100
      #Subset the TK function for energy above the 40% threshold.
      dur=subset(TKfunc.left,TKfunc.left[,2]>= noisethreshold)
      #Subtract the max time value from the minimum time value for the click duration.
      duration=1000000*(max(dur[,1])-min(dur[,1]))


      #Create spectrum.
      bar=spec(FFF@left,f=f,wl=FFTsize, norm = FALSE, correction = "amplitude", plot=F)
      #Convert amplitude to relative dB.
      reldB=(20*log10(bar[,2]))
      #Conver -Inf to NA
      reldB[!is.finite(reldB)] <- NA

      #Calibration Curve
      newClick=data.frame(Freq=(bar[,1]*1000),Sensitivity = reldB)


      #Apply the correct Calibration Curve
      ch092= ifelse(HTI92ch0 %in% HTI92,"T","F")
      ch=ifelse(grepl("ch0",ClickNo, fixed=T),"T","F")
      if(ch=="T" & ch092=="T") {
        PredHP92Values= predict.gam(GamOutput92,newdata=newClick)
        # normalize values relative to value at 1kHz
        PredHP92Values= PredHP92Values - PredHP92Values[1]
        clicksens=reldB-PredHP92Values
      }
      if(ch=="F" | ch092=="F") {
        PredHP96Values= predict.gam(GamOutput96,newdata=newClick)
        # normalize values relative to value at 1kHz
        PredHP96Values= PredHP96Values - PredHP96Values[1]
        clicksens=reldB-PredHP96Values
      }

      #Adjust so the peak is at 0.
      Adj4zero=clicksens-max(clicksens)
      TotCalibr= cbind(newClick$Freq/1000,Adj4zero)


      dbBW10=Q(TotCalibr,f=f,level=-10,plot = FALSE)
      DS=as.data.frame(t(unlist(dbBW10)))
      names(DS)=c("Q_10db", "PeakHz_10dB", "fmin_10db","fmax_10db", "BW_10db")
      DS$centerHz_10db=DS$fmax_10db-(DS$BW_10db/2)

      dbBW3=Q(TotCalibr,f=f,level=-3, plot = FALSE)
      temp=as.data.frame(t(unlist(dbBW3)))
      names(temp)=c("Q_3db", "PeakHz_3dB", "fmin_3db","fmax_3db", "BW_3db")
      temp$centerHz_3db=temp$fmax_3db-(temp$BW_3db/2)

      DS=cbind(DS,temp)
      DSc=cbind(DS,com)
      DSc$duration=duration
      DSc$ClickNo=ClickNo
      DSc$BinaryFile=BinaryFile
      DSc$noiselevel=noiselevel
      DStemp=rbind(DStemp,DSc)
    }
    DSalmost=rbind(DSalmost,DStemp)
  }
  Descriptive_stats=rbind(Descriptive_stats,DSalmost)
  #assign(station,foo)
}

Final=Descriptive_stats[,c(15,17,13,14,16,1:12,19,18)]
write.csv(Final,"Raw_DescriptiveStatistics_ST_PASCAL_BWs_20180119.csv", row.names = FALSE)
