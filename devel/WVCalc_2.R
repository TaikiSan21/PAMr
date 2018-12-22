library(PAMr)
library(PAMmisc)
library(sp)
library(splancs)
library(ggplot2)
library(PamBinaries)
# myFile<-'D:/Binaries/BW clicks/20071019/Click_Detector_Click_Detector_Clicks_20071019_160115.pgdf'
myFile <- '../Data/AnneHelp/Click_Detector_Click_Detector_Clicks_20071019_160115.pgdf'
BinData<-loadPamguardBinaryFile(myFile)
data<-BinData$data$`33001995`
n<-256
fs<-200000
Q<-.998
c<-1
plotWV<-TRUE


#select 300 samples around click center
if (length(data$wave[, c])<=301){clip<-data$wave
} else{
  ClickCenter<-round(nrow(data$wave)/2)
  clip<-data$wave[,c]
  clip<-clip[((ClickCenter-150):(ClickCenter+150))]}

#Wigner-Ville Transform of clip
WVList<-wignerTransform(clip,n,fs,plotWV)
wvMat<-t(WVList$tfr)

#Calculate contour around the 99% percentile of the plot
#### HERE CAN USE t AND f FROM WVList
#### UNSURE IF SHOULD START AT 0.
wvLines<-contourLines(WVList$t,WVList$f,wvMat,levels=quantile(wvMat,Q))

#If there are multiple contours, save the largest
blobL<-unlist(lapply(wvLines,function(x) length(x$x)))
blobLines<-wvLines[which.max(blobL)]
xvalues<-blobLines[[1]][["x"]]
yvalues<-blobLines[[1]][["y"]]

#Calculate sweep, duration and peak frequency
sweep<-max(yvalues) - min(yvalues)
wvlength<-max(xvalues) - min(xvalues)
MaxWV<-which(wvMat==max(wvMat),arr.ind=TRUE)
peakfreq<-WVList$f[MaxWV[2]]

#Create more points along the blob contour
xvalues[length(xvalues)+1]<-xvalues[1]
yvalues[length(yvalues)+1]<-yvalues[1]
BlobLine<-SpatialLines(list(Lines(Line(cbind(xvalues,yvalues)),ID="blob"))) #This might work better on a polygon like BlobPoly1 (line79)
ExtraPoints<-spsample(BlobLine,600,type="regular")
ExtraX<-ExtraPoints@coords[,1]
ExtraY<-ExtraPoints@coords[,2]

#Define upper and lower portions of blob by median yvalue
LowInd<-which(ExtraY<median(yvalues))
xLow<- ExtraX[LowInd]
yLow<-ExtraY[LowInd]
xHigh<- ExtraX[-LowInd]
yHigh<- ExtraY[-LowInd]

#calculate the midpoint between y values
midY<-numeric()
midX<-numeric()
for(l in (1:length(xLow))){
  match<-which.min(abs((xHigh-xLow[l])))
  if(length(match)==0){
    midY[l]<-NA
    midX[l]<-NA
  }else{midY[l]<-(yHigh[match]+yLow[l])/2
 midX[l]<-xLow[l]
   }}

#Create Polygon of x and y values. Find the area.
BlobXAll<-c(xLow,xHigh)
BlobYAll<-c(yLow,yHigh)

Blob<-cbind(BlobXAll,BlobYAll)
BlobOrder<-orderPoints(BlobXAll,BlobYAll,BlobXAll[1],BlobYAll[1],clockwise=TRUE)
BlobPoly<-Blob[BlobOrder,]
BlobA<-areapl(BlobPoly)

    # BlobPoly1<-Polygons(list(Polygon(BlobPoly)),ID="blob")

quarterTimes<-seq(from=min(BlobXAll),to=max(BlobXAll),length=5)
Blob<-Blob[order(Blob[,1]),]
BlobQA<-vector()

for (b in (1:4)){
  Quarters<-which(Blob[,1]>=quarterTimes[b] & Blob[,1]<=quarterTimes[b+1])
  BlobQ<-Blob[Quarters,]
  BlobQOrder<-orderPoints(BlobQ[,1],BlobQ[,2],mean(BlobQ[,1]),mean(BlobQ[,2]),clockwise=TRUE)
  BlobQPoly<-BlobQ[BlobQOrder,]
  BlobQA[b]<-areapl(BlobQPoly)
  BlobQ<-vector()
  BlobQOrder<-vector()
  BlobQPoly<-vector()}

#Calculate Slope of Midline
BlobLM<-lm(midY~midX)
slopeM<-as.numeric(BlobLM$coefficients[2])

#Divide midline into quarters based on time - if there aren't at least 12 points, interpolate
#Calculate mean slope of midline quartiles using linear regression
slopeQ<-numeric()
quarterTimes<- seq(from=min(midX),to=max(midX),length=5)
if(length(midX)<=12){
  ExtraX<-seq(from=min(midX),to=max(midX),length=20)
}else{ExtraX<-midX}

midlineMod<-smooth.spline(midX,midY)
midlineSpline<-predict(midlineMod,ExtraX)
midSplineY<-midlineSpline$y

for(m in 1:4){
  Quarters<-which(ExtraX>=quarterTimes[m] & ExtraX<=quarterTimes[m+1])
  slopeQLM<-lm(midSplineY[Quarters]~ExtraX[Quarters])
  slopeQ[m]<-as.numeric(slopeQLM$coefficients[2])
}
remove(quarterTimes)
remove(ExtraX)

#Find Ridge along peak frequencies within the blob
RidgeStart<-min(which(WVList$t>=min(xvalues)))
RidgeStop<-max(which(WVList$t<max(xvalues)))
RidgeX<-RidgeStart:RidgeStop
RidgeTime<-WVList$t[RidgeX]

RidgePeak<-numeric(length(RidgeX))
for (r in 1:length(RidgeX)){
  SliceMax<-max(wvMat[RidgeX[r],])
  RidgePeak[r]<-WVList$f[which(wvMat[RidgeX[r],]==SliceMax)]
}
#Calculate slope of Ridge (peak frequencies)
slopeRLM<-lm(RidgePeak~RidgeTime)
slopeRM<-as.numeric(slopeRLM$coefficients[2])

#Divide Ridge into quarters based on time - if there aren't at least 12 points, interpolate
#Calculate mean slope of peak ridge quarters using linear regression
slopeRQ<-numeric()
quarterTimes<- seq(from=min(RidgeTime),to=max(RidgeTime),length=5)
if(length(RidgeTime)<=12){
  ExtraX<-seq(from=min(RidgeTime),to=max(RidgeTime),length=20)
}else{ExtraX<-RidgeTime}

RidgeMod<-smooth.spline(RidgeTime,RidgePeak)
RidgeSpline<-predict(RidgeMod,ExtraX)
ExtraRidge<-RidgeSpline$y

for(r in 1:4){
  Quarters<-which(ExtraX>=quarterTimes[r] & ExtraX<=quarterTimes[r+1])
  slopeRQLM<-lm(ExtraRidge[Quarters]~ExtraX[Quarters])
  slopeRQ[r]<-as.numeric(slopeRQLM$coefficients[2])
}
remove(ExtraX)
remove(quarterTimes)

#Need to add MaxDist to this once the midline is figured out
CHWVParams<-data.frame(WVsweep=sweep,WVdur=wvlength,WVpeakfreq=peakfreq,slopeQ1=slopeQ[1],slopeQ2=slopeQ[2],
                       slopeQ3=slopeQ[3],slopeQ4=slopeQ[4],slopeM=slopeM,slopeRQ1=slopeRQ[1],slopeRQ2=slopeRQ[2],
                       slopeRQ3=slopeRQ[3],slopeRQ4=slopeRQ[4],slopeRM=slopeRM, BlobA=BlobA,
                       BlobQA1=BlobQA[1],BlobQA2=BlobQA[2],BlobQA3=BlobQA[3],BlobQA4=BlobQA[4])
if(c==1){
  WVParams<-CHWVParams
}else{WVParams<-rbind(CHWVParams,WVParams)}
}

return(WVParams)
}
