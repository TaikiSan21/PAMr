#Calculate features from Wigner-Ville transform
#Anne Simonis 29Nov2018

#Inputs
#data: output from click detector, containing 2 parts (data$wave = waveform and data$sampleRate = sample rate)
#n: size of the output 
#fs: sample rate
#Q: Quantile of energy to define contour in WV transform [0 - 1], Example: 99.8% would be Q=.998
#plotWV: logical to produce a plot of the Wigner-Ville Transform

#Outputs
#WVsweep: distance between highest and lowest frequency (Units=Hz)
#WVdur: time between start and stop (Units=S)
#WVpeakfreq: peak frequency (Units=Hz)
#Wvmiddist: maximum distance from the midline (Units=Hz)
#WVpeakdist: maximum distance from the peak ridge (Units=Hz)
#slope(Q1:Q4)M: Mean slope in each quarter of the midline (Units = Hz/s)
#slopeM: Mean slope of the midline (Units=Hz/s)
#slopeR(Q1:Q4)M: Mean slope of the ridge of peak frequencies (Units = Hz/s)
#slopeRM: Mean slope of the ridge of peak frequencies (Units=Hz/s)
#BlobA: Area of the blob (Units=?)
#BlobA(Q1:Q4): Area of each quarter of the blob  (Units=?)
#WVConcave: Is the blob concave (T/F)

#Requires packages: Rmisc, contoureR, splancs, sp,raster,contoureR,rgeos

WVCalc<-function(data,n=256,fs=200000,Q=.998,plotWV=FALSE){
 
   for (c in 1:ncol(data$wave)){
    
     #select 300 samples around click center
     if (length(data$wave[,c])<300){clip<-data$wave
     } else{
       ClickCenter<-round(nrow(data$wave)/2)
       clip<-data$wave[,c]
       clip<-clip[((ClickCenter-150):(ClickCenter+150))]}
     
     #Wigner-Ville Transform of clip
     WVList<-wignerTransform(clip,n,fs,plotWV)
     wvMat<-t(WVList$tfr)
     
     #Calculate contour around the 99% percentile of the plot
     wvLines<-contourLines(WVList$t,WVList$f,wvMat,levels=quantile(wvMat,Q))
     
     #If there are multiple contours, save the largest 
     blobL<-unlist(lapply(wvLines,function(x) length(x$x)))
     blobLines<-wvLines[which.max(blobL)]
     xvalues<-blobLines[[1]][["x"]]
     yvalues<-blobLines[[1]][["y"]]
     BlobLine<-SpatialLines(list(Lines(Line(cbind(xvalues,yvalues)),ID="blob"))) 
     
     #Calculate sweep, duration and peak frequency
     sweep<-max(yvalues) - min(yvalues) 
     wvlength<-max(xvalues) - min(xvalues) 
     MaxWV<-which(wvMat==max(wvMat),arr.ind=TRUE)
     peakfreq<-WVList$f[MaxWV[2]]
     
     #calculate the midpoint between y values between outermost edges of blob and the Ridge of peak frequencies 
     #Create logical "Concavity" parameter if blob is convex, which may be troublesome later
     midY<-numeric()
     RidgePeak<-numeric()
     middist<-numeric()
     peakdist<-numeric()
     concavity<-FALSE
     vertY<-seq(min(yvalues),max(yvalues),length.out=5)
     
     for (m in 1:length(xvalues)){
       vertX<-rep(xvalues[m],5)
       vertline<-SpatialLines(list(Lines(Line(cbind(vertX,vertY)),ID="vertline")))
       BlobInt<-gIntersection(BlobLine,vertline) #this sometimes throws errors - need to skip over bad points 
       
       # points(BlobInt@coords,col="red",pch='x')
       midY[m]<-mean(BlobInt@bbox[2,])
       middist[m]<-max(abs( BlobInt@bbox[2,] - midY[m]))
       if (length( BlobInt@bbox[2,])>2){concavity=T}
       
       SliceY<-which(WVList$f>min( BlobInt@bbox[2,]) & WVList$f<max( BlobInt@bbox[2,]))
       if (length(SliceY)>=1){ 
         TimeSamp<-min(which(WVList$t>=xvalues[m]))
         SliceMax<-which.max(wvMat[TimeSamp,SliceY])
         RidgePeak[m]<-WVList$f[SliceY[SliceMax]]
         peakdist[m]<-max(abs( BlobInt@bbox[2,] - RidgePeak[m]))
       }else{RidgePeak[m]<-NA}
     }
     
     WVmiddist<-max(middist)
     WVpeakdist<-max(peakdist[is.finite(peakdist)])
     
     Ridge<-cbind(xvalues,RidgePeak)
     Ridge<-Ridge[is.finite(Ridge[,2]),] #Omit NA Values
     
     #Create Polygon of x and y values. Find the area. 
     Blob<-cbind(xvalues,yvalues)
     BlobOrder<-orderPoints(xvalues,yvalues,xvalues[1],yvalues[1],clockwise=TRUE)
     BlobPoly<-Blob[BlobOrder,]
     BlobA<-areapl(BlobPoly)
     
     #Calculate quarterly values
     #If there aren't enough points to draw blob-lettes, subsample additional points along the blob
     quarterTimes<-seq(from=min(xvalues),to=max(xvalues),length=5)
     Blob<-Blob[order(Blob[,1]),]
     BlobQA<-vector()
     
     for (b in (1:4)){
       Quarters<-which(Blob[,1]>=quarterTimes[b] & Blob[,1]<=quarterTimes[b+1])
       if (length(Quarters)<4){
         ExtraPoints<-spsample(BlobLine,100,type="regular")
         Blob<-ExtraPoints@coords
         Quarters<-which(Blob[,1]>=quarterTimes[b] & Blob[,1]<=quarterTimes[b+1])
       }
       BlobQ<-Blob[Quarters,]
       BlobQOrder<-orderPoints(BlobQ[,1],BlobQ[,2],mean(BlobQ[,1]),mean(BlobQ[,2]),clockwise=TRUE)
       BlobQPoly<-BlobQ[BlobQOrder,]
       BlobQA[b]<-areapl(BlobQPoly)
       BlobQ<-vector()
       BlobQOrder<-vector()
       BlobQPoly<-vector()}
     
     #Calculate Slope of Midline
     BlobLM<-lm(midY~xvalues,na.action = na.omit)
     slopeM<-as.numeric(BlobLM$coefficients[2])
     
     #Divide midline into quarters based on time
     #Calculate mean slope of midline quartiles using linear regression 
     slopeQ<-numeric()
     quarterTimes<- seq(from=min(xvalues),to=max(xvalues),length=5)
     for(m in 1:4){ 
       Quarters<-which(xvalues>=quarterTimes[m] & xvalues<=quarterTimes[m+1])
       if (length(Quarters)<4){
         ExtraX<-seq(min(xvalues),max(xvalues),length=20)
         MidMod<-smooth.spline(midY~xvalues)
         ExtraMid<-predict(MidMod,ExtraX)
         midY<-ExtraMid$y
         Quarters<-which(xvalues>=quarterTimes[m] & xvalues<=quarterTimes[m+1])
       }
       slopeQLM<-lm(midY[Quarters]~xvalues[Quarters],na.action = na.omit)
       slopeQ[m]<-as.numeric(slopeQLM$coefficients[2])
     }
     
     #Calculate slope of Ridge (peak frequencies)
     slopeRLM<-lm(Ridge[,2]~Ridge[,1],na.action = na.omit)
     slopeRM<-as.numeric(slopeRLM$coefficients[2])
     
     #Divide Ridge into quarters based on time
     #Calculate mean slope of peak ridge quarters using linear regression
     slopeRQ<-numeric()
     
     for(r in 1:4){ 
       Quarters<-which(Ridge[,1]>=quarterTimes[r] & Ridge[,1]<=quarterTimes[r+1])
       if (length(Quarters)<4){
         ExtraX<-seq(min(Ridge[,1]),max(Ridge[,1]),length=20)
         RidgeMod<-smooth.spline(Ridge[,2]~Ridge[,1])
         ExtraRidge<-predict(RidgeMod,ExtraX)
         Ridge<-cbind(ExtraRidge$x,ExtraRidge$y)
         Quarters<-which(Ridge[,1]>=quarterTimes[r] & Ridge[,1]<=quarterTimes[r+1])
         }
       slopeRQLM<-lm(Ridge[Quarters,2]~Ridge[Quarters,1],na.action = na.omit)
       slopeRQ[r]<-as.numeric(slopeRQLM$coefficients[2])
     }
     
     #Need to add MaxDist to this once the midline is figured out 
     CHWVParams<-data.frame(WVsweep=sweep,WVdur=wvlength,WVpeakfreq=peakfreq,WVmiddist=WVmiddist,WVpeakdist=WVpeakdist,
                            slopeQ1=slopeQ[1],slopeQ2=slopeQ[2],slopeQ3=slopeQ[3],slopeQ4=slopeQ[4],slopeM=slopeM,
                            slopeRQ1=slopeRQ[1],slopeRQ2=slopeRQ[2],slopeRQ3=slopeRQ[3],slopeRQ4=slopeRQ[4],slopeRM=slopeRM,
                            BlobA=BlobA,BlobAQ1=BlobQA[1],BlobAQ2=BlobQA[2],BlobAQ3=BlobQA[3],BlobAQ4=BlobQA[4],WVConcave=concavity)
     if(c==1){
       WVParams<-CHWVParams
     }else{WVParams<-rbind(CHWVParams,WVParams)}
   }
  WVParams
  }