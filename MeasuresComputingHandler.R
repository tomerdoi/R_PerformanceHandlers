require(ROCR)
require(zoo)
require(prediction)
install.packages('prediction')
library(prediction)
library(ROCR)
library(zoo)


fastAUC <- function(probs, class) {
  x <- probs
  y <- class
  x1 = x[y==1]; n1 = length(x1); 
  x2 = x[y==0]; n2 = length(x2);
  r = rank(c(x1,x2))  
  auc = (sum(r[1:n1]) - n1*(n1+1)/2) / n1 / n2
  return(auc)
}


calculateEqualError <- function( userScores, impostorScores ) {
  
  predictions <- c( userScores, impostorScores );
  labels <- c( rep( 0, length( userScores ) ),
               rep( 1, length( impostorScores ) ) );
  
  pred <- prediction( predictions, labels );
  
  missrates <- pred@fn[[1]] / pred@n.pos[[1]];
  farates <- pred@fp[[1]] / pred@n.neg[[1]];
  
  # Find the point on the ROC with miss slightly >= fa, and the point
  # next to it with miss slightly < fa.
  
  dists <- missrates - farates;
  idx1 <- which( dists == min( dists[ dists >= 0 ] ) );
  idx2 <- which( dists == max( dists[ dists < 0 ] ) );
  stopifnot( length( idx1 ) == 1 );
  stopifnot( length( idx2 ) == 1 );
  stopifnot( abs( idx1 - idx2 ) == 1 );
  
  # Extract the two points as (x) and (y), and find the point on the
  # line between x and y where the first and second elements of the
  # vector are equal.  Specifically, the line through x and y is:
  #   x + a*(y-x) for all a, and we want a such that
  #   x[1] + a*(y[1]-x[1]) = x[2] + a*(y[2]-x[2]) so
  #   a = (x[1] - x[2]) / (y[2]-x[2]-y[1]+x[1])
  
  x <- c( missrates[idx1], farates[idx1] );
  y <- c( missrates[idx2], farates[idx2] );
  a <- ( x[1] - x[2] ) / ( y[2] - x[2] - y[1] + x[1] );
  eer <- x[1] + a * ( y[1] - x[1] );
  
  return( eer );
}

calcTPRForFPRZero<-function (scores, labels)
{
  maxBenignScore=max(scores[which(labels==0)])
  TP=length(scores[which(labels==1 & scores>maxBenignScore)])
  FN=length(scores[which(labels==1 & scores<maxBenignScore)])
  TPR= TP/(TP+FN)
  return (TPR)
}

calcTPRForFPR0.001<-function (scores, labels)
{
  maxBenignScore=tail(sort(scores),round(length(scores)*0.001))[1]
  TP=length(scores[which(labels==1 & scores>maxBenignScore)])
  FN=length(scores[which(labels==1 & scores<maxBenignScore)])
  TPR= TP/(TP+FN)
  return (TPR)
}

calcTPRForFPR0.01<-function (scores, labels)
{
  maxBenignScore=tail(sort(scores),round(length(scores)*0.01))[1]
  TP=length(scores[which(labels==1 & scores>maxBenignScore)])
  FN=length(scores[which(labels==1 & scores<maxBenignScore)])
  TPR= TP/(TP+FN)
  return (TPR)
}

calcTPRForFPR0.05<-function (scores, labels)
{
  maxBenignScore=tail(sort(scores),round(length(scores)*0.05))[1]
  TP=length(scores[which(labels==1 & scores>maxBenignScore)])
  FN=length(scores[which(labels==1 & scores<maxBenignScore)])
  TPR= TP/(TP+FN)
  return (TPR)
}

calcTPRForFPR0.1<-function (scores, labels)
{
  maxBenignScore=tail(sort(scores),round(length(scores)*0.1))[1]
  TP=length(scores[which(labels==1 & scores>maxBenignScore)])
  FN=length(scores[which(labels==1 & scores<maxBenignScore)])
  TPR= TP/(TP+FN)
  return (TPR)
}












calcFNRForFPRZero<-function (scores, labels)
{
  maxBenignScore=max(scores[which(labels==0)])
  TP=length(scores[which(labels==1 & scores>maxBenignScore)])
  FN=length(scores[which(labels==1 & scores<maxBenignScore)])
  FNR= FN/(TP+FN)
  return (FNR)
}

calcFNRForFPR0.001<-function (scores, labels)
{
  maxBenignScore=tail(sort(scores),round(length(scores)*0.001))[1]
  TP=length(scores[which(labels==1 & scores>maxBenignScore)])
  FN=length(scores[which(labels==1 & scores<maxBenignScore)])
  FNR= FN/(TP+FN)
  return (FNR)
}

calcFNRForFPR0.01<-function (scores, labels)
{
  maxBenignScore=tail(sort(scores),round(length(scores)*0.01))[1]
  TP=length(scores[which(labels==1 & scores>maxBenignScore)])
  FN=length(scores[which(labels==1 & scores<maxBenignScore)])
  FNR= FN/(TP+FN)
  return (FNR)
}

calcFNRForFPR0.05<-function (scores, labels)
{
  maxBenignScore=tail(sort(scores),round(length(scores)*0.05))[1]
  TP=length(scores[which(labels==1 & scores>maxBenignScore)])
  FN=length(scores[which(labels==1 & scores<maxBenignScore)])
  FNR= FN/(TP+FN)
  return (FNR)
}

calcFNRForFPR0.1<-function (scores, labels)
{
  maxBenignScore=tail(sort(scores),round(length(scores)*0.1))[1]
  TP=length(scores[which(labels==1 & scores>maxBenignScore)])
  FN=length(scores[which(labels==1 & scores<maxBenignScore)])
  FNR= FN/(TP+FN)
  return (FNR)
}












calculateMeasuresOfScoresFolder <- function(folderPath)
  
{
  
  labelsArp <- read.csv('D:/datasets/KitsuneDatasets/labels/ip_ARP_isBad.csv',sep=",",header=TRUE, skip = 1000000)
  labelsFuz <- read.csv('D:/datasets/KitsuneDatasets/labels/ip_FUZ_isBad.csv',sep=",",header=TRUE, skip = 1000000)
  labelsPHD <- read.csv('D:/datasets/KitsuneDatasets/labels/ip_PHD_isBad.csv',sep=",",header=TRUE, skip = 1000000)
  labelsPHD2 <- read.csv('D:/datasets/KitsuneDatasets/labels/ip_PHD2_isBad.csv',sep=",",header=TRUE, skip = 1000000)
  labelsPort <- read.csv('D:/datasets/KitsuneDatasets/labels/ip_PORT_isBad.csv',sep=",",header=TRUE, skip = 1000000)
  labelsRTSP4 <- read.csv('D:/datasets/KitsuneDatasets/labels/ip_RTSP4_isBad.csv',sep=",",header=TRUE, skip = 1000000)
  labelsSSDP <-  read.csv('D:/datasets/KitsuneDatasets/labels/ip_SSDP_isBad.csv',sep=",",header=TRUE, skip = 1000000)
  labelsSSL <-  read.csv('D:/datasets/KitsuneDatasets/labels/ip_SSL_isBad.csv',sep=",",header=TRUE, skip = 1000000)
  labelsSSL2 <-  read.csv('D:/datasets/KitsuneDatasets/labels/ip_SSL2_isBad.csv',sep=",",header=TRUE, skip = 1000000)
  labelsSYN <-  read.csv('D:/datasets/KitsuneDatasets/labels/ip_SYN_isBad.csv',sep=",",header=TRUE, skip = 1000000)
  labelsRTSP <- read.csv('D:/datasets/KitsuneDatasets/labels/RTSP_labels.csv',sep=",",header=TRUE, skip = 1000000)
  
  scoreFilesList=list.files(folderPath)
  print('name,auc,eer,TPR0.001,TPR0.01,TPR0.05,TPR0.1,TPRForZeroFP,FNR0.001,FNR0.01,FNR0.05,FNR0.1,FNRForZeroFP')
  for (f in scoreFilesList)
  {
    
    fileLength=length(count.fields(paste(folderPath,f, sep="")))
    fileLength=fileLength*1
    if (fileLength<1000000)
      next
    
    data <- read.csv(paste(folderPath,f, sep=""),sep=",",header=FALSE, skip = 1000000)
    scores=data
    if (grepl('SYN_lab',f)==TRUE)
    {
      
      
      labels=labelsSYN
      name='SYN_lab'
    }
    
    
    else if (grepl('etterArp',f)==TRUE)
    {
      
      
      labels=labelsArp
      name='etterArp'
      
      
    }
    
    else if (grepl('fuzzing',f)==TRUE)
    {
      
      
      labels=labelsFuz
      name='fuzzing'
    }
    
    else if (grepl('Passive_Sniffing',f)==TRUE)
    {
      
      
      labels=labelsArp
      name='Passive_Sniffing'
    }
    
    else if (grepl('phiddle',f)==TRUE)
    {
      
      
      labels=labelsPHD2
      name='phiddle'
    }
    
    else if (grepl('port_scan',f)==TRUE)
    {
      
      
      labels=labelsPort
      name='port_scan'
    }
    
    else if (grepl('RTSP_RMSE',f)==TRUE)
    {
      
      
      labels=labelsRTSP
      name='RTSP_RMSE'
    }
    
    else if (grepl('RTSP_4',f)==TRUE)
    {
      
      
      labels=labelsRTSP4
      name='RTSP_4'
    }
    
    
    
    else if (grepl('SSDP',f)==TRUE)
    {
      
      
      labels=labelsSSDP
      name='SSDP'
    }
    
    else if (grepl('SSL_lab',f)==TRUE)
    {
      
      
      labels=labelsSSL
      name='SSL_lab'
    }
    
    else if (grepl('ssl_renego',f)==TRUE)
    {
      
      
      labels=labelsSSL2
      name='ssl_renego'
    }
    
   
    else
      next
    
    auc=fastAUC(scores$V1,labels$X0)
    eer=calculateEqualError(labels$X0,scores$V1)
    TPR0.001=calcTPRForFPR0.001(scores$V1,labels$X0)
    TPR0.01=calcTPRForFPR0.01(scores$V1,labels$X0)
    TPR0.05=calcTPRForFPR0.05(scores$V1,labels$X0)
    TPR0.1=calcTPRForFPR0.1(scores$V1,labels$X0)
    TPRForZeroFP=calcTPRForFPRZero(scores$V1,labels$X0)

    FNR0.001=calcFNRForFPR0.001(scores$V1,labels$X0)
    FNR0.01=calcFNRForFPR0.01(scores$V1,labels$X0)
    FNR0.05=calcFNRForFPR0.05(scores$V1,labels$X0)
    FNR0.1=calcFNRForFPR0.1(scores$V1,labels$X0)
    FNRForZeroFP=calcFNRForFPRZero(scores$V1,labels$X0)
    print(paste(name,paste(auc,paste(eer,paste(TPR0.001,paste(TPR0.01,paste(TPR0.05,paste(TPR0.1,paste(TPRForZeroFP,paste(FNR0.001,paste(FNR0.01,paste(FNR0.05,paste(FNR0.1,FNRForZeroFP, sep=","), sep=","), sep=","), sep=","), sep=","), sep=","), sep=","), sep=","), sep=","), sep=","),sep=","),sep=","))
  }
  
}

