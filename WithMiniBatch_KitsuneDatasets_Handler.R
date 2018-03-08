
require(ROCR)
require(zoo)

perf_bf_pcStream<- function (folderPath,totalResPath)
{
  resFileList=list.files(folderPath)
  
  
  startConnectdalmini =  40397
  startAttackdalmini =  40397
  
  startConnectecobee =  13113
  startAttackecobee =  13113
  
  startConnectennio =  34694
  startAttackennio =  34694
  
  startConnectphillips =  160139
  startAttackphillips =  160139
  
  startConnectprovision737=  55171
  startAttackprovision737 =  55171
  
  startConnectprovision838=  91557
  startAttackprovision838 =  91557
  
  startConnectsamsung =  46819
  startAttacksamsung =  46819
  
  startConnectsimplehome1002 =  42786
  startAttacksimplehome1002 = 42786
  
  startConnectsimplehome1003 =  17938
  startAttacksimplehome1003 =  17938
  
  
  
  
  #dictionary creation
  dicAuc=c()
  names(dicAuc)=c()
  
  dicEer=c()
  names(dicEer)=c()
  
  dicFileName=c()
  names(dicFileName)=c()
  
  for (f in resFileList)
  {
    
    fileLength=length(count.fields(paste(folderPath,f, sep="")))
    fileLength=fileLength*1
    
    
    data <- read.csv(paste(folderPath,f, sep=""),sep=",",header=TRUE)
    
    
    scores=data$score
    lab=data$label
    name='firstname'
    
    
    
    
    if (grepl('dalmini',f)==TRUE)
    {
      startConnect=startConnectdalmini
      startAttack=startAttackdalmini
      name='dalmini'
    }
    
    else if (grepl('ecobee',f)==TRUE)
    {
      startConnect=startConnectecobee
      startAttack=startAttackecobee
      name='ecobee'
    }
    
    else if (grepl('ennio',f)==TRUE)
    {
      startConnect=startConnectennio
      startAttack=startAttackennio
      name='ennio'
    }
    
    else if (grepl('phillips',f)==TRUE)
    {
      startConnect=startConnectphillips
      startAttack=startAttackphillips
      name='phillips'
    }
    
    else if (grepl('provision737',f)==TRUE)
    {
      startConnect=startConnectprovision737
      startAttack=startAttackprovision737
      name='provision737'
    }
    
    else if (grepl('provision838',f)==TRUE)
    {
      startConnect=startConnectprovision838
      startAttack=startAttackprovision838
      name='provision838'
    }
    
    else if (grepl('samsung',f)==TRUE)
    {
      startConnect=startConnectsamsung
      startAttack=startAttacksamsung
      name='samsung'
    }
    
    else if (grepl('simplehome1002',f)==TRUE)
    {
      startConnect=startConnectsimplehome1002
      startAttack=startAttacksimplehome1002
      name='simplehome1002'
    }
    
    
    else if (grepl('simplehome1003',f)==TRUE)
    {
      startConnect=startConnectsimplehome1003
      startAttack=startAttacksimplehome1003
      name='simplehome1003'
    }
    
    else
      next
    
    words <- strsplit(f, "_")
    name=paste(name,words[[1]][16],sep="_")
    
    limit=startConnect
    startConnect=fileLength-1-startConnect
    startAttack=fileLength-1-startAttack
    
    
    
    #dictionary init
    if (name %in% names(dicAuc)==FALSE)
    {
      dicAuc[name]=0
      dicEer[name]=1000000000
      
      
    }
    
    
    outPath=paste(name,'_pcStream_results.txt')
    #calc measures
    
    
    
    #scores=scores[limit:length(scores)]
    #lab=lab[limit:length(lab)]
    
    auc=fastAUC(scores,lab)
    
    eer=calculateEqualError(lab,scores)
    wideScan=widescan(scores,lab,startAttack )
    
    auc=fastAUC(scores,lab)
    
    
    travAuc=1-auc
    auc=max(auc,travAuc)
    zfpr=zFPR(scores,lab,startConnect,startAttack)
    
    
    
    if (auc>dicAuc[name]  )
    {
      
      dicAuc[name]=auc
      dicEer[name]=eer
      dicFileName[name]=f
    }
    
    else if (auc==dicAuc[name])
    {
      if (eer<dicEer[name])
      {
        dicAuc[name]=auc
        dicEer[name]=eer
        dicFileName[name]=f
      }
    }
    
    #write to files
    cat('Filename is: ', file=outPath, append=TRUE, sep = "")
    cat(f, file=outPath, append=TRUE, sep = "\n")
    
    cat('EqualError is: ', file=outPath, append=TRUE, sep = "")
    cat(eer, file=outPath, append=TRUE, sep = "\n")
    
    cat('Widescan is: TP,distance/TP,FN : ', file=outPath, append=TRUE, sep = "")
    cat(wideScan, file=outPath, append=TRUE, sep = "\n")
    
    cat('AUC is: ', file=outPath, append=TRUE, sep = "")
    cat(auc, file=outPath, append=TRUE, sep = "\n")
    
    cat('zfpr is: TP,TN,FP,FN,Dc,Da,TPz,TNz,FPz,FNz,Dcz,Daz,TPz2,TNz2,FPz2,FNz2,Dcz2,Daz2', file=outPath, append=TRUE, sep = "")
    for (e in zfpr)
    {
      cat(e, file=outPath, append=TRUE, sep = "\n")
    }
    
    cat('\n', file=outPath, append=TRUE, sep = "\n")
    print(paste('Finished ',f, sep=""))
    
  }
  
  #write total results
  
  for (k in names(dicAuc))
  {
    #cat('Filename is: ', file=totalResPath, append=TRUE, sep = "")
    cat(k, file=totalResPath, append=TRUE, sep = "")
    cat(',', file=totalResPath, append=TRUE, sep = "")
    
    
    #cat('EqualError is: ', file=totalResPath, append=TRUE, sep = "")
    cat(dicEer[k], file=totalResPath, append=TRUE, sep = "")
    cat(',', file=totalResPath, append=TRUE, sep = "")
    
    #cat('AUC is: ', file=totalResPath, append=TRUE, sep = "")
    cat(dicAuc[k], file=totalResPath, append=TRUE, sep = "\n")
    
  }
  
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
widescan <- function(scores,labels,startAttack)
{
  thr = max(scores[1:startAttack], na.rm=TRUE)+.0000001
  labels[is.na(labels)]=0
  labs = which(labels==1)
  
  w = 50;
  TP=0
  FP=0
  FN=0
  distance = 0
  
  N = length(scores)
  
  for( ind in labs)
  {
    lbound = max(ind-w,1)
    rbound = min(ind+w,N)
    count = sum(scores[lbound:rbound]>thr,na.rm = TRUE)
    if(count>0)
    {
      TP=TP+1
      distance = distance+ min(abs(which(scores[lbound:rbound]>thr)-(rbound-lbound)/2))
    }
    else
    {
      FN=FN+1
    }
  }
  return(c(TP,distance/TP,FN))
}
# Helpful function adapted from: https://stat.ethz.ch/pipermail/r-help/2005-September/079872.html
fastAUC <- function(probs, class) {
  x <- probs
  y <- class
  x1 = x[y==1]; n1 = length(x1); 
  x2 = x[y==0]; n2 = length(x2);
  r = rank(c(x1,x2))  
  auc = (sum(r[1:n1]) - n1*(n1+1)/2) / n1 / n2
  return(auc)
}

zFPR <- function(scores,labels,startConnect,startAttack)
{
  print("Computing scores")
  thr = max(scores[1:startConnect], na.rm=TRUE)
  labels[is.na(labels)]=0
  scores[is.na(scores)]=0
  #labs = which(labels==1)
  
  
  N = length(scores)
  pred = scores>thr
  tp = pred[startConnect:length(scores)]==1 & labels[startConnect:length(scores)]==1
  TP = sum(tp,na.rm = TRUE)
  FN = sum(pred[startConnect:length(scores)]==0 & labels[startConnect:length(scores)]==1,na.rm = TRUE)
  FP = sum(pred[startConnect:length(scores)]==1 & labels[startConnect:length(scores)]==0,na.rm = TRUE)
  TN = sum(pred[startConnect:length(scores)]==0 & labels[startConnect:length(scores)]==0,na.rm = TRUE)
  Dc = which(tp)[1]
  Da = which(tp)[1]-(startAttack-startConnect)
  
  print("Computing highest thresh")
  #of all the negatives, give me the one i gave thr largest score
  thr = max(scores[which(labels==1)])
  pred = scores>thr
  tpz = pred[startConnect:length(scores)]==1 & labels[startConnect:length(scores)]==1
  TPz = sum(tpz,na.rm = TRUE)
  FNz = sum(pred[startConnect:length(scores)]==0 & labels[startConnect:length(scores)]==1,na.rm = TRUE)
  FPz = sum(pred[startConnect:length(scores)]==1 & labels[startConnect:length(scores)]==0,na.rm = TRUE)
  TNz = sum(pred[startConnect:length(scores)]==0 & labels[startConnect:length(scores)]==0,na.rm = TRUE)
  Dcz = which(tpz)[1]
  Daz = which(tpz)[1]-(startAttack-startConnect)
  
  print("Computing lowest thresh")
  #of all the negatives, give me the one i gave thr largest score
  thr = max(scores[1:round(startAttack/2)],na.rm = TRUE)
  pred = scores>thr
  tpz2 = pred[startConnect:length(scores)]==1 & labels[startConnect:length(scores)]==1
  TPz2 = sum(tpz2,na.rm = TRUE)
  FNz2 = sum(pred[startConnect:length(scores)]==0 & labels[startConnect:length(scores)]==1,na.rm = TRUE)
  FPz2 = sum(pred[startConnect:length(scores)]==1 & labels[startConnect:length(scores)]==0,na.rm = TRUE)
  TNz2 = sum(pred[startConnect:length(scores)]==0 & labels[startConnect:length(scores)]==0,na.rm = TRUE)
  Dcz2 = which(tpz2)[1]
  Daz2 = which(tpz2)[1]-(startAttack-startConnect)
  
  
  return(list(c(TP,TN,FP,FN,Dc,Da),c(TPz,TNz,FPz,FNz,Dcz,Daz),c(TPz2,TNz2,FPz2,FNz2,Dcz2,Daz2)))
}
perf <- function(d_names,DATAs,isBadIP,startConnect,startAttack)
{
  #Calculate performance
  RES = matrix(0,20,length(d_names))
  colnames(RES) = d_names
  rownames(RES) = c("TP","FP","TPR","FPR","DDc","DDa","TPzall","FPzall","TPRzall","FPRzall","DDczall","DDazall","TPztrn","FPztrn","TPRztrn","FPRztrn","DDcztrn","DDaztrn","AUC","EER")
  
  for( d in 1:length(d_names))
  {
    print(paste("Workign on ",d_names[d]))
    if(d==3)
    {
      res = zFPR(rollmean(DATAs[[d]],100),isBadIP[100:length(isBadIP)],startConnect,startAttack) #(TP,TN,FP,FN,Dc,Da,..
    }
    else
    {
      res = zFPR(DATAs[[d]],isBadIP,startConnect,startAttack) #(TP,TN,FP,FN,Dc,Da,..
    }
    RES[1,d] = res[[1]][1]
    RES[2,d] = res[[1]][3]
    RES[3,d] = res[[1]][1]/(res[[1]][1]+res[[1]][4])
    RES[4,d] = res[[1]][3]/(res[[1]][3]+res[[1]][2])
    RES[5,d] = res[[1]][5]*0.001
    RES[6,d] = res[[1]][6]* 0.001
    
    RES[7,d] = res[[2]][1]
    RES[8,d] = res[[2]][3]
    RES[9,d] = res[[2]][1]/(res[[2]][1]+res[[2]][4])
    RES[10,d] = res[[2]][3]/(res[[2]][3]+res[[2]][2])
    RES[11,d] = res[[2]][5]*0.001
    RES[12,d] = res[[2]][6]* 0.001
    
    RES[13,d] = res[[3]][1]
    RES[14,d] = res[[3]][3]
    RES[15,d] = res[[3]][1]/(res[[2]][1]+res[[2]][4])
    RES[16,d] = res[[3]][3]/(res[[2]][3]+res[[2]][2])
    RES[17,d] = res[[3]][5]*0.001
    RES[18,d] = res[[3]][6]* 0.001
    
    print("Computing AUC")
    RES[19,d] = fastAUC(DATAs[[d]],isBadIP)
    print("Computing EER")
    RES[20,d] = calculateEqualError(DATAs[[d]][!isBadIP],DATAs[[d]][isBadIP])
    
  }
  return(RES)
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
print('auc,eer,name')
for (f in scoreFilesList)
{
  
  fileLength=length(count.fields(paste(folderPath,f, sep="")))
  fileLength=fileLength*1
  if (fileLength<1000000)
    next
 
  data <- read.csv(paste(folderPath,f, sep=""),sep=",",header=FALSE, skip = 1000000)
  scores=data
  
  
  if (grepl('etterArp',f)==TRUE)
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
    
    
    labels=labelsPHD
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
  
  else if (grepl('SYN_lab',f)==TRUE)
  {
    
    
    labels=labelsSYN
    name='SYN_lab'
  }
  
  else
    next
  
  auc=fastAUC(scores$V1,labels$X0)
  eer=calculateEqualError(labels$X0,scores$V1)
  print(paste(auc,paste(eer,name, sep=","), sep=","))
}

}



scoresArp <- read.csv('C:/Users/tomerdoi/Desktop/gmm/etterArp_RMSE_Scores_MiniBatch_maxClusterSize_10_Gmm_Regular_1000_.csv',sep=",",header=FALSE, skip = 1000000)
scoresFuz <- read.csv('C:/Users/tomerdoi/Desktop/gmm/fuzzing_RMSE_Scores_MiniBatch_maxClusterSize_10_Gmm_Regular_1000_.csv',sep=",",header=FALSE, skip = 1000000)
scoresPHD <- read.csv('C:/Users/tomerdoi/Desktop/gmm/Passive_Sniffing_3-005_RMSE_Scores_MiniBatch_maxClusterSize_10_Gmm_Regular_1000_.csv',sep=",",header=FALSE, skip = 1000000)
scoresPHD2 <- read.csv('C:/Users/tomerdoi/Desktop/gmm/phiddle_09_08_RMSE_Scores_MiniBatch_maxClusterSize_10_Gmm_Regular_1000_.csv',sep=",",header=FALSE, skip = 1000000)
scoresPort <- read.csv('C:/Users/tomerdoi/Desktop/gmm/port_scan_RMSE_Scores_MiniBatch_maxClusterSize_10_Gmm_Regular_1000_.csv',sep=",",header=FALSE, skip = 1000000)
scoresRTSP4 <- read.csv('C:/Users/tomerdoi/Desktop/gmm/RTSP_4-003_RMSE_Scores_MiniBatch_maxClusterSize_10_Gmm_Regular_1000_.csv',sep=",",header=FALSE, skip = 1000000)
scoresSSDP <- read.csv('C:/Users/tomerdoi/Desktop/gmm/SSDP_lab_1-002_RMSE_Scores_MiniBatch_maxClusterSize_10_Gmm_Regular_1000_.csv',sep=",",header=FALSE, skip = 1000000)
scoreSSL <- read.csv('C:/Users/tomerdoi/Desktop/gmm/SSL_lab_1-004_RMSE_Scores_MiniBatch_maxClusterSize_10_Gmm_Regular_1000_.csv',sep=",",header=FALSE, skip = 1000000)
scoresSSL2 <- read.csv('C:/Users/tomerdoi/Desktop/gmm/ssl_renego_RMSE_Scores_MiniBatch_maxClusterSize_10_Gmm_Regular_1000_.csv',sep=",",header=FALSE, skip = 1000000)
scoresSYN <- read.csv('C:/Users/tomerdoi/Desktop/gmm/SYN_lab_1-001_RMSE_Scores_MiniBatch_maxClusterSize_10_Gmm_Regular_1000_.csv',sep=",",header=FALSE, skip = 1000000)
scoresRTSP <- read.csv('C:/Users/tomerdoi/Desktop/gmm/RTSP_RMSE_Scores_MiniBatch_maxClusterSize_10_Gmm_Regular_1000_.csv',sep=",",header=FALSE, skip = 1000000)

#AUC
aucArp=fastAUC(scoresArp$V1,labelsArp$X0)
aucArp=max(aucArp,1-aucArp)

aucFuz=fastAUC(scoresFuz$V1,labelsFuz$X0)
aucFuz=max(aucFuz,1-aucFuz)

aucPHD=fastAUC(scoresPHD$V1,labelsPHD$X0)
aucPHD=max(aucPHD,1-aucPHD)

aucPHD2=fastAUC(scoresPHD2$V1,labelsPHD2$X0)
aucPHD2=max(aucPHD2,1-aucPHD2)

aucPort=fastAUC(scoresPort$V1,labelsPort$X0)
aucPort=max(aucPort,1-aucPort)

aucRTSP4=fastAUC(scoresRTSP4$V1,labelsRTSP4$X0)
aucRTSP4=max(aucRTSP4,1-aucRTSP4)

aucSSDP=fastAUC(scoresSSDP$V1,labelsSSDP$X0)
aucSSDP=max(aucSSDP,1-aucSSDP)

aucSSL=fastAUC(scoreSSL$V1,labelsSSL$X0)
aucSSL=max(aucSSL,1-aucSSL)

aucSSL2=fastAUC(scoresSSL2$V1,labelsSSL2$X0)
aucSSL2=max(aucSSL2,1-aucSSL2)

aucSYN=fastAUC(scoresSYN$V1,labelsSYN$X0)
aucSYN=max(aucSYN,1-aucSYN)



#EER

eerArp=calculateEqualError(labelsArp$X0,scoresArp$V1)

eerFuz=calculateEqualError(labelsFuz$X0,scoresFuz$V1)

eerPHD=calculateEqualError(labelsPHD$X0,scoresPHD$V1)

eerPHD2=calculateEqualError(labelsPHD2$X0,scoresPHD2$V1)

eerPort=calculateEqualError(labelsPort$X0,scoresPort$V1)

eerRTSP4=calculateEqualError(labelsRTSP4$X0,scoresRTSP4$V1)

eerSSDP=calculateEqualError(labelsSSDP$X0,scoresSSDP$V1)

eerSSL=calculateEqualError(labelsSSL$X0,scoreSSL$V1)

eerSSL2=calculateEqualError(labelsSSL2$X0,scoresSSL2$V1)

eerSYN=calculateEqualError(labelsSYN$X0,scoresSYN$V1)




data <- read.csv(paste(folderPath,f, sep=""),sep=",",header=TRUE)
data <- read.csv(paste(folderPath,f, sep=""),sep=",",header=TRUE)
data <- read.csv(paste(folderPath,f, sep=""),sep=",",header=TRUE)
data <- read.csv(paste(folderPath,f, sep=""),sep=",",header=TRUE)
data <- read.csv(paste(folderPath,f, sep=""),sep=",",header=TRUE)
data <- read.csv(paste(folderPath,f, sep=""),sep=",",header=TRUE)

perf_bf_pcStream('D:/thesis_data/results/paperAlgoComparison/results-PC-stream/ProfilloT_withLabels/withLabels/', 'totalResultsAucAndEerPCStreamProfilloTDatasets.csv')

scores<-read.csv('C:/Users/tomerdoi/Desktop/Darpa1999_HOpp_RMSE_Scores.csv',header=TRUE,sep=",")

counter=1
 scores_mof=c()
 for (s in scores$score){
      scores_mof[counter]=s
       counter=counter+1
 }
   
 summary(scores_mof)

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
 
 
 scores_Arp <- read.csv('D:/thesis_data/results/Kitsune_variations_scores/MiniBatch/MB10000/etterArp_RMSE_Scores_MiniBatch_maxClusterSize_10_MiniBatchSize_10000_.csv',sep=",",header=FALSE, skip = 1000000)
 scores_Fuz <- read.csv('D:/thesis_data/results/Kitsune_variations_scores/MiniBatch/MB10000/fuzzing_RMSE_Scores_MiniBatch_maxClusterSize_10_MiniBatchSize_10000_.csv',sep=",",header=FALSE, skip = 1000000)
 scores_PHD <- read.csv('D:/thesis_data/results/Kitsune_variations_scores/MiniBatch/MB10000/Passive_Sniffing_3-005_RMSE_Scores_MiniBatch_maxClusterSize_10_MiniBatchSize_10000_.csv',sep=",",header=FALSE, skip = 1000000)
 scores_PHD2 <- read.csv('D:/thesis_data/results/Kitsune_variations_scores/MiniBatch/MB10000/phiddle_09_08_RMSE_Scores_MiniBatch_maxClusterSize_10_MiniBatchSize_10000_.csv',sep=",",header=FALSE, skip = 1000000)
 scores_Port_Scan <- read.csv('D:/thesis_data/results/Kitsune_variations_scores/MiniBatch/MB10000/port_scan_RMSE_Scores_MiniBatch_maxClusterSize_10_MiniBatchSize_10000_.csv',sep=",",header=FALSE, skip = 1000000)
 scores_RTSP4 <- read.csv('D:/thesis_data/results/Kitsune_variations_scores/MiniBatch/MB10000/RTSP_4-003_RMSE_Scores_MiniBatch_maxClusterSize_10_MiniBatchSize_10000_.csv',sep=",",header=FALSE, skip = 1000000)
 scores_RTSP <- read.csv('D:/thesis_data/results/Kitsune_variations_scores/MiniBatch/MB10000/RTSP_RMSE_Scores_MiniBatch_maxClusterSize_10_MiniBatchSize_10000_.csv',sep=",",header=FALSE, skip = 1000000)
 scores_SSDP <- read.csv('D:/thesis_data/results/Kitsune_variations_scores/MiniBatch/MB10000/SSDP_lab_1-002_RMSE_Scores_MiniBatch_maxClusterSize_10_MiniBatchSize_10000_.csv',sep=",",header=FALSE, skip = 1000000)
 scores_SSL <- read.csv('D:/thesis_data/results/Kitsune_variations_scores/MiniBatch/MB10000/SSL_lab_1-004_RMSE_Scores_MiniBatch_maxClusterSize_10_MiniBatchSize_10000_.csv',sep=",",header=FALSE, skip = 1000000)
 scores_SSL2 <- read.csv('D:/thesis_data/results/Kitsune_variations_scores/MiniBatch/MB10000/ssl_renego_RMSE_Scores_MiniBatch_maxClusterSize_10_MiniBatchSize_10000_.csv',sep=",",header=FALSE, skip = 1000000)
 scores_SYN <- read.csv('D:/thesis_data/results/Kitsune_variations_scores/MiniBatch/MB10000/SYN_lab_1-001_RMSE_Scores_MiniBatch_maxClusterSize_10_MiniBatchSize_10000_.csv',sep=",",header=FALSE, skip = 1000000)
 
 
 
 for (i in c(31:47))
 {
   scores<-read.csv(paste(paste('C:/Users/tomerdoi/Documents/Visual Studio 2015/Projects/scores10to50/scores_RTSP4_',i,sep=""),'.txt',sep=""),header=FALSE,sep=",", skip=1000000)
   auc=fastAUC(scores$V1,labelsRTSP4$X0)
   print(i)
   print(auc)
   print('Finished')
 }
 
 
#labels<-read.csv('C:/Users/tomerdoi/Desktop/Darpa_1999_first3Weeks_labels.csv',header=TRUE,sep=",")
#jpeg('Kitsune_dalmini_syn_mirai_GP_Third.jpg')
#plot(scores_mof,xlab="Packet Number", main="Ensemble of DAs using Dendogram on Dalmini syn Mirai Attack",col=ifelse(labels>0,"red","black"),ylab="Anomaly Score")
#legend( "topleft"
#       , inset = 0.01, 
#      , cex = 0.8
#     , legend = c("Benign", "Malicious"), 
#    , title="Packet Label"
#   , col = c("black","red"), 
#  , pt.bg = c("black","red")
# , pch = c(21,22), horiz=TRUE
#)"
#dev.off()
