
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


perf_bf_pcStream('D:/thesis_data/results/paperAlgoComparison/results-PC-stream/ProfilloT_withLabels/withLabels/', 'totalResultsAucAndEerPCStreamProfilloTDatasets.csv')


