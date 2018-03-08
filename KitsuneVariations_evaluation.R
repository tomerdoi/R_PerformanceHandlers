require(ROCR)
require(zoo)


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

getLabels<- function ()
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
  
  labelPaths=vector(mode="list",length=length(names))
  labelPaths[[1]]=labelsArp
  labelPaths[[2]]=labelsFuz
  labelPaths[[3]]=labelsPHD
  labelPaths[[4]]=labelsPHD2
  labelPaths[[5]]=labelsPort
  labelPaths[[6]]=labelsRTSP4
  labelPaths[[7]]=labelsSSDP
  labelPaths[[8]]=labelsSSL
  labelPaths[[9]]=labelsSSL2
  labelPaths[[10]]=labelsSYN
  
  names(labelPaths)=c("etterArp" ,  "fuzzing" ,  "Passive_Sniffing"  , "phiddle"  ,"port_scan"  ,"RTSP_4-003", "SSDP_lab",  "SSL_lab",   "ssl_renego",  "SYN_lab")
  
}

evaluate_MiniBatch<-function (folderPath, outFile)
{
  getLabels()
  paths=list.files(folderPath)
  #cat("MiniBatchSize, MaxClusterSize, AUC", "C:/Users/tomerdoi/Documents/MiniBatchResults.csv",sep="\n", append=FALSE)
  
  write.table("Dataset, AUC, EER", file=outFile, sep=",", append=TRUE, col.names=FALSE)

  for (p in paths)
  {
  
    
    if (grepl("pcapParsed_Cameras",p)==TRUE)
    {
      next
    }
    path=paste(folderPath,p,sep="/")
    print(path)
    scores <- read.csv(path,sep=",",header=TRUE, skip = 1000000)
    
   
    for (key in names(labelPaths))
    {
     
      if (grepl(key,p)==TRUE)
      {
        print("found")
        print (key)
        labels=labelPaths[[key]]
      }
     
    }
    
    words=gsub("maxAE","_maxAE",p)
    words=strsplit(words,"_")
    print(words)
    
    if (grepl("etter",p)==TRUE || grepl("fuz",p)==TRUE || grepl("RTSP_RMSE",p)==TRUE)
    {
      mb=words[[1]][6]
      maxCS=words[[1]][8]
    }
    else if (grepl("port",p)==TRUE || grepl("RTSP_4",p)==TRUE || grepl("ssl_renego",p)==TRUE )
    {
      mb=words[[1]][7]
      maxCS=words[[1]][9]
    }
    else 
    {
      mb=words[[1]][8]
      maxCS=words[[1]][10]
    }
    auc=fastAUC(scores$X0.0,labels$X0)
    auc=max(auc,1-auc) 
   
    EER=calculateEqualError(labels$X0,scores$X0.0)
    #row=paste(paste(mb,maxCS,sep=" "),auc,sep=" ")
    row=paste(p,auc,sep=",")
    row=paste(row,EER,sep=",")
    #cat(row, "C:/Users/tomerdoi/Documents/MiniBatchResults.csv",sep="\n", append=TRUE)
    write.table(row[1], file=outFile, sep=",", append=TRUE, col.names=FALSE)
    print(paste(p,"was evaluated",sep=" "))
  }
}

evaluate_LSTM<-function (folderPath)
{
  #getLabels()
  paths=list.files(folderPath)
  #cat("MiniBatchSize, MaxClusterSize, AUC", "C:/Users/tomerdoi/Documents/MiniBatchResults.csv",sep="\n", append=FALSE)
  
  write.table("MiniBatchSize, MaxClusterSize, AUC", file="LSTMKitsuneResults.csv", sep=",", append=TRUE, col.names=FALSE)
 
  for (p in paths)
  {
   
    if (grepl("pcapParsed_Cameras",p)==TRUE)
    {
      next
    }
    path=paste(folderPath,p,sep="/")
    scores <- read.csv(path,sep=",",header=TRUE, skip = 1000000)
    
    
    
    for (key in names(labelPaths))
    {
      
      if (grepl(key,p)==TRUE)
      {
        labels=labelPaths[[key]]
      }
    }
    
    words=gsub("miniBatch","_miniBatch",p)
    words=strsplit(words,"_")
    
    print(words)
    
    if (grepl("etter",p)==TRUE || grepl("fuz",p)==TRUE || grepl("RTSP_RMSE",p)==TRUE)
    {
      mb=words[[1]][6]
      maxCS=words[[1]][8]
    }
    else if (grepl("port",p)==TRUE || grepl("RTSP_4",p)==TRUE || grepl("ssl_renego",p)==TRUE )
    {
      mb=words[[1]][7]
      maxCS=words[[1]][9]
    }
    else 
    {
      mb=words[[1]][8]
      maxCS=words[[1]][10]
    }
    #mb=words[[1]][6]
    #maxCS=words[[1]][8]
    auc=fastAUC(scores$X0.0,labels$X0)
    auc=max(auc,1-auc)
    row=paste(paste(mb,maxCS,sep=","),auc,sep=",")
    
    #cat(row, "C:/Users/tomerdoi/Documents/MiniBatchResults.csv",sep="\n", append=TRUE)
    write.table(row[[1]], file="LSTMKitsuneResults.csv", sep=",", append=TRUE, col.names=FALSE)
    print(paste(p,"was evaluated",sep=" "))
  }
}



  