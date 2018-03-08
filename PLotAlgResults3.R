setwd("D:/Kitsune Exp/Results")
require(ROCR)
require(zoo)
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
  thr = max(scores[!isBadIP],na.rm = TRUE)
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
  thr = max(scores[1:ceil(startAttack/2)],na.rm = TRUE)
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



d_names = c("GMM","KIT","ISO","AE1","GMMi")
RTSP_inter_packet_time = 0.001687887
SYN_inter_packet_time = 0.001143597
SSL_inter_packet_time = 0.0006471068
SSDP_inter_packet_time = 0.0005994514
PHD_inter_packet_time = 0.001259613
PORT_inter_packet_time = 0.001845132


###################
#### RTSP ########
###################
#Load adata
orig_datalen = 1946985 
data = list()
data[1] <- read_csv("D:/Kitsune Exp/Results/RES_GMM_RTSP_1mil.csv", col_names = FALSE) * -1
data[2] <- read_csv("D:/Kitsune Exp/Results/RES_Kitsune_RTSP.csv", col_names = FALSE)[1000001:orig_datalen,]
data[3] <- read_csv("D:/Kitsune Exp/Results/RES_IsoF_RTSP.csv", col_names = FALSE)  * -1
data[4] <- read_csv("D:/Kitsune Exp/Results/RES_1AE_RTSP.csv", col_names = FALSE)
data[5] <- read_csv("D:/Kitsune Exp/results-GMM/GMM-after execution fixing/RTSP.txt",  col_names = FALSE)
datalen = length(data[[1]])

#Set Labels
startConnect = 1720647 - 1000000
startAttack = 1720647 - 1000000
ip_RTSP = read_csv("ip_RTSP.csv")$x[1000001:orig_datalen]
victimIP = "192.168.2.13"
ben = 1:startAttack
mal = startAttack:datalen
goodIPs = unique(ip_RTSP[ben])
goodIPs = goodIPs[goodIPs!=victimIP] #all ips before start attack -victim IP
#goodIPs = goodIPs[goodIPs!="192.168.2.14"] #all ips before start attack -victim IP
badIPs = setdiff(unique(ip_RTSP[mal]),goodIPs) #all new ips after start attack + victim IP
isBadIP = ip_RTSP %in% badIPs
isBadIP[1:startAttack]=FALSE
isBadIP[which(ip_RTSP=="192.168.2.20")]=TRUE

RES_RTSP = perf(d_names,data,isBadIP,startConnect,startAttack)
round(RES_RTSP,2)



###################
#### SSDP ########
###################
#Load adata
orig_datalen = 4077266 
data = list()
data[1] <- read_csv("D:/Kitsune Exp/Results/RES_GMM_SSDP_lab_1-002_1mil.csv", col_names = FALSE) * -1
data[2] <- read_csv("D:/Kitsune Exp/Results/RES_Kitsune_SSDP_lab_1-002.csv", col_names = FALSE)[1000002:orig_datalen,]
data[3] <- read_csv("D:/Kitsune Exp/Results/RES_IsoF_SSDP_lab_1-002.csv", col_names = FALSE)  * -1
data[4] <- read_csv("D:/Kitsune Exp/Results/RES_1AE_SSDP_lab_1-002.csv", col_names = FALSE)
data[5] <- read_csv("D:/Kitsune Exp/results-GMM/GMM-after execution fixing/SSDP_lab_1-002.txt",  col_names = FALSE) * -1 #(classes ar eflipped here)
datalen = length(data[[1]])

#Set Labels
startConnect = 2607120 - 1000000
startAttack = 2612406 - 1000000
isBadIP = read_csv("ip_SSDP_isBad.csv")$x[1000002:orig_datalen]

#Calculate performance
RES_SSDP = perf(d_names,data,isBadIP,startConnect,startAttack)
round(RES_SSDP,2)



###################
#### Port Scan ########
###################
#Load adata
orig_datalen = 1697851 
data = list()
data[1] <- read_csv("D:/Kitsune Exp/Results/RES_GMM_port_scan_1mil.csv", col_names = FALSE) * -1
data[2] <- read_csv("D:/Kitsune Exp/Results/RES_Kitsune_port_scan.csv", col_names = FALSE)[1000002:orig_datalen,]
data[3] <- read_csv("D:/Kitsune Exp/Results/RES_IsoF_port_scan.csv", col_names = FALSE)  * -1
data[4] <- read_csv("D:/Kitsune Exp/Results/RES_1AE_port_scan.csv", col_names = FALSE)
data[5] <- read_csv("D:/Kitsune Exp/results-GMM/GMM-after execution fixing/port_scan.txt",  col_names = FALSE) * -1 #(classes ar eflipped here)

datalen = length(data[[1]])

#Set Labels
startConnect = 1301000 - 1000000
startAttack = 1301000 - 1000000
isBadIP = read_csv("ip_PORT_isBad.csv")$x[1000002:orig_datalen]

RES_PORT = perf(d_names,data,isBadIP,startConnect,startAttack)
round(RES_PORT,2)



# ###################
# #### SSL ########
# ###################
# #Load adata
# orig_datalen = 6084492 
# data = list()
# data[1] <- read_csv("D:/Kitsune Exp/Results/RES_GMM_SSL_lab_1-004_1mil.csv", col_names = FALSE) * -1
# data[2] <- read_csv("D:/Kitsune Exp/Results/RES_Kitsune_SSL_lab_1-004.csv", col_names = FALSE)[1000002:orig_datalen,]
# data[3] <- read_csv("D:/Kitsune Exp/Results/RES_IsoF_SSL_lab_1-004.csv", col_names = FALSE)  * -1
# data[4] <- read_csv("D:/Kitsune Exp/Results/RES_1AE_SSL_lab_1-004.csv", col_names = FALSE)
# datalen = length(data[[1]])
# 
# #Set Labels
# startConnect = 5827396 - 1000000
# startAttack = 5838285 - 1000000
# isBadIP = read_csv("ip_SSL_isBad.csv")$x[1000002:orig_datalen]
# 
# RES_SSL = perf(d_names,data,isBadIP,startConnect,startAttack)
# round(RES_SSL,2)



###################
#### SYN ########
###################
#Load adata
orig_datalen = 2771276 
data = list()
data[1] <- read_csv("D:/Kitsune Exp/Results/RES_GMM_SYN_lab_1-001_1mil.csv", col_names = FALSE) * -1
data[2] <- read_csv("D:/Kitsune Exp/Results/RES_Kitsune_SYN_lab_1-001.csv", col_names = FALSE)[1000002:orig_datalen,]
data[3] <- read_csv("D:/Kitsune Exp/Results/RES_IsoF_SYN_lab_1-001.csv", col_names = FALSE)  * -1
data[4] <- read_csv("D:/Kitsune Exp/Results/RES_1AE_SYN_lab_1-001.csv", col_names = FALSE)
data[5] <- read_csv("D:/Kitsune Exp/results-GMM/GMM-after execution fixing/SYN_lab_1-001.txt", col_names = FALSE)[2:(orig_datalen-1000000),]
datalen = length(data[[1]])

#Set Labels
startConnect = 2682312 - 1000000
startAttack = 2697806 - 1000000
isBadIP = read_csv("ip_SYN_isBad.csv")$x[1000002:orig_datalen]

RES_SYN = perf(d_names,data,isBadIP,startConnect,startAttack)
round(RES_SYN,2)



# ###################
# #### Phiddle ########
# ###################
# #Load adata
# orig_datalen = 4554925 
# data = list()
# data[1] <- read_csv("D:/Kitsune Exp/Results/RES_GMM_Passive_Sniffing_3-005_1mil.csv", col_names = FALSE) * -1
# data[2] <- read_csv("D:/Kitsune Exp/Results/RES_Kitsune_Passive_Sniffing_3-005.csv", col_names = FALSE)[1000002:orig_datalen,]
# data[3] <- read_csv("D:/Kitsune Exp/Results/RES_IsoF_Passive_Sniffing_3-005.csv", col_names = FALSE)  * -1
# data[4] <- read_csv("D:/Kitsune Exp/Results/RES_1AE_Passive_Sniffing_3-005.csv", col_names = FALSE)
# data[5] <- read_csv("D:/Kitsune Exp/results-GMM/GMM-after execution fixing/SYN_lab_1-001.csv", col_names = FALSE)
# 
# datalen = length(data[[1]])
# 
# #Set Labels
# startConnect= 3520000 - 1000000
# startAttack = 3910000 - 1000000
# isBadIP = read_csv("ip_PHD_isBad.csv")$x[1000002:orig_datalen]
# 
# #Calculate performance
# RES_PHD = perf(d_names,data,isBadIP,startConnect,startAttack)
# round(RES_PHD,2)



###################
#### RTSP4 ########
###################
#Load adata
orig_datalen = 2472401 
data = list()
data[1] <- read_csv("D:/Kitsune Exp/Results/RES_GMM_RTSP_4-003_1mil.csv", col_names = FALSE) * -1
data[2] <- read_csv("D:/Kitsune Exp/Results/RES_Kitsune_RTSP_4-003.csv", col_names = FALSE)[1000002:orig_datalen,]
data[3] <- read_csv("D:/Kitsune Exp/Results/RES_IsoF_RTSP_4-003.csv", col_names = FALSE)  * -1
data[4] <- read_csv("D:/Kitsune Exp/Results/RES_1AE_RTSP_4-003.csv", col_names = FALSE)
data[5] <- read_csv("D:/Kitsune Exp/results-GMM/GMM-after execution fixing/RTSP_4-003.txt", col_names = FALSE)

datalen = length(data[[1]])

#Set Labels
startConnect= 2131043 - 1000000
startAttack = 2143935 - 1000000
isBadIP = read_csv("ip_RTSP4_isBad.csv")$x[1000002:orig_datalen]

#Calculate performance
RES_RTSP4 = perf(d_names,data,isBadIP,startConnect,startAttack)
round(RES_RTSP4,2)



###################
#### SSL2 ########
###################
#Load adata
orig_datalen = 2207571 
data = list()
data[1] <- read_csv("D:/Kitsune Exp/Results/RES_GMM_ssl_renego_1mil.csv", col_names = FALSE) * -1
data[2] <- read_csv("D:/Kitsune Exp/Results/RES_KitsuneC_ssl_renego.csv", col_names = FALSE)[1000002:orig_datalen,]
data[3] <- read_csv("D:/Kitsune Exp/Results/RES_IsoF_ssl_renego.csv", col_names = FALSE)  * -1
data[4] <- read_csv("D:/Kitsune Exp/Results/RES_1AE_ssl_renego.csv", col_names = FALSE)
data[5] <- read_csv("D:/Kitsune Exp/results-GMM/GMM-after execution fixing/ssl_renego.txt", col_names = FALSE)

datalen = length(data[[1]])

#Set Labels
startConnect = 1320000 - 1000000
startAttack = 1320000 - 1000000
isBadIP = read_csv("ip_SSL2_isBad.csv")$x[1000002:orig_datalen]

#Calculate performance
RES_SSL2 = perf(d_names,data,isBadIP,startConnect,startAttack)
round(RES_SSL2,2)


###################
#### ARP ########
###################
#Load adata
orig_datalen = 2504267 
data = list()
data[1] <- read_csv("D:/Kitsune Exp/Results/RES_GMM_etterArp_1mil.csv", col_names = FALSE) * -1
data[2] <- read_csv("D:/Kitsune Exp/Results/RES_KitsuneC_etterArp.csv", col_names = FALSE)[1000002:orig_datalen,]
data[3] <- read_csv("D:/Kitsune Exp/Results/RES_IsoF_etterArp.csv", col_names = FALSE)  * -1
data[4] <- read_csv("D:/Kitsune Exp/Results/RES_1AE_etterArp.csv", col_names = FALSE)
data[5] <- read_csv("D:/Kitsune Exp/results-GMM/GMM-after execution fixing/etterArp.txt", col_names = FALSE)

datalen = length(data[[1]])

#Set Labels
startConnect = 1300000 - 1000000
startAttack = 1300000 - 1000000
isBadIP = read_csv("ip_ARP_isBad.csv")$x[1000002:orig_datalen]

#Calculate performance
RES_ARP = perf(d_names,data,isBadIP,startConnect,startAttack)
round(RES_ARP,2)


###################
#### PHD2 ########
###################
#Load adata
orig_datalen = 2278689 
data = list()
data[1] <- read_csv("D:/Kitsune Exp/Results/RES_GMM_phiddle_09_08_1mil.csv", col_names = FALSE) * -1
data[2] <- read_csv("D:/Kitsune Exp/Results/RES_KitsuneC_phiddle_09_08.csv", col_names = FALSE)[1000002:orig_datalen,]
data[3] <- read_csv("D:/Kitsune Exp/Results/RES_IsoF_phiddle_09_08.csv", col_names = FALSE)  * -1
data[4] <- read_csv("D:/Kitsune Exp/Results/RES_1AE_phiddle_09_08.csv", col_names = FALSE)
data[5] <- read_csv("D:/Kitsune Exp/results-GMM/GMM-after execution fixing/phiddle_09_08.txt", col_names = FALSE)

datalen = length(data[[1]])

#Set Labels
startConnect = 1300000 - 1000000
startAttack = 1300000 - 1000000
isBadIP = read_csv("ip_PHD2_isBad.csv")$x[1000002:orig_datalen]

#Calculate performance
RES_PHD2 = perf(d_names,data,isBadIP,startConnect,startAttack)
round(RES_PHD2,2)

###################
#### FUZ ########
###################
#Load adata
orig_datalen = 2244139 
data = list()
data[1] <- read_csv("D:/Kitsune Exp/Results/RES_GMM_fuzzing_1mil.csv", col_names = FALSE) * -1
data[2] <- read_csv("D:/Kitsune Exp/Results/RES_KitsuneC_fuzzing.csv", col_names = FALSE)[1000002:orig_datalen,]
data[3] <- read_csv("D:/Kitsune Exp/Results/RES_IsoF_fuzzing.csv", col_names = FALSE)  * -1
data[4] <- read_csv("D:/Kitsune Exp/Results/RES_1AE_fuzzing.csv", col_names = FALSE)
data[5] <- read_csv("D:/Kitsune Exp/results-GMM/GMM-after execution fixing/fuzzing.txt", col_names = FALSE)

datalen = length(data[[1]])

#Set Labels
startConnect = 1300000 - 1000000
startAttack = 1300000 - 1000000
isBadIP = read_csv("ip_FUZ_isBad.csv")$x[1000002:orig_datalen]

#Calculate performance
RES_FUZ = perf(d_names,data,isBadIP,startConnect,startAttack)
round(RES_FUZ,2)




round(RES_PORT,5)
round(RES_RTSP,5)
round(RES_SSDP,5)
#round(RES_SSL,5)
round(RES_SYN,5)
#round(RES_PHD,5)
round(RES_RTSP4,5)
round(RES_SSL2,5)
round(RES_ARP,5)
round(RES_PHD2,5)
round(RES_FUZ,5)

#####################




