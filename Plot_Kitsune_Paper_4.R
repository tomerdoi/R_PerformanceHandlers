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

perf <- function(d_names,DATAs,isBadIP,startConnect,startAttack)
{
  #Calculate performance
  RES = matrix(0,16,length(d_names))
  colnames(RES) = d_names
  rownames(RES) = c("TP_wFPR0","FP_wFPR0","TN_wFPR0","FN_wFPR0","TPR_wFPR0","FNR_wFPR0","TNR_wFPR0","TP_wFPR0p001","FP_wFPR0p001","TN_wFPR0p001","FN_wFPR0p001","TPR_wFPR0p001","FNR_wFPR0p001","TNR_wFPR0p001","AUC","EER")
  
  for( d in 1:length(d_names))
  {
    print(paste("Workign on ",d_names[d]))
  #  data = DATAs[[d]]
    #if(d==3)
  #  {
      data = rollmean(DATAs[[d]],30,fill=0,align="right")
 #   }

    
    pred = prediction(data,isBadIP)
    
    #FPR 0.0
    fpr=0.0 
    perff = performance(pred,"fpr")
    #thr=perff@x.values[[1]][tail(which(perff@y.values[[1]]<=fpr),1)]
    thr=max(data[!isBadIP],na.rm = TRUE)
    
    TPwfpr0 = sum(data>thr & isBadIP==1,na.rm = TRUE)
    FPwfpr0 = sum(data>thr & isBadIP==0,na.rm = TRUE)
    TNwfpr0 = sum(data<=thr & isBadIP==0,na.rm = TRUE)
    FNwfpr0 = sum(data<=thr & isBadIP==1,na.rm = TRUE)
    TPRwfpr0 = TPwfpr0/(TPwfpr0+FNwfpr0)#perff@y.values[[1]][tail(which(performance(pred,"tpr","fpr")@x.values[[1]]<=fpr),1)]
    FNRwfpr0 = 1-TPRwfpr0#perff@y.values[[1]][tail(which(performance(pred,"fnr","fpr")@x.values[[1]]<=fpr),1)]
    TNRwfpr0 = TNwfpr0/(TNwfpr0+FPwfpr0)#perff@y.values[[1]][tail(which(performance(pred,"tnr","fpr")@x.values[[1]]<=fpr),1)]
    
    fpr=0.001 
    thr=perff@x.values[[1]][tail(which(perff@y.values[[1]]<=fpr),1)]
    TPwfpr0p001 = sum(data>=thr & isBadIP==1,na.rm = TRUE)
    FPwfpr0p001 = sum(data>=thr & isBadIP==0,na.rm = TRUE)
    TNwfpr0p001 = sum(data<thr & isBadIP==0,na.rm = TRUE)
    FNwfpr0p001 = sum(data<thr & isBadIP==1,na.rm = TRUE)
    TPRwfpr0p001 = TPwfpr0p001/(TPwfpr0p001+FNwfpr0p001)#perff@y.values[[1]][tail(which(performance(pred,"tpr","fpr")@x.values[[1]]<=fpr),1)]
    FNRwfpr0p001 = 1-TPRwfpr0p001#perff@y.values[[1]][tail(which(performance(pred,"fnr","fpr")@x.values[[1]]<=fpr),1)]
    TNRwfpr0p001 = TNwfpr0p001/(TNwfpr0p001+FPwfpr0p001)#perff@y.values[[1]][tail(which(performance(pred,"tnr","fpr")@x.values[[1]]<=fpr),1)]   
    #FPRwfpr0p001 = TNwfpr0p001/(FPwfpr0p001+TNwfpr0p001)#perff@y.values[[1]][tail(which(performance(pred,"tnr","fpr")@x.values[[1]]<=fpr),1)]   
    
    RES[1,d] = TPwfpr0 
    RES[2,d] = FPwfpr0 
    RES[3,d] = TNwfpr0 
    RES[4,d] = FNwfpr0 
    RES[5,d] = TPRwfpr0
    RES[6,d] = FNRwfpr0
    RES[7,d] = TNRwfpr0
    
    RES[8,d] = TPwfpr0p001 
    RES[9,d] = FPwfpr0p001 
    RES[10,d] = TNwfpr0p001 
    RES[11,d] = FNwfpr0p001 
    RES[12,d] = TPRwfpr0p001
    RES[13,d] = FNRwfpr0p001
    RES[14,d] = TNRwfpr0p001
    
    print("Computing AUC")
    RES[15,d] = fastAUC(DATAs[[d]],isBadIP)
    print("Computing EER")
    RES[16,d] = calculateEqualError(DATAs[[d]][!isBadIP],DATAs[[d]][isBadIP])

  }
  return(RES)
}



d_names = c("GMM","KIT","ISO","AE1","GMMi","Suricata","pcStream", "Kit_MiniBatch_1k")
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
data[5] <- read_csv("D:/Kitsune Exp/results-GMM/GMM-after execution fixing/RTSP.txt",  col_names = FALSE)[2:(length(data[[1]])+1),]
data[6] <- read_csv("D:/Kitsune Exp/Results/RES_suricata_RTSP.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])+1):(orig_datalen),]
data[7] <- read_csv("D:/Kitsune Exp/Results/RES_pcStream_RTSP.csv", col_names = FALSE)[1000001:orig_datalen,]
data[8]<-read.csv("D:/datasets/KitsuneDatasets/RTSP_RMSE_Scores10000.csv", col_names=FALSE)[1000001:orig_datalen,]
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

#d_names,DATAs,isBadIP,startConnect,startAttack)

###################
#### SSDP ########
###################
#Load adata
orig_datalen = 4077266 
data = list()
data[1] <- read_csv("D:/Kitsune Exp/Results/RES_GMM_SSDP_lab_1-002_1mil.csv", col_names = FALSE) * -1
data[2] <- read_csv("D:/Kitsune Exp/Results/RES_Kitsune_SSDP_lab_1-002.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])):(orig_datalen-1),]
data[3] <- read_csv("D:/Kitsune Exp/Results/RES_IsoF_SSDP_lab_1-002.csv", col_names = FALSE)  * -1
data[4] <- read_csv("D:/Kitsune Exp/Results/RES_1AE_SSDP_lab_1-002.csv", col_names = FALSE)
data[5] <- read_csv("D:/Kitsune Exp/results-GMM/GMM-after execution fixing/SSDP_lab_1-002.txt",  col_names = FALSE)[2:(length(data[[1]])+1),] * -1 #(classes ar eflipped here)
data[6] <- read_csv("D:/Kitsune Exp/Results/RES_suricata_SSDP_lab_1-002.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])+1):(orig_datalen),]
data[7] <- read_csv("D:/Kitsune Exp/Results/RES_pcStream_SSDP_lab_1-002.csv", col_names = FALSE)[1000002:orig_datalen,]
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
data[2] <- read_csv("D:/Kitsune Exp/Results/RES_Kitsune_port_scan.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])):(orig_datalen-1),]
data[3] <- read_csv("D:/Kitsune Exp/Results/RES_IsoF_port_scan.csv", col_names = FALSE)  * -1
data[4] <- read_csv("D:/Kitsune Exp/Results/RES_1AE_port_scan.csv", col_names = FALSE)
data[5] <- read_csv("D:/Kitsune Exp/results-GMM/GMM-after execution fixing/port_scan.txt",  col_names = FALSE)[2:(length(data[[1]])+1),] * -1 #(classes ar eflipped here)
data[6] <- read_csv("D:/Kitsune Exp/Results/RES_suricata_port_scan.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])+1):(orig_datalen),]
data[7] <- read_csv("D:/Kitsune Exp/Results/RES_pcStream_port_scan.csv", col_names = FALSE)[1000002:orig_datalen,]

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
# data[2] <- read_csv("D:/Kitsune Exp/Results/RES_Kitsune_SSL_lab_1-004.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])):(orig_datalen-1),]
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
data[2] <- read_csv("D:/Kitsune Exp/Results/RES_Kitsune_SYN_lab_1-001.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])):(orig_datalen-1),]
data[3] <- read_csv("D:/Kitsune Exp/Results/RES_IsoF_SYN_lab_1-001.csv", col_names = FALSE)  * -1
data[4] <- read_csv("D:/Kitsune Exp/Results/RES_1AE_SYN_lab_1-001.csv", col_names = FALSE)
data[5] <- read_csv("D:/Kitsune Exp/results-GMM/GMM-after execution fixing/SYN_lab_1-001.txt", col_names = FALSE)[2:(length(data[[1]])+1),]
data[6] <- read_csv("D:/Kitsune Exp/Results/RES_suricata_SYN_lab_1-001.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])+1):(orig_datalen),]
data[7] <- read_csv("D:/Kitsune Exp/Results/RES_pcStream_SYN_lab_1-001.csv", col_names = FALSE)[1000002:orig_datalen,]
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
# data[2] <- read_csv("D:/Kitsune Exp/Results/RES_Kitsune_Passive_Sniffing_3-005.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])):(orig_datalen-1),]
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



# ###################
# #### RTSP4 ########
# ###################
# #Load adata
# orig_datalen = 2472401 
# data = list()
# data[1] <- read_csv("D:/Kitsune Exp/Results/RES_GMM_RTSP_4-003_1mil.csv", col_names = FALSE) * -1
# data[2] <- read_csv("D:/Kitsune Exp/Results/RES_Kitsune_RTSP_4-003.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])):(orig_datalen-1),]
# data[3] <- read_csv("D:/Kitsune Exp/Results/RES_IsoF_RTSP_4-003.csv", col_names = FALSE)  * -1
# data[4] <- read_csv("D:/Kitsune Exp/Results/RES_1AE_RTSP_4-003.csv", col_names = FALSE)
# data[5] <- read_csv("D:/Kitsune Exp/results-GMM/GMM-after execution fixing/RTSP_4-003.txt", col_names = FALSE)
# 
# datalen = length(data[[1]])
# 
# #Set Labels
# startConnect= 2131043 - 1000000
# startAttack = 2143935 - 1000000
# isBadIP = read_csv("ip_RTSP4_isBad.csv")$x[1000002:orig_datalen]
# 
# #Calculate performance
# RES_RTSP4 = perf(d_names,data,isBadIP,startConnect,startAttack)
# round(RES_RTSP4,2)



###################
#### SSL2 ########
###################
#Load adata
orig_datalen = 2207571 
data = list()
data[1] <- read_csv("D:/Kitsune Exp/Results/RES_GMM_ssl_renego_1mil.csv", col_names = FALSE) * -1
data[2] <- read_csv("D:/Kitsune Exp/Results/RES_KitsuneC_ssl_renego.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])):(orig_datalen-1),]
data[3] <- read_csv("D:/Kitsune Exp/Results/RES_IsoF_ssl_renego.csv", col_names = FALSE)  * -1
data[4] <- read_csv("D:/Kitsune Exp/Results/RES_1AE_ssl_renego.csv", col_names = FALSE)
data[5] <- read_csv("D:/Kitsune Exp/results-GMM/GMM-after execution fixing/ssl_renego.txt", col_names = FALSE)[2:(length(data[[1]])+1),]
data[6] <- read_csv("D:/Kitsune Exp/Results/RES_suricata_ssl_renego.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])+1):(orig_datalen),]
data[7] <- read_csv("D:/Kitsune Exp/Results/RES_pcStream_ssl_renego.csv", col_names = FALSE)[1000002:orig_datalen,]
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
data[2] <- read_csv("D:/Kitsune Exp/Results/RES_KitsuneC_etterArp.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])):(orig_datalen-1),]
data[3] <- read_csv("D:/Kitsune Exp/Results/RES_IsoF_etterArp.csv", col_names = FALSE)  * -1
data[4] <- read_csv("D:/Kitsune Exp/Results/RES_1AE_etterArp.csv", col_names = FALSE)
data[5] <- read_csv("D:/Kitsune Exp/results-GMM/GMM-after execution fixing/etterArp.txt", col_names = FALSE)[2:(length(data[[1]])+1),]
data[6] <- read_csv("D:/Kitsune Exp/Results/RES_suricata_etterArp.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])+1):(orig_datalen),]
data[7] <- read_csv("D:/Kitsune Exp/Results/RES_pcStream_etterArp.csv", col_names = FALSE)[1000002:orig_datalen,]

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
data[2] <- read_csv("D:/Kitsune Exp/Results/RES_KitsuneC_phiddle_09_08.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])):(orig_datalen-1),]
data[3] <- read_csv("D:/Kitsune Exp/Results/RES_IsoF_phiddle_09_08.csv", col_names = FALSE)  * -1
data[4] <- read_csv("D:/Kitsune Exp/Results/RES_1AE_phiddle_09_08.csv", col_names = FALSE)
data[5] <- read_csv("D:/Kitsune Exp/results-GMM/GMM-after execution fixing/phiddle_09_08.txt", col_names = FALSE)[2:(length(data[[1]])+1),]
data[6] <- read_csv("D:/Kitsune Exp/Results/RES_suricata_phiddle_09_08.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])+1):(orig_datalen),]
data[7] <- read_csv("D:/Kitsune Exp/Results/RES_pcStream_phiddle_09_08.csv", col_names = FALSE)[1000002:orig_datalen,]

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
data[2] <- read_csv("D:/Kitsune Exp/Results/RES_KitsuneC_fuzzing.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])):(orig_datalen-1),]
#X <- read_csv("D:/Kitsune Exp/Results/RES_KitsuneC_fuzzing.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])):(orig_datalen-1),]
#data[[2]] <- as.numeric(rollmean(X$X1,5,fill=0,align = "right"))
data[3] <- read_csv("D:/Kitsune Exp/Results/RES_IsoF_fuzzing.csv", col_names = FALSE)  * -1
data[4] <- read_csv("D:/Kitsune Exp/Results/RES_1AE_fuzzing.csv", col_names = FALSE)
#X <- read_csv("D:/Kitsune Exp/Results/RES_1AE_fuzzing.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])):(orig_datalen-1),]
#data[[4]] <- as.numeric(rollmean(X$X1,5,fill=0,align = "right"))
data[5] <- read_csv("D:/Kitsune Exp/results-GMM/GMM-after execution fixing/fuzzing.txt", col_names = FALSE)[2:(length(data[[1]])+1),]
data[6] <- read_csv("D:/Kitsune Exp/Results/RES_suricata_fuzzing.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])+1):(orig_datalen),]
data[7] <- read_csv("D:/Kitsune Exp/Results/RES_pcStream_fuzzing.csv", col_names = FALSE)[1000002:orig_datalen,]

datalen = length(data[[1]])

#Set Labels
startConnect = 1300000 - 1000000
startAttack = 1300000 - 1000000
isBadIP = read_csv("ip_FUZ_isBad.csv")$x[1000002:orig_datalen]
#isBadIP = isBadF[1000002:orig_datalen]

#Calculate performance
RES_FUZ = perf(d_names,data,isBadIP,startConnect,startAttack)
round(RES_FUZ,4)


#### MIRAI ########
###################
#Load adata
orig_datalen = 764137 
data = list()
data[1] <- read_csv("D:/Kitsune Exp/Results/RES_GMM_mirai2_1mil.csv", col_names = FALSE) * -1
data[2] <- read_csv("D:/Kitsune Exp/Results/RES_KitsuneC_mirai2.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])):(orig_datalen-1),]
data[3] <- read_csv("D:/Kitsune Exp/Results/RES_IsoF_mirai2.csv", col_names = FALSE)  * -1
data[4] <- read_csv("D:/Kitsune Exp/Results/RES_1AE_mirai2.csv", col_names = FALSE)
dm <- read_csv("D:/Kitsune Exp/results-GMM/GMM-after execution fixing/mirai2.csv", col_names = FALSE)[2:(length(data[[1]])+1),]
dmr = zeros(length(data[[1]]),1)
dmr[(length(data[[1]])-length(dm$X1)+1):length(data[[1]])] = dm$X1
data[[5]] <- as.numeric(dmr)
data[6] <- read_csv("D:/Kitsune Exp/Results/RES_suricata_mirai2.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])+1):(orig_datalen),]
data[7] <- read_csv("D:/Kitsune Exp/Results/RES_pcStream_mirai2.csv", col_names = FALSE)[(orig_datalen-length(data[[1]])+1):(orig_datalen),]

datalen = length(data[[1]])

#Set Labels
startConnect = 121659
startAttack = 121659
isBadIP = read_csv("mirai2_isBad.csv")$x[(orig_datalen-length(data[[1]])+1):orig_datalen]

#Calculate performance
RES_MIRAI = perf(d_names,data,isBadIP,startConnect,startAttack)
round(RES_MIRAI,2)

#####################

round(RES_PORT,4)
round(RES_RTSP,4)
round(RES_SSDP,4)
#round(RES_SSL,5)
round(RES_SYN,4)
#round(RES_PHD,5)
#round(RES_RTSP4,4)
round(RES_SSL2,4)
round(RES_ARP,4)
round(RES_PHD2,4)
round(RES_FUZ,4)

#####################


round(RES_PORT,4)
round(RES_RTSP,4)
round(RES_SSDP,4)
#round(RES_SSL,5)
round(RES_SYN,4)
#round(RES_PHD,5)
#round(RES_RTSP4,4)
round(RES_SSL2,4)
round(RES_ARP,4)
round(RES_PHD2,4)
round(RES_FUZ,4)

#############
metrics=c("TP_wFPR0p001","FP_wFPR0p001","TN_wFPR0p001","FN_wFPR0p001","TPR_wFPR0p001","TNR_wFPR0p001","FNR_wFPR0p001")
algs = c("Suricata","ISO","GMM","GMMi","pcStream","KIT","AE1")

RES=rbind(RES_PORT[metrics,algs],RES_FUZ[metrics,algs],RES_RTSP[metrics,algs],RES_ARP[metrics,algs],RES_PHD2[metrics,algs],RES_SSDP[metrics,algs],RES_SYN[metrics,algs],RES_SSL2[metrics,algs],RES_MIRAI[metrics,algs])

##############
metrics=c("AUC")
RES_auc=rbind(RES_PORT[metrics,algs],RES_FUZ[metrics,algs],RES_RTSP[metrics,algs],RES_ARP[metrics,algs],RES_PHD2[metrics,algs],RES_SSDP[metrics,algs],RES_SYN[metrics,algs],RES_SSL2[metrics,algs],RES_MIRAI[metrics,algs])
for(i in 1:dim(RES_auc)[1]){for( j in 1:dim(RES_auc)[2]){RES_auc[i,j]=max(RES_auc[i,j],1-RES_auc[i,j])}}
write.csv(file="auc.csv",RES_auc)

##############
metrics=c("EER")
RES_eer=rbind(RES_PORT[metrics,algs],RES_FUZ[metrics,algs],RES_RTSP[metrics,algs],RES_ARP[metrics,algs],RES_PHD2[metrics,algs],RES_SSDP[metrics,algs],RES_SYN[metrics,algs],RES_SSL2[metrics,algs],RES_MIRAI[metrics,algs])
write.csv(file="eer.csv",RES_eer)

############
metrics=c("TPR_wFPR0")
RES_fpr0=rbind(RES_PORT[metrics,algs],RES_FUZ[metrics,algs],RES_RTSP[metrics,algs],RES_ARP[metrics,algs],RES_PHD2[metrics,algs],RES_SSDP[metrics,algs],RES_SYN[metrics,algs],RES_SSL2[metrics,algs],RES_MIRAI[metrics,algs])
write.csv(file="tpr_fp0.csv",RES_fpr0)

##########
metrics=c("FNR_wFPR0")#,"FP_wFPR0","TN_wFPR0","FN_wFPR0","TPR_wFPR0","TNR_wFPR0","FNR_wFPR0")
RES_fpr0=rbind(RES_PORT[metrics,algs],RES_FUZ[metrics,algs],RES_RTSP[metrics,algs],RES_ARP[metrics,algs],RES_PHD2[metrics,algs],RES_SSDP[metrics,algs],RES_SYN[metrics,algs],RES_SSL2[metrics,algs],RES_MIRAI[metrics,algs])
write.csv(file="fnr_fp0.csv",RES_fpr0)


