library('stream')
detectAnomalies<-function(inPath,outPath)
{
  resFileList=list.files(inPath)
  #check for attack start
  if (grepl('dalmini',inPath)==TRUE)
    attStart=40397
  if (grepl('ecobee',inPath)==TRUE)
    attStart=13113
  if (grepl('ennio',inPath)==TRUE)
    attStart=34694
  if (grepl('phillips',inPath)==TRUE)
    attStart=160139
  
  if (grepl('provision737',inPath)==TRUE)
    attStart=55171
  if (grepl('provision838',inPath)==TRUE)
    attStart=91557
  
  if (grepl('samsung',inPath)==TRUE)
    attStart=46819
  if (grepl('simplehome1002',inPath)==TRUE)
    attStart=42786
  if (grepl('simplehome1003',inPath)==TRUE)
    attStart=17938
  
  #rm(stream)
  #rm(dstream1)
  #'D:/datasets/KitsuneDatasets/ps2.csv'
  
  for (f in resFileList)
  {
  fileLength=length(count.fields(paste(inPath,f, sep="")))
  stream=DSD_ReadCSV(paste(inPath,f, sep=""), take=NULL, class=NULL, loop=FALSE,sep=",", header=FALSE, skip=0, colClasses = NA)
  #stream <- DSD_BarsAndGaussians(noise=.05)
  #stream <- DSD_ScaleStream(stream,n=1000)
  stream <- DSD_ScaleStream(stream,n=1000)
  #stream <- DSD_ScaleStream(stream,n=1000)
  #stream_scaled<-DSD_ScaleStream('D:/datasets/KitsuneDatasets/ps2.csv',center=TRUE,scale=TRUE)
  #stream=stream_scaled
  #stream <- DSD_Gaussians( k=3,d=2, noise=.05)
  #stream <- DSD_BarsAndGaussians(noise=.05)
  #plot(stream)
  dstream1 <- DSC_DStream(gridsize=100, Cm=3, N=1000)
  #dstream1
  #update(dstream1, stream,999000 )
  #for (yr in seq(1:999000 ) )
  #for (yr in seq(1:999000 ) )
  for (yr in seq(1:attStart-1000 ) )
  {
    if (yr%%1000==0)
    {
      print(yr)
    }
    update(dstream1, stream,1 )
    #update(dstream1, stream,1 )
  }
  print('finished update')
  #dstream1
  #assignments=get_assignment(dstream1,get_points(stream,fileLength-1000000))
  index=1
  assignments=c()
  #assignments=get_assignment(dbstream,get_points(stream,fileLength-1000000))
  for (yr in seq(1:(fileLength-attStart )))
  {
    if (yr%%1000==0)
    {
      print(yr)
    }
    #update(dbstream, stream,999000 )
    assignments=get_assignment(dstream1,get_points(stream,1))
    #write.csv(assignments, file = outPath,append=TRUE)
    #cat(assignments, file=outPath, append=TRUE, sep = "\n")
    cat(assignments, file=paste(paste(inPath,'d_stream_profilloT_', sep=""),f,sep=""), append=TRUE, sep = "\n")
    index=index+1
  }
  #assignments
  #summary(assignments)
  #write.csv(assignments, file = outPath)
  #close_stream(stream)
  rm(stream)
  rm(dstream1)
  }
}



#detectAnomalies('D:/datasets/KitsuneDatasets/port_scan.csv','port_scan_d.csv')
#detectAnomalies('D:/datasets/KitsuneDatasets/fuzzing.csv','fuzzing_d.csv')
#detectAnomalies('D:/datasets/KitsuneDatasets/Passive_Sniffing_3-005.csv','Passive_Sniffing_3-005_d.csv')
#detectAnomalies('D:/datasets/KitsuneDatasets/phiddle_09_08.csv','phiddle_09_08_d.csv')
#detectAnomalies('D:/datasets/KitsuneDatasets/RTSP.csv','RTSP_d.csv')
#detectAnomalies('D:/datasets/KitsuneDatasets/RTSP_4-003.csv','RTSP_4-003_d.csv')
detectAnomalies('D:/datasets/KitsuneDatasets/SSDP_lab_1-002.csv','SSDP_lab_1-002_d.csv')
#detectAnomalies('D:/datasets/KitsuneDatasets/SSL_lab_1-004.csv','SSL_lab_1-004_d.csv')
#detectAnomalies('D:/datasets/KitsuneDatasets/ssl_renego.csv','ssl_renego_d.csv')
#detectAnomalies('D:/datasets/KitsuneDatasets/SYN_lab_1-001.csv','SYN_lab_1-001_d.csv')
#detectAnomalies('D:/datasets/KitsuneDatasets/etterArp.csv','etterArp_d.csv')