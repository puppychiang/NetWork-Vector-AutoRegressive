rm(list=ls())   # 清除所有變數
#====================================== 安裝套件 ===========================================
{packages_name=c("MASS","rstudioapi","lubridate","dplyr","fGarch","vars","cpm","ecp","xtable",
                 "astsa","tseries","lmtest","caret","e1071","PerformanceAnalytics","data.table")
if (length(setdiff(packages_name, rownames(installed.packages()))) > 0) {
  # install packages if needed
  install.packages(setdiff(packages_name , rownames(installed.packages())))  }
sapply(packages_name, require, character.only = TRUE)}                    # Loading packages
#===========================================================================================

#====================================== 函數使用 ===========================================
# RMSE、MAE函數 
{rmse=function(error){sqrt(mean(error^2))}
mae =function(error){mean(abs(error))}}
#===========================================================================================

#設定路徑
#===========================================================================================
dir_path =dirname(rstudioapi::getSourceEditorContext()$path)        #直接讀取R檔案存放的路徑
#===========================================================================================
#        1      2      3      4      5        6        7         8          9          10      11     12     13      14      15     16     17      18       19       20
name=c('澳洲','日本','南韓','中國','香港','新加坡','馬來西亞','印度','沙烏地阿拉伯','土耳其','南非','法國','英國','俄羅斯','德國','巴西','美國','加拿大','墨西哥','阿根廷')
f=c(1:20)
# 選擇國家
#year=2020
c=17

#讀取欲預測的國家的原始檔
#==========================================================================================================
setwd(paste0(dir_path,'/欣翰/market_2020/原始檔')) ; getwd()
if(file.exists(paste0(name[c],'.csv'))==TRUE ){print('Yes! file exists')}else{print('No! file not exists')}
# 讀取指定國家的'日期'與'logrt'
logrt=fread(paste0(name[c],'.csv'),select=c('Date','logrt_100'))  # 用fread讀取速度較快，也可以指定要讀取哪一行
logrt=as.matrix(logrt)

# 讀取'各國開盤收盤順序矩陣'
setwd(dir_path) ; getwd()
adj_order = read.csv('各國開盤收盤順序矩陣.csv')
adj_order = adj_order[,2:21]

# 讀取'指定國家的所有配適資料'
setwd(paste0(dir_path,'/欣翰/market_2020')) ; getwd()
para=    fread(paste0('parameters_',name[c],'.csv'),header=TRUE)   # ARMA-GARCH 配適參數
res=     fread(paste0('residuals_',name[c],'.csv'),header=TRUE)    # ARMA-GARCH 標準化殘差(std residuals)
sigma=   fread(paste0('sigma_',name[c],'.csv'),header=TRUE)        # ARMA-GARCH 異值標準差
fitted=  fread(paste0('fitted_',name[c],'.csv'),header=TRUE)       # ARMA-GARCH 配適值
#==========================================================================================================

#檢查原始logrt與配適結果
#==========================================================================================================
# 選擇從哪一天開始配適
{d=20090102                                             # 先給定要預測的日期
n1=which(as.Date(as.matrix(res[,1]))==as.Date(ymd(d)))  # 將指定日期對應表格中是第幾列擷取出來
if(length(n1)==0){cat('沒有對應的日期,請換一天\n')}     # 檢查資料中有沒有這一天
if(length(n1)!=0){cat('這一天ok\n')}}

# 選擇到哪一天結束配適
{d=20191230                                               # 先給定要預測的日期
  n2=which(as.Date(as.matrix(res[,1]))==as.Date(ymd(d)))  # 將指定日期對應表格中是第幾列擷取出來
  if(length(n2)==0){cat('沒有對應的日期,請換一天\n')}     # 檢查資料中有沒有這一天
  if(length(n2)!=0){cat('這一天ok\n')}}
#==========================================================================================================
(n2-n1+1)

#============================== 建立4種方法預測30天的矩陣===================================
{forecast=matrix(NA,(n2-n1+1),4)
colnames(forecast)=c('REAL','AR(1)','ARMA-GARCH','NAR-GARCH')
#forecast[,1]=as.numeric(logrt[(n+249+1):(n+249+30),2])
sigma_pre=matrix(NA,(n2-n1+1),1)}
#===========================================================================================

#============================== 建立11種指標平均值的矩陣====================================
mse_ave_ar1  =matrix(NA,6,(n2-n1+1))
mse_ave_garch=matrix(NA,6,(n2-n1+1))
# mse_ave_var  =matrix(NA,6,(n2-n1+1))
mse_ave_nar  =matrix(NA,6,(n2-n1+1))
#===========================================================================================

# 幫忙紀錄每一天的配適情況，要疊加
x=1

tempresiduals=list()
for(ii in f){
  cat(ii)
  tempresiduals[[ii]]=fread(paste0('residuals_',name[ii],'.csv'),header=TRUE)  # 讀取要合併的國家的標準化殘差
}

start=Sys.time()
for(n in n1:n2){
  cat(x,'\n')
  # 畫配適圖
  #==========================================================================================================
  # {plot(logrt[n:(n+249),2],type = 'l',xlab='',xaxt="n",ylab='logrt',main=paste0(name[c],'  ',ymd(logrt[(n+249),1])))
  #   points(as.numeric(fitted[n,2:251])+as.numeric(res[n,2:251])*as.numeric(sigma[n,2:251]),type='l',col=2,lwd=1)
  #   points(as.numeric(fitted[n,2:251]),type='l',col=4,lwd=2)
  #   abline(h=0,col=1)
  #   legend("bottomleft",legend=c(paste0(name[c]," Real logrt"), paste0(name[c]," ARMA-GARCH"),
  #                                paste0(name[c]," ARMA-GARCH-check")),col=c(1,4,2), lty=1:1, cex=1.2, box.lty=0,bty="n")}
  #==========================================================================================================
  
  #讀取20國相對於欲預測的國家的'最新收盤標準化殘差'和'日期',並且合併成矩陣,矩陣以res_file1~res_file5表示
  #==========================================================================================================
  setwd(paste0(dir_path,'/欣翰/market_2020')) ; getwd()
  res_file1=matrix(NA,250,21)                      # 建立一期鄰接矩陣需要用到的最新收盤標準化殘差矩陣
  res_file1[,1]=as.character(res$Date[(n-249):n])  # 第一欄是要預測的國家的日期
  res_file1[,(c+1)]=as.numeric(res[n,2:251])       # 第 c欄是要預測的國家的標準化殘差
  res_file2=res_file1  # 建立二期鄰接矩陣需要用到的最新收盤標準化殘差矩陣
  res_file3=res_file1  # 建立三期鄰接矩陣需要用到的最新收盤標準化殘差矩陣
  res_file4=res_file1  # 建立四期鄰接矩陣需要用到的最新收盤標準化殘差矩陣
  res_file5=res_file1  # 建立五期鄰接矩陣需要用到的最新收盤標準化殘差矩陣
  
  # 先讀取所有國家'最新收盤標準化殘差'和'日期',併成一個list,假如進85行的for迴圈才讀檔會很慢
  # s=Sys.time()
  full_res=list()
  full_res[[c]]=res_file1[,c(1,(c+1))]  # 先把要預測的國家的'標準化殘差'擺好，其他國家依照這個日期挑選最新收盤
  for(i in f[-c]){
    cat(i,' ',name[i],'\n')
    temp = tempresiduals[[i]]
    m=NULL
    if(adj_order[c,i]==0){b=0}else{b=1}  # 給定所有國家跟要預測的國家要差幾天，b=1代表差1天，b=0代表差0天
    while(length(m)==0){  # 先找要合併的國家最新收盤日是哪一天
      #cat(b,'\n')
      m=which(as.Date(as.matrix(temp[,1]))==(as.Date(logrt[(n+249),1])-b))  ## 給定所有國家跟要預測的國家要差幾天，b=1代表差1天，b=0代表差0天 
      b=b+1  # 如果差一天沒有對應的日期，就繼續迭帶往上找
    }
    
    temp_1=matrix(NA,250,2)                  # 建立要合併的國家的日期加標準化殘差
    temp_1[,1]=as.matrix(temp[(m-249):m,1])  # 第一欄是要合併的國家的日期
    temp_1[,2]=as.numeric(temp[m,2:251])     # 第二欄是要合併的國家的標準化殘差
    full_res[[i]]=temp_1
  }
  # e=Sys.time()
  # e-s
  
  for(i in f[-c]+1){
    cat(x,name[i-1],'\n')
    temp_1=full_res[[i-1]]  # 讀取要合併的國家的標準化殘差
    #============================================
    #cat('一期\n')
    for(j in 1:250){         # 從第一天開始找其他國家最新的收盤日期
      m=NULL                 # m是存找到的最新收盤日期的位置
      if(adj_order[c,(i-1)]==0){b=0}else{b=1}  # 給定所有國家跟要預測的國家要差幾天，b=1代表差1天，b=0代表差0天
      while(length(m)==0){   
        if(as.Date(temp_1[1,1])>(as.Date(res_file1[j,1])-b)){
          res_file1[j,i]=0   # 如果要找的欲預測國家的日期已經比要合併的國家第一天的日期還要小,標準化殘差先賦予0,跳出迴圈
          m=0                # m賦予0只是為了跳出外面for迴圈的條件
          break
        }
        #cat(b,'\n')
        m=which(as.Date(temp_1[,1])==(as.Date(res_file1[j,1])-b))
        b=b+1 
      }
      if(m==0){next}         # m等於0代表剛剛選不到對應日期,標準化殘先給0,然後跳下一筆繼續迴圈
      #cat(temp_1[m,1],'\n')
      res_file1[j,i]=temp_1[m,2]
    }
    #============================================
    #cat('二期\n')
    for(j in 1:250){
      m=NULL
      if(adj_order[c,(i-1)]==0){b=0}else{b=1}  # 給定所有國家跟要預測的國家要差幾天，b=1代表差1天，b=0代表差0天
      while(length(m)==0){                 # 找欲預測國家的日期對應要合併的國家的最新收盤日期是哪一天
        if(as.Date(temp_1[1,1])>(as.Date(res_file2[j,1])-b)){
          res_file2[j,i]=0   # 如果要找的欲預測國家的日期已經比要合併的國家第一天的日期還要小,標準化殘差先賦予0,跳出迴圈
          m=0                # m賦予0只是為了跳出外面for迴圈的條件
          break
        }
        m=which(as.Date(temp_1[,1])==(as.Date(res_file2[j,1])-1-b))
        b=b+1 
      }
      if(m==0){next}         # m等於0代表剛剛選不到對應日期,標準化殘先給0,然後跳下一筆繼續迴圈
      #cat(temp_1[m,1],'\n')
      res_file2[j,i]=temp_1[m,2]
    }
    #============================================
    #cat('三期\n')
    for(j in 1:250){
      m=NULL
      if(adj_order[c,(i-1)]==0){b=0}else{b=1}  # 給定所有國家跟要預測的國家要差幾天，b=1代表差1天，b=0代表差0天
      while(length(m)==0){  # 找欲預測國家的日期對應要合併的國家的最新收盤日期是哪一天
        if(as.Date(temp_1[1,1])>(as.Date(res_file3[j,1])-b)){
          res_file3[j,i]=0   # 如果要找的欲預測國家的日期已經比要合併的國家第一天的日期還要小,標準化殘差先賦予0,跳出迴圈
          m=0                # m賦予0只是為了跳出外面for迴圈的條件
          break
        }
        m=which(as.Date(temp_1[,1])==(as.Date(res_file3[j,1])-2-b))
        b=b+1 
      }
      if(m==0){next}         # m等於0代表剛剛選不到對應日期,標準化殘先給0,然後跳下一筆繼續迴圈
      #cat(temp_1[m,1],'\n')
      res_file3[j,i]=temp_1[m,2]
    }
    #============================================
    #cat('四期\n')
    for(j in 1:250){
      m=NULL
      if(adj_order[c,(i-1)]==0){b=0}else{b=1}  # 給定所有國家跟要預測的國家要差幾天，b=1代表差1天，b=0代表差0天
      while(length(m)==0){  # 找欲預測國家的日期對應要合併的國家的最新收盤日期是哪一天
        if(as.Date(temp_1[1,1])>(as.Date(res_file4[j,1])-b)){
          res_file4[j,i]=0   # 如果要找的欲預測國家的日期已經比要合併的國家第一天的日期還要小,標準化殘差先賦予0,跳出迴圈
          m=0                # m賦予0只是為了跳出外面for迴圈的條件
          break
        }
        m=which(as.Date(temp_1[,1])==(as.Date(res_file4[j,1])-3-b))
        b=b+1 
      }
      if(m==0){next}         # m等於0代表剛剛選不到對應日期,標準化殘先給0,然後跳下一筆繼續迴圈
      #cat(temp_1[m,1],'\n')
      res_file4[j,i]=temp_1[m,2]
    }
    #============================================
    #cat('五期\n')
    for(j in 1:250){
      m=NULL
      if(adj_order[c,(i-1)]==0){b=0}else{b=1}  # 給定所有國家跟要預測的國家要差幾天，b=1代表差1天，b=0代表差0天
      while(length(m)==0){  # 找欲預測國家的日期對應要合併的國家的最新收盤日期是哪一天
        if(as.Date(temp_1[1,1])>(as.Date(res_file5[j,1])-b)){
          res_file5[j,i]=0   # 如果要找的欲預測國家的日期已經比要合併的國家第一天的日期還要小,標準化殘差先賦予0,跳出迴圈
          m=0                # m賦予0只是為了跳出外面for迴圈的條件
          break
        }
        m=which(as.Date(temp_1[,1])==(as.Date(res_file5[j,1])-4-b))
        b=b+1 
      }
      if(m==0){next}         # m等於0代表剛剛選不到對應日期,標準化殘先給0,然後跳下一筆繼續迴圈
      #cat(temp_1[m,1],'\n')
      res_file5[j,i]=temp_1[m,2]
    }
  }
  colnames(res_file1)=c('Date',name)
  colnames(res_file2)=c('Date',name)
  colnames(res_file3)=c('Date',name)
  colnames(res_file4)=c('Date',name)
  colnames(res_file5)=c('Date',name)
  #==========================================================================================================
  
  # 依照合併所有國家的標準化殘差(res_file1~res_file5)建立一到五期鄰接矩陣(向量) 
  #==========================================================================================================
  # 挑選cor.test的方法有 "pearson" "kendall" "spearman"
  method = "pearson"
  # 滯後一期的網絡
  data = res_file1[,-1]  # 把日期欄先拿掉
  nn=20
  adjacency1 = matrix(0,1,nn)
  for(i in 1:nn){
    x1=as.numeric(data[,c]); y1=as.numeric(data[,i])  
    (res1 = cor.test(x1, y1, method = method)) # check
    if (res1$p.value<0.01){
      adjacency1[1,i]=1
    }
  }
  adjacency1[1,c]=0
  nnn=rowSums(adjacency1)
  if(nnn!=0){
    for(i in 1:nn){
      if(adjacency1[1,i]!=0){
        adjacency1[1,i]=adjacency1[1,i]/nnn
      }
    }
  }
  colnames(adjacency1)=name
  
  # 滯後二期的網絡
  data = res_file2[,-1]  # 把日期欄先拿掉
  adjacency2 = matrix(0,1,nn)
  for(i in 1:nn){
    x1=as.numeric(data[,c]); y1=as.numeric(data[,i])  
    (res1 = cor.test(x1, y1, method = method)) # check
    if (res1$p.value<0.01){
      adjacency2[1,i]=1
    }
  }
  adjacency2[1,c]=0
  nnn = rowSums(adjacency2)
  if(nnn!=0){
    for(i in 1:nn){
      if(adjacency2[1,i]!=0){
        adjacency2[1,i]=adjacency2[1,i]/nnn
      }
    }
  }
  colnames(adjacency2)=name
  
  # 滯後三期的網絡
  data = res_file3[,-1]  # 把日期欄先拿掉
  adjacency3 = matrix(0,1,nn)
  for(i in 1:nn){
    x1=as.numeric(data[,c]); y1=as.numeric(data[,i])  
    (res1 = cor.test(x1, y1, method = method)) # check
    if (res1$p.value<0.01){
      adjacency3[1,i]=1
    }
  }
  adjacency3[1,c]=0
  nnn = rowSums(adjacency3)
  if(nnn!=0){
    for(i in 1:nn){
      if(adjacency3[1,i]!=0){
        adjacency3[1,i]=adjacency3[1,i]/nnn
      }
    }
  }
  colnames(adjacency3)=name
  
  # 滯後四期的網絡
  data = res_file4[,-1]  # 把日期欄先拿掉
  adjacency4 = matrix(0,1,nn)
  for(i in 1:nn){
    x1=as.numeric(data[,c]); y1=as.numeric(data[,i])  
    (res1 = cor.test(x1, y1, method = method)) # check
    if (res1$p.value<0.01){
      adjacency4[1,i]=1
    }
  }
  adjacency4[1,c]=0
  nnn = rowSums(adjacency4)
  if(nnn!=0){
    for(i in 1:nn){
      if(adjacency4[1,i]!=0){
        adjacency4[1,i]=adjacency4[1,i]/nnn
      }
    }
  }
  colnames(adjacency4)=name
  
  # 滯後五期的網絡
  data = res_file5[,-1]  # 把日期欄先拿掉
  adjacency5 = matrix(0,1,nn)
  for(i in 1:nn){
    x1=as.numeric(data[,c]); y1=as.numeric(data[,i])  
    (res1 = cor.test(x1, y1, method = method)) # check
    if (res1$p.value<0.01){
      adjacency5[1,i]=1
    }
  }
  adjacency5[1,c]=0
  nnn = rowSums(adjacency5)
  if(nnn!=0){
    for(i in 1:nn){
      if(adjacency5[1,i]!=0){
        adjacency5[1,i]=adjacency5[1,i]/nnn
      }
    }
  }
  colnames(adjacency5)=name
  #==========================================================================================================
  
  # 計算五個網絡效應項
  #==========================================================================================================
  NAR = matrix(NA,250,7)
  for(i in 1:250){
    NAR[i,1] = adjacency1%*%as.numeric(res_file1[i,-1])
  }
  for(i in 1:250){
    NAR[i,2] = adjacency2%*%as.numeric(res_file2[i,-1])
  }
  for(i in 1:250){
    NAR[i,3] = adjacency3%*%as.numeric(res_file3[i,-1])
  }
  for(i in 1:250){
    NAR[i,4] = adjacency4%*%as.numeric(res_file4[i,-1])
  }
  for(i in 1:250){
    NAR[i,5] = adjacency5%*%as.numeric(res_file5[i,-1])
  }
  NAR[,6]=as.numeric(res_file1[,2])  # 真實標準化殘差
  colnames(NAR)=c('neteffect1','neteffect2','neteffect3','neteffect4','neteffect5','std.residuals','fitted_residuals')
  #==========================================================================================================
  
  # 殘差估計及GARCH-NAR配適情況
  #==========================================================================================================
  p = para$p[n]
  q = para$q[n]
  P = para$P[n]
  Q = para$Q[n]
  if(p>1){
    NARnotinmodel=NAR[(1:p),]             # p代表AR(p) 所以前4天不會有估計值，標準化殘差為0的前幾天不進入估計模型
    NARnotinmodel[,7]=NARnotinmodel[,6]   # 將不進入估計模型那幾天的fitted_residuals直接給定為residuals，而residuals為0
    NAR=NAR[-(1:p),]
    NAR=as.data.frame(NAR)
  }
  if(p==1){
    NARnotinmodel=as.matrix(t(NAR[(1:p),]))   # p代表AR(p) 所以前4天不會有估計值，標準化殘差為0的前幾天不進入估計模型
    NARnotinmodel[,7]=NARnotinmodel[,6]       # 將不進入估計模型那幾天的fitted_residuals直接給定為residuals，而residuals為0
    NAR=NAR[-(1:p),]
    NAR=as.data.frame(NAR)
  }
  # 剩餘的天數進入估計模型
  res.fit = lm(std.residuals ~ neteffect1 + neteffect2 + neteffect3 + neteffect4 + neteffect5 -1, data = NAR)  
  #summary(res.fit)
  NAR[,7]=res.fit$fitted.values      # 標準化殘差的配適值放在 NAR[,7]
  NAR=rbind(NARnotinmodel,NAR)
  
  # rp:把'真實值'、'GARCH配適值'、'NAR-GARCH配適值'併起來
  rp = cbind(as.numeric(logrt[n:(n+249),2]),as.numeric(fitted[n,2:251]),as.numeric(fitted[n,2:251])+NAR[,7]*as.numeric(sigma[n,2:251]))    
  
  # 配適圖
  #  setwd(paste0(dir_path,'/欣翰/配適圖(大)')) ; getwd()
  #png(paste0(name[c],'  ',ymd(logrt[(n+249),1]),' 過去250天配適情況.png'), height = 800 , width = 1700)
  # {plot(rp[,1],type = 'l',xlab='',xaxt="n",ylab='logrt',main=paste0(name[c],'  ',ymd(logrt[(n+249),1]),' 過去250天配適情況'))
  # points(rp[,2],type='l',col=4,lwd=2)
  # points(rp[,3],type='l',col=2,lwd=2)
  # abline(h=0,col=1)
  # legend("bottomleft",legend=c(paste0(name[c]," Real logrt"), paste0(name[c]," GARCH"),
  #         paste0(name[c]," GARCH-NAR")),col=c(1,4,2), lty=1:1, cex=1.2, box.lty=0,bty="n")}
  #dev.off()
  # end=Sys.time()
  # end-start
  #}
  #==========================================================================================================
  
  # 利用 AR(1) 進行配適 
  #==========================================================================================================
  Adj.Close=ts(logrt[n:(n+249),2])
  lag1=lag(Adj.Close)                               # 將Adj.Close滯後一期的資料挑出來
  price=ts.intersect(Adj.Close,lag1,dframe = TRUE)  # 將Adj.Close原始資料和Adj.Close滯後一期的資料併起來
  price$Adj.Close=as.numeric(price$Adj.Close)
  price$lag1=as.numeric(price$lag1)
  linear = lm(formula=Adj.Close~lag1, data=price)
  # rp 把 真實值、GARCH配適值、GARCH+NAR配適值、AR(1)配適值 併起來
  rp=cbind(rp,as.numeric(c(Adj.Close[1],predict(linear))))
  #==========================================================================================================
  
  # 將順序換成'REAL','AR(1)','ARMA-GARCH','NAR-GARCH'
  #==========================================================================================================
  colnames(rp) = c('REAL','ARMA-GARCH','NAR-GARCH','AR(1)')
  rownames(rp) = 1:250
  rp=rp[,c(1,4,2,3)]   # 將順序換成 'REAL','AR(1)','ARMA-GARCH','NAR-GARCH'
  #==========================================================================================================
  
  # 配適結果
  #==========================================================================================================
  #####################################
  # 計算4種方法的 RMSE 和 MAE 和 PMSE #
  #####################################
  # 分別取出指定國家logrt 2006-2019 的 (40%,60%分位數) (20%,80%分位數) (10%,90%分位數) (5%,95%分位數)
  qua=quantile(as.numeric(as.character(logrt[1:(nrow(logrt)),2])),c(0.4,0.6,0.2,0.8,0.1,0.9,0.05,0.95))
  # 取出各百分位數所有的點
  num4060=which((rp[,1]<=qua[1])|(qua[2]<=rp[,1])) # 40%
  num2080=which((rp[,1]<=qua[3])|(qua[4]<=rp[,1])) # 20%
  num1090=which((rp[,1]<=qua[5])|(qua[6]<=rp[,1])) # 10%
  num0595=which((rp[,1]<=qua[7])|(qua[8]<=rp[,1])) # 5 %
  
  # AR(1) RMSE MAE
  mse=matrix(NA,6,4)
  mse[1,1]=rmse(rp[,1]-rp[,2])
  mse[2,1]= mae(rp[,1]-rp[,2])
  # GARCH Rmse MAE
  mse[1,2]=rmse(rp[,1]-rp[,3])
  mse[2,2]= mae(rp[,1]-rp[,3])
  # NAR Rmse MAE
  mse[1,3]=rmse(rp[,1]-rp[,4])
  mse[2,3]= mae(rp[,1]-rp[,4])
  
  # AR(1) Pmse
  mse[3,1]=mean((rp[num4060,1]-rp[num4060,2])/rp[num4060,1])
  mse[4,1]=mean((rp[num2080,1]-rp[num2080,2])/rp[num2080,1])
  mse[5,1]=mean((rp[num1090,1]-rp[num1090,2])/rp[num1090,1])
  mse[6,1]=mean((rp[num0595,1]-rp[num0595,2])/rp[num0595,1])
  # GARCH Pmse
  mse[3,2]=mean((rp[num4060,1]-rp[num4060,3])/rp[num4060,1])
  mse[4,2]=mean((rp[num2080,1]-rp[num2080,3])/rp[num2080,1])
  mse[5,2]=mean((rp[num1090,1]-rp[num1090,3])/rp[num1090,1])
  mse[6,2]=mean((rp[num0595,1]-rp[num0595,3])/rp[num0595,1])
  # # NAR Pmse
  mse[3,3]=mean((rp[num4060,1]-rp[num4060,4])/rp[num4060,1])
  mse[4,3]=mean((rp[num2080,1]-rp[num2080,4])/rp[num2080,1])
  mse[5,3]=mean((rp[num1090,1]-rp[num1090,4])/rp[num1090,1])
  mse[6,3]=mean((rp[num0595,1]-rp[num0595,4])/rp[num0595,1])
  
  # colnames(mse)=c('AR(1)','ARMA-GARCH','ARMA-GARCH-VAR','ARMA-GARCH-NAR')
  # rownames(mse)=c('RMSE','MAE','PMSE 40%','PMSE 20%','PMSE 10%','PMSE 5%')
  # mse
  
  mse_ave_ar1[,x]  =mse[,1]
  mse_ave_garch[,x]=mse[,2]
  mse_ave_nar[,x]  =mse[,3]
  #==========================================================================================================

  x=x+1
}
end=Sys.time()
end-start


# 儲存'配適MSE'
setwd(paste0(dir_path,'/欣翰/MSE配適')) ; getwd()
rownames(mse_ave_ar1)=c('RMSE','MAE','PMSE 40%','PMSE 20%','PMSE 10%','PMSE 5%')
rownames(mse_ave_garch)=c('RMSE','MAE','PMSE 40%','PMSE 20%','PMSE 10%','PMSE 5%')
rownames(mse_ave_nar)=c('RMSE','MAE','PMSE 40%','PMSE 20%','PMSE 10%','PMSE 5%')
write.csv(mse_ave_ar1,paste0(name[c],'ar1配適結果.csv'))
write.csv(mse_ave_garch,paste0(name[c],'garch配適結果.csv'))
write.csv(mse_ave_nar,paste0(name[c],'nar配適結果.csv'))



