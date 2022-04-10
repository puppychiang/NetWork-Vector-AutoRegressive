#====================================== 安裝套件 ===========================================
{packages_name=c("rstudioapi","lubridate","dplyr","fGarch")
if (length(setdiff(packages_name, rownames(installed.packages()))) > 0) {
  # install packages if needed
  install.packages(setdiff(packages_name , rownames(installed.packages())))  }
sapply(packages_name, require, character.only = TRUE)}                    # Loading packages
#===========================================================================================

# 20國
#====================================== 設定路徑 ===========================================
dir_path =dirname(rstudioapi::getSourceEditorContext()$path)        #直接讀取R檔案存放的路徑
#===========================================================================================
name=c('澳洲','日本','南韓','中國','香港','新加坡','馬來西亞','印度','沙烏地阿拉伯','土耳其','南非','法國','英國','俄羅斯','德國','巴西','美國','加拿大','墨西哥','阿根廷')
setwd(paste0(dir_path,'/欣翰/market_2020/原始檔')) ; getwd()
moving=250                                  # 窗口大小250天
c=1                                         # 指定國家
logrt=read.csv(paste0(name[c],'.csv'))      # 讀取指定國家大盤資料(含logrt)


t=nrow(logrt)  # 計算2006-2020共有幾天
# para=        matrix(NA,((t-moving+1)-(n1-moving+1)),4+1)    # +1是Date欄位
# res=         matrix(NA,((t-moving+1)-(n1-moving+1)),250+1)  # +1是Date欄位
# sigma=       matrix(NA,((t-moving+1)-(n1-moving+1)),250+1)  # +1是Date欄位
# fitted=      matrix(NA,((t-moving+1)-(n1-moving+1)),250+1)  # +1是Date欄位
para=        matrix(NA,(t-moving+1),4+1)    # +1是Date欄位
res=         matrix(NA,(t-moving+1),250+1)  # +1是Date欄位
sigma=       matrix(NA,(t-moving+1),250+1)  # +1是Date欄位
fitted=      matrix(NA,(t-moving+1),250+1)  # +1是Date欄位
colnames(para)=c('Date','p','q','P','Q')
colnames(res)=c('Date',1:250)
colnames(sigma)=c('Date',1:250)
colnames(fitted)=c('Date',1:250)
n=4*4*3*3


aa=1
for(a in 1:(t-moving+1)){
  i=1
  aic=NULL
  para_temp=   matrix(NA,4,n)
  res_temp=    matrix(NA,250,n)  
  sigma_temp=  matrix(NA,250,n)
  fitted_temp= matrix(NA,250,n)
  tryCatch({  # 自動偵錯,for loop中遇到error可跳過該次並繼續執行
    for(p in 1:4){
      for(q in 0:3){
        for(P in 1:3){
          for(Q in 1:3){
            fit=eval(parse(text=paste0("garchFit(~arma(",p,",",q,")+garch(",P,",",Q,"),
                                     data=logrt$logrt_100[a:(a+250-1)],cond.dist = c('sstd'),trace=F)")))
            cat('第',c,'個國家 ',name[c],'  第',i,'個模型','  ',as.character(ymd(logrt[(a+249),1])),' 第',a,'天\n')
            bt = Box.test(residuals(fit,standardize=T),lag=5,type='Ljung')
            bt1 = Box.test((residuals(fit,standardize=T))^2,lag=5,type='Ljung')
            if(bt$p.value>0.05){
              if(bt1$p.value>0.05){
                aic[i]=fit@fit$ics[1] 
                para_temp[,i]=as.matrix(c(p,q,P,Q))
                res_temp[,i]=   as.matrix(residuals(fit,standardize=T))
                sigma_temp[,i]= as.matrix(fit@sigma.t)
                fitted_temp[,i]=as.matrix(fit@fitted)
              }
            }
            i=i+1
          }
        }
      }
    }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
  min=which.min(aic)
  print(min)
  para[aa,1]=as.character(logrt[(a+249),1])  ; para[aa,2:5] = para_temp[,min]      
  res[aa,1]=as.character(logrt[(a+249),1])   ; res[aa,2:251]=   res_temp[,min]      # 取aic最小的residuals
  sigma[aa,1]=as.character(logrt[(a+249),1]) ; sigma[aa,2:251]= sigma_temp[,min]    # 取aic最小的sigma
  fitted[aa,1]=as.character(logrt[(a+249),1]); fitted[aa,2:251]=fitted_temp[,min]   # 取aic最小的garch fitted value
  aa=aa+1
}


#============================================匯出=============================================
# 儲存 ARMA-GARCH 配適的模型參數、標準化殘差、異值標準差、配適值
{
  setwd(paste0(dir_path,'/欣翰/market_2020')) ; getwd()
  write.csv(para,paste0('parameters_',name[c],'.csv'),row.names = F)# ARMA-GARCH 模型參數
  write.csv(res,paste0('residuals_',name[c],'.csv'),row.names = F)  # ARMA-GARCH 標準化殘差
  write.csv(sigma,paste0('sigma_',name[c],'.csv'),row.names = F)    # ARMA-GARCH 異值標準差
  write.csv(fitted,paste0('fitted_',name[c],'.csv'),row.names = F)  # ARMA-GARCH 配適值
}
