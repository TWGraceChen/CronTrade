# 資料集：農產品交易行情
# 資料來源：農業開放資料服務平台https://agridata.coa.gov.tw/api.aspx#operations-tag-%E4%BA%A4%E6%98%93%E8%A1%8C%E6%83%85

# 匯入資料
cron <- read.csv("output.csv")
nrow(cron) #354157

# 因市場名稱與代號非一對一，故重新寫入
market <- read.csv("market.csv")
cron$MarketName <- merge(x = cron, y = market, by = "MarketCode", all.x = TRUE)$MarketName.y


# 資料前處理
## 排除價格為0的資料，資料筆數為349644
cron <- cron[cron$Upper_Price > 0 & cron$Middle_Price > 0 & cron$Lower_Price > 0 & cron$Avg_Price > 0,]
nrow(cron) # 349644

## 民國年轉西元年
cron$TransDate <- as.Date(paste(as.numeric(substr(cron$TransDate,1,3)) + 1911,substr(cron$TransDate,4,9),sep=""),
                format="%Y.%m.%d",tz="UTC")

## 將星期萃取成類別變數wd
cron$TransWd <- format(cron$TransDate,format = "%w")
cron$TransWd <- factor(cron$TransWd,
                       labels=paste("星期",c("日","一","二","三","四","五","六"),sep=""))


## 將月份萃取成類別變數month
cron$TransMonth <- format(cron$TransDate,format="%m")

# 敘述統計
summary(cron[c(6:10)])
                                                                           

# 將連續型資料以boxplot確認是否有離群值
par(family='STKaiti')
layout(matrix(c(1,1,1,1,2,2),1))
boxplot(cron[6:9],xlab="價格(元/公斤)",names = c("上價","中價","下價","平均價"),cex.axis=1.5,cex.lab=1.5)
boxplot(cron[10],xlab="交易量(公斤)",cex.lab=1.5)
dev.off()


# 平均價離群值

outlier <- function(rawdata){
  iqr <- IQR(rawdata)
  notOutlier <- rawdata >= quantile(rawdata,0.25) - 1.5 * iqr & rawdata <= quantile(rawdata,0.75) + 1.5 * iqr
  notOutlier
}

plotoutlier <- function(cronname){
  par(family='STKaiti',mfrow=c(1,2))
  rawdata <- cron$Avg_Price[cron$CropName==cronname]
  hist(rawdata,freq=FALSE,main=paste(cronname,'原始',sep='\n'),xlab='平均價')
  points(rawdata,rep(0.0005,length(rawdata)),col=ifelse(outlier(rawdata),'black','blue'),cex=0.8)
  curve(dnorm(x,mean(rawdata),sd(rawdata)),add=TRUE,col="red",lwd=2)
  #lines(density(rawdata,kernel="gaussian",n=100),col="blue")
  
  
  hist(rawdata[outlier(rawdata)],freq=FALSE,main=paste(cronname,'調整後',sep='\n'),xlab='平均價')
  curve(dnorm(x,mean(rawdata[outlier(rawdata)]),sd(rawdata[outlier(rawdata)])),add=TRUE,col="red",lwd=2)
  #lines(density(rawdata[outlier(rawdata)],kernel="gaussian",n=100),col="blue")
}

sort(table(cron$CropName[cron$Avg_Price > quantile(cron$Avg_Price,0.75)]),decreasing = TRUE)[1:11]
plotoutlier('毛豆-豆仁')
plotoutlier('小番茄-玉女')
plotoutlier('茭白筍-去殼')

plotoutlier('櫻桃-進口')
plotoutlier('蘆筍-進口')
plotoutlier('玉米-進口')
plotoutlier('韭菜-韭菜黃')
plotoutlier('蘆筍-綠蘆筍')



# 去除離群值資料
allcrop <- unique(cron$CropName)

cron.new <- NULL
for (i in 1:length(allcrop)) {
  print(allcrop[i])
  ds <- cron[cron$CropName == allcrop[i],]
  cron.new = rbind(cron.new,ds[outlier(ds$Avg_Price) & outlier(ds$Upper_Price) & outlier(ds$Middle_Price)& outlier(ds$Lower_Price),])
}



# 去除離群資料後的敘述統計
nrow(cron.new)
1 - (nrow(cron.new) / nrow(cron))
summary(cron.new[6:10])

par(family='STKaiti')
layout(matrix(c(1,1,1,1,2,2),1))
boxplot(cron.new[6:9],xlab="價格(元/公斤)",names = c("上價","中價","下價","平均價"),cex.axis=1.5,cex.lab=1.5)
boxplot(cron.new[10],xlab="交易量(公斤)",cex.lab=1.5)
dev.off()




# 2019年度各市場總交易量
par(family='STKaiti')
market.total <- sort(tapply(cron.new$Trans_Quantity/10000,cron.new$MarketName,sum))
bp <- barplot(market.total,horiz = TRUE,cex.names=0.7,las=1,
        main="2019年度市場交易量",xlab="交易量(萬公斤)",ylab="市場")
text(ifelse(market.total<5000,market.total+3000,market.total-2000),bp,
     labels = paste(round(market.total,2),' , ',round(market.total * 100 / sum(market.total),2),'%',sep=''),
     col='blue')


# 2019年度交易星期分佈：
pivot.market <- tapply(cron.new$Trans_Quantity,list("星期"=cron.new$TransWd,"市場"=cron.new$MarketName),sum)
pivot.market <- ifelse(is.na(pivot.market),0,pivot.market)
pivot.market <- pivot.market / rep(colSums(pivot.market),each=7)
week.color <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
par(mfrow = c(1,1),xpd=NA,oma=c(0,0,2,0),family='STKaiti')
barplot(pivot.market,horiz = TRUE,cex.names=0.7,las=1,
      col=week.color,xlab="交易量(%)",ylab="市場")
legend(par("usr")[1],par("usr")[4],paste("星期",c("日","一","二","三","四","五","六"),sep=""),
       pch=15,col=week.color,box.lty=0, horiz=TRUE,xjust=0, yjust=-0.001)
text(0.5,30,labels = "2019年交易星期分佈",cex=1)
dev.off()



# 2019年度Top 10交易量農產品

for (i in 1:length(allcrop)) {
  if (grepl('-',allcrop[i])) {
    cron.new$CropName2[cron.new$CropName==allcrop[i]] <- substr(allcrop[i],1,which(strsplit(allcrop[i], "")[[1]]=="-")-1)
  } else {
    cron.new$CropName2[cron.new$CropName==allcrop[i]] <- allcrop[i]  
    
  }
}

par(family='STKaiti')
topcron <- tail(sort(tapply(cron.new$Trans_Quantity/10000,cron.new$CropName2,sum)),20)
bp <- barplot(topcron,horiz = TRUE,cex.names=0.8,las=1,main="2019年度Top 10交易量農產品",ylab='農產品',xlab='交易量(萬公斤)')
text(ifelse(topcron>5000,topcron-700,topcron+700),bp,labels=round(topcron,2),col='blue')
abline(v = seq(0,10000,2000),col = "gray", lty = 3)



# ds <-merge(
#   aggregate(cron.new$Middle_Price, by=list(cron.new$CropName), FUN=min),
#   aggregate(cron.new$Middle_Price, by=list(cron.new$CropName), FUN=max),
#   by='Group.1')
# colnames(ds) <- c('cronname','min','max')
# ds <- ds[order(ds$min,decreasing = TRUE),]
# 
# plot(0,type="n",ylim=c(1,881),xlim=c(1,1490),xaxt = "n")
# segments(c(1:length(ds$cronname)),ds$min,c(1:length(ds$cronname)),ds$max)



# 檢驗農產品上價及下價是否有顯著差異

signcron <- NULL
notsigncron <- NULL
for (i in 1:length(allcrop)) {
  crop = allcrop[i]
  print(paste('===',crop,'===',sep=''))
  upper.price <- cron.new[cron.new$CropName==crop,6]
  lower.price <- cron.new[cron.new$CropName==crop,8]
  res <- try(t.test(upper.price, lower.price, paired = TRUE))
  if (grepl(pattern = "Error", x = res) == FALSE) {
    t.test.p <- t.test(upper.price, lower.price, paired = TRUE)$p.value
    if (is.na(t.test.p) ) {
      cat("無法檢定\n")
    } else if (t.test.p < 0.05) {
      # cat("上價與下價有顯著差異,p-value為：",t.test.p,"\n")
      signcron <- c(signcron,crop)
    } else {
      # cat("上價與下價無顯著差異,p-value為：",t.test.p,"\n")
      notsigncron <- c(notsigncron,crop)
    }
  }

}

length(allcrop)
length(signcron)
length(notsigncron)
length(allcrop) - length(signcron) - length(notsigncron)


plotttest <- function(name) {
  # name <- '椰子'
  upper <- cron.new$Upper_Price[cron.new$CropName==name]
  lower <- cron.new$Lower_Price[cron.new$CropName==name]
  x.lim <- c(mean(lower) - 3 * sd(lower),mean(upper) + 3 * sd(upper))
  y.lim <- c(0,dnorm(mean(lower),mean(lower),sd(lower)))
  curve(dnorm(x,mean(upper),sd(upper)),xlim = x.lim,ylim=y.lim,col="red",lwd=2,
        main=name,xlab="價格",ylab="Density")
  curve(dnorm(x,mean(lower),sd(lower)),col="blue",lwd=2,add=TRUE)
  legend('topright',col=c('red','blue'),legend=c('上價','下價'),lwd = 2)
}


sort(table(cron.new$CropName[cron.new$CropName %in% signcron]),decreasing = TRUE)[1:10]
par(mfrow=c(2,2),family='STKaiti')
plotttest('花椰菜-青梗')
plotttest('茄子-麻荸茄')
plotttest('甘藍-初秋')
plotttest('香蕉')
dev.off()


sort(table(cron.new$CropName[cron.new$CropName %in% notsigncron]),decreasing = TRUE)[1:10]
par(mfrow=c(2,2),family='STKaiti')
plotttest('虎斑木葉-虎斑木葉')
plotttest('鳳梨-進口')
plotttest('小可愛-(混色)')
plotttest('楊梅')
dev.off()

# 農作物交易量和資料筆數對上下價顯著性的影響
ds <-merge(
  aggregate(cron.new$Trans_Quantity, by=list(cron.new$CropName), FUN=sum),
aggregate(cron.new$Trans_Quantity, by=list(cron.new$CropName), FUN=length),
by='Group.1')

colnames(ds) <- c('農作物名稱','交易量','資料筆數')
ds$col <- ifelse(ds$農作物名稱 %in% signcron,'red',
       ifelse(ds$農作物名稱 %in% notsigncron,'blue','white'))
par(family='STKaiti')
plot(ds$交易量,ds$資料筆數,col=ds$col,xlab='交易量',ylab='資料筆數',
     main='交易量和資料筆數對上下價差異顯著性',
     pch=16,cex=0.5,xlim=c(0,300000),ylim=c(0,800))
abline(h=150,v=72000, lty = 3)

axis(2,at=150,col="red",col.ticks="red")
axis(1,at=72000,col="red",col.ticks="red")
legend('topright',col=c('red','blue'),legend=c('顯著','不顯著'),pch = 16)

# 相關分析
signcron <- NULL
notsigncron <- NULL
for (i in 1:length(allcrop)) {
  crop = allcrop[i]
  # crop = '香蕉'
  print(paste('===',crop,'===',sep=''))
  quantity <- cron.new[cron.new$CropName==crop,10]
  price <- cron.new[cron.new$CropName==crop,9]
  res <- try(cor.test(price,quantity))
  if (grepl(pattern = "Error", x = res) == FALSE) {
    pvalue <- cor.test(price,quantity)$p.value
    if (is.na(pvalue) ) {
      cat("無法檢定\n")
    } else if (pvalue < 0.05) {
      cat("價格和交易量有相關,p-value為：",pvalue,"\n")
      signcron <- c(signcron,crop)
    } else {
      cat("價格和交易量無顯著相關,p-value為：",pvalue,"\n")
      notsigncron <- c(notsigncron,crop)
    }
  }
  
}

plotcortest <- function(name) {
  # name <- '椰子'
  quantity <- cron.new$Trans_Quantity[cron.new$CropName==name]
  price <- cron.new$Avg_Price[cron.new$CropName==name]
  plot(price,quantity,main=name,xlab='平均價',ylab='交易量',pch=16,cex=0.5)
  abline(lm(quantity~price),col="red")
}
length(allcrop)
length(signcron)
length(notsigncron)
length(allcrop) - length(signcron) - length(notsigncron)

sort(table(cron.new$CropName[cron.new$CropName %in% signcron]),decreasing = TRUE)[1:10]
par(mfrow=c(2,2),family='STKaiti')
plotcortest('花椰菜-青梗')
plotcortest('甘藍-初秋')
plotcortest('冬瓜-白皮')
plotcortest('胡瓜-黑刺')
dev.off()

sort(table(cron.new$CropName[cron.new$CropName %in% notsigncron]),decreasing = TRUE)[1:10]
par(mfrow=c(2,2),family='STKaiti')
plotcortest('茄子-麻荸茄')
plotcortest('香蕉')
plotcortest('絲瓜-絲瓜')
plotcortest('扁蒲-花蒲')
dev.off()


