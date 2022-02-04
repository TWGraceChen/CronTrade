
# 用途 
從政府資料開放平台以api方式蒐集農產品交易行情資料(https://data.gov.tw/dataset/8066) ，並匯入MySQL中。

# Package 
- json 
- ssl 
- urllib.request 
- datetime 
- sys 
- configparser 
- pymysql

# 使用方法 
1. git clone 
2. 安裝package 
3. 編輯Config.ini 

```
[database]  
host = 127.0.0.1 --> Mysql的host  
user = root --> Mysql的user name  
passwd = **** --> Mysql的password  
db = cron --> 要寫入的database，必須事先create  
table = transcron --> 寫入的table，不需先create  

[daterange]  
from = 2018-01-01  -->匯入的日期起始  
to = 2019-05-25  -->匯入的日期結束
執行python3 gen_cron_opendata.py
```
# 欄位敘述

https://agridata.coa.gov.tw/api.aspx#operations-tag-%E4%BA%A4%E6%98%93%E8%A1%8C%E6%83%85

- CropCode(string):農產品代碼
- CropName(string):農產品名稱
- MarketName(string):市場名稱
- TransDate(string):交易日期
- MarketCode(string):市場代號
- Upper_Price(number):上價(元/公斤)
- Middle_Price(number):中價(元/公斤)
- Lower_Price(number):下價(元/公斤)
- Avg_Price(number):平均價(元/公斤)
- Trans_Quantity(number):交易量(公斤)
