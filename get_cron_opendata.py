import json 
import ssl
import urllib.request
from datetime import datetime, timedelta
import sys
import configparser
import csv


def getdata(start,end):
    url = 'https://agridata.coa.gov.tw/api/v1/AgriProductsTransType/?Start_time='+start+'&End_time='+end
    ssl._create_default_https_context = ssl._create_unverified_context
    print(url)
    data = urllib.request.urlopen(url).read()
    data = json.loads(data)
    return data




conf = configparser.ConfigParser()
conf.read('Config.ini')
start = conf['daterange']['from']
end = conf['daterange']['to']
filename = conf['file']['name']
date = datetime.strptime(start, '%Y-%m-%d')
date_end = datetime.strptime(end, '%Y-%m-%d')


with open(filename, 'w', newline='') as csvfile:
    fieldnames = ['TransDate','CropCode','CropName','MarketCode','MarketName','Upper_Price','Middle_Price','Lower_Price','Avg_Price','Trans_Quantity']
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
    writer.writeheader()

    while date <= date_end:
        date_str = str(date.year - 1911)+date.strftime('.%m.%d')
        datesql = date.strftime('%Y-%m-%d')
        print(date_str)
        data = getdata(date_str,date_str)
        writer.writerows(data["Data"])    
        
        date = date + timedelta(days=1)