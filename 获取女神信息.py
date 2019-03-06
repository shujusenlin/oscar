
# -*- coding: utf-8 -*-
"""
Created on Sat Mar  2 16:22:04 2019

@author: Administrator
"""

import pandas as pd
import os
from bs4 import BeautifulSoup  
from selenium import webdriver 
import requests
import math
from selenium import webdriver 
import re

## 获取提名名单
os.chdir('D:/爬虫/奥斯卡影后')

header = {'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win32; x32; rv:54.0) Gecko/20100101 Firefox/54.0',
          'Connection': 'keep-alive'}
cookies ='v=3; iuuid=1A6E888B4A4B29B16FBA1299108DBE9CDCB327A9713C232B36E4DB4FF222CF03; webp=true; ci=1%2C%E5%8C%97%E4%BA%AC; __guid=26581345.3954606544145667000.1530879049181.8303; _lxsdk_cuid=1646f808301c8-0a4e19f5421593-5d4e211f-100200-1646f808302c8; _lxsdk=1A6E888B4A4B29B16FBA1299108DBE9CDCB327A9713C232B36E4DB4FF222CF03; monitor_count=1; _lxsdk_s=16472ee89ec-de2-f91-ed0%7C%7C5; __mta=189118996.1530879050545.1530936763555.1530937843742.18'
cookie = {}
for line in cookies.split(';'):
    name, value = cookies.strip().split('=', 1)
    cookie[name] = value   

queens = []

for k in range(1,10):
    if k == 1:
        url = 'http://award.mtime.com/3/award/31/index.html'
    else:
        url = 'http://award.mtime.com/3/award/31/index-{}.html'.format(str(k))

    html = requests.get(url,cookies=cookie, headers=header).content
    bsObj = BeautifulSoup(html.decode('utf-8'),'lxml')
    this_queens = bsObj.find_all('div',attrs={'class':'review_list'})
    queens = queens+this_queens
    print(k)

## 获取历届信息
queens_df = pd.DataFrame(columns=['name','english_name','year','is_winner','img','profile','film'])
for k in range(len(queens)):
    if k%2 ==0:
        is_winner = 1
    else:
        is_winner = 0
    names = [l.attrs['alt'].split(' ')[0] for l in queens[k].find_all('img')]
    english_names = [l.attrs['alt'].split(' ')[1] for l in queens[k].find_all('img')]
    years = 2019 - math.floor(k/2)
    imgs = [l.attrs['src'].split(' ')[0] for l in queens[k].find_all('img')]
    indexs = [l.find('a').attrs['href'].split(' ')[0] for l in queens[k].find_all('dl')]
    films = [l.attrs['href'].split(' ')[0] for l in queens[k].find_all('a')]
    films = [films[k] for k in range(len(films)) if k %3 ==2]
    this_df= pd.DataFrame({'name':names,
                           'english_name':english_names,
                                 'year':[years]*len(names),
                                 'is_winner':[is_winner]*len(names),
                                 'img':imgs,
                                 'profile':indexs,
                                 'film':films})
    queens_df=queens_df.append(this_df,ignore_index=True)
queens_df = queens_df.drop_duplicates()

queens_df.to_excel('初步统计.xlsx')
queens_df = pd.read_excel('综合统计结果.xlsx')

## 获取女神信息
queens_name = queens_df[['profile']].drop_duplicates().reset_index(level=None, drop=True)

queens_name['photo'] = 'unknown'
queens_name['height_star'] = 'unknown'
queens_name['born_home'] = 'unknown'
queens_name['count_score'] = 'unknown'
err_list=[]
driver = webdriver.Chrome()
driver.maximize_window()    
driver.close() 
driver.switch_to_window(driver.window_handles[0])  

for i in range(queens_name.shape[0]):
    url = queens_name['profile'][i]
    js='window.open("'+url+'")'
    driver.execute_script(js)
    driver.close() 
    driver.switch_to_window(driver.window_handles[0])
    try:
        queens_name['photo'][i] = driver.find_element_by_xpath('//*[@id="personDetailRegion"]/div[1]/span/a').get_attribute('href')
        queens_name['height_star'][i] = driver.find_element_by_xpath('//*[@id="personDetailRegion"]/dl[1]').text        
        queens_name['born_home'][i] = driver.find_element_by_xpath('//*[@id="personDetailRegion"]/dl[2]').text          
        queens_name['count_score'][i] = driver.find_element_by_xpath('//*[@id="personRating"]/div[2]').text
    except:
        err_list=err_list+[i]
        

## 文字处理
queens_name['height'] = [re.search('\d{3,}cm',k)[0] if re.search('\d{3,}cm',k) is not None else 'unknown' for k in queens_name['height_star'] ]    
queens_name['star'] = [re.search('\D{2,}',k)[0][0:2] if re.search('\D{2,}',k) is not None else 'unknown' for k in queens_name['height_star']] 
queens_name['born'] = [float(k[0:4]) if k!='' else 'unknown' for k in queens_name['born_home'] ]    
queens_name['count'] = [float(re.search('\d*人参与',k)[0].replace('人参与','')) if re.search('\d*人参与',k) is not None else 0 for k in queens_name['count_score'] ]   
queens_name['score'] = [float(re.search('平均喜爱度 \d{2}',k)[0].replace('平均喜爱度 ','')) if re.search('平均喜爱度 \d{2}',k) is not None else 0 for k in queens_name['count_score'] ]   

queens_info = pd.merge(queens_df,queens_name[['profile','height','born','count','score','star']],on='profile',how='left')


queens_info.to_excel('影后信息初步.xlsx')