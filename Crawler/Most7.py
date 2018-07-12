# -*- coding: utf-8 -*-
"""
Created on Thu Apr 19 22:12:29 2018

@author: rymyung
"""

import requests
from bs4 import BeautifulSoup
import pandas as pd

def getMost7(user_df) :
    
    name_list = []
    champ_name_list = []
    cs_list = []
    KDA_list = []
    kill_list = []
    death_list = []
    assist_list = []
    winratio_list = []
    play_num_list = []
    
    for j in range(len(user_df)) :
        
        names = user_df['summonerName'][j].replace(' ', '')
        url = 'http://www.op.gg/summoner/userName=' + names
        
        response = requests.get(url)
        parser = BeautifulSoup(response.content, 'html.parser')
        
        champs = parser.find_all('div', attrs = {'class' : 'ChampionBox Ranked'})
        
        for i in range(len(champs)) :
            try :
                champ_name = champs[i].find('div', attrs = {'class' : 'ChampionName'}).text.replace('\n', '').replace('\t', '')
            except AttributeError :
                champ_name = ""
            
            try :
                cs = champs[i].find('div', attrs = {'class' : 'ChampionMinionKill tip'}).text.replace('\n', '').replace('\t', '')
            except AttributeError :
                cs = ""
                
            try :
                KDA = champs[i].find('span', attrs = {'class' : 'KDA'}).text
            except AttributeError :
                KDA = ""
                
            try :
                KDAEach = champs[i].find('div', attrs = {'class' : 'KDAEach'})
                kill = KDAEach.find('span', attrs = {'class' : 'Kill'}).text
                death = KDAEach.find('span', attrs = {'class' : 'Death'}).text
                assist = KDAEach.find('span', attrs = {'class' : 'Assist'}).text
            except AttributeError :
                kill = ""
                death = ""
                assist = ""
                
            try :
                winratio = champs[i].find('div', attrs = {'title' : 'Win Ratio'}).text.replace('\n', '').replace('\t', '')
            except AttributeError :
                winratio = ""
                
            try :
                play_num = champs[i].find('div', attrs = {'class' : 'Title'}).text.split(' ')[0]
            except AttributeError :
                play_num = ""
            
            
            name_list.append(names)
            champ_name_list.append(champ_name)
            cs_list.append(cs)
            KDA_list.append(KDA)
            kill_list.append(kill)
            death_list.append(death)
            assist_list.append(assist)
            winratio_list.append(winratio)
            play_num_list.append(play_num)
        
        if j+1 % 10 == 0 :
            print('{} of {} is done.'.format(j+1, len(user_df)))
        
    most7 = pd.DataFrame(columns = ['name', 'champ', 'cs', 'kda', 'kill', 'death', 'assist', 'winratio', 'play'])
    most7['name'] = name_list
    most7['champ'] = champ_name_list
    most7['cs'] = cs_list
    most7['kda'] = KDA_list
    most7['kill'] = kill_list
    most7['death'] = death_list
    most7['assist'] = assist_list
    most7['winratio'] = winratio_list
    most7['play'] = play_num_list
    
    return most7

user = pd.read_csv('crawled_data/user.csv', engine = 'python')
most7 = getMost7(user)
