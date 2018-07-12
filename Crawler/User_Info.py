# -*- coding: utf-8 -*-
"""
Created on Thu Apr 19 17:29:52 2018

@author: rymyung
"""

import requests
from bs4 import BeautifulSoup
import pandas as pd

def getUserInfo(page_num) :
    
    ranking_list = []
    team_list = []
    name_list = []
    tier_list = []
    LP_list = []
    level_list = []
    win_list = []
    lose_list = []
    winratio_list = []
    
    for p in range(page_num) :
        url = "http://www.op.gg/ranking/ladder/page=" + str(p+1)
        
        response = requests.get(url)
        parser = BeautifulSoup(response.content, 'html.parser')
        
        
        if p == 0 :
            # Top 5
            top5 = parser.find_all('li', attrs = {'class' : 'ranking-highest__item'})
            
            for i in range(len(top5)) :
                ranking = top5[i].find_all('div', attrs = {'class' : 'ranking-highest__rank'})[0].text
                team = top5[i].find_all('div', attrs = {'class' : 'ranking-highest__team'})[0].text.replace('\t', '').replace('\n', '')
                name = top5[i].find_all('a', attrs = {'class' : 'ranking-highest__name'})[0].text
                
                top5_detail = top5[i].find_all('div', attrs = {'class' : 'ranking-highest__tierrank'})[0]
                tier = top5_detail.find_all('span')[0].text.replace('\t', '').replace('\n', '')
                LP = top5_detail.find_all('b')[0].text.replace('\t', '').replace('\n', '').replace(',', '').replace('LP', '').strip()
                level = top5_detail.find_all('div', attrs = {'class' : 'ranking-highest__level'})[0].text.replace('\t', '').replace('\n', '').replace('Lv.', '')
                
                win = top5[i].find('div', attrs = {'class' : 'winratio-graph__text winratio-graph__text--left'}).text
                lose = top5[i].find('div', attrs = {'class' : 'winratio-graph__text winratio-graph__text--right'}).text
                winratio = top5[i].find('span', attrs = {'class' : 'winratio__text'}).text
                
                ranking_list.append(ranking)
                team_list.append(team)
                name_list.append(name)
                tier_list.append(tier)
                LP_list.append(LP)
                level_list.append(level)
                win_list.append(win)
                lose_list.append(lose)
                winratio_list.append(winratio)
                
        
        users = parser.find_all('tr', attrs = {'class' : 'ranking-table__row '})
        
        for i in range(len(users)) :
            ranking = users[i].find('td', attrs = {'class' : 'ranking-table__cell ranking-table__cell--rank'}).text
            name = users[i].find('td', attrs = {'class' : 'ranking-table__cell ranking-table__cell--summoner'}).find('span').text
            tier = users[i].find('td', attrs = {'class' : 'ranking-table__cell ranking-table__cell--tier'}).text.replace('\t', '').replace('\n', '')
            LP = users[i].find('td', attrs = {'class' : 'ranking-table__cell ranking-table__cell--lp'}).text.replace('\t', '').replace('\n', '').replace(',', '').replace('LP', '').strip()
            level = users[i].find('td', attrs = {'class' : 'ranking-table__cell ranking-table__cell--level'}).text.replace('\t', '').replace('\n', '')
            team = users[i].find('td', attrs = {'class' : 'ranking-table__cell ranking-table__cell--team'}).text.replace('\t', '').replace('\n', '')
            
            winratios = users[i].find('td', attrs = {'class' : 'ranking-table__cell ranking-table__cell--winratio'})
            win = winratios.find('div', attrs = {'class' : 'winratio-graph__text winratio-graph__text--left'}).text
            lose = winratios.find('div', attrs = {'class' : 'winratio-graph__text winratio-graph__text--right'}).text
            winratio = winratios.find('span', attrs = {'class' : 'winratio__text'}).text
            
            ranking_list.append(ranking)
            team_list.append(team)
            name_list.append(name)
            tier_list.append(tier)
            LP_list.append(LP)
            level_list.append(level)
            win_list.append(win)
            lose_list.append(lose)
            winratio_list.append(winratio)
            
        print("{} of {} are done.".format(p+1, page_num))
    
    user_info = pd.DataFrame(columns = ['name', 'ranking', 'team', 'tier', 'LP', 'level', 'win', 'lose', 'winratio'])
    user_info['name'] = name_list
    user_info['ranking'] = ranking_list
    user_info['team'] = team_list
    user_info['tier'] = tier_list
    user_info['LP'] = LP_list
    user_info['level'] = level_list
    user_info['win'] = win_list
    user_info['lose'] = lose_list
    user_info['winratio'] = winratio_list
    
    return user_info

