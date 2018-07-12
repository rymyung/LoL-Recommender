# -*- coding: utf-8 -*-
"""
Created on Thu Apr 19 23:23:13 2018

@author: rymyung
"""

from selenium import webdriver
import time
import pandas as pd
from bs4 import BeautifulSoup


def getGameRecord(user_df) :
    
    name_list = []
    play_champs_list = []
    type_list = []
    result_list = []
    length_list = []
    kill_list = []
    death_list = []
    assist_list = []
    kda_list = []
    multi_list = []
    cs_list = []
    ckrate_list = []
    mmr_list = []
    nick_list = []
    champs_all_list = []
    for j in range(len(user_df)) :
        try :
            names = user_df['name'][j].replace(' ', '')
            url = 'http://www.op.gg/summoner/userName=' + names
            
            
            driver = webdriver.Chrome('C:/Users/\Ro_PC/Dropbox/Public/ETC/webdriver/chromedriver.exe')
            driver.get(url)
            
            # Click Solo Rank
            driver.find_element_by_xpath('//*[@id="SummonerLayoutContent"]/div[1]/div[2]/div/div[1]/div/ul/li[2]/a').click()
            time.sleep(2)
            
            page = 3
            while True :
                # Scroll Down
                driver.execute_script("window.scrollTo(0,0); window.scrollTo(0, document.body.scrollHeight);")
                time.sleep(2)
                
                # Click More
                element = driver.find_element_by_xpath("//*[@id=\"SummonerLayoutContent\"]/div[1]/div[2]/div/div[2]/div[" + str(page) + "]/a")
                driver.execute_script("arguments[0].click();", element)
                time.sleep(2)
                
                # Get Game Result
                contents = driver.find_elements_by_class_name('GameItemWrap')
                check = contents[-1].text.split('\n')
                
                page += 1
                if check[1] == "한달 전" :
                    break
                
            html_source = driver.page_source
            
            parser = BeautifulSoup(html_source, "html.parser")
            games = parser.find_all('div', attrs = {'class' : 'GameItemWrap'})
            
            for k in range(len(games)) :
                gametype = games[k].find('div', attrs = {'class' : 'GameType'}).text.replace('\n', '').replace('\t', '')
                gameresult = games[k].find('div', attrs = {'class' : 'GameResult'}).text.replace('\n', '').replace('\t', '')
                gamelength = games[k].find('div', attrs = {'class' : 'GameLength'}).text
                
                kill_info = games[k].find_all('span', attrs = {'class' : 'Kill'})
                kill = kill_info[0].text
                if len(kill_info) > 1 :
                    multikill = kill_info[1].text
                else :
                    multikill = ""
                death = games[k].find('span', attrs = {'class' : 'Death'}).text
                assist = games[k].find('span', attrs = {'class' : 'Assist'}).text
                kda = games[k].find('span', attrs = {'class' : 'KDARatio'}).text
                
                gameCS = games[k].find('div', attrs = {'class' : 'CS'}).text.replace('\n', '').replace('\t', '').replace('(', '').replace(')', '').split(' ')[1]
                gmaeckrate = games[k].find('div', attrs = {'class' : 'CKRate tip'}).text.replace('\n', '').replace('\t', '').split(' ')[1]
                gameMMR = games[k].find('div', attrs = {'class' : 'MMR'}).text.split(' ')[2]
                
                teams = games[k].find_all('div', attrs = {'class' : 'Team'})
                
                try :
                    team_check = teams[0].find('div', attrs = {'class' : 'Summoner Requester'}).find('div', attrs = {'class' : 'SummonerName'}).text.replace('\n', '')
                    team = teams[0]
                except :
                    team_check = teams[1].find('div', attrs = {'class' : 'Summoner Requester'}).find('div', attrs = {'class' : 'SummonerName'}).text.replace('\n', '')
                    team = teams[1]
                    
                nicknames_list = []
                nicknames = team.find_all('div', attrs = {'class' : 'SummonerName'})
                for i in range(len(nicknames)) :
                    nicknames_list.append(nicknames[i].text.replace('\n', ''))
                
                champs_list = []
                champs = team.find_all('div', attrs = {'class' : 'ChampionImage'})
                for i in range(len(champs)) :
                    champs_list.append(champs[i].text.split('\n')[1])
                    
                name_list.append(names)
                play_champs_list.append(team_check)
                type_list.append(gametype)
                result_list.append(gameresult)
                length_list.append(gamelength)
                kill_list.append(kill)
                death_list.append(death)
                assist_list.append(assist)
                kda_list.append(kda)
                multi_list.append(multikill)
                cs_list.append(gameCS)
                ckrate_list.append(gmaeckrate)
                mmr_list.append(gameMMR)
                nick_list.append(nicknames_list)
                champs_all_list.append(champs_list)
                
        except : 
            continue
    
        driver.close()
        
        if j+1 % 10 == 0 :
            print("{} of {} are done.".format(j+1, len(user_df)))
    
    game_df = pd.DataFrame(columns = ['name', 'play_champ', 'type', 'result', 'length', 'kill', 'death', 'assist', 'kda', 'multi', 'cs', 'ckrate', 'mmr', 'teams', 'champs'])
    game_df['name'] = name_list
    game_df['play_champ'] = play_champs_list
    game_df['type'] = type_list
    game_df['result'] = result_list
    game_df['length'] = length_list
    game_df['kill'] = kill_list
    game_df['death'] = death_list
    game_df['assist'] = assist_list
    game_df['kda'] = kda_list
    game_df['multi'] = multi_list
    game_df['cs'] = cs_list
    game_df['ckrate'] = ckrate_list
    game_df['mmr'] = mmr_list
    game_df['teams'] = nick_list
    game_df['champs'] = champs_all_list
    
    return game_df
