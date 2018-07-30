# -*- coding: utf-8 -*-
"""
Created on Thu Jul 12 21:36:24 2018

@author: rymyu
"""


import requests
from bs4 import BeautifulSoup
import pandas as pd
from selenium import webdriver
import time

def getUserInfo(page_num) :
    
    ranking_list = []
    #team_list = []
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
                #team = top5[i].find_all('div', attrs = {'class' : 'ranking-highest__team'})[0].text.replace('\t', '').replace('\n', '')
                name = top5[i].find_all('a', attrs = {'class' : 'ranking-highest__name'})[0].text
                
                top5_detail = top5[i].find_all('div', attrs = {'class' : 'ranking-highest__tierrank'})[0]
                tier = top5_detail.find_all('span')[0].text.replace('\t', '').replace('\n', '')
                LP = top5_detail.find_all('b')[0].text.replace('\t', '').replace('\n', '').replace(',', '').replace('LP', '').strip()
                level = top5_detail.find_all('div', attrs = {'class' : 'ranking-highest__level'})[0].text.replace('\t', '').replace('\n', '').replace('Lv.', '')
                
                win = top5[i].find('div', attrs = {'class' : 'winratio-graph__text winratio-graph__text--left'}).text
                lose = top5[i].find('div', attrs = {'class' : 'winratio-graph__text winratio-graph__text--right'}).text
                winratio = top5[i].find('span', attrs = {'class' : 'winratio__text'}).text
                
                ranking_list.append(ranking)
                #team_list.append(team)
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
            #team = users[i].find('td', attrs = {'class' : 'ranking-table__cell ranking-table__cell--team'}).text.replace('\t', '').replace('\n', '')
            
            winratios = users[i].find('td', attrs = {'class' : 'ranking-table__cell ranking-table__cell--winratio'})
            win = winratios.find('div', attrs = {'class' : 'winratio-graph__text winratio-graph__text--left'}).text
            lose = winratios.find('div', attrs = {'class' : 'winratio-graph__text winratio-graph__text--right'}).text
            winratio = winratios.find('span', attrs = {'class' : 'winratio__text'}).text
            
            ranking_list.append(ranking)
            #team_list.append(team)
            name_list.append(name)
            tier_list.append(tier)
            LP_list.append(LP)
            level_list.append(level)
            win_list.append(win)
            lose_list.append(lose)
            winratio_list.append(winratio)
            
        print("{} of {} are done.".format(p+1, page_num))
    
    user_info = pd.DataFrame(columns = ['name', 'ranking', 'tier', 'LP', 'level', 'win', 'lose', 'winratio'])
    user_info['name'] = name_list
    user_info['ranking'] = ranking_list
    #user_info['team'] = team_list
    user_info['tier'] = tier_list
    user_info['LP'] = LP_list
    user_info['level'] = level_list
    user_info['win'] = win_list
    user_info['lose'] = lose_list
    user_info['winratio'] = winratio_list
    
    return user_info


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
        
        names = user_df['name'][j].replace(' ', '')
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
        
        if (j+1) % 10 == 0 :
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



def getGameRecord(user_df) :
    
    name_list = []
    #play_champs_list = []
    type_list = []
    result_list = []
    length_list = []
    time_list = []
    kill_list = []
    death_list = []
    assist_list = []
    kda_list = []
    multi_list = []
    cs_list = []
    ckrate_list = []
    mmr_list = []
    mynick_list = []
    mychamps_all_list = []
    oppnick_list = []
    oppchamps_all_list = []
    
    
    for j in range(len(user_df)) :
        try :
            names = user_df['name'][j].replace(' ', '')
            url = 'http://www.op.gg/summoner/userName=' + names
            
            
            driver = webdriver.Chrome('C:/Users/Ro_Laptop/Dropbox/Public/ETC/webdriver/chromedriver.exe')
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
                if check[1] == "4달 전" :
                    break
                
            html_source = driver.page_source
            
            parser = BeautifulSoup(html_source, "html.parser")
            games = parser.find_all('div', attrs = {'class' : 'GameItemWrap'})
            
            for k in range(len(games)) :
                gametype = games[k].find('div', attrs = {'class' : 'GameType'}).text.replace('\n', '').replace('\t', '')
                gameresult = games[k].find('div', attrs = {'class' : 'GameResult'}).text.replace('\n', '').replace('\t', '')
                gamelength = games[k].find('div', attrs = {'class' : 'GameLength'}).text
                gameTime = str(games[k].find('div', attrs = {'class' : 'TimeStamp'})).split("title")[1].split("\"")[1]
                
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
                gameckrate = games[k].find('div', attrs = {'class' : 'CKRate tip'}).text.replace('\n', '').replace('\t', '').split(' ')[1]
                gameMMR = games[k].find('div', attrs = {'class' : 'MMR'}).text.split(' ')[2]
                
                teams = games[k].find_all('div', attrs = {'class' : 'Team'})
                
                try :
                    team_check = teams[0].find('div', attrs = {'class' : 'Summoner Requester'}).find('div', attrs = {'class' : 'SummonerName'}).text.replace('\n', '')
                    myteam = teams[0]
                    oppteam = teams[1]
                except :
                    team_check = teams[1].find('div', attrs = {'class' : 'Summoner Requester'}).find('div', attrs = {'class' : 'SummonerName'}).text.replace('\n', '')
                    myteam = teams[1]
                    oppteam = teams[0]
                    
                mynicknames_list = []
                mynicknames = myteam.find_all('div', attrs = {'class' : 'SummonerName'})
                for i in range(len(mynicknames)) :
                    mynicknames_list.append(mynicknames[i].text.replace('\n', ''))
                
                mychamps_list = []
                mychamps = myteam.find_all('div', attrs = {'class' : 'ChampionImage'})
                for i in range(len(mychamps)) :
                    mychamps_list.append(mychamps[i].text.split('\n')[1])
                    
                oppnicknames_list = []
                oppnicknames = oppteam.find_all('div', attrs = {'class' : 'SummonerName'})
                for i in range(len(oppnicknames)) :
                    oppnicknames_list.append(oppnicknames[i].text.replace('\n', ''))
                
                oppchamps_list = []
                oppchamps = oppteam.find_all('div', attrs = {'class' : 'ChampionImage'})
                for i in range(len(oppchamps)) :
                    oppchamps_list.append(oppchamps[i].text.split('\n')[1])
                    
                    
                name_list.append(names)
                #play_champs_list.append(team_check)
                type_list.append(gametype)
                result_list.append(gameresult)
                length_list.append(gamelength)
                time_list.append(gameTime)
                kill_list.append(kill)
                death_list.append(death)
                assist_list.append(assist)
                kda_list.append(kda)
                multi_list.append(multikill)
                cs_list.append(gameCS)
                ckrate_list.append(gameckrate)
                mmr_list.append(gameMMR)
                mynick_list.append(mynicknames_list)
                mychamps_all_list.append(mychamps_list)
                oppnick_list.append(oppnicknames_list)
                oppchamps_all_list.append(oppchamps_list)
                
        except : 
            continue
    
            driver.close()
        
        if (j+1) % 10 == 0 :
            print("{} of {} are done.".format((j+1), len(user_df)))
    
        
    game_df = pd.DataFrame(columns = ['name', 'type', 'result', 'length', 'time', 'kill', 'death', 'assist', 'kda', 'multi', 'cs', 'ckrate', 'mmr', 'myTeams', 'myChamps', 'enemyTeams', 'enemyChamps'])
    game_df['name'] = name_list
    game_df['type'] = type_list
    game_df['result'] = result_list
    game_df['length'] = length_list
    game_df['time'] = time_list
    game_df['kill'] = kill_list
    game_df['death'] = death_list
    game_df['assist'] = assist_list
    game_df['kda'] = kda_list
    game_df['multi'] = multi_list
    game_df['cs'] = cs_list
    game_df['ckrate'] = ckrate_list
    game_df['mmr'] = mmr_list
    game_df['myTeams'] = mynick_list
    game_df['myChamps'] = mychamps_all_list
    game_df['enemyTeams'] = oppnick_list
    game_df['enemyChamps'] = oppchamps_all_list

    
    return game_df


def main() :
    user_info = getUserInfo(10)
    user_info.to_csv("C:/Users/rymyu/Dropbox/Public/공부/github/LoL-Recommender/Data/crawled_data_opgg/user.csv", header = True, encoding = 'cp949', index = False)
    
    most7 = getMost7(user_info)
    most7.to_csv('C:/Users/rymyu/Dropbox/Public/공부/github/LoL-Recommender/Data/crawled_data_opgg/most7.csv', header = True, encoding = 'cp949', index = False)
    
    
    game_df = getGameRecord(user_info)
    game_df.to_csv("C:/Users/rymyu/Dropbox/Public/공부/github/LoL-Recommender/Data/crawled_data_opgg/game_record.csv", header = True, encoding = 'cp949', index = False)


if __name__ == "__main__" :
    main()
    
user = pd.read_csv("C:/Users/Ro_Laptop/Dropbox/Public/공부/github/LoL-Recommender/Data/crawled_data_opgg/user.csv", engine = "python")
temp = user.iloc[10:310,]

game_temp = getGameRecord(temp)
gameRecord = pd.concat([gameRecord, game_temp])
