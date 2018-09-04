# -*- coding: utf-8 -*-
"""
Created on Sat May 26 15:45:02 2018

@author: Ro_Laptop
"""
import requests
import json
import time
import pandas as pd

# Set API Key
api_key = 'RGAPI-4806a25d-e22d-4d15-ab9d-9156c9984c7a' # Need to chage everyday

# Get Summoner's Names
challenger_url = "https://kr.api.riotgames.com/lol/league/v3/challengerleagues/by-queue/RANKED_SOLO_5x5?api_key=" + api_key
challenger_r = requests.get(challenger_url)
challenger_data = json.loads(challenger_r.text)


name_list = []
for entry in challenger_data['entries'] :
    name_list.append(entry['playerOrTeamName'])
    
master_url = "https://kr.api.riotgames.com/lol/league/v3/masterleagues/by-queue/RANKED_SOLO_5x5?api_key=" + api_key
master_r = requests.get(master_url)
master_data = json.loads(master_r.text)

for entry in master_data['entries'] :
    name_list.append(entry['playerOrTeamName'])   


# Get Account Id
'''
2,000 requests / 1 minute
'''
account_list = []
k = 1

time.sleep(600)
for name in name_list[800:] :
    
    while True :
        account_url = 'https://kr.api.riotgames.com/lol/summoner/v3/summoners/by-name/' + name.replace(' ', '%20') + '?api_key=' + api_key
        account_r = requests.get(account_url)
        account_data = json.loads(account_r.text)
        
        try :
            account_list.append(account_data['accountId'])
            break
        
        except :
            print("error")
            time.sleep(60)
            
    if k % 10 == 0 :
        print('{} of {} is done.'.format(k, len(name_list)))
        
    k += 1
    
user = pd.DataFrame(columns = ['accountId', 'summonerName'])
user['accountId'] = account_list
user['summonerName'] = name_list[0:828]
user_new = user.loc[user['accountId'] != 'error',]
user.to_csv('user2.csv', index = False, encoding = 'cp949')


# Get Game Id
'''
1,000 requests / 10 seconds
'''
user = pd.read_csv('user.csv', engine = 'python')

account_list2 = []
game_list = []
k = 0
for i in range(len(user)) :
    accountId = user.loc[i, 'accountId']
    match1_url = 'https://kr.api.riotgames.com/lol/match/v3/matchlists/by-account/' + str(accountId) + '?api_key=' + api_key
    match1_r = requests.get(match1_url)
    match1_data = json.loads(match1_r.text)
    
    try :
        for match in match1_data['matches'] :
            account_list2.append(accountId)
            game_list.append(match['gameId'])

    except :
        account_list2.append(accountId)
        game_list.append('error')
        k+=1
        print('{} error'.format(k))
    
    if (i+1) % 20 == 0 :
        time.sleep(1)
    
    if (i+1) % 100 == 0 :
        print('{} of {} is done.'.format(i+1, len(user)))
        time.sleep(120)


play_temp = pd.DataFrame(columns = ['accountId', 'gameId'])
play_temp['accountId'] = account_list2
play_temp['gameId'] = game_list
play_temp = play_temp.loc[play_temp['gameId']!='error',]
play_temp.to_csv('play_temp_master.csv', encoding = 'cp949')


# Game 
'''
500 requests / 10 seconds
'''

gameId_list2 = []

gameLength_list = []
queue_list = []
map_list = []
season_list = []
version_list = []
mode_list = []
type_list = []

accountId_list3 = []
champId_list = []
team_list = []
kill_list = []
death_list = []
assist_list = []
dealing_list = []
damaged_list = []
cs_list = []
result_list = []


k = 0
g = 1
game_list = list(set(play_temp['gameId']))
for gameId in game_list :
    game = 'https://kr.api.riotgames.com/lol/match/v3/matches/' + str(gameId) + '?api_key=' + api_key
    game_r = requests.get(game)
    game_data = json.loads(game_r.text)
    
    
    for i in range(10) :
        try :
            gameId_list2.append(gameId)
            gameLength_list.append(game_data['gameDuration']) # Second
            queue_list.append(game_data['queueId']) # queueId == 420 : 5v5 Ranked Solo Games
            map_list.append(game_data['mapId']) # mapId == 11 : Summoner's Rift
            season_list.append(game_data['seasonId']) # seasonId == 11 : SEASON 2018
            version_list.append(game_data['gameVersion']) # Game Version
            mode_list.append(game_data['gameMode']) # gameMode == CLASSIC : Classic Summoner's Rift and Twisted Treeline Games
            type_list.append(game_data['gameType']) # gameType == MATCHED_GAME : All games except custom & tutorial games
    
            
            accountId = game_data['participantIdentities'][i]['player']['accountId']
            participantId = game_data['participantIdentities'][i]['participantId']
            
            accountId_list3.append(accountId)
            team_list.append(game_data['participants'][participantId-1]['teamId'])
            champId_list.append(game_data['participants'][participantId-1]['championId'])
            kill_list.append(game_data['participants'][participantId-1]['stats']['kills'])
            death_list.append(game_data['participants'][participantId-1]['stats']['deaths'])
            assist_list.append(game_data['participants'][participantId-1]['stats']['assists'])
            dealing_list.append(game_data['participants'][participantId-1]['stats']['totalDamageDealtToChampions'])
            damaged_list.append(game_data['participants'][participantId-1]['stats']['totalDamageTaken'])
            cs_list.append(game_data['participants'][participantId-1]['stats']['totalMinionsKilled'])
            if game_data['participants'][participantId-1]['stats']['win'] == True :
                result_list.append('Win')
            elif game_data['participants'][participantId-1]['stats']['win'] == False :
                result_list.append('Lose')
            else :
                result_list.append('Others')
        
        except :
            try :
                time.sleep(120)
                gameId_list2.append(gameId)
                gameLength_list.append(game_data['gameDuration']) # Second
                queue_list.append(game_data['queueId']) # queueId == 420 : 5v5 Ranked Solo Games
                map_list.append(game_data['mapId']) # mapId == 11 : Summoner's Rift
                season_list.append(game_data['seasonId']) # seasonId == 11 : SEASON 2018
                version_list.append(game_data['gameVersion']) # Game Version
                mode_list.append(game_data['gameMode']) # gameMode == CLASSIC : Classic Summoner's Rift and Twisted Treeline Games
                type_list.append(game_data['gameType']) # gameType == MATCHED_GAME : All games except custom & tutorial games
        
                
                accountId = game_data['participantIdentities'][i]['player']['accountId']
                participantId = game_data['participantIdentities'][i]['participantId']
                
                accountId_list3.append(accountId)
                team_list.append(game_data['participants'][participantId-1]['teamId'])
                champId_list.append(game_data['participants'][participantId-1]['championId'])
                kill_list.append(game_data['participants'][participantId-1]['stats']['kills'])
                death_list.append(game_data['participants'][participantId-1]['stats']['deaths'])
                assist_list.append(game_data['participants'][participantId-1]['stats']['assists'])
                dealing_list.append(game_data['participants'][participantId-1]['stats']['totalDamageDealtToChampions'])
                damaged_list.append(game_data['participants'][participantId-1]['stats']['totalDamageTaken'])
                cs_list.append(game_data['participants'][participantId-1]['stats']['totalMinionsKilled'])
                if game_data['participants'][participantId-1]['stats']['win'] == True :
                    result_list.append('Win')
                elif game_data['participants'][participantId-1]['stats']['win'] == False :
                    result_list.append('Lose')
                else :
                    result_list.append('Others')
                    
            except :
                gameId_list2.append(gameId)
                gameLength_list.append('error')
                queue_list.append('error')
                map_list.append('error')
                season_list.append('error')
                version_list.append('error')
                mode_list.append('error')
                type_list.append('error')
                
                accountId_list3.append('error')
                champId_list.append('error')
                team_list.append('error')
                kill_list.append('error')
                death_list.append('error')
                assist_list.append('error')
                dealing_list.append('error')
                damaged_list.append('error')
                cs_list.append('error')
                result_list.append('error')
                
                print('{} errors'.format(k))
                k += 1
            
    if k > 200 :
        break
   
    if g % 20 == 0 :
        time.sleep(1)
       
    if g % 100 == 0 :
        print('{} of {} is done.'.format(str(g), len(game_list)))
        time.sleep(120)
    g += 1

gameId_final_list = []
for i in list(set(gameId_list2)) :
    for j in range(10) :
        gameId_final_list.append(i)


match = pd.DataFrame(columns = ['gameLength', 'queue', 'map', 'season', 'version', 'mode', 'type'])
match['gameId'] = gameId_final_list
match['gameLength'] = gameLength_list
match['queue'] = queue_list
match['map'] = map_list
match['season'] = season_list
match['version'] = version_list
match['mode'] = mode_list
match['type'] = type_list

play = pd.DataFrame(columns = ['gameId', 'accountId', 'champId', 'team', 'kill', 'death', 'assist', 'dealing', 'damaged', 'cs', 'result'])
play['gameId'] = gameId_final_list
play['accountId'] = accountId_list3
play['champId'] = champId_list
play['team'] = team_list
play['kill'] = kill_list
play['death'] = death_list
play['assist'] = assist_list
play['dealing'] = dealing_list
play['damaged'] = damaged_list
play['cs'] = cs_list
play['result'] = result_list

match.to_csv("match_master.csv", index = False, encoding = 'cp949')
play.to_csv("play_master.csv", index = False, encoding = 'cp949')

# Champ
champId_list2 = []
champName_list = []
for value in champ_dic['data'].values() :
    champId_list2.append(value['id'])
    champName_list.append(value['name'])
   
champ = pd.DataFrame(columns = ['champId', 'champName'])
champ['champId'] = champId_list2
champ['champName'] = champName_list
champ.to_csv('champ.csv', index = False, encoding = 'cp949')


len(play)
###############
play = pd.read_csv('crawled_data/play.csv', engine = 'python')
accountId = play.loc[1,'accountId']
gameId = play.loc[1, 'gameId']
champId = play.loc[1, 'champId']
url = 'https://kr.api.riotgames.com/lol/match/v3/matchlists/by-account/'+ str(accountId) + '?champion=' + str(champId) + '&api_key=' + api_key
r = requests.get(url)
data = json.loads(r.text)

lane_list = []
for i in range(10) :
    accountId = play.loc[i,'accountId']
    gameId = play.loc[i, 'gameId']
    champId = play.loc[i, 'champId']
    k = False
    
    url = 'https://kr.api.riotgames.com/lol/match/v3/matchlists/by-account/'+ str(accountId) + '?champion=' + str(champId) + '&api_key=' + api_key
    r = requests.get(url)
    data = json.loads(r.text)

    for match in data['matches'] :
        if gameId == match['gameId'] :
            k = True
            lane_list.append(match['lane'])
            break
        
        if k == False :
            lane_list.append('error')

i = 1284
accountId = play.loc[i,'accountId']
gameId = play.loc[i, 'gameId']
champId = play.loc[i, 'champId']
result = ""

url = 'https://kr.api.riotgames.com/lol/match/v3/matchlists/by-account/'+ str(accountId) + '?champion=' + str(champId) + '&api_key=' + api_key
r = requests.get(url)
data = json.loads(r.text)
data['matches']
for match in data['matches'] :
    if gameId == match['gameId'] :
        result = match['lane']
        break
result


url2 = 'https://kr.api.riotgames.com/lol/match/v3/matchlists/by-account/' + str(accountId) + '?beginIndex=100&' + '?champion=' + str(champId) + '&api_key=' + api_key
r2 = requests.get(url2)
data2 = json.loads(r2.text)
data2
for match in data2['matches'] :
    if gameId == match['gameId'] :
        print(match)
