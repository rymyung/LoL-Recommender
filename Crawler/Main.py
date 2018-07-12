# -*- coding: utf-8 -*-
"""
Created on Sun Apr 22 04:02:21 2018

@author: Ro_PC
"""


from User_Info import getUserInfo
from Most7 import getMost7
from games import getGameRecord


user_info = getUserInfo(20)
user_info.to_csv("user_info.csv", header = True, encoding = 'cp949', index = False)

most7 = getMost7(user_info)
most7.to_csv('most7.csv', header = True, encoding = 'cp949', index = False)


game_df = getGameRecord(user_info)
game_df.to_csv("game_record.csv", header = True, encoding = 'cp949', index = False)
