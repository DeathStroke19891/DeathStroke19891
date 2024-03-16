#!/usr/bin/python

import sys
import subprocess

direction = sys.argv[1]
current = sys.argv[2]
dic = {"一": 1, "二": 2, "三": 3, "四": 4, "五": 5, "六": 6, "七": 7, "八": 8, "九": 9,
       "十": 10}
if direction == "down":
    subprocess.run(['hyprctl', 'dispatch', 'workspace', str(max(1,min(9,dic[current]+1)))])
else:
    subprocess.run(['hyprctl', 'dispatch', 'workspace', str(max(1,min(9,dic[current]-1)))])
