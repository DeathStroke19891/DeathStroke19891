#!/usr/bin/python

import subprocess
import json

hyprland_json = subprocess.run(['hyprctl','workspaces','-j'],stdout = subprocess.PIPE).stdout.decode()
workspaces_unsorted = json.loads(hyprland_json)
workspaces = sorted(workspaces_unsorted, key=lambda x: x['id'])

hyprland_active_json = subprocess.run(['hyprctl','activeworkspace','-j'],stdout = subprocess.PIPE).stdout.decode()
active_workspace = json.loads(hyprland_active_json)

numbers = "一二三四五"

literal_start = "eww update work_zhong_when=\"(box :class \\\"workspaces\\\" :orientation \\\"v\\\" "
literal_end = ")\""

button = ""

for i in range(1,6):
    button_boilerplate_current = "(button :class \\\"workspace_current\\\""
    button_boilerplate_empty = "(button :class \\\"workspace_empty\\\" :onclick \\\"hyprctl dispatch workspace "
    button_boilerplate_not_empty = "(button :class \\\"workspace_not_empty\\\" :onclick \\\"hyprctl dispatch workspace "
    index = -1
    for j in range(len(workspaces)):
        if(workspaces[j]['id']==i):
            index = j
    if(index>0):
        if(active_workspace['id'] == i):
            button+=button_boilerplate_current + " \\\"" + numbers[i-1] + "\\\")"
            continue
        if(workspaces[index]['windows']!=0):
            button+=button_boilerplate_not_empty + str(i) + "; ./scripts/workspaces.py\\\" "+ "\\\"" + numbers[i-1] + "\\\")"
            continue
        button+=button_boilerplate_empty + str(i) + "; ./scripts/workspaces.py\\\" "+ "\\\"" + numbers[i-1] + "\\\")"
    else:
        button+=button_boilerplate_empty + str(i) + "; ./scripts/workspaces.py\\\" "+ "\\\"" + numbers[i-1] + "\\\")"

literal = literal_start + button + literal_end
subprocess.Popen(literal, shell=True)
