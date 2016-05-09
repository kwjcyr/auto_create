#! /usr/bin/python
import re
import ConfigParser

config_file_path="conf.xml"
cf = ConfigParser.ConfigParser()
cf.read(config_file_path)

map_example=eval(cf.get("mapconf","map_example"))
map_ccc=eval(cf.get("mapconf","map_ccc"))

for indexc in map_ccc:
    src = file(map_example, "r+")
    strccc = re.compile('{ccc}')
    strc = re.compile('{c}')    
    des = file("map/map_"+indexc+indexc+"_mct.F90", "w+")
    tmpccc = strccc.sub(indexc,src.read())
    tmpc = strc.sub(indexc[0],tmpccc)
    des.writelines(tmpc)
    src.close()
    des.close()

