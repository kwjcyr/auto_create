#!/usr/bin/python
import re
import ConfigParser

config_file_path="conf.xml"
cf = ConfigParser.ConfigParser()
cf.read(config_file_path)

srcfile = eval(cf.get("mapconf","srcfile"))
dictpair = eval(cf.get("mapconf","dictpair"))
dom = eval(cf.get("mapconf","dom"))
infodata = eval(cf.get("mapconf","infodata"))
global_grid_sizes = eval(cf.get("mapconf","global_grid_sizes"))
areasrc_dst = eval(cf.get("mapconf","areasrc_dst"))
samegrid = eval(cf.get("mapconf","samegrid"))
without_samegrid = eval(cf.get("mapconf","without_samegrid"))
before_samegrid=eval(cf.get("mapconf","before_samegrid"))
fraction=eval(cf.get("mapconf","fraction"))

src = open(srcfile,'r')
lines = src.readlines()
src.close()
for k,pair in dictpair.iteritems():
    listwrite = []
    i = 0
    while i < len(lines):
        flag = 1
        line = lines[i]
        if '{list}' in line:
            keys = re.findall(r'key\=\"(.*?)\"',line)
            keyslist = vars()[keys[0]]
            if k in keyslist:
                line = lines[i+1][1:]
            else:
                line = lines[i+2][1:]
                if not line.strip():
                    flag = 0
            i = i + 2
        elif '<list>' in line:
            tmplist = []
            keys = re.findall(r'key\=\"(.*?)\"',line)
            keyslist = vars()[keys[0]]
            while '</list>' not in line:
                i = i + 1
                line = lines[i]
                if '</list>' not in line:
                    tmplist.append(line)     
            if k in keyslist:
                for tl in tmplist:
                    tmpline = tl
                    tmpline = tmpline.replace('{ccc1}',pair[0]).replace('{ccc2}',pair[1])
                    tmpline = tmpline.replace('{c1}',pair[0][0]).replace('{c2}',pair[1][0])
                    listwrite.append(tmpline)
            i = i + 1
            line = lines[i]
        tmpline = line
        tmpline = tmpline.replace('{ccc1}',pair[0]).replace('{ccc2}',pair[1])
        tmpline = tmpline.replace('{c1}',pair[0][0]).replace('{c2}',pair[1][0])
        if flag:
            listwrite.append(tmpline)
        i = i + 1 
    desfile = "mapping/"+"map_"+pair[0]+pair[1]+"_mct.F90"
    des = open(desfile,'w')
    des.writelines(listwrite)
    des.close()
    

