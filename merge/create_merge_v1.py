#!/usr/bin/python
import re
import ConfigParser

config_file_path="conf.xml"
cf = ConfigParser.ConfigParser()
cf.read(config_file_path)

srcfile = eval(cf.get("mergeconf","srcfile"))
#srcfile = "mrg_x2{c}_mct.F90"
modlist = eval(cf.get("mergeconf","modlist"))
cother = eval(cf.get("mergeconf","cother"))
#modlist = ['glc','sno','lnd','ice']
#cother = {'glc':['sno'],'sno':['glc'],'lnd':['atm'],'ice':['atm','ocn']}

src = open(srcfile,'r')
lines = src.readlines()
src.close()
for mod in modlist:
    listwrite = []
    i = 0
    while i < len(lines):
        line = lines[i]
        if '<list>' in line:
            key = re.findall(r'key\=\"(.*?)\"',line)
            keyslist = vars()[key[0]][mod]
            tmplist=[] 
            while '</list>' not in line:
                i = i + 1
                line = lines[i]
                if '</list>' not in line:
                    tmplist.append(line)
            for co in keyslist:
                for tl in tmplist:
                    tl = tl.replace('{ccc}',mod).replace('{c}',mod[0])
                    tl = tl.replace('{ccco}',co).replace('{co}',co[0])
                    listwrite.append(tl)
            i = i + 1
            line = lines[i]
        tmpline = line
        tmpline = tmpline.replace('{ccc}',mod).replace('{c}',mod[0])
        tmpline = tmpline.replace('{co}',cother[mod][0][0])
        listwrite.append(tmpline)
        i = i + 1
    desfile = "merge/mrg_x2"+mod[0]+"_mct.F90"
    des = open(desfile,'w')
    des.writelines(listwrite)
    des.close()
