#!/usr/bin/python
import re
import ConfigParser

srcfile = "ccc_comp_mct.F90"
#modccc = ['atm','lnd','ocn','glc','ice']
modccc = ['atm','ocn']

runtypemap = {"runtype":['          runtype = "initial"','          runtype = "continue"','          runtype = "branch"'],
             "nsrest":['          nsrest = 0','          nsrest = 1','          nsrest = 3']}

src = open(srcfile,'r')
lines = src.readlines()
src.close()

for ccc in modccc:
    xmlconf = ccc+"_conf.xml"
    xmlcf = ConfigParser.ConfigParser()
    xmlcf.read(xmlconf)

    xmlfile = ccc+"_conf.F90"
    xmlsrc = open(xmlfile,'r')
    xmllines = xmlsrc.readlines()
    xmlsrc.close()

    listwrite = []
    i = 0
    while i < len(lines):
        if '{list}' in lines[i]:
            key = re.findall(r'key=\"(.*?)\".*?value=\"(.*?)\"',lines[i])
            #print key[0][0],key[0][1]
            thismap = vars()[key[0][0]]
            mapkey=xmlcf.get("conf","runtypemap_key")
            #print thismap[mapkey]
            index=key[0][1]
            listwrite.append(thismap[mapkey][int(index)]+"\n")
            i = i+1
            print thismap[mapkey][int(index)]
            
        if 'xmlinsert' in lines[i]:
            id = re.findall(r'xmlinsert\(list\[(.*?)\]\)',lines[i])[0]
            j = 0
            while j < len(xmllines):
                while j<len(xmllines) and "[start "+id+"]" not in xmllines[j]:
                    j = j + 1
                j = j + 1
                if j>=len(xmllines):
                    break
                while "[end "+id+"]" not in xmllines[j]:
                    listwrite.append(xmllines[j])
                    j = j + 1
                break
            i = i+1
        flag = 1
        line = lines[i]
        tmpline = line
        tmpline = tmpline.replace('{ccc}',ccc).replace('{c}',ccc[0])
        if flag:
            listwrite.append(tmpline)
        i = i + 1 
    desfile = ccc+"_comp_mct.F90"
    des = open(desfile,'w')
    des.writelines(listwrite)
    des.close()
