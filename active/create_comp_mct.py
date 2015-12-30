#!/usr/bin/python
import re
import ConfigParser

srcfile = "ccc_comp_mct.F90"
modccc = ['atm','lnd','ocn','glc','ice']

src = open(srcfile,'r')
lines = src.readlines()
src.close()

for ccc in modccc:
    xmlfile = ccc+"_conf.F90"
    xmlsrc = open(xmlfile,'r')
    xmllines = xmlsrc.readlines()
    xmlsrc.close()

    listwrite = []
    i = 0
    while i < len(lines):
        if 'xmlinsert' in lines[i]:
            id = re.findall(r'xmlinsert\(list\[(.*?)\]\)',lines[i])[0]
            j = 0
            while j < len(xmllines):
                while "[start "+id+"]" not in xmllines[j]:
                    j = j + 1
                j = j + 1
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
