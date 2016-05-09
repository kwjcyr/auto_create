#!/usr/bin/python
import re
import ConfigParser

srcfile = "ccc_CplIndices.F90"
modccc = [['ocn','POP']]

src = open(srcfile,'r')
lines = src.readlines()
src.close()

for ccc in modccc:

    xmlconf = ccc[0]+"_conf.xml"
    xmlcf = ConfigParser.ConfigParser()
    xmlcf.read(xmlconf)
    attr_c2x = xmlcf.get("conf","attr_c2x")
    attr_x2c = xmlcf.get("conf","attr_x2c")

    listwrite = []
    i = 0
    while i < len(lines):
        if '{list}' in lines[i]:
            key = re.findall(r'key=\"(.*?)\"',lines[i])
            array = vars()[key[0]]
            for attr in array:
                
        line = lines[i]
        tmpline = line
        tmpline = tmpline.replace('{ccc}',ccc[0]).replace('{c}',ccc[0][0]).replace('{CCC}',ccc[1])
        listwrite.append(tmpline)
        i = i + 1
    desfile = ccc[1]+"_CplIndies.F90"
    des = open(desfile,'w')
    des.writelines(listwrite)
    des.close()
