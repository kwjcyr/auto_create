#!/usr/bin/python
import re
import ConfigParser
config_file_path="conf.xml"
cf = ConfigParser.ConfigParser()
cf.read(config_file_path)
srcfile = eval(cf.get("ccsm_before","srcfile"))
desfile = eval(cf.get("ccsm_before","desfile"))

ccc1 = eval(cf.get("ccsm_before","ccc1"))
ccc1a1 = eval(cf.get("ccsm_before","ccc1a1"))
c1 = eval(cf.get("ccsm_before","c1"))
ccc2 = eval(cf.get("ccsm_before","ccc2"))
ccc3 = eval(cf.get("ccsm_before","ccc3"))
c3 = eval(cf.get("ccsm_before","c3"))
ccc4 = eval(cf.get("ccsm_before","ccc4"))
map1 = eval(cf.get("ccsm_before","map1"))
c5 = eval(cf.get("ccsm_before","c5"))
cc6 = eval(cf.get("ccsm_before","cc6"))
ccc6 = eval(cf.get("ccsm_before","ccc6"))
ccc7 = eval(cf.get("ccsm_before","ccc7"))
ccc8 = eval(cf.get("ccsm_before","ccc8"))


src = open(srcfile,'r+')
lines = src.readlines()
src.close()

i = 0
listwrite=[]
while i < len(lines):
    line = lines[i]
    if '{list}' in line:
        key = re.findall(r'key\=\"(.*?)\"',line)
        mylist = vars()[key[0]]
        i = i+1
        line = lines[i]
        for c in mylist:
	    if isinstance(c,list):
		tmplist = line[1:].replace('{ccc1}',c[0])
		tmplist = tmplist.replace('{ccc2}',c[1])
     	    else:
            	tmplist = line[1:].replace('{c}',c)
            	tmplist = tmplist.replace('{ccc}',c)
            	tmplist = tmplist.replace('{cc}',c)
 	    if 'gcamgcam' in tmplist:
		tmplist = tmplist.replace('gcamgcam','gcamcam')
            listwrite.append(" "+tmplist)
    else:
        listwrite.append(line)
    i = i+1
des = open(desfile,'w')
des.writelines(listwrite)
des.close()

