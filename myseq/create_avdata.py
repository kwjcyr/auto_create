#!/usr/bin/python
import re
srcfile = 'seq_avdata_mod_exp.F90'
desfile = 'seq_avdata_mod_ret.F90'

listc = ['a','l','o','i','r','g','s']
cimport = ['a','l','o','i','g','s']
fractionc = ['a','l','i','o','g']
mergec = {'a':['l','i','o'],'l':['a'],'r':['o'],'s':['g'],'i':['a','o'],'o':['a','i'],'g':['s']}

listccc = ['atm','lnd','ice','ocn','rof','glc','sno']


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
	flag = 0
	if isinstance(mylist,dict):
	    flag = 1
        for c in mylist:
	    if flag:
		clist = mylist[c]
		for co in clist:
		    listwrite.append(" "+line[1:].replace('{c}',c).replace('{co}',co))
	    else:
            	listwrite.append(" "+line[1:].replace('{c}',c)) 
    else:
        listwrite.append(line)
    i = i+1
des = open(desfile,'w') 
des.writelines(listwrite)
des.close()
