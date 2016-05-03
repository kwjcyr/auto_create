#!/usr/bin/python
import ConfigParser
config_file_path="conf.xml"
cf = ConfigParser.ConfigParser()
cf.read(config_file_path)
srcfile = eval(cf.get("ccsm_pre_init","srcfile"))
desfile = eval(cf.get("ccsm_pre_init","desfile"))
CCC = eval(cf.get("ccsm_pre_init","CCC"))

#srcfile="ccsm_pre_init_example.F90"
#desfile="ccsm_pre_init.F90"
#CCC=['CPL','OCN','ATM','LND','ICE','GLC','WRF','GEA']

src = open(srcfile, "r+")
listwrite = []
clist = []
while True:
    line = src.readline()
    if '{lists}' in line:  
	#print line
	#listwrite.append(line)
	line = src.readline()
        #print line
	#listwrite.append(line)
	
	if 'CPL' in line:
	    clist = CCC[1:]
	else:
	    clist = CCC 
	
	for c in clist:
	    listwrite.append("   "+line[1:].replace('{CCC}',c).replace('{ccc}',c.lower()))
    elif line:
	listwrite.append(line)
    else:
	break
src.close()

des = open(desfile,'w')
for listw in listwrite:
    des.writelines(listw)
des.close()


