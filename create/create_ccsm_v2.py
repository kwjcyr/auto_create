#!/usr/bin/python
import ConfigParser
config_file_path="conf.xml"
cf = ConfigParser.ConfigParser()
cf.read(config_file_path)

srcfile=eval(cf.get("ccsm_init","srcfile"))
desfile=eval(cf.get("ccsm_init","desfile"))
inputccc=eval(cf.get("ccsm_init","inputccc"))
CCC=eval(cf.get("ccsm_init","CCC"))
c0=eval(cf.get("ccsm_init","c0"))
ccc0=eval(cf.get("ccsm_init","ccc0"))
ccc_init_mct = eval(cf.get("ccsm_init","ccc_init_mct"))
inputccc_both= eval(cf.get("ccsm_init","inputccc_both"))
mrg_init_ccc=eval(cf.get("ccsm_init","mrg_init_ccc"))
mapping_init=eval(cf.get("ccsm_init","mapping_init"))

src = open(srcfile, "r+")
listwrite = []
clist = []
sign=0
while True:
    line = src.readline()
    if '{list}' in line:  
	#print line
	#listwrite.append(line)
	line = src.readline()
        #print line
	#listwrite.append(line)
	if '{CCC}' in line and '{c}' not in line:	
	    if 'CPL' in line:
	        clist = CCC[1:]
	    else:
	        clist = CCC
	    for c in clist:
            	listwrite.append(' '+line[1:].replace('{CCC}',c).replace('{ccc}',c.lower()))
	elif '{c}' in line and '{ccc0}' not in line:
	    clist = inputccc 
   	    for c in clist:
	    	listwrite.append(' '+line[1:].replace('{c}',c[1]).replace('{CCC}',c[0]))
        elif '{c0}' in line:
	    clist = c0
	    for c in range(len(clist)):
		if c < len(clist)-1:
		    tmplist0 = ' '+line[1:].replace('{c0}',clist[c])
		else:
		    tmplist0 = ' '+line[1:].replace('{c0}',clist[c]).replace(', &','')
                listwrite.append(tmplist0)
	elif '{ccc0}' in line:
	    for c in ccc0:
	    	tmplist0 = ' '+line[1:].replace('{ccc0}',c).replace('{c}',c[0])
            	listwrite.append(tmplist0)
	elif '{ccc2}' in line:
	     clist = inputccc_both
	     for c in range(len(clist)):
             	if 'prognostic' in line and 'rof' in clist[c][1]:
                    listwrite.append(' '+line[1:].replace('{ccc2}','ocnrof').replace('{CCC}',clist[c][0]))
                else:
                    listwrite.append(' '+line[1:].replace('{ccc2}',clist[c][1]).replace('{CCC}',clist[c][0]))
    elif '<list>' in line:
	tmplist = []
	line = src.readline()
	sign = 0
	while '</list>' not in line: 
	    tmplist.append(line)
	    if sign ==0 and '{CCC}' in line and '{c}' not in line:
                if 'CPL' in line:
                    clist = CCC[1:]
                else:
                    clist = CCC
            	sign = 1
            elif sign ==0 and '{c}' in line:
            	clist = inputccc
            	sign = 2
       	    elif sign ==0 and '{ccc2}' in line:
		clist = inputccc_both
		sign = 3
	    elif sign ==0 and '{c2}' in line and '{c2}' in line:
		clist = mrg_init_ccc
		sign = 4
	    elif sign ==0 and '{segment1}' in line:
		clist = mapping_init
		sign = 5
	    else:
           	#print "err"
		pass
	    line = src.readline()
	#print tmplist	
	#print clist,sign
	for c in range(len(clist)):
	    for tl in tmplist:
		if sign == 1:
		     	str = ' '+tl[1:].replace('{CCC}',clist[c])
			str = str.replace('{ccc}',clist[c].lower())
			str = str.replace('{c}',clist[c][0].lower())
			if '{ccc_init_mct_attr}' in str:
			    str = str.replace('{ccc_init_mct_attr}',ccc_init_mct[c])
			    #print str
			listwrite.append(str)
		elif sign == 2:
		     listwrite.append(' '+tl[1:].replace('{c}',clist[c][1]).replace('{CCC}',clist[c][0]))
		elif sign == 3:
		     if 'prognostic' in tl and 'rof' in clist[c][1]:
		     	listwrite.append(' '+tl[1:].replace('{ccc2}','ocnrof').replace('{CCC}',clist[c][0]))
		     else:
		     	listwrite.append(' '+tl[1:].replace('{ccc2}',clist[c][1]).replace('{CCC}',clist[c][0]))
		elif sign == 4:
		    if '0' in clist[c][5]:
			if clist[c][0] == 'ice' or clist[c][0] == 'ocn' or clist[c][0] == 'lnd' or clist[c][0] == 'glc' or clist[c][0] == 'sno':
			    tmplist4 = ' '+tl[2:].replace('{ccc}',clist[c][0]).replace('{c}',clist[c][1]).replace('{c1}',clist[c][2])
			else:
			    tmplist4 = ' '+tl[1:].replace('{ccc}',clist[c][0]).replace('{c}',clist[c][1]).replace('{c1}',clist[c][2])
                        if '{other}' in tmplist4:
                            if len(clist[c])>6 :
                                tmplist4 = tmplist4.replace('{other}',clist[c][6])
                            else:
                                tmplist4 = tmplist4.replace('{other}','')
			if '{c3}' in tmplist4:
                            for cn in range(len(clist[c][4])):
                                tmplist4 = ' '+tl[1:].replace('{c2}',clist[c][3]).replace('{c3}',clist[c][4][cn]).replace('{d}','')
                                listwrite.append(tmplist4)
                        else:
                            listwrite.append(tmplist4)
		    else:
		    	tmplist4 = ' '+tl[1:].replace('{ccc}',clist[c][0]).replace('{c}',clist[c][1]).replace('{c1}',clist[c][2])
			if '{other}' in tmplist4:
			    if len(clist[c])>6 : 
				tmplist4 = tmplist4.replace('{other}',clist[c][6])
			    else:
				tmplist4 = tmplist4.replace('{other}','')
		    	if '{c3}' in tmplist4:
		    	    for cn in range(len(clist[c][4])):
			    	tmplist4 = ' '+tl[1:].replace('{c2}',clist[c][3]).replace('{c3}',clist[c][4][cn]).replace('{d}',clist[c][5]) 
                            	listwrite.append(tmplist4)
		    	else:
			    listwrite.append(tmplist4)
		elif sign == 5:	
		    if '{ccc1}2{ccc2}' in tl:
			if '2' in clist[c][3] or '+1' in clist[c][3]:
			    tmplist5 = '  '+tl[2:].replace('{ccc1}',clist[c][1]).replace('{ccc2}',clist[c][2])
			    tmplist5 = tmplist5.replace('{c1}',clist[c][1][0]).replace('{c2}',clist[c][2][0]) 
			else:
			    tmplist5 = ' '+tl[1:].replace('{ccc1}',clist[c][1]).replace('{ccc2}',clist[c][2])
                            tmplist5 = tmplist5.replace('{c1}',clist[c][1][0]).replace('{c2}',clist[c][2][0])
		    elif '{ccc2}2{ccc1}' in tl:
                        if '2' in clist[c][3] or '-1' in clist[c][3]:
                            tmplist5 = '  '+tl[2:].replace('{ccc1}',clist[c][1]).replace('{ccc2}',clist[c][2])
                            tmplist5 = tmplist5.replace('{c1}',clist[c][1][0]).replace('{c2}',clist[c][2][0])
		    	else:
			    tmplist5 = ' '+tl[1:].replace('{ccc1}',clist[c][1]).replace('{ccc2}',clist[c][2])
                            tmplist5 = tmplist5.replace('{c1}',clist[c][1][0]).replace('{c2}',clist[c][2][0])
		    else:
		    	tmplist5 = ' '+tl[1:].replace('{segment1}',clist[c][0]).replace('{ccc1}',clist[c][1]).replace('{ccc2}',clist[c][2])
		    listwrite.append(tmplist5)
    elif line:
	listwrite.append(line)
    else:
	break
src.close()

des = open(desfile,'w')
for listw in listwrite:
    des.writelines(listw)
des.close()
