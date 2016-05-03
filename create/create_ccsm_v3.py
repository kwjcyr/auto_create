#!/usr/bin/python
import re
import ConfigParser
config_file_path="conf.xml"
cf = ConfigParser.ConfigParser()
cf.read(config_file_path)

srcfile=eval(cf.get("ccsm_run","srcfile"))
desfile=eval(cf.get("ccsm_run","desfile"))
inputccc=eval(cf.get("ccsm_run","inputccc"))
CCC=eval(cf.get("ccsm_run","CCC"))
CCC1=eval(cf.get("ccsm_run","CCC1"))
c0=eval(cf.get("ccsm_run","c0"))
ccc0=eval(cf.get("ccsm_run","ccc0"))
ccc_init_mct = eval(cf.get("ccsm_run","ccc_init_mct"))
inputccc_both=eval(cf.get("ccsm_run","inputccc_both"))
mrg_init_ccc=eval(cf.get("ccsm_run","mrg_init_ccc"))
mapping_init=eval(cf.get("ccsm_run","mapping_init"))
dict_cpl2ccc = eval(cf.get("ccsm_run","dict_cpl2ccc"))
dict_prepmodel = eval(cf.get("ccsm_run","dict_prepmodel"))
dict_mulmodel = eval(cf.get("ccsm_run","dict_mulmodel"))
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
	    elif sign ==0 and '{CCC1}' in line:
		clist = CCC1
		sign = 6 
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
    		elif sign == 6:
		    tmplist6 = ' '+tl[1:].replace('{CCC1}',clist[c][0]).replace('{ccc}',clist[c][1])
		    listwrite.append(tmplist6)
    elif '{xmlinsert}' in line:
	fname = line.split(',')[0].split('(')[1]
	tmpname = line.split(',')[1] 
	modname = line.split(',')[2].split(')')[0]
	#print fname,tmpname,modname
	s = vars()[tmpname][modname]
	#print s
	fname = 'create/'+fname
	modulefile = open(fname,'r+')
	if tmpname == "mapping_init":
	    for tmpline in modulefile:
	    	listwrite.append(tmpline.replace('{CCC}',s[0]).replace('{C}',s[1]). \
			replace('{ccc}',s[2]).replace('{c}',s[3]).replace('{segment}',s[4]))
    	elif tmpname == "dict_mulmodel" or tmpname == "dict_prepmodel":
	    while 1:
	    	tmpline = modulefile.readline()
		if not tmpline:
		    break
		if '{list}' in tmpline:
		    tmpkey = re.search(r'\[(.*?)\]',tmpline) 
		    key = tmpkey.groups()[0]
		    tmpl = s[4][key]
		    tmpline = modulefile.readline()
		    tmplist = []
		    for t in tmpl:
		    	tmplist.append(tmpline[1:].replace('{'+key+'}',t))
		    for tl in tmplist:
			tl = tl.replace('x2r_rr,','       ')
			tl = tl.replace('{CCC}',s[0]).replace('{C}',s[1]).replace('{ccc}',s[2]).replace('{c}',s[3])
			listwrite.append(tl)
		elif '<list>' in tmpline:
		    keys = re.findall(r'\[(.*?)\]',tmpline)
		    tmpls = []
		    for key in keys:
                    	tmpls.append(s[4][key])
		    tmplists = []
		    tl = []
        	    tmpline = modulefile.readline()
        	    while '</list>' not in tmpline:
			tmplists.append(tmpline)
			tmpline = modulefile.readline()
		    #print  "cyr",tmpls[0],len(tmpls[0])
		    for c in range(len(tmpls[0])):
			for tmpline in tmplists:
			    for key in keys:
				#print "key",key
				#print tmpls
				tmpline = tmpline.replace('{'+key+'}',s[4][key][c])
			    #print "cyr",tl
			    tl.append(tmpline[1:])	
		    for t in tl:    
			listwrite.append(t.replace('{CCC}',s[0]).replace('{C}',s[1]).replace('{ccc}',s[2]).replace('{c}',s[3]))
		else:
                    listwrite.append(tmpline.replace('{CCC}',s[0]).replace('{C}',s[1]).replace('{ccc}',s[2]).replace('{c}',s[3]))
    elif line:
	listwrite.append(line)
    else:
	break
src.close()

des = open(desfile,'w')
for listw in listwrite:
    des.writelines(listw)
des.close()
