#!/usr/bin/python
srcfile="ccsm_init_exp.F90"
desfile="ccsm_init3.F90"
inputccc=[['ATM','a'],['LND','l'],['LND','r'],['LND','s'],\
	  ['ICE','i'],['OCN','o'],['GLC','g'],['WRF','w'],\
	  ['WRF','m'],['ATM','c'],['GEA','ge'],['ATM','ca']]
CCC=['CPL','OCN','ATM','LND','ICE','GLC','WRF','GEA']
ccc_init_mct = ['EClock_o, cdata_oo, x2o_oo, o2x_oo, NLFilename=NLFilename',
		'EClock_a, cdata_aa, cdata_cc, cdata_caca, x2a_aa, a2x_aa, x2c_cc1, x2c_cc2, c2x_cc1, c2x_cc2,x2ca_caca1, x2ca_caca2, ca2x_caca1, ca2x_caca2,twoway_coupling, twoway_nudging, NLFilename=NLFilename',	    
		'EClock_l, cdata_ll, x2l_ll, l2x_ll, cdata_rr,r2x_rr, cdata_ss, x2s_ss, s2x_ss,NLFilename=NLFilename',		    'EClock_i, cdata_ii, x2i_ii, i2x_ii,NLFilename=NLFilename',
                'EClock_g, cdata_gg, x2g_gg, g2x_gg,NLFilename=NLFilename',
		'EClock_w, cdata_ww, cdata_mm, x2w_ww, w2x_ww,x2m_mm1, x2m_mm2, m2x_mm, twoway_coupling, twoway_nudging,NLFilename=NLFilename',
		'EClock_ge, cdata_gege, x2chem_chemchem1,x2chem_chemchem2,chem2x_chemchem']

inputccc_both=[['ATM','atm','a'],['ATM','wrf','w'],['ATM','geatm','g'],\
	       ['LND','lnd','l'],['LND','rof','r'],['LND','sno','s'],\
               ['ICE','ice','i'],['OCN','ocn','o'],['GLC','glc','g']]

mrg_init_ccc=[['atm','a','a','a',['l','o','i'],'0',',xao_ax'],\
	      ['wrf','w','m','m',['c'],'1'],\
	      ['wrf','w','m','m',['c'],'2'],\
	      ['cam','c','c','c',['m'],'0'],\
	      ['geam','ge','ge','chem',['ca'],'1'],\
	      ['geam','ge','ge','chem',['ca'],'2'],\
	      ['cam','ca','ca','ca',['chem'],'0'],\
	      ['ice','i','i','i',['a','o'],'0'],\
	      ['ocn','o','o','o',['a','i','r'],'0'],\
	      ['lnd','l','l','l',['a'],'0'],\
	      ['glc','g','g','g',['s'],'0'],\
	      ['sno','s','s','s',['g'],'0']]
mapping_init=[['ocn_present','atm','ocn','2'],\
	      ['ice_present .and. ocn_present','ocn','ice','2'],\
	      ['ice_present','atm','ice','-1'],\
	      ['rof_present .and. ocnrof_prognostic','rof','ocn','+1'],\
	      ['lnd_present','atm','lnd','2'],\
	      ['sno_present .and. glc_present','sno','glc','2'],\
	      ['wrf_present','cam','wrf','2'],\
	      ['geatm_present','cam','gea','2']]


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
	elif '{c}' in line:
	    clist = inputccc 
   	    for c in clist:
	    	listwrite.append(' '+line[1:].replace('{c}',c[1]).replace('{CCC}',c[0]))
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
	print tmplist	
	print clist,sign
	for c in range(len(clist)):
	    for tl in tmplist:
		if sign == 1:
		     	str = ' '+tl[1:].replace('{CCC}',clist[c])
			str = str.replace('{ccc}',clist[c].lower())
			if '{ccc_init_mct_attr}' in str:
			    str = str.replace('{ccc_init_mct_attr}',ccc_init_mct[c])
			    print str
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
