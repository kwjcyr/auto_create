#!/usr/bin/python
import re
import ConfigParser

srcfile = "ccc_comp_mct.F90"
#modccc = ['atm','lnd','ocn','glc','ice']
modccc = ['atm']

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
        if '<list>' in lines[i] and 'export' in lines[i]:
            i = i+1
            line = lines[i]
            print line
            task = "export"
            c=xmlcf.get(task,"{c}")
            x=xmlcf.get(task,"{x}") 
            x1_1=xmlcf.get(task,"{x1_1}") 
            x1_2=xmlcf.get(task,"{x1_2}") 
            ccc_out=xmlcf.get(task,"{ccc_out}") 
            listx1=eval(xmlcf.get(task,"{listx1}"))
            listx2=eval(xmlcf.get(task,"{listx2}"))
            print listx1
            print listx2
            line = line.replace('{c}',c).replace('{ccc_out}',ccc_out).replace('{x}',x)
            for x1 in listx1:
                content = line.replace('{x1}',x1_1).replace('{x2}',x1[0]).replace('{x3}',x1[1])      
                if 'shum' in content:
                    content = content.replace('i)','i+1)')
                content = content.replace('{x4}','')
                listwrite.append(content)
            for x2 in listx2:
                print x2
                x20 = x2[0]
                if len(x2) < 2:
                    x21 = x20
                else:
                    x21 = x2[1]
                content = line.replace('{x1}',x1_2).replace('{x2}',x20).replace('{x3}',x21)
                if len(x2)<3:
                    content = content.replace('{x4}','')
                else:
                    content = content.replace('{x4}',x2[2])
                listwrite.append(content)
            i = i+2
      
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
