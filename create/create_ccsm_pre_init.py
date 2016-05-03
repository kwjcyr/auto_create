#!/usr/bin/python
filename="ccsm_pre_init_example.F90"
CCC=['CPL','OCN','ATM','LND','ICE','GLC','WRF','GEA']
lists=['list2', 'list3','list4','list5','list6']

for list in lists:
    src = file(filename, "r+")

str=`grep -n -A1 $list $file | grep -v $list`; 
echo ${str}

startline=${str%%-*}
content=${str#*-!}

#echo ${startline}
#echo ${content}

starti=1

echo "$content" |grep -q CPL
if [ $? -eq 0 ]
then
    echo "2:include"
    starti=2
fi

for i in `seq ${sum} -1 ${starti}`
do
	#echo ${content},${arr[$i]}
	tmpcontent1=${content//'{CCC}'/${arr[$i]}}
	small=$(echo ${arr[$i]}|tr '[A-Z]' '[a-z]')
	#echo ${tmpcontent1}
	#echo ${small}
	
	tmpcontent2=${tmpcontent1//'{ccc}'/${small}}
	echo ${tmpcontent2}
	sed -i "${startline}a ${tmpcontent2}"  $file
done
echo ${startline}

done


import re

CCC=['atm','lnd','rof','sno','ice','glc','ocn','cam','wrf','gcam','gea']

#src = file("map_ccc2ccc_mct.F90", "r+")
#strccc = re.compile('{ccc}')
#strc = re.compile('{c}')

for indexc in CCC:
    src = file("map_ccc2ccc_mct.F90", "r+")
    strccc = re.compile('{ccc}')
    strc = re.compile('{c}')
    des = file("map_"+indexc+indexc+"_mct.F90", "w+")
    tmpccc = strccc.sub(indexc,src.read())
    tmpc = strc.sub(indexc[0],tmpccc)
    des.writelines(tmpc)
    src.close()
    des.close()
