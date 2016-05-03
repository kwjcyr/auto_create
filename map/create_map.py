#! /usr/bin/python
import re
example="map_ccc2ccc_mct.F90"
CCC=['atm','lnd','rof','sno','ice','glc','ocn','cam','wrf','gcam','gea']

#src = file("map_ccc2ccc_mct.F90", "r+")
#strccc = re.compile('{ccc}')
#strc = re.compile('{c}')

for indexc in CCC:
    src = file(example, "r+")
    strccc = re.compile('{ccc}')
    strc = re.compile('{c}')    
    des = file("map_"+indexc+indexc+"_mct.F90", "w+")
    tmpccc = strccc.sub(indexc,src.read())
    tmpc = strc.sub(indexc[0],tmpccc)
    des.writelines(tmpc)
    src.close()
    des.close()

