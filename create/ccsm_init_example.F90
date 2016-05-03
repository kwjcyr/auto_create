!{list}
!call seq_cdata_init(cdata_{c}x, CPLID, dom_{c}x, gsMap_{c}x, infodata,'cdat{c}_{c}x' )

!{list}
!call seq_cdata_init(cdata_{c}{c}, {CCC}ID, dom_{c}{c}, gsMap_{c}{c},infodata, 'cdata_{c}{c}')


   if (drv_threading) then
      if (iamroot_GLOID) write(logunit,*) ' '   
      if (iamroot_GLOID) write(logunit,'(2A)    ') subname,' Test Threading in driver'
      call seq_comm_setnthreads(nthreads_GLOID)
      if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'nthreads_GLOID = ',nthreads_GLOID,seq_comm_getnthreads()
!<list>
!   call seq_comm_setnthreads(nthreads_{CCC}ID)
!   if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'nthreads_{CCC}ID = ',nthreads_{CCC}ID,seq_comm_getnthreads()
!</list>
