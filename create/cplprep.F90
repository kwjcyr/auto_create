         !----------------------------------------------------
         ! {ccc} prep
         !----------------------------------------------------

         if (iamin_CPLID .and. {ccc}_prognostic) then
            call t_drvstartf('DRIVER_{CCC}PREP',cplrun=.true.,barrier=mpicom_CPLID)
            if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)

!<list> [ccc1] [c1]
!           ! Map {ccc1} to {ccc}
!           call t_drvstartf('driver_{ccc}prep_{ccc1}2{ccc}',barrier=mpicom_CPLID)
!           call map_{ccc1}2{ccc}_mct( cdata_{c1}x, {c1}2x_{c1}x,cdata_{c}x, {c1}2x_{c}x )
!           call t_drvstopf  ('driver_{ccc}prep_{ccc1}2{ccc}')
!</list>                        
 

            !merge {ccc} inputs
            call t_drvstartf('driver_{ccc}prep_mrgx2{c}',barrier=mpicom_CPLID)
            call mrg_x2{c}_run_mct( cdata_{c}x, & 
!<list> [c1]            
!                                   {c1}2x_{c}x, &
!</list>                                        
                                        x2{c}_{c}x )
            call t_drvstopf  ('driver_{ccc}prep_mrgx2{c}')

            if (info_debug > 1) then
               call t_drvstartf('driver_{ccc}prep_diagav',barrier=mpicom_CPLID)
               call seq_diag_avect_mct(cdata_{c}x,x2{c}_{c}x,'send{ccc}')
               call t_drvstopf  ('driver_{ccc}prep_diagav')
            endif
                        
            if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
            call t_drvstopf  ('DRIVER_{CCC}PREP',cplrun=.true.)
         endif  
