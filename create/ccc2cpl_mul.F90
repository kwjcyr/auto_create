
      !----------------------------------------------------------
      ! {ccc} -> cpl
      !----------------------------------------------------------
      
      if (( &
!{list} [ccc0]
!       {ccc0}_present .or. &
        ) .and. {ccc}run_alarm) then
         if (iamin_CPL{CCC}ID) then
            call t_drvstartf ('DRIVER_{C}2C',barrier=mpicom_CPL{CCC}ID)
!<list> [ccc0] [c0]    
!           if({ccc0}_present) then       
!               call t_drvstartf ('driver_{c0}2c_{ccc0}{c0}2{ccc0}x',barrier=mpicom_CPL{CCC}ID)
!               call map_{ccc0}{c0}2{ccc0}x_mct( cdata_{c0}{c0}, {c0}2x_{c0}{c0}, cdata_{c0}x, {c0}2x_{c0}x)
!               call t_drvstopf  ('driver_{c0}2c_{ccc0}{c0}2{ccc0}x')
!           endif
!</list>            
            call t_drvstartf ('driver_{c}2c_infoexch',barrier=mpicom_CPL{CCC}ID)
            call seq_infodata_exchange(infodata,CPL{CCC}ID,'{ccc}2cpl_run')
            call t_drvstopf  ('driver_{c}2c_infoexch')
            call t_drvstopf  ('DRIVER_{C}2C')
         endif

         if (iamin_CPLID) then
            call t_drvstartf  ('DRIVER_{CCC}POST',cplrun=.true.,barrier=mpicom_CPLID)
            if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
            if (info_debug > 1) then
               call t_drvstartf ('driver_{ccc}post_diagav',barrier=mpicom_CPLID)
!<list> [ccc0] [c0]        
!              if ({ccc0}_present) then       
!                  call seq_diag_avect_mct(cdata_{c0}x,{c0}2x_{c0}x,'recv {ccc0}')
!              endif
!</list>               
               call t_drvstopf  ('driver_{ccc}post_diagav')
            endif
            
            if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
            call t_drvstopf  ('DRIVER_{CCC}POST',cplrun=.true.)
         endif

      endif
