      !----------------------------------------------------------
      ! {ccc} -> cpl
      !----------------------------------------------------------
      
      if ({segment}) then
         if (iamin_CPL{CCC}ID) then
            call t_drvstartf ('DRIVER_{C}2C',barrier=mpicom_CPL{CCC}ID)
            call t_drvstartf ('driver_{c}2c_{ccc}{c}2{ccc}x',barrier=mpicom_CPL{CCC}ID)
            call map_{ccc}{c}2{ccc}x_mct( cdata_{c}{c}, {c}2x_{c}{c}, cdata_{c}x, {c}2x_{c}x)
            call t_drvstopf  ('driver_{c}2c_{ccc}{c}2{ccc}x')
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
               call seq_diag_avect_mct(cdata_{c}x,{c}2x_{c}x,'recv {ccc}')
               call t_drvstopf  ('driver_{ccc}post_diagav')
            endif
            if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
            call t_drvstopf  ('DRIVER_{CCC}POST',cplrun=.true.)
         endif

      endif
