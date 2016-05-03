      !----------------------------------------------------------
      ! Run {ccc} Model
      !----------------------------------------------------------

      if ({ccc}_present .and. {ccc}run_alarm .and. iamin_{CCC}ID) then
         if (run_barriers) then
            call t_drvstartf ('DRIVER_{CCC}_RUN_BARRIER')
            call mpi_barrier(mpicom_{CCC}ID,ierr)
            call t_drvstopf ('DRIVER_{CCC}_RUN_BARRIER')
         endif
         call t_drvstartf ('DRIVER_{CCC}_RUN',barrier=mpicom_{CCC}ID)
         if (drv_threading) call seq_comm_setnthreads(nthreads_{CCC}ID)
         if ({ccc}_prognostic) call mct_avect_vecmult(x2{c}_{c}{c},drv2mdl_{c}{c},seq_flds_x2{c}_fluxes)
         call {ccc}_run_mct( EClock_{c}, cdata_{c}{c}, x2{c}_{c}{c}, {c}2x_{c}{c})
         call mct_avect_vecmult({c}2x_{c}{c},mdl2drv_{c}{c},seq_flds_{c}2x_fluxes)
         if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
         call t_drvstopf  ('DRIVER_{CCC}_RUN')
      endif
