      !----------------------------------------------------------
      ! Run {ccc} Model
      !----------------------------------------------------------

      if (( &
!{list} [ccc0]             
!            {ccc0}_present .or. &               
             ) .and. {ccc}run_alarm .and. iamin_{CCC}ID) then
         if (run_barriers) then
            call t_drvstartf ('DRIVER_{CCC}_RUN_BARRIER')
            call mpi_barrier(mpicom_{CCC}ID,ierr)
            call t_drvstopf ('DRIVER_{CCC}_RUN_BARRIER')
         endif
         call t_drvstartf ('DRIVER_{CCC}_RUN',barrier=mpicom_{CCC}ID)
         if (drv_threading) call seq_comm_setnthreads(nthreads_{CCC}ID)
!<list> [ccc1] [c1]       
!        if ({ccc1}_prognostic) 
!            call mct_avect_vecmult(x2{c1}_{c1}{c1},drv2mdl_{c1}{c1},seq_flds_x2{c1}_fluxes)
!        endif  
!</list>       
         call {ccc}_run_mct( EClock_{c} &
!{list} [c0]
!                ,cdata_{c0}{c0}, x2{c0}_{c0}{c0}, {c0}2x_{c0}{c0} &
                )
         call mct_avect_vecmult({c}2x_{c}{c},mdl2drv_{c}{c},seq_flds_{c}2x_fluxes)
!<list>  [ccc2] [c2]
!        if ({ccc2}_present) then
!               call
!               mct_avect_vecmult({c2}2x_{c2}{c2},mdl2drv_{c2}{c2},seq_flds_{c2}2x_fluxes)
!        endif
!</list>         
         if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
         call t_drvstopf  ('DRIVER_{CCC}_RUN')
      endif
