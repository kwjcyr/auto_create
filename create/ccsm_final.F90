subroutine ccsm_final()

   !------------------------------------------------------------------------
   ! Finalization of all models
   !------------------------------------------------------------------------

 103  format( 5A )
   ! TODO finalize routines need to be cleaned up 

   call t_barrierf ('DRIVER_FINAL_BARRIER', mpicom_GLOID)
   call t_startf ('DRIVER_FINAL')

   call seq_timemgr_EClockGetData( EClock_d, stepno=endstep)
   call shr_mem_getusage(msize,mrss)

   if (iamin_ATMID) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_ATMID)
      call atm_final_mct( )
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif
   if (iamin_WRFID) then  !juanxiong he
      if (drv_threading) call seq_comm_setnthreads(nthreads_WRFID)
      call wrf_final_mct( )
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif
   if (iamin_GEAID) then  !juanxiong he
      if (drv_threading) call seq_comm_setnthreads(nthreads_GEAID)
      call geatm_final_mct( )
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif      
   if (iamin_LNDID) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_LNDID)
      call lnd_final_mct( )
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif
   if (iamin_ICEID) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_ICEID)
      call ice_final_mct( )
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif
   if (iamin_OCNID) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_OCNID)
      call ocn_final_mct( )
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif
   if (iamin_GLCID) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLCID)
      call glc_final_mct( )
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif

   !------------------------------------------------------------------------
   ! End the run cleanly
   !------------------------------------------------------------------------

   call seq_io_finalize()
   
   call shr_mpi_min(msize,msize0,mpicom_GLOID,'driver
msize0',all=.true.)
   call shr_mpi_max(msize,msize1,mpicom_GLOID,'driver
msize1',all=.true.)
   call shr_mpi_min(mrss,mrss0,mpicom_GLOID,'driver mrss0',all=.true.)
   call shr_mpi_max(mrss,mrss1,mpicom_GLOID,'driver mrss1',all=.true.)
   if ( iamroot_CPLID )then
      call seq_timemgr_EClockGetData( EClock_d, curr_ymd=ymd,
curr_tod=tod, dtime=dtime)
      write(logunit,'(//)')
      write(logunit,FormatA) subname, 'SUCCESSFUL TERMINATION OF
CPL7-CCSM'
      write(logunit,FormatD) subname, '  at YMD,TOD = ',ymd,tod
      simDays = (endStep-begStep)*dtime/(24._r8*3600._r8)
      write(logunit,FormatR) subname, '# simulated days (this run) = ',
simDays
      write(logunit,FormatR) subname, 'compute time (hrs)          = ',
(Time_end-Time_begin)/3600._r8
      if ( (Time_end /= Time_begin) .and. (simDays /= 0.0_r8) )then
         SYPD =
shr_const_cday*simDays/(days_per_year*(Time_end-Time_begin))
         write(logunit,FormatR) subname, '# simulated years / cmp-day =
', SYPD
      endif
      write(logunit,FormatR) subname,' pes min memory highwater  (MB)  =
',mrss0
      write(logunit,FormatR) subname,' pes max memory highwater  (MB)  =
',mrss1
      write(logunit,FormatR) subname,' pes min memory last usage (MB)  =
',msize0
      write(logunit,FormatR) subname,' pes max memory last usage (MB)  =
',msize1
      write(logunit,'(//)')
      close(logunit)
   endif

   call t_stopf  ('DRIVER_FINAL')
   call t_prf(trim(timing_dir)//'/ccsm_timing', mpicom_GLOID)
   call t_finalizef()

end subroutine ccsm_final
