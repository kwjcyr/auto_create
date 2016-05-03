subroutine ccsm_run()

 101  format( A, 2i8, 12A, A, F8.2, A, F8.2 )
 102  format( A, 2i8, A, 5L3 )
 103  format( 5A )
 104  format( A, 2i8)
 105  format( A, 2i8, A, f10.2, A, f10.2, A, A, i5, A, A)
 106  format( A, f23.12)

   call seq_infodata_putData(infodata,atm_phase=1,lnd_phase=1,ocn_phase=1,ice_phase=1)
   call seq_timemgr_EClockGetData( EClock_d, stepno=begstep)
   call seq_timemgr_EClockGetData( EClock_d, dtime=dtime)
   ncpl = 86400/dtime
   dtstep_acc = 0._r8
   dtstep_cnt = 0
   stop_alarm = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_stop)
   if (seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_datestop)) then
      if (iamroot_CPLID) then
          write(logunit,*) ' '
          write(logunit,103) subname,' NOTE: Stopping from alarm STOP DATE'
          write(logunit,*) ' '
      endif
      stop_alarm = .true.
   endif

   !----------------------------------------------------------
   ! Beginning of basic time step loop
   !----------------------------------------------------------

   call t_startf ('DRIVER_RUN_LOOP_BSTART')
   call mpi_barrier(mpicom_GLOID,ierr)
   call t_stopf ('DRIVER_RUN_LOOP_BSTART')
   Time_begin = mpi_wtime()
   Time_bstep = mpi_wtime()
   do while ( .not. stop_alarm)

      call t_startf('DRIVER_RUN_LOOP')
      call t_drvstartf ('DRIVER_CLOCK_ADVANCE',cplrun=.true.)

      !----------------------------------------------------------
      ! Advance sync clock time (this is time that models should have before 
      ! they return to the driver).  Write timestamp and run alarm status
      !----------------------------------------------------------

      call seq_timemgr_clockAdvance( seq_SyncClock)
      call seq_timemgr_EClockGetData( EClock_d, curr_ymd=ymd, curr_tod=tod )
      call shr_cal_date2ymd(ymd,year,month,day)
      stop_alarm    = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_stop)
      atmrun_alarm = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_atmrun)
      icerun_alarm = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_icerun)
      lndrun_alarm = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_lndrun)
      ocnrun_alarm = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_ocnrun)
      rofrun_alarm = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_rofrun)
      glcrun_alarm = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_glcrun)
      snorun_alarm = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_snorun)
      ocnnext_alarm = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_ocnnext)
      restart_alarm = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_restart)
      history_alarm = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_history)
      histavg_alarm = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_histavg)
      tprof_alarm   = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_tprof)

      ! this probably belongs in seq_timemgr somewhere using proper clocks
      t1hr_alarm = .false.
      t2hr_alarm = .false.
      t3hr_alarm = .false.
      t6hr_alarm = .false.
      t12hr_alarm = .false.
      t24hr_alarm = .false.
      if (mod(tod, 3600) == 0) t1hr_alarm = .true.
      if (mod(tod, 7200) == 0) t2hr_alarm = .true.
      if (mod(tod,10800) == 0) t3hr_alarm = .true.
      if (mod(tod,21600) == 0) t6hr_alarm = .true.
      if (mod(tod,43200) == 0) t12hr_alarm = .true.
      if (tod == 0) t24hr_alarm = .true.

      call seq_infodata_putData(infodata, glcrun_alarm=glcrun_alarm)

      if (seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_datestop)) then
         if (iamroot_CPLID) then
             write(logunit,*) ' '
             write(logunit,103) subname,' NOTE: Stopping from alarm STOP DATE'
             write(logunit,*) ' '
         endif
         stop_alarm = .true.
      endif

      ! update the orbital data as needed
      if (trim(orb_mode) == trim(seq_infodata_orb_variable_year)) then
         orb_nyear =  orb_iyear + (year - orb_iyear_align)
         if (orb_nyear /= orb_cyear) then
            orb_cyear = orb_nyear
            call shr_orb_params(orb_cyear, orb_eccen, orb_obliq, orb_mvelp, &
                                orb_obliqr, orb_lambm0, orb_mvelpp, iamroot_CPLID)
            call seq_infodata_putData(infodata,orb_eccen=orb_eccen,orb_obliqr=orb_obliqr, &
                 orb_lambm0=orb_lambm0,orb_mvelpp=orb_mvelpp)
         endif
      endif

      ! override ocnrun_alarm and ocnnext_alarm for first ocn run
      ! skip_ocean_run is initialized above to true if it's a startup
      ! if it's not a startup, ignore all of this
      ! stop the overide on the second ocnrun_alarm

      if (ocnrun_alarm) ocnrun_count = ocnrun_count + 1
      if (ocnrun_count > 1) skip_ocean_run = .false.
      if (skip_ocean_run) then
         ocnrun_alarm = .false.
         ocnnext_alarm = .false.
      endif

      if (iamroot_CPLID) then
         if (loglevel > 1) then
            write(logunit,102) ' Alarm_state: model date = ',ymd,tod, &
               ' aliog run alarms = ',  
                        atmrun_alarm,&                        
                        icerun_alarm,&                        
                        lndrun_alarm,&                        
                        ocnrun_alarm,&                        
                        rofrun_alarm,&                        
                        glcrun_alarm,&                        
                        snorun_alarm,&                        
            write(logunit,102) ' Alarm_state: model date = ',ymd,tod, &
               ' 1.2.3.6.12 run alarms = ',  t1hr_alarm, t2hr_alarm, &
                         t3hr_alarm, t6hr_alarm, t12hr_alarm, t24hr_alarm
            call shr_sys_flush(logunit)
         endif
      endif

      call t_drvstopf  ('DRIVER_CLOCK_ADVANCE',cplrun=.true.)

      !----------------------------------------------------------
      ! OCN/ICE PREP
      ! Map for ice prep and atmocn flux
      !----------------------------------------------------------

      if (iamin_CPLID .and. (ice_present.or.ocn_present)) then
         call t_drvstartf ('DRIVER_OCNPREP',cplrun=.true.,barrier=mpicom_CPLID)
         if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
         call t_drvstartf ('driver_ocnprep_atm2ocn',barrier=mpicom_CPLID)
         call map_atm2ocn_mct( cdata_ax, a2x_ax, cdata_ox, a2x_ox, &
                               fluxlist=seq_flds_a2x_fluxes, statelist=seq_flds_a2x_states )
         call t_drvstopf  ('driver_ocnprep_atm2ocn')
         if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
         call t_drvstopf  ('DRIVER_OCNPREP',cplrun=.true.)
      endif

      !----------------------------------------------------------
      ! OCN SETUP
      !----------------------------------------------------------

      if (ocn_present .and. ocnrun_alarm) then

         !----------------------------------------------------
         ! "startup" wait
         !----------------------------------------------------

         if (iamin_CPLOCNID .and. cpl2ocn_first) then
            ! want to know the time the ocean pes waited for the cpl pes
            !   at the first ocnrun_alarm, min ocean wait is wait time
            ! do not use t_barrierf here since it can be "off", use mpi_barrier
            if (iamin_OCNID) call t_drvstartf ('DRIVER_C2O_INITWAIT')
            call mpi_barrier(mpicom_CPLOCNID,ierr)
            if (iamin_OCNID) call t_drvstopf  ('DRIVER_C2O_INITWAIT')
            cpl2ocn_first = .false.
         endif

         !----------------------------------------------------
         ! ocn prep
         !----------------------------------------------------

         if (iamin_CPLID .and. ocn_prognostic) then
            call t_drvstartf ('DRIVER_OCNPREP',cplrun=.true.,barrier=mpicom_CPLID)
            if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
            call t_drvstartf ('driver_ocnprep_avg',barrier=mpicom_CPLID)
            ! temporary formation of average
!           call mct_accum_average(x2oacc_ox)
            if (x2oacc_ox_cnt > 0) then
               x2oacc_ox%data%rAttr = x2oacc_ox%data%rAttr / (x2oacc_ox_cnt*1.0_r8)
            endif
            x2oacc_ox_cnt = 0
            call t_drvstopf  ('driver_ocnprep_avg')
            if (rof_present .and. ocnrof_prognostic) then
               ! Map runoff to ocn, average, put in x2oacc_ox
               if (r2xacc_rx_cnt > 0) then
                  call t_drvstartf ('driver_ocnprep_ravg',barrier=mpicom_CPLID)
                  r2xacc_rx%data%rAttr = r2xacc_rx%data%rAttr / (r2xacc_rx_cnt*1.0_r8)
                  r2xacc_rx_cnt = 0
                  call t_drvstopf ('driver_ocnprep_ravg')
                  call t_drvstartf ('driver_ocnprep_rof2ocn',barrier=mpicom_CPLID)
                  call map_rof2ocn_mct( cdata_rx, r2xacc_rx%data, cdata_ox, r2x_ox ) 
                  if (do_hist_r2x) then
                     call seq_hist_writeaux(EClock_d,'r2xacc','domr',cdata_rx,r2xacc_rx%data, &
                          rof_nx,rof_ny,1)
                  endif
                  call t_drvstopf  ('driver_ocnprep_rof2ocn')
                  call t_drvstartf ('driver_ocnprep_rofcopy',barrier=mpicom_CPLID)
                  call mct_aVect_copy(aVin=r2x_ox, aVout=x2oacc_ox%data)
                  call t_drvstopf  ('driver_ocnprep_rofcopy')
               endif
            endif
            if (info_debug > 1) then
               call t_drvstartf ('driver_ocnprep_diagav',barrier=mpicom_CPLID)
               call seq_diag_avect_mct(cdata_ox,x2oacc_ox%data,'send ocn')
               call t_drvstopf  ('driver_ocnprep_diagav')
            endif
            if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
            call t_drvstopf  ('DRIVER_OCNPREP',cplrun=.true.)
         endif

         !----------------------------------------------------
         ! cpl -> ocn
         !----------------------------------------------------	

         if (iamin_CPLOCNID .and. ocn_prognostic) then
            call t_drvstartf ('DRIVER_C2O',barrier=mpicom_CPLOCNID)
            call t_drvstartf ('driver_c2o_ocnx2ocno',barrier=mpicom_CPLOCNID)
            call map_ocnx2ocno_mct(cdata_ox, x2oacc_ox%data, cdata_oo, x2o_oo)
            call t_drvstopf  ('driver_c2o_ocnx2ocno')
            call t_drvstartf ('driver_c2o_infoexch',barrier=mpicom_CPLOCNID)
            call seq_infodata_exchange(infodata,CPLOCNID,'cpl2ocn_run')
            call t_drvstopf  ('driver_c2o_infoexch')
            call t_drvstopf  ('DRIVER_C2O')
         endif

         !----------------------------------------------------
         ! cpl -> ocn
         !----------------------------------------------------

         if (iamin_CPLOCNID .and. ocn_prognostic) then
            call t_drvstartf ('DRIVER_C2O',barrier=mpicom_CPLOCNID)
            call t_drvstartf ('driver_c2o_ocnx2ocno',barrier=mpicom_CPLOCNID)
            call map_ocnx2ocno_mct( cdata_ox, x2oacc_ox%data, cdata_oo, x2o_oo)
            call t_drvstopf  ('driver_c2o_ocnx2ocno')
            call t_drvstartf ('driver_c2o_infoexch',barrier=mpicom_CPLOCNID)
            call seq_infodata_exchange(infodata,CPLOCNID,'cpl2ocn_run')
            call t_drvstopf  ('driver_c2o_infoexch')
            call t_drvstopf  ('DRIVER_C2O')
         endif

      endif
  
      !----------------------------------------------------------
      ! LND SETUP
      !----------------------------------------------------------

      if (lnd_present .and. lndrun_alarm) then

         !----------------------------------------------------
         ! lnd prep
         !----------------------------------------------------

         if (iamin_CPLID) then
            call t_drvstartf ('DRIVER_LNDPREP',cplrun=.true.,barrier=mpicom_CPLID)
            if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)

            if (lnd_prognostic) then
               call t_drvstartf ('driver_lndprep_atm2lnd',barrier=mpicom_CPLID)
               call map_atm2lnd_mct( cdata_ax, a2x_ax, cdata_lx, a2x_lx )
               call t_drvstopf  ('driver_lndprep_atm2lnd')
               call t_drvstartf ('driver_lndprep_mrgx2l',barrier=mpicom_CPLID)
               call mrg_x2l_run_mct( cdata_lx, a2x_lx, x2l_lx )
               call t_drvstopf  ('driver_lndprep_mrgx2l')
               if (info_debug > 1) then
                  call t_drvstartf ('driver_lndprep_diagav',barrier=mpicom_CPLID)
                  call seq_diag_avect_mct(cdata_lx,x2l_lx,'send lnd')
                  call t_drvstopf  ('driver_lndprep_diagav')
               endif
            endif

            if (glc_present .and. sno_prognostic) then
               if (info_debug > 1) then
                  call t_drvstartf ('driver_lndprep_diagav',barrier=mpicom_CPLID)
                  call seq_diag_avect_mct(cdata_sx,x2s_sx,'send sno')
                  call t_drvstopf  ('driver_lndprep_diagav')
               endif
            endif

            if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
            call t_drvstopf  ('DRIVER_LNDPREP',cplrun=.true.)
         endif

         !----------------------------------------------------
         ! cpl -> lnd
         !----------------------------------------------------

         if (iamin_CPLLNDID) then
            call t_drvstartf ('DRIVER_C2L',barrier=mpicom_CPLLNDID)
            if (lnd_prognostic) then
               call t_drvstartf ('driver_c2l_lndx2lndl',barrier=mpicom_CPLLNDID)
               call map_lndx2lndl_mct( cdata_lx, x2l_lx, cdata_ll, x2l_ll)
               call t_drvstopf  ('driver_c2l_lndx2lndl')
            endif
            if (glc_present .and. sno_prognostic) then
               call t_drvstartf ('driver_c2l_snox2snos',barrier=mpicom_CPLLNDID)
               call map_snox2snos_mct( cdata_sx, x2s_sx, cdata_ss, x2s_ss)
               call t_drvstopf  ('driver_c2l_snox2snos')
            endif
            if (lnd_prognostic .or. sno_prognostic) then
               call t_drvstartf ('driver_c2l_infoexch',barrier=mpicom_CPLLNDID)
               call seq_infodata_exchange(infodata,CPLLNDID,'cpl2lnd_run')
               call t_drvstopf  ('driver_c2l_infoexch')
            endif
            call t_drvstopf  ('DRIVER_C2L')
         endif

      endif

      !----------------------------------------------------------
      ! ICE SETUP
      ! Note that for atm->ice mapping below will leverage the assumption that the
      ! ice and ocn are on the same grid and that mapping of atm to ocean is 
      ! done already for use by atmocn flux and ice model prep
      !----------------------------------------------------------

      if (ice_present .and. icerun_alarm) then

         !----------------------------------------------------
         ! ice prep
         !----------------------------------------------------

         if (iamin_CPLID .and. ice_prognostic) then
            call t_drvstartf ('DRIVER_ICEPREP',cplrun=.true.,barrier=mpicom_CPLID)
            if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
            call t_drvstartf ('driver_iceprep_ocn2ice',barrier=mpicom_CPLID)
            call map_ocn2ice_mct( cdata_ox, o2x_ox, cdata_ix, o2x_ix )
            call t_drvstopf  ('driver_iceprep_ocn2ice')
            
            call t_drvstartf ('driver_iceprep_atm2ice',barrier=mpicom_CPLID)
            call map_ocn2ice_mct( cdata_ox, a2x_ox, cdata_ix, a2x_ix )
!tcx fails            call map_atm2ice_mct( cdata_ax, a2x_ax, cdata_ix, a2x_ix )
            call t_drvstopf  ('driver_iceprep_atm2ice')
            
            call t_drvstartf ('driver_iceprep_mrgx2i',barrier=mpicom_CPLID)
            call mrg_x2i_run_mct( cdata_ix, a2x_ix, o2x_ix, x2i_ix )
            call t_drvstopf  ('driver_iceprep_mrgx2i')

            if (info_debug > 1) then
               call t_drvstartf ('driver_iceprep_diagav',barrier=mpicom_CPLID)
               call seq_diag_avect_mct(cdata_ix,x2i_ix,'send ice')
               call t_drvstopf  ('driver_iceprep_diagav')
            endif
            if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
            call t_drvstopf  ('DRIVER_ICEPREP',cplrun=.true.)
         endif
         !----------------------------------------------------
         ! cpl -> ice
         !----------------------------------------------------	

         if (iamin_CPLICEID .and. ice_prognostic) then
            call t_drvstartf ('DRIVER_C2I',barrier=mpicom_CPLICEID)
            call t_drvstartf ('driver_c2i_icex2icei',barrier=mpicom_CPLICEID)
            call map_icex2icei_mct(cdata_ix, x2iacc_ix%data, cdata_ii, x2i_ii)
            call t_drvstopf  ('driver_c2i_icex2icei')
            call t_drvstartf ('driver_c2i_infoexch',barrier=mpicom_CPLICEID)
            call seq_infodata_exchange(infodata,CPLICEID,'cpl2ice_run')
            call t_drvstopf  ('driver_c2i_infoexch')
            call t_drvstopf  ('DRIVER_C2I')
         endif
         !----------------------------------------------------
         ! cpl -> ice
         !----------------------------------------------------

         if (iamin_CPLICEID .and. ice_prognostic) then
            call t_drvstartf ('DRIVER_C2I',barrier=mpicom_CPLICEID)
            call t_drvstartf ('driver_c2i_icex2icei',barrier=mpicom_CPLICEID)
            call map_icex2icei_mct( cdata_ix, x2i_ix, cdata_ii, x2i_ii)
            call t_drvstopf  ('driver_c2i_icex2icei')
            call t_drvstartf ('driver_c2i_infoexch',barrier=mpicom_CPLICEID)
            call seq_infodata_exchange(infodata,CPLICEID,'cpl2ice_run')
            call t_drvstopf  ('driver_c2i_infoexch')
            call t_drvstopf  ('DRIVER_C2I')
         endif

      endif

      !----------------------------------------------------------
      ! Run Ocn Model
      !----------------------------------------------------------

      if (ocn_present .and. ocnrun_alarm .and. iamin_OCNID) then
         if (run_barriers) then
            call t_drvstartf ('DRIVER_OCN_RUN_BARRIER')
            call mpi_barrier(mpicom_OCNID,ierr)
            call t_drvstopf ('DRIVER_OCN_RUN_BARRIER')
         endif
         call t_drvstartf ('DRIVER_OCN_RUN',barrier=mpicom_OCNID)
         if (drv_threading) call seq_comm_setnthreads(nthreads_OCNID)
         if (ocn_prognostic) call mct_avect_vecmult(x2o_oo,drv2mdl_oo,seq_flds_x2o_fluxes)
         call ocn_run_mct( EClock_o, cdata_oo, x2o_oo, o2x_oo)
         call mct_avect_vecmult(o2x_oo,mdl2drv_oo,seq_flds_o2x_fluxes)
         if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
         call t_drvstopf  ('DRIVER_OCN_RUN')
      endif
 
      !----------------------------------------------------------
      ! Run Ice Model
      !----------------------------------------------------------

      if (ice_present .and. icerun_alarm .and. iamin_ICEID) then
         if (run_barriers) then
            call t_drvstartf ('DRIVER_ICE_RUN_BARRIER')
            call mpi_barrier(mpicom_ICEID,ierr)
            call t_drvstopf ('DRIVER_ICE_RUN_BARRIER')
         endif
         call t_drvstartf ('DRIVER_ICE_RUN',barrier=mpicom_ICEID)
         if (drv_threading) call seq_comm_setnthreads(nthreads_ICEID)
         if (ice_prognostic) call mct_avect_vecmult(x2i_ii,drv2mdl_ii,seq_flds_x2i_fluxes)
         call ice_run_mct( EClock_i, cdata_ii, x2i_ii, i2x_ii)
         call mct_avect_vecmult(i2x_ii,mdl2drv_ii,seq_flds_i2x_fluxes)
         if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
         call t_drvstopf  ('DRIVER_ICE_RUN')
      endif

      !----------------------------------------------------------
      ! Run Land Model
      !----------------------------------------------------------

      if ((lnd_present.or.rof_present.or.sno_present) .and. &
           lndrun_alarm .and. iamin_LNDID) then
         if (run_barriers) then
            call t_drvstartf ('DRIVER_LND_RUN_BARRIER')
            call mpi_barrier(mpicom_LNDID,ierr)
            call t_drvstopf ('DRIVER_LND_RUN_BARRIER')
         endif
         call t_drvstartf ('DRIVER_LND_RUN',barrier=mpicom_LNDID)
         if (drv_threading) call seq_comm_setnthreads(nthreads_LNDID)
         if (lnd_prognostic) then
            call mct_avect_vecmult(x2l_ll,drv2mdl_ll,seq_flds_x2l_fluxes)
         endif
         if (sno_prognostic) then
            call mct_avect_vecmult(x2s_ss,drv2mdl_ss,seq_flds_x2s_fluxes)
         endif
         call lnd_run_mct( EClock_l, cdata_ll, x2l_ll, l2x_ll, &
                                     cdata_rr,         r2x_rr, &
                                     cdata_ss, x2s_ss, s2x_ss)
         call mct_avect_vecmult(l2x_ll,mdl2drv_ll,seq_flds_l2x_fluxes)
         if (rof_present) then
            call mct_avect_vecmult(r2x_rr,mdl2drv_rr,seq_flds_r2x_fluxes)
         endif
         if (sno_present) then
            call mct_avect_vecmult(s2x_ss,mdl2drv_ss,seq_flds_s2x_fluxes)
         endif
         if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
         call t_drvstopf  ('DRIVER_LND_RUN')
      endif

      !----------------------------------------------------------
      ! ocn -> cpl, tight coupling (sequential type mode)
      !----------------------------------------------------------

      if (ocean_tight_coupling) then
      if (ocn_present .and. ocnnext_alarm) then
         if (iamin_CPLOCNID) then
            call t_drvstartf ('DRIVER_O2C',barrier=mpicom_CPLOCNID)
            call t_drvstartf ('driver_o2c_ocno2ocnx',barrier=mpicom_CPLOCNID)
            call map_ocno2ocnx_mct( cdata_oo, o2x_oo, cdata_ox, o2x_ox)
            call t_drvstopf  ('driver_o2c_ocno2ocnx')
            call t_drvstartf ('driver_o2c_infoexch',barrier=mpicom_CPLOCNID)
            call seq_infodata_exchange(infodata,CPLOCNID,'ocn2cpl_run')
            call t_drvstopf  ('driver_o2c_infoexch')
            call t_drvstopf  ('DRIVER_O2C')
         endif
         if (iamin_CPLID) then
            call t_drvstartf  ('DRIVER_OCNPOST',cplrun=.true.,barrier=mpicom_CPLID)
            if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
            if (info_debug > 1) then
               call t_drvstartf ('driver_ocnpost_diagav',barrier=mpicom_CPLID)
               call seq_diag_avect_mct(cdata_ox,o2x_ox,'recv ocn')
               call t_drvstopf  ('driver_ocnpost_diagav')
            endif
            if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
            call t_drvstopf  ('DRIVER_OCNPOST',cplrun=.true.)
         endif
      endif
      endif

      !----------------------------------------------------------
      ! OCN PREP
      !----------------------------------------------------------

      if (ocn_present .and. iamin_CPLID) then
         call t_drvstartf ('DRIVER_ATMOCNP',cplrun=.true.,barrier=mpicom_CPLID)
         if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
         if (ocn_prognostic) then

            ! Map ice to ocn
            if (ice_present) then
               call t_drvstartf ('driver_atmocnp_ice2ocn',barrier=mpicom_CPLID)
               call map_ice2ocn_mct( cdata_ix, i2x_ix, cdata_ox, i2x_ox)
               call t_drvstopf  ('driver_atmocnp_ice2ocn')
            endif

            ! Merge ocn inputs
            call t_drvstartf ('driver_atmocnp_mrgx2o',barrier=mpicom_CPLID)
            call mrg_x2o_run_mct( cdata_ox, a2x_ox, i2x_ox, xao_ox, fractions_ox, x2o_ox )
            call t_drvstopf  ('driver_atmocnp_mrgx2o')

            ! Accumulate ocn inputs
            ! Form partial sum of tavg ocn inputs (virtual "send" to ocn) 
            call t_drvstartf ('driver_atmocnp_accum',barrier=mpicom_CPLID)
!     !         call mct_accum_accumulate(x2o_ox, x2oacc_ox)
            if (x2oacc_ox_cnt == 0) then
               x2oacc_ox%data%rAttr = x2o_ox%rAttr
            else
               x2oacc_ox%data%rAttr = x2oacc_ox%data%rAttr + x2o_ox%rAttr
            endif
            x2oacc_ox_cnt = x2oacc_ox_cnt + 1
            call t_drvstopf  ('driver_atmocnp_accum')
         endif
 
         ! Compute atm/ocn fluxes (virtual "recv" from ocn)
         call t_drvstartf ('driver_atmocnp_flux',barrier=mpicom_CPLID)
         if (trim(aoflux_grid) == 'ocn') then
            call seq_flux_atmocn_mct( cdata_ox, a2x_ox, o2x_ox, xao_ox)
            call seq_flux_ocnalb_mct(cdata_ox,xao_ox,fractions_ox)
         else if (trim(aoflux_grid) == 'atm') then
            call seq_flux_atmocn_mct( cdata_ax, a2x_ax, o2x_ax, xao_ax)
            call seq_flux_ocnalb_mct(cdata_ox,xao_ox,fractions_ox)
         else if (trim(aoflux_grid) == 'exch') then
            call seq_flux_atmocnexch_mct( cdata_ax, cdata_ox, a2x_ax, o2x_ox, xao_ax, xao_ox, &
                                       fractions_ax, fractions_ox)
            call seq_flux_ocnalb_mct(cdata_ox,xao_ox,fractions_ox)
         endif  ! aoflux_grid
         call t_drvstopf  ('driver_atmocnp_flux')
         
         if (trim(aoflux_grid) == 'atm') then
            call t_drvstartf ('driver_atmocnp_atm2ocn',barrier=mpicom_CPLID)
! this mapping has to be done with area overlap mapping for all fields 
! due to the masking of the xao_ax data and the fact that a2oS is bilinear
!            call map_atm2ocn_mct( cdata_ax, xao_ax, cdata_ox, xao_ox, &
!                                  fluxlist=seq_flds_xao_fluxes, statelist=seq_flds_xao_states)
            call map_atm2ocn_mct( cdata_ax, xao_ax, cdata_ox, xao_ox, &
                                   fluxlist=seq_flds_xao_states//":"//seq_flds_xao_fluxes)
            call t_drvstopf  ('driver_atmocnp_atm2ocn')
         endif

         if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
         call t_drvstopf  ('DRIVER_ATMOCNP',cplrun=.true.)
      endif

      !----------------------------------------------------------
      ! lnd -> cpl
      !----------------------------------------------------------

      if ((lnd_present.or.rof_present.or.sno_present) .and. lndrun_alarm) then

         if (iamin_CPLLNDID) then
            call t_drvstartf ('DRIVER_L2C',barrier=mpicom_CPLLNDID)
            if (lnd_present) then
               call t_drvstartf ('driver_l2c_lndl2lndx',barrier=mpicom_CPLLNDID)
               call map_lndl2lndx_mct( cdata_ll, l2x_ll, cdata_lx, l2x_lx)
               call t_drvstopf  ('driver_l2c_lndl2lndx')
            endif
            if (rof_present) then
               call t_drvstartf ('driver_l2c_rofr2rofx',barrier=mpicom_CPLLNDID)
               call map_rofr2rofx_mct( cdata_rr, r2x_rr, cdata_rx, r2x_rx)
               call t_drvstopf  ('driver_l2c_rofr2rofx')
            endif
            if (sno_present .and. glc_prognostic .and. glcrun_alarm) then
               call t_drvstartf ('driver_l2c_snos2snox',barrier=mpicom_CPLLNDID)
               call map_snos2snox_mct( cdata_ss, s2x_ss, cdata_sx, s2x_sx)
               call t_drvstopf  ('driver_l2c_snos2snox')
            endif
            call t_drvstartf ('driver_l2c_infoexch',barrier=mpicom_CPLLNDID)
            call seq_infodata_exchange(infodata,CPLLNDID,'lnd2cpl_run')
            call t_drvstopf  ('driver_l2c_infoexch')
            call t_drvstopf  ('DRIVER_L2C')
         endif

         if (iamin_CPLID) then
            call t_drvstartf  ('DRIVER_LNDPOST',cplrun=.true.,barrier=mpicom_CPLID)
            if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
            if (info_debug > 1) then
               call t_drvstartf ('driver_lndpost_diagav',barrier=mpicom_CPLID)
               if (lnd_present) then
                  call seq_diag_avect_mct(cdata_lx,l2x_lx,'recv lnd')
               endif
               if (rof_present) then
                  call seq_diag_avect_mct(cdata_rx,r2x_rx,'recv roff')
               endif
               if (sno_present .and. glc_prognostic .and. glcrun_alarm) then
                  call seq_diag_avect_mct(cdata_sx,s2x_sx,'recv sno')
               endif
               call t_drvstopf  ('driver_lndpost_diagav')
            endif
            if (rof_present .and. ocnrof_prognostic) then
               call t_drvstartf ('driver_lndpost_raccum',barrier=mpicom_CPLID)
               ! better to flux correct here if flux_epbalfact varies
               ! over the accumulation period
               call seq_infodata_GetData(infodata, flux_epbalfact = flux_epbalfact)
               if (r2xacc_rx_cnt == 0) then
                  r2xacc_rx%data%rAttr = r2x_rx%rAttr * flux_epbalfact
               else
                  r2xacc_rx%data%rAttr = r2xacc_rx%data%rAttr + r2x_rx%rAttr * flux_epbalfact
               endif
               r2xacc_rx_cnt = r2xacc_rx_cnt + 1
               call t_drvstopf ('driver_lndpost_raccum')
            endif
            if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
            call t_drvstopf  ('DRIVER_LNDPOST',cplrun=.true.)
         endif

      endif   ! run alarm, lnd_present

      !----------------------------------------------------------
      ! GLC SETUP
      !----------------------------------------------------------

      if (sno_present .and. glcrun_alarm) then
         if (iamin_CPLID .and. glc_prognostic) then
            call t_drvstartf ('DRIVER_GLCPREP',cplrun=.true.,barrier=mpicom_CPLID)
            if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)

            ! Map sno to glc
            call t_drvstartf ('driver_glcprep_sno2glc',barrier=mpicom_CPLID)
            call map_sno2glc_mct( cdata_sx, s2x_sx, cdata_gx, s2x_gx ) 
            call t_drvstopf  ('driver_glcprep_sno2glc')

            ! Merge glc inputs
            call t_drvstartf ('driver_glcprep_mrgx2g',barrier=mpicom_CPLID)
            call mrg_x2g_run_mct( cdata_gx, s2x_gx, x2g_gx)
            call t_drvstopf  ('driver_glcprep_mrgx2g')

            if (info_debug > 1) then
               call t_drvstartf ('driver_glcprep_diagav',barrier=mpicom_CPLID)
               call seq_diag_avect_mct(cdata_gx,x2g_gx,'send glc')
               call t_drvstopf  ('driver_glcprep_diagav')
            endif

            if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
            call t_drvstopf  ('DRIVER_GLCPREP',cplrun=.true.)
         endif
         !----------------------------------------------------
         ! cpl -> glc
         !----------------------------------------------------	

         if (iamin_CPLGLCID .and. glc_prognostic) then
            call t_drvstartf ('DRIVER_C2G',barrier=mpicom_CPLGLCID)
            call t_drvstartf ('driver_c2g_glcx2glcg',barrier=mpicom_CPLGLCID)
            call map_glcx2glcg_mct(cdata_gx, x2gacc_gx%data, cdata_gg, x2g_gg)
            call t_drvstopf  ('driver_c2g_glcx2glcg')
            call t_drvstartf ('driver_c2g_infoexch',barrier=mpicom_CPLGLCID)
            call seq_infodata_exchange(infodata,CPLGLCID,'cpl2glc_run')
            call t_drvstopf  ('driver_c2g_infoexch')
            call t_drvstopf  ('DRIVER_C2G')
         endif
         !----------------------------------------------------
         ! cpl -> glc
         !----------------------------------------------------

         if (iamin_CPLGLCID .and. glc_prognostic) then
            call t_drvstartf ('DRIVER_C2G',barrier=mpicom_CPLGLCID)
            call t_drvstartf ('driver_c2g_glcx2glcg',barrier=mpicom_CPLGLCID)
            call map_glcx2glcg_mct( cdata_gx, x2g_gx, cdata_gg, x2g_gg)
            call t_drvstopf  ('driver_c2g_glcx2glcg')
            call t_drvstartf ('driver_c2g_infoexch',barrier=mpicom_CPLGLCID)
            call seq_infodata_exchange(infodata,CPLGLCID,'cpl2glc_run')
            call t_drvstopf  ('driver_c2g_infoexch')
            call t_drvstopf  ('DRIVER_C2G')
         endif
      endif

      !----------------------------------------------------------
      ! budget with old fractions
      !----------------------------------------------------------

#if (defined NEW_BUDGET)
      if (iamin_CPLID .and. do_budgets) then
         call t_drvstartf ('DRIVER_BUDGET1',cplrun=.true.,budget=.true.,barrier=mpicom_CPLID)
         if (lnd_present) then
            call seq_diag_lnd_mct(dom_lx, fractions_lx, l2x_l=l2x_lx, x2l_l=x2l_lx)
         endif
         if (rof_present) then
            call seq_diag_rtm_mct(dom_rx, r2x_r=r2x_rx)
         endif
         if (ocn_present) then
         !  call seq_diag_ocn_mct(dom_ox, fractions_ox, o2x_o=o2x_ox, x2o_o=x2o_ox, xao_o=xao_ox)
            call seq_diag_ocn_mct(dom_ox, fractions_ox, o2x_o=o2x_ox, x2o_o=x2o_ox, xao_o=xao_ox, r2x_o=r2x_ox)
         endif
         if (ice_present) then
            call seq_diag_ice_mct(dom_ix, fractions_ix, x2i_i=x2i_ix)
         endif
         call t_drvstopf  ('DRIVER_BUDGET1',cplrun=.true.,budget=.true.)
      endif
#endif

      !----------------------------------------------------------
      ! ice -> cpl
      !----------------------------------------------------------

      if (ice_present .and. icerun_alarm) then
         if (iamin_CPLICEID) then
            call t_drvstartf ('DRIVER_I2C',barrier=mpicom_CPLICEID)
            call t_drvstartf ('driver_i2c_icei2icex',barrier=mpicom_CPLICEID)
            call map_icei2icex_mct( cdata_ii, i2x_ii, cdata_ix, i2x_ix)
            call t_drvstopf  ('driver_i2c_icei2icex')
            call t_drvstartf ('driver_i2c_infoexch',barrier=mpicom_CPLICEID)
            call seq_infodata_exchange(infodata,CPLICEID,'ice2cpl_run')
            call t_drvstopf  ('driver_i2c_infoexch')
            call t_drvstopf  ('DRIVER_I2C')
         endif

         if (iamin_CPLID) then
            call t_drvstartf  ('DRIVER_ICEPOST',cplrun=.true.,barrier=mpicom_CPLID)
            if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
            if (info_debug > 1) then
               call t_drvstartf ('driver_icepost_diagav',barrier=mpicom_CPLID)
               call seq_diag_avect_mct(cdata_ix,i2x_ix,'recv ice')
               call t_drvstopf  ('driver_icepost_diagav')
            endif
            if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
            call t_drvstopf  ('DRIVER_ICEPOST',cplrun=.true.)
         endif

      endif   ! run alarm, ice_present

      !----------------------------------------------------------
      ! Update fractions based on new ice fractions
      !----------------------------------------------------------

      if (iamin_CPLID) then
         call t_drvstartf ('DRIVER_FRACSET',cplrun=.true.,barrier=mpicom_CPLID)
         if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
         call t_drvstartf ('driver_fracset_fracset',barrier=mpicom_CPLID)
         call seq_frac_set(i2x_ix, &
                           cdata_ax, cdata_ix, cdata_lx, cdata_ox, cdata_gx, &
                           ice_present, ocn_present, lnd_present, glc_present,  &
                           fractions_ax, fractions_ix, fractions_lx, fractions_ox, &
                           fractions_gx)
         call t_drvstopf  ('driver_fracset_fracset')
         if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
         call t_drvstopf  ('DRIVER_FRACSET',cplrun=.true.)
      endif

      !----------------------------------------------------------
      ! ATM SETUP
      !----------------------------------------------------------

      if (atm_present .and. atmrun_alarm) then
 
         !----------------------------------------------------------
         ! atm prep
         !----------------------------------------------------------

         if (iamin_CPLID) then
         if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
         call t_drvstartf ('DRIVER_ATMPREP',cplrun=.true.,barrier=mpicom_CPLID)
         if (atm_prognostic) then
            if (ocn_present) then
               if (trim(aoflux_grid) == 'ocn') then
                  call t_drvstartf ('driver_atmprep_ocn2atm2',barrier=mpicom_CPLID)
                  call map_ocn2atm_mct( cdata_ox, xao_ox, cdata_ax, xao_ax, &
                                        fractions_o=fractions_ox, fractions_a=fractions_ax, &
                                        fluxlist=seq_flds_xao_fluxes, statelist=seq_flds_xao_states )
                  call t_drvstopf  ('driver_atmprep_ocn2atm2')
               endif
            endif
            if (ocn_present) then
               call t_drvstartf ('driver_atmprep_ocn2atm1',barrier=mpicom_CPLID)
               call map_ocn2atm_mct( cdata_ox, o2x_ox, cdata_ax, o2x_ax, &
                                     fractions_o=fractions_ox, fractions_a=fractions_ax, &
                                     statelist=seq_flds_o2x_states )
               call map_ocn2atm_mct( cdata_ox, o2x_ox, cdata_ax, o2x_ax, &
                                     fluxlist=seq_flds_o2x_fluxes )
               call map_ocn2atm_mct( cdata_ox, xao_ox, cdata_ax, xao_ax, &
                                     fractions_o=fractions_ox, fractions_a=fractions_ax, &
                                     statelist=seq_flds_xao_albedo )
               call t_drvstopf  ('driver_atmprep_ocn2atm1')
            endif
            if (ice_present) then
               call t_drvstartf ('driver_atmprep_ice2atm',barrier=mpicom_CPLID)
               call map_ice2atm_mct( cdata_ix, i2x_ix, cdata_ax, i2x_ax, &
                                     fractions_i=fractions_ix, fractions_a=fractions_ax, &
                                     fluxlist=seq_flds_i2x_fluxes, statelist=seq_flds_i2x_states )
               call t_drvstopf  ('driver_atmprep_ice2atm')
            endif
            if (lnd_present) then
               call t_drvstartf ('driver_atmprep_lnd2atm',barrier=mpicom_CPLID)
               call map_lnd2atm_mct( cdata_lx, l2x_lx, cdata_ax, l2x_ax , &
                                     fractions_l=fractions_lx, fractions_a=fractions_ax, &
                                     fluxlist=seq_flds_l2x_fluxes, statelist=seq_flds_l2x_states )
               call t_drvstopf  ('driver_atmprep_lnd2atm')
            endif
            call t_drvstartf ('driver_atmprep_mrgx2a',barrier=mpicom_CPLID)
            call mrg_x2a_run_mct( cdata_ax, l2x_ax, o2x_ax, xao_ax, i2x_ax, fractions_ax, x2a_ax )
            call t_drvstopf  ('driver_atmprep_mrgx2a')

            if (info_debug > 1) then
               call t_drvstartf ('driver_atmprep_diagav',barrier=mpicom_CPLID)
               call seq_diag_avect_mct(cdata_ax,x2a_ax,'send atm')
               call t_drvstopf  ('driver_atmprep_diagav')
            endif
         endif  ! atm_prognostic
         call t_drvstopf  ('DRIVER_ATMPREP',cplrun=.true.)
         if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
         endif  ! CPLID

         !----------------------------------------------------------
         ! cpl -> atm
         !----------------------------------------------------------

         if (iamin_CPLATMID .and. atm_prognostic) then
            call t_drvstartf ('DRIVER_C2A',barrier=mpicom_CPLATMID)
            call t_drvstartf ('driver_c2a_atmx2atma',barrier=mpicom_CPLATMID)
            call map_atmx2atma_mct( cdata_ax, x2a_ax, cdata_aa, x2a_aa)
            call t_drvstopf  ('driver_c2a_atmx2atma')
            call t_drvstartf ('driver_c2a_infoexch',barrier=mpicom_CPLATMID)
            call seq_infodata_exchange(infodata,CPLATMID,'cpl2atm_run')
            call t_drvstopf  ('driver_c2a_infoexch')
            call t_drvstopf  ('DRIVER_C2A')
         endif

      endif

      !----------------------------------------------------------
      ! RUN atm model, phase = 1, juanxiong he
      !----------------------------------------------------------

      if (atm_present .and. atmrun_alarm .and. iamin_ATMID) then
         if (run_barriers) then
            call t_drvstartf ('DRIVER_ATM_RUN_BARRIER')
            call mpi_barrier(mpicom_ATMID,ierr)
            call t_drvstopf ('DRIVER_ATM_RUN_BARRIER')
         endif
         call t_drvstartf ('DRIVER_ATM_RUN',barrier=mpicom_ATMID)
         if (drv_threading) call seq_comm_setnthreads(nthreads_ATMID)
         if (atm_prognostic) call mct_avect_vecmult(x2a_aa,drv2mdl_aa,seq_flds_x2a_fluxes)
         call mct_avect_vecmult(a2x_aa,mdl2drv_aa,seq_flds_a2x_fluxes)
         integration_phase = 1 !juanxiong he
         call atm_run_mct( EClock_a, EClock_w, cdata_aa, cdata_cc, cdata_caca, x2a_aa, a2x_aa, &
                           x2c_cc1, x2c_cc2, c2x_cc1, c2x_cc2, &
                           x2ca_caca1, x2ca_caca2, ca2x_caca1, ca2x_caca2, &
                           twoway_coupling, twoway_nudging, integration_phase ) !juanxiong he
         if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
         call t_drvstopf  ('DRIVER_ATM_RUN')
      endif

      !----------------------------------------------------------
      ! Run Glc Model
      !----------------------------------------------------------

      if (glc_present .and. glcrun_alarm .and. iamin_GLCID) then
         if (run_barriers) then
            call t_drvstartf ('DRIVER_GLC_RUN_BARRIER')
            call mpi_barrier(mpicom_GLCID,ierr)
            call t_drvstopf ('DRIVER_GLC_RUN_BARRIER')
         endif
         call t_drvstartf ('DRIVER_GLC_RUN',barrier=mpicom_GLCID)
         if (drv_threading) call seq_comm_setnthreads(nthreads_GLCID)
         if (glc_prognostic) call mct_avect_vecmult(x2g_gg,drv2mdl_gg,seq_flds_x2g_fluxes)
         call glc_run_mct( EClock_g, cdata_gg, x2g_gg, g2x_gg)
         call mct_avect_vecmult(g2x_gg,mdl2drv_gg,seq_flds_g2x_fluxes)
         if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
         call t_drvstopf  ('DRIVER_GLC_RUN')
      endif
 
      !----------------------------------------------------------
      ! glc -> cpl
      !----------------------------------------------------------

      if (glc_present .and. sno_prognostic .and. glcrun_alarm) then

         if (iamin_CPLGLCID) then
            call t_drvstartf ('DRIVER_G2C',barrier=mpicom_CPLGLCID)
            call t_drvstartf ('driver_g2c_glcg2glcx',barrier=mpicom_CPLGLCID)
            call map_glcg2glcx_mct( cdata_gg, g2x_gg, cdata_gx, g2x_gx)
            call t_drvstopf  ('driver_g2c_glcg2glcx')
            call t_drvstartf ('driver_g2c_infoexch',barrier=mpicom_CPLGLCID)
            call seq_infodata_exchange(infodata,CPLGLCID,'glc2cpl_run')
            call t_drvstopf  ('driver_g2c_infoexch')
            call t_drvstopf  ('DRIVER_G2C')
         endif

         if (iamin_CPLID) then
            call t_drvstartf  ('DRIVER_GLCPOST',cplrun=.true.,barrier=mpicom_CPLID)
            if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
            if (info_debug > 1) then
               call t_drvstartf ('driver_glcpost_diagav',barrier=mpicom_CPLID)
               call seq_diag_avect_mct(cdata_gx,g2x_gx,'recv glc')
               call t_drvstopf  ('driver_glcpost_diagav')
            endif
            call t_drvstartf ('driver_glcpost_glc2sno',barrier=mpicom_CPLID)
            call map_glc2sno_mct( cdata_gx, g2x_gx, cdata_sx, g2x_sx )
            call t_drvstopf  ('driver_glcpost_glc2sno')
            call t_drvstartf ('driver_glcpost_mrgx2s',barrier=mpicom_CPLID)
            call mrg_x2s_run_mct( cdata_sx, g2x_sx, x2s_sx )
            call t_drvstopf  ('driver_glcpost_mrgx2s')
            if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
            call t_drvstopf  ('DRIVER_GLCPOST',cplrun=.true.)
         endif

      endif   ! run alarm, glc_present

      !----------------------------------------------------------
      ! atm -> cpl for wrf, juanxiong he
      !----------------------------------------------------------
      if (atm_present .and. atmrun_alarm) then
         if (iamin_CPLATMID) then
            call t_drvstartf ('DRIVER_A2C',barrier=mpicom_CPLATMID)
            call t_drvstartf ('driver_a2c_atma2atmx',barrier=mpicom_CPLATMID)
            call map_cama2camx_mct(cdata_cc, c2x_cc1, cdata_cx, c2x_cx1)  !juanxiong he
            call map_cama2camx_mct(cdata_cc, c2x_cc2, cdata_cx, c2x_cx2)  !juanxiong he
            call t_drvstopf  ('driver_a2c_atma2atmx')
            call t_drvstartf ('driver_a2c_infoexch',barrier=mpicom_CPLATMID)
            call seq_infodata_exchange(infodata,CPLATMID,'atm2cpl_run')
            call t_drvstopf  ('driver_a2c_infoexch')
            call t_drvstopf  ('DRIVER_A2C')
         endif
      endif ! run alarm

     !juanxiong he 
     if (wrf_present .and. wrfrun_alarm) then

         !----------------------------------------------------------
         ! wrf prep
         !----------------------------------------------------------

         if (iamin_CPLID) then
         if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
         call t_drvstartf ('DRIVER_WRFPREP',cplrun=.true.,barrier=mpicom_CPLID)

         call t_drvstartf ('driver_wrfprep_cam2wrf',barrier=mpicom_CPLID)
         call map_cam2wrf_mct( cdata_cx, c2x_cx1, cdata_mx, c2x_mx1 , &
                                    statelist=seq_flds_c2x_states )
         call map_cam2wrf_mct( cdata_cx, c2x_cx2, cdata_mx, c2x_mx2 , &
                                    statelist=seq_flds_c2x_states )
         call t_drvstopf  ('driver_wrfprep_cam2wrf')

         if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Calling mrg_x2w_run_mct'
         call mrg_x2w_run_mct( cdata_mx, c2x_mx1, x2m_mx1)
         call mrg_x2w_run_mct( cdata_mx, c2x_mx2, x2m_mx2)

         call t_drvstopf  ('DRIVER_WRFPREP',cplrun=.true.)
         if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
         endif  ! CPLID              

         !----------------------------------------------------------
         ! cpl -> wrf, 3d
         !----------------------------------------------------------

         if (iamin_CPLWRFID .and. wrf_prognostic) then
            call t_drvstartf ('DRIVER_C2W',barrier=mpicom_CPLWRFID)
            call t_drvstartf ('driver_c2w_wrfx2wrfa',barrier=mpicom_CPLWRFID)
            call map_wrfx2wrfw_mct( cdata_mx, x2m_mx1, cdata_mm, x2m_mm1)   ! juaxniong he
            call map_wrfx2wrfw_mct( cdata_mx, x2m_mx2, cdata_mm, x2m_mm2)   ! juaxniong he
            call t_drvstopf  ('driver_c2w_wrfx2wrfa')
            call t_drvstartf ('driver_c2w_infoexch',barrier=mpicom_CPLWRFID)
            call seq_infodata_exchange(infodata,CPLWRFID,'cpl2wrf_run')
            call t_drvstopf  ('driver_c2w_infoexch')
            call t_drvstopf  ('DRIVER_C2W')
         endif

      endif

      !----------------------------------------------------------
      ! RUN wrf model
      !----------------------------------------------------------

      if (wrf_present .and. wrfrun_alarm .and. iamin_WRFID) then
         call t_drvstartf ('DRIVER_WRF_RUN',barrier=mpicom_WRFID)
         if (drv_threading) call seq_comm_setnthreads(nthreads_WRFID)
         call wrf_run_mct( EClock_a, EClock_w, cdata_ww, cdata_mm, x2w_ww, w2x_ww, x2m_mm1, &
                           x2m_mm2, m2x_mm, twoway_coupling, twoway_nudging ) !juanxiong he
         if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
         call t_drvstopf  ('DRIVER_WRF_RUN')
      endif

      !----------------------------------------------------------
      ! wrf -> cpl
      !----------------------------------------------------------
      if (wrf_present) then
         if (iamin_CPLWRFID.and.twoway_coupling) then
            call t_drvstartf ('DRIVER_M2X',barrier=mpicom_CPLWRFID)
            call t_drvstartf ('driver_m2x_wrfw2wrfx',barrier=mpicom_CPLWRFID)
            call map_wrfw2wrfx_mct(cdata_mm, m2x_mm, cdata_mx, m2x_mx)  !juanxiong he
            call t_drvstopf  ('driver_m2x_wrfw2wrfx')
            call t_drvstartf ('driver_m2x_infoexch',barrier=mpicom_CPLWRFID)
            call seq_infodata_exchange(infodata,CPLWRFID,'wrf2cpl_run')
            call t_drvstopf  ('driver_m2x_infoexch')
            call t_drvstopf  ('DRIVER_M2X')
         endif

         if (iamin_CPLID) then
            call t_drvstartf ('DRIVER_WRFPOST',cplrun=.true.,barrier=mpicom_CPLID)
            if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
            if (info_debug > 1) then
               call t_drvstartf ('driver_wrfpost_diagav',barrier=mpicom_CPLID)
               call seq_diag_avect_mct(cdata_mx,m2x_mx,'recv wrf')
               call t_drvstopf  ('driver_wrfpost_diagav')
            endif
            if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
            call t_drvstopf  ('DRIVER_WRFPOST',cplrun=.true.)
         endif
      endif
      !----------------------------------------------------------
      ! budget with new fractions
      !----------------------------------------------------------

      !----------------------------------------------------------
      ! atm -> cpl for geatm, juanxiong he
      !----------------------------------------------------------
      if (atm_present .and. atmrun_alarm) then
         if (iamin_CPLATMID) then
            call t_drvstartf ('DRIVER_A2C',barrier=mpicom_CPLATMID)
            call t_drvstartf ('driver_a2ca_gatma2atmx',barrier=mpicom_CPLATMID)
            call map_gcama2camx_mct(cdata_caca, ca2x_caca1, cdata_cax, ca2x_cax1)  !juanxiong he
            call map_gcama2camx_mct(cdata_caca, ca2x_caca2, cdata_cax, ca2x_cax2)  !juanxiong he
            call t_drvstopf  ('driver_a2ca_gatma2atmx')
            call t_drvstartf ('driver_a2ca_infoexch',barrier=mpicom_CPLATMID)
            call seq_infodata_exchange(infodata,CPLATMID,'atm2cpl_run')
            call t_drvstopf  ('driver_a2ca_infoexch')
            call t_drvstopf  ('DRIVER_A2C')
         endif
      endif ! run alarm

     !juanxiong he 
     if (geatm_present .and. gearun_alarm) then

         !----------------------------------------------------------
         ! gea prep
         !----------------------------------------------------------

         if (iamin_CPLID) then
         if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
         call t_drvstartf ('DRIVER_GEAPREP',cplrun=.true.,barrier=mpicom_CPLID)

         call t_drvstartf ('driver_geaprep_cam2gea',barrier=mpicom_CPLID)
         call map_cam2gea_mct( cdata_cax, ca2x_cax1, cdata_gex, ca2x_chemx1 , &
                               statelist=seq_flds_ca2x_states )
         call map_cam2gea_mct( cdata_cax, ca2x_cax2, cdata_gex, ca2x_chemx2 , &
                               statelist=seq_flds_ca2x_states )
         call t_drvstopf  ('driver_geaprep_cam2gea')

         if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Calling mrg_x2w_run_mct'
         call mrg_x2ge_run_mct( cdata_gex, ca2x_chemx1, x2chem_chemx1)
         call mrg_x2ge_run_mct( cdata_gex, ca2x_chemx2, x2chem_chemx2)

         call t_drvstopf  ('DRIVER_GEAPREP',cplrun=.true.)
         if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
         endif  ! CPLID              

         !----------------------------------------------------------
         ! cpl -> gea, 3d
         !----------------------------------------------------------

         if (iamin_CPLGEAID .and. geatm_prognostic) then
            call t_drvstartf ('DRIVER_CA2GE',barrier=mpicom_CPLGEAID)
            call t_drvstartf ('driver_ca2ge_geax2geaa',barrier=mpicom_CPLGEAID)
            call map_geax2geaw_mct( cdata_gex, x2chem_chemx1, cdata_gege, x2chem_chemchem1)   ! juaxniong he
            call map_geax2geaw_mct( cdata_gex, x2chem_chemx2, cdata_gege, x2chem_chemchem2)   ! juaxniong he
            call t_drvstopf  ('driver_ca2ge_geax2geaa')
            call t_drvstartf ('driver_ca2ge_infoexch',barrier=mpicom_CPLGEAID)
            call seq_infodata_exchange(infodata,CPLGEAID,'cpl2gea_run')
            call t_drvstopf  ('driver_ca2ge_infoexch')
            call t_drvstopf  ('DRIVER_CA2GE')
         endif

      endif

      !----------------------------------------------------------
      ! RUN geatm model
      !----------------------------------------------------------

      if (geatm_present .and. gearun_alarm .and. iamin_GEAID) then
         call t_drvstartf ('DRIVER_GEA_RUN',barrier=mpicom_GEAID)
         if (drv_threading) call seq_comm_setnthreads(nthreads_GEAID)
         call geatm_run_mct( EClock_a, EClock_ge, cdata_gege, &
                             x2chem_chemchem1, x2chem_chemchem2, chem2x_chemchem ) !juanxiong he
         if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
         call t_drvstopf  ('DRIVER_GEA_RUN')
      endif

      !----------------------------------------------------------
      ! geatm -> cpl
      !----------------------------------------------------------
      if (geatm_present) then
         if (iamin_CPLGEAID.and.twoway_coupling) then
            call t_drvstartf ('DRIVER_GEA2X',barrier=mpicom_CPLGEAID)
            call t_drvstartf ('driver_chem2x_geaw2geax',barrier=mpicom_CPLGEAID)
            call map_geaw2geax_mct(cdata_gege, chem2x_chemchem, cdata_gex, chem2x_chemx)  !juanxiong he
            call t_drvstopf  ('driver_chem2x_geaw2geax')
            call t_drvstartf ('driver_chem2x_infoexch',barrier=mpicom_CPLGEAID)
            call seq_infodata_exchange(infodata,CPLGEAID,'gea2cpl_run')
            call t_drvstopf  ('driver_chem2x_infoexch')
            call t_drvstopf  ('DRIVER_GEA2X')
         endif

         if (iamin_CPLID) then
            call t_drvstartf ('DRIVER_GEAPOST',cplrun=.true.,barrier=mpicom_CPLID)
            if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
            if (info_debug > 1) then
               call t_drvstartf ('driver_geapost_diagav',barrier=mpicom_CPLID)
               call seq_diag_avect_mct(cdata_gex,chem2x_chemx,'recv geatm')
               call t_drvstopf  ('driver_geapost_diagav')
            endif
            if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
            call t_drvstopf  ('DRIVER_GEAPOST',cplrun=.true.)
         endif
      endif
      !---------------------------------------------------------- 
      ! RUN atm model, phase = 2, juanxiong he 
      !---------------------------------------------------------- 

         !----------------------------------------------------------
         ! wrf -> cpl -> atm
         !----------------------------------------------------------

         if (iamin_CPLID.and.twoway_coupling) then

             ! wrf->cpl, juanxiong he
             if (wrf_present) then
               call t_drvstartf ('driver_atmprep_wrf2atm',barrier=mpicom_CPLID)
               call map_wrf2cam_mct( cdata_mx, m2x_mx, cdata_cx, m2x_cx , &
                                     fluxlist=seq_flds_w2x_fluxes, statelist=seq_flds_w2x_states )
               call t_drvstopf  ('driver_atmprep_wrf2atm')
            endif
            call t_drvstartf ('driver_atmprep_mrgx2c',barrier=mpicom_CPLID)
            if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Calling mrg_x2c_run_mct'
            call mrg_x2c_run_mct( cdata_cx, m2x_cx, x2c_cx1 )
            call t_drvstopf  ('driver_atmprep_mrgx2c')
         endif

         if (iamin_CPLATMID .and. atm_prognostic.and.twoway_coupling) then
            !cpl->cam, juanxiong he
            call t_drvstartf ('DRIVER_C2A',barrier=mpicom_CPLATMID)
            call t_drvstartf ('driver_c2a_atmx2atma',barrier=mpicom_CPLATMID)
            call map_camx2cama_mct( cdata_cx, x2c_cx1, cdata_cc, x2c_cc1)
            call t_drvstopf  ('driver_c2a_atmx2atma')

            call t_drvstartf ('driver_c2a_infoexch',barrier=mpicom_CPLATMID)
            call seq_infodata_exchange(infodata,CPLATMID,'cpl2atm_run')
            call t_drvstopf  ('driver_c2a_infoexch')
            call t_drvstopf  ('DRIVER_C2A')
         endif

      if (atm_present .and. atmrun_alarm .and. iamin_ATMID) then
         call t_drvstartf ('DRIVER_ATM_RUN',barrier=mpicom_ATMID)
         if (drv_threading) call seq_comm_setnthreads(nthreads_ATMID)
         integration_phase = 2 !juanxiong he
         call atm_run_mct( EClock_a, EClock_w, cdata_aa, cdata_cc, cdata_caca, x2a_aa, a2x_aa, &
                           x2c_cc1, x2c_cc2, c2x_cc1, c2x_cc2, &
                           x2ca_caca1, x2ca_caca2, ca2x_caca1, ca2x_caca2, &
                           twoway_coupling, twoway_nudging, integration_phase )
         call mct_avect_vecmult(a2x_aa,mdl2drv_aa,seq_flds_a2x_fluxes)
         if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
         call t_drvstopf  ('DRIVER_ATM_RUN')
      endif

      !----------------------------------------------------------
      ! atm -> cpl for ccsm, juanxiong he
      !----------------------------------------------------------
                   
      if (atm_present .and. atmrun_alarm) then
         if (iamin_CPLATMID) then
            call t_drvstartf ('DRIVER_A2C',barrier=mpicom_CPLATMID)
            call t_drvstartf ('driver_a2c_atma2atmx',barrier=mpicom_CPLATMID)
            call map_atma2atmx_mct( cdata_aa, a2x_aa, cdata_ax, a2x_ax)
            call t_drvstopf  ('driver_a2c_atma2atmx')
            call t_drvstartf ('driver_a2c_infoexch',barrier=mpicom_CPLATMID)
            call seq_infodata_exchange(infodata,CPLATMID,'atm2cpl_run')
            call t_drvstopf  ('driver_a2c_infoexch')
            call t_drvstopf  ('DRIVER_A2C')
         endif

         if (iamin_CPLID) then
            call t_drvstartf ('DRIVER_ATMPOST',cplrun=.true.,barrier=mpicom_CPLID)
            if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
            if (info_debug > 1) then
               call t_drvstartf ('driver_atmpost_diagav',barrier=mpicom_CPLID)
               call seq_diag_avect_mct(cdata_ax,a2x_ax,'recv atm')
               call t_drvstopf  ('driver_atmpost_diagav')
            endif
            if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
            call t_drvstopf  ('DRIVER_ATMPOST',cplrun=.true.)
         endif
      
      endif ! run alarm

      !----------------------------------------------------------
      ! budget with new fractions
      !----------------------------------------------------------

#if (defined NEW_BUDGET)
      if (iamin_CPLID .and. do_budgets) then
         call t_drvstartf ('DRIVER_BUDGET2',cplrun=.true.,budget=.true.,barrier=mpicom_CPLID)
         if (atm_present) then
            call seq_diag_atm_mct(dom_ax, fractions_ax, a2x_a=a2x_ax, x2a_a=x2a_ax)
         endif
         if (ice_present) then
            call seq_diag_ice_mct(dom_ix, fractions_ix, i2x_i=i2x_ix)
         endif
         call t_drvstopf  ('DRIVER_BUDGET2',cplrun=.true.,budget=.true.)
      endif
      if (iamin_CPLID .and. do_budgets) then
         call t_drvstartf ('DRIVER_BUDGET3',cplrun=.true.,budget=.true.,barrier=mpicom_CPLID)
         call seq_diag_accum_mct()
         call t_drvstopf  ('DRIVER_BUDGET3',cplrun=.true.,budget=.true.)
      endif
      if (iamin_CPLID .and. do_budgets) then
         call t_drvstartf ('DRIVER_BUDGETF',cplrun=.true.,budget=.true.,barrier=mpicom_CPLID)
         if (.not. dead_comps) call seq_diag_print_mct(EClock_d,stop_alarm,budget_inst, &
            budget_daily, budget_month, budget_ann, budget_ltann, budget_ltend)
         call seq_diag_zero_mct(EClock=EClock_d)
         call t_drvstopf  ('DRIVER_BUDGETF',cplrun=.true.,budget=.true.)
      endif
#endif

      !----------------------------------------------------------
      ! ocn -> cpl, loose coupling (concurrent type mode)
      !----------------------------------------------------------

      if (.not.ocean_tight_coupling) then
      if (ocn_present .and. ocnnext_alarm) then
         if (iamin_CPLOCNID) then
            call t_drvstartf ('DRIVER_O2C',barrier=mpicom_CPLOCNID)
            call t_drvstartf ('driver_o2c_ocno2ocnx',barrier=mpicom_CPLOCNID)
            call map_ocno2ocnx_mct( cdata_oo, o2x_oo, cdata_ox, o2x_ox)
            call t_drvstopf  ('driver_o2c_ocno2ocnx')
            call t_drvstartf ('driver_o2c_infoexch',barrier=mpicom_CPLOCNID)
            call seq_infodata_exchange(infodata,CPLOCNID,'ocn2cpl_run')
            call t_drvstopf  ('driver_o2c_infoexch')
            call t_drvstopf  ('DRIVER_O2C')
         endif
         if (iamin_CPLID) then
            call t_drvstartf  ('DRIVER_OCNPOST',cplrun=.true.,barrier=mpicom_CPLID)
            if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
            if (info_debug > 1) then
               call t_drvstartf ('driver_ocnpost_diagav',barrier=mpicom_CPLID)
               call seq_diag_avect_mct(cdata_ox,o2x_ox,'recv ocn')
               call t_drvstopf  ('driver_ocnpost_diagav')
            endif
            if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
            call t_drvstopf  ('DRIVER_OCNPOST',cplrun=.true.)
         endif
      endif
      endif

      !----------------------------------------------------------
      ! Save driver level restart information
      !----------------------------------------------------------

      if ( restart_alarm .and. iamin_CPLID) then
         call t_drvstartf ('DRIVER_RESTART',cplrun=.true.,barrier=mpicom_CPLID)
         if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
         if (iamroot_CPLID) then
            write(logunit,104) ' Write restart file at ',ymd,tod
            call shr_sys_flush(logunit)
         endif
         call seq_rest_write(EClock_d,seq_SyncClock)
         if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
         call t_drvstopf  ('DRIVER_RESTART',cplrun=.true.)
      endif

      !----------------------------------------------------------
      ! Write history file, only AVs on CPLID
      !----------------------------------------------------------

      if (iamin_CPLID) then

         call t_drvstartf ('DRIVER_HISTORY',cplrun=.true.,barrier=mpicom_CPLID)
         if ( history_alarm) then
            if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
            if (iamroot_CPLID) then
               write(logunit,104) ' Write history file at ',ymd,tod
               call shr_sys_flush(logunit)
            endif
            call seq_hist_write(EClock_d)
            if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
         endif

         if (do_histavg) then
            call seq_hist_writeavg(EClock_d,histavg_alarm)
         endif

         if (do_hist_a2x) then
            if (trim(hist_a2x_flds) == 'all') then
               call seq_hist_writeaux(EClock_d,'a2x','doma',cdata_ax,a2x_ax,atm_nx,atm_ny,ncpl)
            else
               call seq_hist_writeaux(EClock_d,'a2x','doma',cdata_ax,a2x_ax,atm_nx,atm_ny,ncpl,&
                                                flds=hist_a2x_flds)
            endif
         endif
         if (do_hist_a2x3hr) then
            if (trim(hist_a2x3hr_flds) == 'all') then
               call seq_hist_writeaux(EClock_d,'a2x3h','doma',cdata_ax,a2x_ax,atm_nx,atm_ny,8,t3hr_alarm)
            else
               call seq_hist_writeaux(EClock_d,'a2x3h','doma',cdata_ax,a2x_ax,atm_nx,atm_ny,8,t3hr_alarm,&
                                                 flds=hist_a2x3hr_flds)
            end if
         endif
         if (do_hist_a2x3hrp) then
            if (trim(hist_a2x3hrp_flds) == 'all') then
               call seq_hist_writeaux(EClock_d,'a2x3h_prec','doma',cdata_ax,a2x_ax,atm_nx,atm_ny,8,t3hr_alarm)
            else
               call seq_hist_writeaux(EClock_d,'a2x3h_prec','doma',cdata_ax,a2x_ax,atm_nx,atm_ny,8,t3hr_alarm,&
                                              flds=hist_a2x3hrp_flds)
            end if
         endif
         if (do_hist_a2x24hr) then
            call seq_hist_writeaux(EClock_d,'a2x1d','doma',cdata_ax,a2x_ax,atm_nx,atm_ny,1,t24hr_alarm)
         endif
         if (do_hist_l2x) then
            call seq_hist_writeaux(EClock_d,'l2x','doml',cdata_lx,l2x_lx,lnd_nx,lnd_ny,ncpl)
         endif
         call t_drvstopf  ('DRIVER_HISTORY',cplrun=.true.)

      end if

      ! --- End timestep clock/timing diagnostics
      call t_drvstartf ('DRIVER_TSTAMP_WRITE',cplrun=.true.)
      if (tod == 0 .or. info_debug > 1) then
         if (iamroot_CPLID) then
            call date_and_time(dstr,tstr)
            Time_estep = mpi_wtime()
            dtstep = time_estep-time_bstep
            dtstep_acc = dtstep_acc + dtstep
            dtstep_cnt = dtstep_cnt + 1
            write(logunit,101) ' tStamp_write: model date = ',ymd,tod, &
               ' wall clock = ',dstr(1:4),'-',dstr(5:6),'-',dstr(7:8),' ',&
                                tstr(1:2),':',tstr(3:4),':',tstr(5:6), &
               ' avg dt = ',dtstep_acc/dtstep_cnt,' dt = ',dtstep 
            Time_bstep = mpi_wtime()
            call shr_sys_flush(logunit)
         endif
      endif
      if (tod == 0) then
         if (iamroot_CPLID .or. iamroot_OCNID .or. iamroot_ATMID .or. &
             iamroot_LNDID .or. iamroot_ICEID .or. iamroot_GLCID) then
            call shr_mem_getusage(msize,mrss)
            write(logunit,105) ' memory_write: model date = ',ymd,tod, &
               ' memory = ',mrss,' MB (highwater)    ',msize,' MB (usage)', &
               '  (pe=',iam_GLOID,' comps=',trim(complist)//')'
         endif
      endif
      if (info_debug > 1) then
         if (iamroot_CPLID) then
            call seq_infodata_GetData(infodata,nextsw_cday=nextsw_cday)
!            write(logunit,106) ' nextsw_cday = ',nextsw_cday
            write(logunit,*) '  nextsw_cday = ',nextsw_cday
         endif
      endif
      call t_drvstopf  ('DRIVER_TSTAMP_WRITE',cplrun=.true.)

      call t_stopf  ('DRIVER_RUN_LOOP')
      ! --- Write out performance data 
      call t_drvstartf  ('DRIVER_TPROF_WRITE',cplrun=.true.)
      if (tprof_alarm) then
         call t_startf("sync1_tprof")
         call mpi_barrier(mpicom_GLOID,ierr)
         call t_stopf("sync1_tprof")

         write(timing_file,'(a,i8.8,a1,i5.5)') trim(tchkpt_dir)//"/ccsm_timing_",ymd,"_",tod
         call t_prf(filename=trim(timing_file), mpicom=mpicom_GLOID, &
                    num_outpe=1)

         call t_startf("sync2_tprof")
         call mpi_barrier(mpicom_GLOID,ierr)
         call t_stopf("sync2_tprof")
      endif
      call t_drvstopf  ('DRIVER_TPROF_WRITE',cplrun=.true.)

   end do   ! driver run loop

   call t_startf ('DRIVER_RUN_LOOP_BSTOP')
   call mpi_barrier(mpicom_GLOID,ierr)
   call t_stopf ('DRIVER_RUN_LOOP_BSTOP')

   Time_end = mpi_wtime()

   !----------------------------------------------------------
   ! Ending of basic time step loop
   !----------------------------------------------------------

end subroutine ccsm_run
