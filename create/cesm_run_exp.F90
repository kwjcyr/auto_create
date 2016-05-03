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
!{list}
!     {ccc0}run_alarm = seq_timemgr_alarmIsOn(EClock_d,seq_timemgr_alarm_{ccc0}run)
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
               ' aliog run alarms = ',  atmrun_alarm, lndrun_alarm, &
                         icerun_alarm, ocnrun_alarm, glcrun_alarm
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

!{xmlinsert}(cpl2ccc.F90,dict_cpl2ccc,ocn);     

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

!{xmlinsert}(cpl2ccc_mul.F90,dict_mulmodel,lnd);        

      endif

      !----------------------------------------------------------
      ! ICE SETUP
      ! Note that for atm->ice mapping below will leverage the assumption that the
      ! ice and ocn are on the same grid and that mapping of atm to ocean is 
      ! done already for use by atmocn flux and ice model prep
      !----------------------------------------------------------

      if (ice_present .and. icerun_alarm) then
cyrcyr
!{xmlinsert}(cplprep.F90,dict_prepmodel,ice);
cyrcyr
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

!{xmlinsert}(cpl2ccc.F90,dict_cpl2ccc,ice);
     
      endif

!{xmlinsert}(runcccmodel.F90,dict_cpl2ccc,ocn);
      
!{xmlinsert}(runcccmodel.F90,dict_cpl2ccc,ice);

!{xmlinsert}(runmulmodel.F90,dict_mulmodel,lnd);

!{xmlinsert}(ccc2cpl.F90,dict_cpl2ccc,ocntightcpl);

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

!{xmlinsert}(ccc2cpl_mul.F90,dict_mulmodel,lnd);

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

!{xmlinsert}(cpl2ccc.F90,dict_cpl2ccc,glc);
     
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

!{xmlinsert}(ccc2cpl.F90,dict_cpl2ccc,ice);

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

!{xmlinsert}(cpl2ccc.F90,dict_cpl2ccc,atm);     

      endif

!{xmlinsert}(runcccmodel.F90,dict_cpl2ccc,atm);

!{xmlinsert}(runcccmodel.F90,dict_cpl2ccc,glc);
 
      !----------------------------------------------------------
      ! glc -> cpl
      !----------------------------------------------------------

!{xmlinsert}(ccc2cpl.F90,dict_cpl2ccc,glc); 

!{xmlinsert}(ccc2cpl.F90,dict_cpl2ccc,atm);     

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

!{xmlinsert}(ccc2cpl.F90,dict_cpl2ccc,ocnloosecpl);     

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
