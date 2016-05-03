subroutine ccsm_init()

 101  format( A, 2i8, 12A, A, F8.2, A, F8.2 )
 102  format( A, 2i8, A, 5L3 )
 103  format( 5A )
 104  format( A, 2i8)
 105  format( A, 2i8, A, f10.2, A, f10.2, A, A, i5, A, A)
 106  format( A, f23.12)

   !--------------------------------------------------------------------------
   ! Print Model heading and copyright message
   !--------------------------------------------------------------------------

   if (iamroot_CPLID) call seq_ccsm_printlogheader()

   !-----------------------------------------------------------------------------
   ! Timer initialization (has to be after mpi init)
   !-----------------------------------------------------------------------------

   call t_initf(NLFileName, LogPrint=.false., mpicom=mpicom_GLOID, &
                MasterTask=iamroot_GLOID)

   if (iamin_CPLID) then
      call seq_io_cpl_init()
   endif

   call t_startf('DRIVER_INIT')

   !-----------------------------------------------------------------------------
   ! Memory test
   !-----------------------------------------------------------------------------
   call shr_mem_init(prt=.true.)

   !-----------------------------------------------------------------------------
   ! Initialize coupled field indices
   !-----------------------------------------------------------------------------

   call seq_flds_set()
   call seq_flds_indices_set( )

   !-----------------------------------------------------------------------------
   ! Initialize infodata
   !-----------------------------------------------------------------------------

   call seq_infodata_init(infodata,nlfilename,GLOID)
   if (iamroot_CPLID) then
      write(logunit,*) ' '
      write(logunit,'(2A)') 'Status of infodata after seq_infodata_init'
      call seq_infodata_print( infodata )
      write(logunit,*) ' '
   endif

   call seq_infodata_GetData(infodata,read_restart=read_restart,restart_file=rest_file, &
        timing_dir=timing_dir, tchkpt_dir=tchkpt_dir)
   call seq_infodata_GetData(infodata, info_debug=info_debug,atm_present=atm_present, &
        lnd_present=lnd_present, ice_present=ice_present,ocn_present=ocn_present, &
        glc_present=glc_present, sno_present=sno_present,wrf_present=wrf_present, & 
        geatm_present=geatm_present, & 
        single_column=single_column, aqua_planet=aqua_planet, &
        ocean_tight_coupling=ocean_tight_coupling,drv_threading=drv_threading)
   call seq_infodata_GetData(infodata, do_histinit=do_histinit)
   call seq_infodata_GetData(infodata, do_budgets=do_budgets,budget_inst=budget_inst, &
        budget_daily=budget_daily, budget_month=budget_month,budget_ann=budget_ann, &
        budget_ltann=budget_ltann, budget_ltend=budget_ltend)
   call seq_infodata_GetData(infodata, &
        histaux_a2x    =do_hist_a2x    , histaux_a2x3hr =do_hist_a2x3hr, &
        histaux_a2x3hrp=do_hist_a2x3hrp, histaux_a2x24hr=do_hist_a2x24hr, &
        histaux_l2x    =do_hist_l2x    , histaux_r2x    =do_hist_r2x)
   call seq_infodata_GetData(infodata, run_barriers = run_barriers)

   call seq_infodata_GetData(infodata, aoflux_grid=aoflux_grid)

   call seq_infodata_GetData(infodata, shr_map_dopole=shr_map_dopole)
   call shr_map_setDopole(shr_map_dopole)

   !-----------------------------------------------------------------------------
   ! Test Threading Setup in driver, happens to be valid on all pes for
   ! all IDs
   !-----------------------------------------------------------------------------

   if (drv_threading) then
      if (iamroot_GLOID) write(logunit,*) ' '
      if (iamroot_GLOID) write(logunit,'(2A)    ') subname,' Test Threading in driver'
      call seq_comm_setnthreads(nthreads_GLOID)
      if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'nthreads_GLOID == ',nthreads_GLOID,seq_comm_getnthreads()
      call seq_comm_setnthreads(nthreads_CPLID)
      if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'nthreads_CPLID = ',nthreads_CPLID,seq_comm_getnthreads()
      if (iamroot_GLOID) write(logunit,*)
      call seq_comm_setnthreads(nthreads_OCNID)
      if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'nthreads_OCNID = ',nthreads_OCNID,seq_comm_getnthreads()
      if (iamroot_GLOID) write(logunit,*)
      call seq_comm_setnthreads(nthreads_ATMID)
      if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'nthreads_ATMID = ',nthreads_ATMID,seq_comm_getnthreads()
      if (iamroot_GLOID) write(logunit,*)
      call seq_comm_setnthreads(nthreads_LNDID)
      if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'nthreads_LNDID = ',nthreads_LNDID,seq_comm_getnthreads()
      if (iamroot_GLOID) write(logunit,*)
      call seq_comm_setnthreads(nthreads_ICEID)
      if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'nthreads_ICEID = ',nthreads_ICEID,seq_comm_getnthreads()
      if (iamroot_GLOID) write(logunit,*)
      call seq_comm_setnthreads(nthreads_GLCID)
      if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'nthreads_GLCID = ',nthreads_GLCID,seq_comm_getnthreads()
      if (iamroot_GLOID) write(logunit,*)
      call seq_comm_setnthreads(nthreads_WRFID)
      if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'nthreads_WRFID = ',nthreads_WRFID,seq_comm_getnthreads()
      if (iamroot_GLOID) write(logunit,*)
      call seq_comm_setnthreads(nthreads_GEAID)
      if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'nthreads_GEAID = ',nthreads_GEAID,seq_comm_getnthreads()
      if (iamroot_GLOID) write(logunit,*)
      call seq_comm_setnthreads(nthreads_GLOID)
   endif

   call seq_cdata_init(cdata_ax, CPLID, dom_ax, gsMap_ax,infodata,'cdata_ax' )
   call seq_cdata_init(cdata_lx, CPLID, dom_lx, gsMap_lx,infodata,'cdatl_lx' )
   call seq_cdata_init(cdata_rx, CPLID, dom_rx, gsMap_rx,infodata,'cdatr_rx' )
   call seq_cdata_init(cdata_sx, CPLID, dom_sx, gsMap_sx,infodata,'cdats_sx' )
   call seq_cdata_init(cdata_ix, CPLID, dom_ix, gsMap_ix,infodata,'cdati_ix' )
   call seq_cdata_init(cdata_ox, CPLID, dom_ox, gsMap_ox,infodata,'cdato_ox' )
   call seq_cdata_init(cdata_gx, CPLID, dom_gx, gsMap_gx,infodata,'cdatg_gx' )
   call seq_cdata_init(cdata_wx, CPLID, dom_wx, gsMap_wx,infodata,'cdatw_wx' )
   call seq_cdata_init(cdata_mx, CPLID, dom_mx, gsMap_mx,infodata,'cdatm_mx' )
   call seq_cdata_init(cdata_cx, CPLID, dom_cx, gsMap_cx,infodata,'cdatc_cx' )
   call seq_cdata_init(cdata_gex, CPLID, dom_gex, gsMap_gex,infodata,'cdatge_gex' )
   call seq_cdata_init(cdata_cax, CPLID, dom_cax, gsMap_cax,infodata,'cdatca_cax' )

   call seq_cdata_init(cdata_aa, ATMID, dom_aa, gsMap_aa,infodata,'cdata_aa')
   call seq_cdata_init(cdata_ll, LNDID, dom_ll, gsMap_ll,infodata,'cdata_ll')
   call seq_cdata_init(cdata_rr, LNDID, dom_rr, gsMap_rr,infodata,'cdata_rr')
   call seq_cdata_init(cdata_ss, LNDID, dom_ss, gsMap_ss,infodata,'cdata_ss')
   call seq_cdata_init(cdata_ii, ICEID, dom_ii, gsMap_ii,infodata,'cdata_ii')
   call seq_cdata_init(cdata_oo, OCNID, dom_oo, gsMap_oo,infodata,'cdata_oo')
   call seq_cdata_init(cdata_gg, GLCID, dom_gg, gsMap_gg,infodata,'cdata_gg')
   call seq_cdata_init(cdata_ww, WRFID, dom_ww, gsMap_ww,infodata,'cdata_ww')
   call seq_cdata_init(cdata_mm, WRFID, dom_mm, gsMap_mm,infodata,'cdata_mm')
   call seq_cdata_init(cdata_cc, ATMID, dom_cc, gsMap_cc,infodata,'cdata_cc')
   call seq_cdata_init(cdata_gege, GEAID, dom_gege, gsMap_gege,infodata,'cdata_gege')
   call seq_cdata_init(cdata_caca, ATMID, dom_caca, gsMap_caca,infodata,'cdata_caca')

   !-----------------------------------------------------------------------------
   ! Initialize time manager
   !-----------------------------------------------------------------------------

   call seq_timemgr_clockInit(seq_SyncClock,nlfilename,read_restart,rest_file,mpicom_gloid,&
      EClock_d, EClock_a, EClock_l, EClock_o, EClock_i, Eclock_g,Eclock_w, Eclock_ge)  
   if (iamroot_CPLID) then
       call seq_timemgr_clockPrint(seq_SyncClock)
   endif

   call seq_infodata_getData(infodata,orb_iyear=orb_iyear,orb_iyear_align=orb_iyear_align,&
      orb_mode=orb_mode)
   if (trim(orb_mode) == trim(seq_infodata_orb_variable_year)) then
      call seq_timemgr_EClockGetData( EClock_d, curr_ymd=ymd)
      call shr_cal_date2ymd(ymd,year,month,day)
      orb_cyear = orb_iyear + (year - orb_iyear_align)
      call shr_orb_params(orb_cyear, orb_eccen, orb_obliq, orb_mvelp, &
                          orb_obliqr, orb_lambm0, orb_mvelpp,iamroot_CPLID)
      call seq_infodata_putData(infodata,orb_eccen=orb_eccen,orb_obliqr=orb_obliqr,&
           orb_lambm0=orb_lambm0,orb_mvelpp=orb_mvelpp)
   endif

   call seq_infodata_putData(infodata,atm_phase=1,lnd_phase=1,ocn_phase=1,ice_phase=1,glc_phase=1,&
                             wrf_phase=1, geatm_phase=1) 

   !-----------------------------------------------------------------------------
   ! If in single column mode overwrite lnd,ocn,ice_present flags
   ! according to 
   ! focndomain file in ocn_in namelist
   ! SCAM can reset the lnd_present, ice_present and ocn_present flags
   !-----------------------------------------------------------------------------

   if (.not.aqua_planet .and. single_column) then
      call seq_infodata_getData( infodata, scmlon=scmlon, scmlat=scmlat)
      call shr_scam_checkSurface(scmlon, scmlat, OCNID, mpicom_OCNID, &
           lnd_present=lnd_present, ice_present=ice_present,ocn_present=ocn_present)
      call seq_infodata_putData( infodata, &
           lnd_present=lnd_present, ocn_present=ocn_present,ice_present=ocn_present)
   endif

   !-----------------------------------------------------------------------------
   ! Component Initialization
   ! Note that within each component initialization, the relevant
   ! x_pxresent flag 
   ! part of CCSMInit (contained as a pointer in cdata_xc) can be
   ! modified
   ! By default, all these flags are set to true
   ! The atm can reset the lnd_present, ice_present and ocn_present
   ! flags based
   ! on aqua_planet, ideal_phys and adiabatic modes
   ! The stub components will reset the present flags to false, all
   ! other
   ! components will set them to true for the purposes of symmetry
   !-----------------------------------------------------------------------------

   call t_startf('driver_init_comps')
   if ( iamroot_CPLID )then
      write(logunit,*) ' '
      write(logunit,F00) 'Initialize each component: atm, wrf, gea, lnd,ocn, and ice'
      call shr_sys_flush(logunit)
   endif

   !-----------------------------------------------------------------------------
   ! Initialization atmospheric component
   !-----------------------------------------------------------------------------

   call t_adj_detailf(+2)

   if (iamin_CPLATMID) then
      call seq_infodata_exchange(infodata,CPLATMID,'cpl2atm_init')
   endif
   if (iamin_ATMID .and. atm_present) then
      if (seq_comm_iamroot(ATMID)) write(logunit,F00) 'Initialize atm component'
      call shr_sys_flush(logunit)
      if (drv_threading) call seq_comm_setnthreads(nthreads_ATMID)
      call atm_init_mct( EClock_a, cdata_aa, cdata_cc, cdata_caca,x2a_aa, a2x_aa, &
                         x2c_cc1, x2c_cc2, c2x_cc1, c2x_cc2, &  
                         x2ca_caca1, x2ca_caca2, ca2x_caca1, ca2x_caca2,& 
                         twoway_coupling, twoway_nudging,NLFilename=NLFilename ) 
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif
   if (iamin_CPLATMID) then
      call seq_infodata_exchange(infodata,CPLATMID,'atm2cpl_init')
   endif

   !-----------------------------------------------------------------------------
   ! Initialization WRF component, juanxiong he
   !-----------------------------------------------------------------------------   
   if (iamin_CPLWRFID) then
      call seq_infodata_exchange(infodata,CPLWRFID,'cpl2wrf_init')
   endif
   if (iamin_WRFID .and. wrf_present) then
      if (seq_comm_iamroot(WRFID)) write(logunit,F00) 'Initialize wrf component'
      call shr_sys_flush(logunit)
      if (drv_threading) call seq_comm_setnthreads(nthreads_WRFID)
      call wrf_init_mct( EClock_w, cdata_ww, cdata_mm, x2w_ww, w2x_ww,x2m_mm1, x2m_mm2, &
                         m2x_mm, twoway_coupling, twoway_nudging,NLFilename=NLFilename )
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif
   if (iamin_CPLWRFID) then
      call seq_infodata_exchange(infodata,CPLWRFID,'wrf2cpl_init')
   endif

   !-----------------------------------------------------------------------------
   ! Initialization GEATM component, juanxiong he
   !-----------------------------------------------------------------------------   

   if (iamin_CPLGEAID) then
      call seq_infodata_exchange(infodata,CPLGEAID,'cpl2gea_init')
   endif
   if (iamin_GEAID .and. geatm_present) then
      if (seq_comm_iamroot(GEAID)) write(logunit,F00) 'Initialize gea component'
      call shr_sys_flush(logunit)
      if (drv_threading) call seq_comm_setnthreads(nthreads_GEAID)
      call geatm_init_mct( EClock_ge, cdata_gege, &
                           x2chem_chemchem1,x2chem_chemchem2,chem2x_chemchem )
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif
   if (iamin_CPLGEAID) then
      call seq_infodata_exchange(infodata,CPLGEAID,'gea2cpl_init')
   endif

   !-----------------------------------------------------------------------------
   ! Initialization land component
   !-----------------------------------------------------------------------------

   if (iamin_CPLLNDID) then
      call seq_infodata_exchange(infodata,CPLLNDID,'cpl2lnd_init')
   endif
   if (iamin_LNDID .and. lnd_present) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_LNDID)
      if (seq_comm_iamroot(LNDID)) write(logunit,F00) 'Initialize lnd component'
      call shr_sys_flush(logunit)
      call lnd_init_mct( EClock_l, cdata_ll, x2l_ll, l2x_ll, &
                                   cdata_rr,         r2x_rr, &
                                   cdata_ss, x2s_ss, s2x_ss,NLFilename=NLFilename )
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
    endif
   if (iamin_CPLLNDID) then
      call seq_infodata_exchange(infodata,CPLLNDID,'lnd2cpl_init')
   endif

   !-----------------------------------------------------------------------------
   ! Initialization ocean component
   !-----------------------------------------------------------------------------

   if (iamin_CPLOCNID) then
      call seq_infodata_exchange(infodata,CPLOCNID,'cpl2ocn_init')
   endif
   if (iamin_OCNID .and. ocn_present) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_OCNID)
      if (seq_comm_iamroot(OCNID)) write(logunit,F00) 'Initialize ocn component'
      call shr_sys_flush(logunit)
      call ocn_init_mct( EClock_o, cdata_oo, x2o_oo, o2x_oo,NLFilename=NLFilename )
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif
   if (iamin_CPLOCNID) then
      call seq_infodata_exchange(infodata,CPLOCNID,'ocn2cpl_init')
   endif

   !-----------------------------------------------------------------------------
   ! Initialization ice component
   !-----------------------------------------------------------------------------

   if (iamin_CPLICEID) then
      call seq_infodata_exchange(infodata,CPLICEID,'cpl2ice_init')
   endif
   if (iamin_ICEID .and. ice_present) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_ICEID)
      if (seq_comm_iamroot(ICEID)) write(logunit,F00) 'Initialize ice component'
      call shr_sys_flush(logunit)
      call ice_init_mct( EClock_i, cdata_ii, x2i_ii, i2x_ii,NLFilename=NLFilename )
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif
   if (iamin_CPLICEID) then
      call seq_infodata_exchange(infodata,CPLICEID,'ice2cpl_init')
   endif

   !-----------------------------------------------------------------------------
   ! Initialization glc component
   !-----------------------------------------------------------------------------

   if (iamin_CPLGLCID) then
      call seq_infodata_exchange(infodata,CPLGLCID,'cpl2glc_init')
   endif
   if (iamin_GLCID .and. glc_present) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLCID)
      if (seq_comm_iamroot(GLCID)) write(logunit,F00) 'Initialize glc component'
      call shr_sys_flush(logunit)
      call glc_init_mct( EClock_g, cdata_gg, x2g_gg, g2x_gg,NLFilename=NLFilename )
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif
   if (iamin_CPLGLCID) then
      call seq_infodata_exchange(infodata,CPLGLCID,'glc2cpl_init')
   endif

   call t_adj_detailf(-2)

   call t_stopf  ('driver_init_comps')

   !-----------------------------------------------------------------------------
   ! Determine final settings for presence of land, ice and ocean and
   ! the prognostic flags
   !-----------------------------------------------------------------------------
   if (iamin_CPLOCNID) then
      call seq_infodata_exchange(infodata,CPLOCNID,'cpl2ocn_init')
   endif
   if (iamin_CPLATMID) then
      call seq_infodata_exchange(infodata,CPLATMID,'cpl2atm_init')
   endif
   if (iamin_CPLLNDID) then
      call seq_infodata_exchange(infodata,CPLLNDID,'cpl2lnd_init')
   endif
   if (iamin_CPLICEID) then
      call seq_infodata_exchange(infodata,CPLICEID,'cpl2ice_init')
   endif
   if (iamin_CPLGLCID) then
      call seq_infodata_exchange(infodata,CPLGLCID,'cpl2glc_init')
   endif
   if (iamin_CPLWRFID) then
      call seq_infodata_exchange(infodata,CPLWRFID,'cpl2wrf_init')
   endif
   if (iamin_CPLGEAID) then
      call seq_infodata_exchange(infodata,CPLGEAID,'cpl2gea_init')
   endif
   if ( iamroot_CPLID) then
      write(logunit,F00) 'Determine final settings for presence of surface components'
      call shr_sys_flush(logunit)
   endif

   call seq_infodata_getData(infodata, &
        atm_present=atm_present, &
        wrf_present=wrf_present, & 
        geatm_present=geatm_present, & 
        lnd_present=lnd_present, &
        rof_present=rof_present, &
        ice_present=ice_present, &
        ocn_present=ocn_present, & 
        glc_present=glc_present, & 
        sno_present=sno_present, & 
        atm_prognostic=atm_prognostic, &
        wrf_prognostic=wrf_prognostic, &
        geatm_prognostic=geatm_prognostic, & 
        lnd_prognostic=lnd_prognostic, &
        ice_prognostic=ice_prognostic, &
        ocn_prognostic=ocn_prognostic, &
        ocnrof_prognostic=ocnrof_prognostic, &
        glc_prognostic=glc_prognostic, &
        sno_prognostic=sno_prognostic, &
        dead_comps=dead_comps, &
        atm_nx=atm_nx, atm_ny=atm_ny, &
        wrf_nx=wrf_nx, wrf_ny=wrf_ny, & 
        geatm_nx=geatm_nx, geatm_ny=geatm_ny, & 
        lnd_nx=lnd_nx, lnd_ny=lnd_ny, &
        rof_nx=rof_nx, rof_ny=rof_ny, &
        ice_nx=ice_nx, ice_ny=ice_ny, &
        glc_nx=glc_nx, glc_ny=glc_ny, &
        sno_nx=sno_nx, sno_ny=sno_ny, &
        ocn_nx=ocn_nx, ocn_ny=ocn_ny, &
        cpl_cdf64=cdf64, &
        atm_aero=atm_aero )

   if (.not. atm_present) then
      call shr_sys_abort('atm must be present')
   endif
   if (ocnrof_prognostic .and. .not.rof_present) then
      if (iamroot_CPLID) then
         write(logunit,F00) 'WARNING: ocnrof_prognostic is TRUE but rof_present is FALSE'
         call shr_sys_flush(logunit)
      endif
   endif
   if (ocn_prognostic .and. .not.ocn_present) then
      call shr_sys_abort('if prognostic ocn must also have ocn present')
   endif
   if (lnd_prognostic .and. .not.lnd_present) then
      call shr_sys_abort('if prognostic lnd must also have lnd present')
   endif
   if (ice_prognostic .and. .not.ice_present) then
      call shr_sys_abort('if prognostic ice must also have ice present')
   endif
   if (glc_prognostic .and. .not.glc_present) then
      call shr_sys_abort('if prognostic glc must also have glc present')
   endif
   if (sno_prognostic .and. .not.sno_present) then
      call shr_sys_abort('if prognostic sno must also have sno present')
   endif
   if (wrf_prognostic .and. .not.wrf_present) then  
      call shr_sys_abort('if prognostic wrf must also have wrf present')
   endif
   if (geatm_prognostic .and. .not.geatm_present) then  
      call shr_sys_abort('if prognostic geatm must also have geatm present')
   endif

   !-----------------------------------------------------------------------------
   ! Set domain check and other flag
   !-----------------------------------------------------------------------------

   domain_check = .true.
   if (single_column         ) domain_check = .false.
   if (dead_comps            ) domain_check = .false.

   ! set skip_ocean_run flag, used primarily for ocn run on first
   ! timestep
   ! use reading a restart as a surrogate from whether this is a startup
   ! run

   skip_ocean_run = .true.
   if ( read_restart) skip_ocean_run = .false.
   ocnrun_count = 0
   cpl2ocn_first = .true.

   do_histavg = .true.
   if (seq_timemgr_histavg_type == seq_timemgr_type_never) then
      do_histavg = .false.
   endif

   !-----------------------------------------------------------------------------
   ! Write output
   ! NOTE- assume that runoff will only be mapped from land to ocean if 
   !       prognostic ocean is true
   !-----------------------------------------------------------------------------

   if (iamroot_CPLID) then
      write(logunit,*  )' '
      write(logunit,F00)'After component initialization:'
      write(logunit,F0L)'atm model present     = ',atm_present
      write(logunit,F0L)'wrf model present     = ',wrf_present 
      write(logunit,F0L)'geatm model present  = ',geatm_present 
      write(logunit,F0L)'lnd model present     = ',lnd_present
      write(logunit,F0L)'ocn model present     = ',ocn_present
      write(logunit,F0L)'ice model present     = ',ice_present
      write(logunit,F0L)'glc model present     = ',glc_present
      write(logunit,F0L)'sno model present     = ',sno_present
      write(logunit,F0L)'atm model prognostic  = ',atm_prognostic
      write(logunit,F0L)'wrf model prognostic  = ',wrf_prognostic 
      write(logunit,F0L)'geatm model prognostic  = ',geatm_prognostic 
      write(logunit,F0L)'lnd model prognostic  = ',lnd_prognostic
      write(logunit,F0L)'ocn model prognostic  = ',ocn_prognostic
      write(logunit,F0L)'ice model prognostic  = ',ice_prognostic
      write(logunit,F0L)'glc model prognostic  = ',glc_prognostic
      write(logunit,F0L)'sno model prognostic  = ',sno_prognostic
      write(logunit,F0L)'lnd rof   present     = ',rof_present
      write(logunit,F0L)'ocn rof   prognostic  = ',ocnrof_prognostic
      write(logunit,F0L)'dead components       = ',dead_comps
      write(logunit,F0L)'domain_check          = ',domain_check
      write(logunit,F0I)'atm_nx,atm_ny         = ',atm_nx,atm_ny
      write(logunit,F0I)'wrf_nx,wrf_ny         = ',wrf_nx,wrf_ny
      write(logunit,F0I)'geatm_nx,geatm_ny         = ',geatm_nx,geatm_ny
      write(logunit,F0I)'lnd_nx,lnd_ny         = ',lnd_nx,lnd_ny
      write(logunit,F0I)'rof_nx,rof_ny         = ',rof_nx,rof_ny
      write(logunit,F0I)'ice_nx,ice_ny         = ',ice_nx,ice_ny
      write(logunit,F0I)'ocn_nx,ocn_ny         = ',ocn_nx,ocn_ny
      write(logunit,F0I)'glc_nx,glc_ny         = ',glc_nx,glc_ny
      write(logunit,F0I)'sno_nx,sno_ny         = ',sno_nx,sno_ny
      write(logunit,F0L)'skip init ocean run   = ',skip_ocean_run
      write(logunit,F0L)'ocean tight coupling  = ',ocean_tight_coupling
      write(logunit,F0L)'cpl_cdf64             = ',cdf64
      write(logunit,F0L)'do_histavg            = ',do_histavg
      write(logunit,F0L)'atm_aero              = ',atm_aero
      write(logunit,*  )' '
      call shr_sys_flush(logunit)
   endif

   !-----------------------------------------------------------------------------
   ! Need to initialize aream, set it to area for now until maps are
   ! read
   !   in some cases, maps are not read at all !!
   ! Need to initialize ascale
   ! Entire domain must have reasonable values before calling xxx2xxx
   ! init
   ! NOTE (tcx) : use cdata%dom instead of dom% due to seg fault on
   ! bluevista I, why?
   !-----------------------------------------------------------------------------

   if (iamin_ATMID .and. atm_present) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_ATMID)
      k1 = mct_aVect_indexRa(cdata_aa%dom%data,"area"  ,perrWith='aa area ')
      k2 = mct_aVect_indexRa(cdata_aa%dom%data,"aream" ,perrWith='aa aream')
      k3 = mct_aVect_indexRa(cdata_aa%dom%data,"ascale",perrWith='aa ascale')
      cdata_aa%dom%data%rAttr(k2,:) = cdata_aa%dom%data%rAttr(k1,:)
      cdata_aa%dom%data%rAttr(k3,:) = 1.0_r8
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif

   if (iamin_LNDID .and. lnd_present) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_LNDID)
      k1 = mct_aVect_indexRa(cdata_ll%dom%data,"area"  ,perrWith='ll area ')
      k2 = mct_aVect_indexRa(cdata_ll%dom%data,"aream" ,perrWith='ll aream')
      k3 = mct_aVect_indexRa(cdata_ll%dom%data,"ascale",perrWith='ll ascale')
      cdata_ll%dom%data%rAttr(k2,:) = cdata_ll%dom%data%rAttr(k1,:)
      cdata_ll%dom%data%rAttr(k3,:) = 1.0_r8
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif

   if (iamin_LNDID .and. rof_present) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_LNDID)
      k1 = mct_aVect_indexRa(cdata_rr%dom%data,"area"  ,perrWith='rr area ')
      k2 = mct_aVect_indexRa(cdata_rr%dom%data,"aream" ,perrWith='rr aream')
      k3 = mct_aVect_indexRa(cdata_rr%dom%data,"ascale",perrWith='rr ascale')
      cdata_rr%dom%data%rAttr(k2,:) = cdata_rr%dom%data%rAttr(k1,:)
      cdata_rr%dom%data%rAttr(k3,:) = 1.0_r8
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif

   if (iamin_LNDID .and. sno_present) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_LNDID)
      k1 = mct_aVect_indexRa(cdata_ss%dom%data,"area"  ,perrWith='ss area ')
      k2 = mct_aVect_indexRa(cdata_ss%dom%data,"aream" ,perrWith='ss aream')
      k3 = mct_aVect_indexRa(cdata_ss%dom%data,"ascale",perrWith='ss ascale')
      cdata_ss%dom%data%rAttr(k2,:) = cdata_ss%dom%data%rAttr(k1,:)
      cdata_ss%dom%data%rAttr(k3,:) = 1.0_r8
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif

   if (iamin_OCNID .and. ocn_present) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_OCNID)
      k1 = mct_aVect_indexRa(cdata_oo%dom%data,"area"  ,perrWith='oo area ')
      k2 = mct_aVect_indexRa(cdata_oo%dom%data,"aream" ,perrWith='oo aream')
      k3 = mct_aVect_indexRa(cdata_oo%dom%data,"ascale",perrWith='oo ascale')
      cdata_oo%dom%data%rAttr(k2,:) = cdata_oo%dom%data%rAttr(k1,:)
      cdata_oo%dom%data%rAttr(k3,:) = 1.0_r8
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif

   if (iamin_ICEID .and. ice_present) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_ICEID)
      k1 = mct_aVect_indexRa(cdata_ii%dom%data,"area"  ,perrWith='ii area ')
      k2 = mct_aVect_indexRa(cdata_ii%dom%data,"aream" ,perrWith='ii aream')
      k3 = mct_aVect_indexRa(cdata_ii%dom%data,"ascale",perrWith='ii ascale')
      cdata_ii%dom%data%rAttr(k2,:) = cdata_ii%dom%data%rAttr(k1,:)
      cdata_ii%dom%data%rAttr(k3,:) = 1.0_r8
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif

   if (iamin_GLCID .and. glc_present) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLCID)
      k1 = mct_aVect_indexRa(cdata_gg%dom%data,"area"  ,perrWith='gg area ')
      k2 = mct_aVect_indexRa(cdata_gg%dom%data,"aream" ,perrWith='gg aream')
      k3 = mct_aVect_indexRa(cdata_gg%dom%data,"ascale",perrWith='gg ascale')
      cdata_gg%dom%data%rAttr(k2,:) = cdata_gg%dom%data%rAttr(k1,:)
      cdata_gg%dom%data%rAttr(k3,:) = 1.0_r8
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif

   !-----------------------------------------------------------------------------
   ! Initialize driver rearrangers and AVs on driver
   ! Initialize cdata_*x data
   ! Zero out x2*_** in case it never gets used then it'll produce zeros
   ! in diags
   !-----------------------------------------------------------------------------
   call t_startf('driver_init_xxx2xxx')

   if (iamin_CPLATMID .and. atm_present) then
      call map_atm2atm_init_mct(cdata_aa, x2a_aa, a2x_aa, ATMID, &
                                cdata_ax, x2a_ax, a2x_ax, CPLID,CPLATMID)
      call mct_avect_zero(x2a_aa)
      call map_atma2atmx_mct(cdata_aa, x2a_aa, cdata_ax, x2a_ax)
   endif

   if (iamin_CPLLNDID .and. lnd_present) then
      call map_lnd2lnd_init_mct(cdata_ll, x2l_ll, l2x_ll, LNDID, &
                                cdata_lx, x2l_lx, l2x_lx, CPLID,CPLLNDID)
      call mct_avect_zero(x2l_ll)
      call map_lndl2lndx_mct(cdata_ll, x2l_ll, cdata_lx, x2l_lx)
   endif

   if (iamin_CPLLNDID .and. rof_present) then
      call map_rof2rof_init_mct(cdata_rr,         r2x_rr, LNDID, &
                                cdata_rx,         r2x_rx, CPLID,CPLLNDID)
      call mct_avect_init(r2xacc_rx%data, r2x_rx,mct_aVect_lsize(r2x_rx))
      call mct_accum_zero(r2xacc_rx)
      r2xacc_rx_cnt = 0
   endif

   if (iamin_CPLLNDID .and. sno_present) then
      call map_sno2sno_init_mct(cdata_ss, x2s_ss, s2x_ss, LNDID, &
                                cdata_sx, x2s_sx, s2x_sx, CPLID,CPLLNDID)
      call mct_avect_zero(x2s_ss)
      call map_snos2snox_mct(cdata_ss, x2s_ss, cdata_sx, x2s_sx)
   endif

   if (iamin_CPLICEID .and. ice_present) then
      call map_ice2ice_init_mct(cdata_ii, x2i_ii, i2x_ii, ICEID, &
                                cdata_ix, x2i_ix, i2x_ix, CPLID,CPLICEID)
      call mct_avect_zero(x2i_ii)
      call map_icei2icex_mct(cdata_ii, x2i_ii, cdata_ix, x2i_ix)
   endif

   if (iamin_CPLGLCID .and. glc_present) then
      call map_glc2glc_init_mct(cdata_gg, x2g_gg, g2x_gg, GLCID, &
                                cdata_gx, x2g_gx, g2x_gx, CPLID,CPLGLCID)
      call mct_avect_zero(x2g_gg)
      call map_glcg2glcx_mct(cdata_gg, x2g_gg, cdata_gx, x2g_gx)
   endif

   if (iamin_CPLOCNID .and. ocn_present) then
      call map_ocn2ocn_init_mct(cdata_oo, x2o_oo, o2x_oo, OCNID, &
                                cdata_ox, x2o_ox, o2x_ox, CPLID,CPLOCNID)
      call mct_avect_zero(x2o_oo)
      call map_ocno2ocnx_mct(cdata_oo, x2o_oo, cdata_ox, x2o_ox)
      call mct_avect_init(x2oacc_ox%data, x2o_ox,mct_aVect_lsize(x2o_ox))
      call mct_accum_zero(x2oacc_ox)
      x2oacc_ox_cnt = 0
   endif

   !-----------------------------------------------------------------------------
   ! GEATM and WRF, Juanxiong He
   !-----------------------------------------------------------------------------

   if (iamin_CPLATMID .and. atm_present) then 
      call map_cam2cam_init_mct(cdata_cc, x2c_cc1, c2x_cc1, ATMID, &
                                cdata_cx, x2c_cx1, c2x_cx1, CPLID,CPLATMID)
      call map_cam2cam_init_mct(cdata_cc, x2c_cc2, c2x_cc2, ATMID, &
                                cdata_cx, x2c_cx2, c2x_cx2, CPLID,CPLATMID)
      call mct_avect_zero(x2c_cc1)
      call mct_avect_zero(x2c_cc2)
      call map_cama2camx_mct(cdata_cc, x2c_cc1, cdata_cx, x2c_cx1)  
      call map_cama2camx_mct(cdata_cc, x2c_cc2, cdata_cx, x2c_cx2)  
   endif

   if (iamin_CPLWRFID .and. wrf_present) then 
      call map_wrf2wrf_init_mct(cdata_mm, x2m_mm1, m2x_mm, WRFID, &
                                cdata_mx, x2m_mx1, m2x_mx, CPLID,CPLWRFID)
      call map_wrf2wrf_init_mct(cdata_mm, x2m_mm2, m2x_mm, WRFID, &
                                cdata_mx, x2m_mx2, m2x_mx, CPLID,CPLWRFID)
      call mct_avect_zero(x2m_mm1)
      call mct_avect_zero(x2m_mm2)
      call map_wrfw2wrfx_mct(cdata_mm, x2m_mm1, cdata_mx, x2m_mx1)   
      call map_wrfw2wrfx_mct(cdata_mm, x2m_mm2, cdata_mx, x2m_mx2)   
   endif

   if (iamin_CPLATMID .and. atm_present) then 
      call map_gcam2cam_init_mct(cdata_caca, x2ca_caca1, ca2x_caca1,ATMID, &
                                 cdata_cax, x2ca_cax1, ca2x_cax1, CPLID,CPLATMID)
      call map_gcam2cam_init_mct(cdata_caca, x2ca_caca2, ca2x_caca2,ATMID, &
                                 cdata_cax, x2ca_cax2, ca2x_cax2, CPLID,CPLATMID)
      call mct_avect_zero(x2ca_caca1)
      call mct_avect_zero(x2ca_caca2)
      call map_gcama2camx_mct(cdata_caca, x2ca_caca1, cdata_cax,x2ca_cax1)  
      call map_gcama2camx_mct(cdata_caca, x2ca_caca2, cdata_cax,x2ca_cax2)  
   endif

   if (iamin_CPLGEAID .and. geatm_present) then 
      call map_gea2gea_init_mct(cdata_gege, x2chem_chemchem1,chem2x_chemchem, GEAID, &
                                cdata_gex, x2chem_chemx1, chem2x_chemx,CPLID, CPLGEAID)
      call map_gea2gea_init_mct(cdata_gege, x2chem_chemchem2,chem2x_chemchem, GEAID, &
                                cdata_gex, x2chem_chemx2, chem2x_chemx,CPLID, CPLGEAID)
      call mct_avect_zero(x2chem_chemchem1)
      call mct_avect_zero(x2chem_chemchem2)
      call map_geaw2geax_mct(cdata_gege, x2chem_chemchem1, cdata_gex,x2chem_chemx1)   
      call map_geaw2geax_mct(cdata_gege, x2chem_chemchem2, cdata_gex,x2chem_chemx2)   
   endif
   !-----------------------------------------------------------------------------
   ! GEATM and WRF, Juanxiong He
   !-----------------------------------------------------------------------------

   call t_stopf  ('driver_init_xxx2xxx')

   !-----------------------------------------------------------------------------
   ! Remainder of initialization
   !-----------------------------------------------------------------------------

   if (iamin_CPLID) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)

      !-----------------------------------------------------------------------------
      ! Allocate attribute vectors for merge components
      !-----------------------------------------------------------------------------
      if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Initializingmerge components'
                       call mrg_x2a_init_mct( cdata_ax, l2x_ax, o2x_ax,i2x_ax, xao_ax )
                       call mrg_x2w_init_mct( cdata_mx, c2x_mx1 )
                       call mrg_x2w_init_mct( cdata_mx, c2x_mx2 )
                       call mrg_x2c_init_mct( cdata_cx, m2x_cx )
                       call mrg_x2ge_init_mct( cdata_gex, ca2x_chemx1 )
                       call mrg_x2ge_init_mct( cdata_gex, ca2x_chemx2 )
                       call mrg_x2ca_init_mct( cdata_cax, chem2x_cax )
      if (ice_present) call mrg_x2i_init_mct( cdata_ix, a2x_ix, o2x_ix )
      if (ocn_present) call mrg_x2o_init_mct( cdata_ox, a2x_ox, i2x_ox, r2x_ox )
      if (lnd_present) call mrg_x2l_init_mct( cdata_lx, a2x_lx) 
      if (glc_present) call mrg_x2g_init_mct( cdata_gx, s2x_gx)
      if (sno_present) call mrg_x2s_init_mct( cdata_sx, g2x_sx)

      !-----------------------------------------------------------------------------
      ! Initialize mapping
      ! Read aream into domains!
      !-----------------------------------------------------------------------------

      call t_startf('driver_init_maps')

      if (ocn_present) then
         if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Initializing atm/ocn mapping'
         call map_atm2ocn_init_mct(cdata_ax, cdata_ox)
         call map_ocn2atm_init_mct(cdata_ox, cdata_ax)
      endif
      if (ice_present .and. ocn_present) then
         if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Initializing ocn/ice mapping'
         call map_ocn2ice_init_mct(cdata_ox, cdata_ix)
         call map_ice2ocn_init_mct(cdata_ix, cdata_ox)
      endif
      if (ice_present) then
         if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Initializing atm/ice mapping'
         call map_ice2atm_init_mct(cdata_ix, cdata_ax)
      endif
      if (rof_present .and. ocnrof_prognostic) then
         if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Initializing rof/ocn mapping'
         call map_rof2ocn_init_mct(cdata_rx, cdata_ox)
      endif
      if (lnd_present) then
         if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Initializing atm/lnd mapping'
         call map_atm2lnd_init_mct(cdata_ax, cdata_lx)
         call map_lnd2atm_init_mct(cdata_lx, cdata_ax)
      endif
      if (sno_present .and. glc_present) then
         if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Initializing sno/glc mapping'
         call map_sno2glc_init_mct(cdata_sx, cdata_gx)
         call map_glc2sno_init_mct(cdata_gx, cdata_sx)
      endif
      if (wrf_present) then     
         if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Initializing wrf/cam mapping'
         call map_cam2wrf_init_mct(cdata_cx, cdata_mx)
         call map_wrf2cam_init_mct(cdata_mx, cdata_cx)
      endif
      if (geatm_present) then    
         if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Initializing geatm/cam mapping'
         call map_cam2gea_init_mct(cdata_cax, cdata_gex)
         call map_gea2cam_init_mct(cdata_gex, cdata_cax)
      endif

      call t_stopf  ('driver_init_maps')

      !-----------------------------------------------------------------------------
      ! Check domains if appropriate
      ! This must be done after the mappers are initialized since
      ! checking is done on each processor and not with a global gather
      !-----------------------------------------------------------------------------

      if (domain_check) then
         if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Performing domain checking'
         call seq_domain_check_mct( cdata_ax, cdata_ix, cdata_lx, cdata_ox, &
                                    cdata_rx, cdata_gx, cdata_sx)
      endif

      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif 

   !-----------------------------------------------------------------------------
   ! Map  dom_*x to dom_** in case any domain fields have been updated
   ! on cpl pes
   ! Initialize area corrections based on aream (read in map_init) and
   ! area
   ! Area correct component initialization output fields
   ! Map initial component AVs from component to coupler pes
   !-----------------------------------------------------------------------------

   if (iamin_CPLATMID .and. atm_present) then
      call map_atmx2atma_mct( cdata_ax, dom_ax%data, cdata_aa,dom_aa%data)
      if (iamin_ATMID) then
         call domain_areafactinit_mct(cdata_aa,mdl2drv_aa,drv2mdl_aa,'areafact_a')
         call mct_avect_vecmult(a2x_aa,mdl2drv_aa,seq_flds_a2x_fluxes)
      endif
      call map_atma2atmx_mct(cdata_aa, a2x_aa, cdata_ax, a2x_ax)
   endif

   if (iamin_CPLLNDID .and. lnd_present) then
      call map_lndx2lndl_mct( cdata_lx, dom_lx%data, cdata_ll,dom_ll%data)
      if (iamin_LNDID) then
         call domain_areafactinit_mct(cdata_ll,mdl2drv_ll,drv2mdl_ll,'areafact_l')
         call mct_avect_vecmult(l2x_ll,mdl2drv_ll,seq_flds_l2x_fluxes)
      endif
      call map_lndl2lndx_mct(cdata_ll, l2x_ll, cdata_lx, l2x_lx)
   endif

   if (iamin_CPLLNDID .and. rof_present) then
      call map_rofx2rofr_mct( cdata_rx, dom_rx%data, cdata_rr,dom_rr%data)
      if (iamin_LNDID) then
         call domain_areafactinit_mct(cdata_rr,mdl2drv_rr,drv2mdl_rr,'areafact_r')
         call mct_avect_vecmult(r2x_rr,mdl2drv_rr,seq_flds_r2x_fluxes)
      endif
      call map_rofr2rofx_mct(cdata_rr, r2x_rr, cdata_rx, r2x_rx)
   endif

   if (iamin_CPLLNDID .and. sno_present) then
      call map_snox2snos_mct( cdata_sx, dom_sx%data, cdata_ss,dom_ss%data)
      if (iamin_LNDID) then
         call domain_areafactinit_mct(cdata_ss,mdl2drv_ss,drv2mdl_ss,'areafact_s')
         call mct_avect_vecmult(s2x_ss,mdl2drv_ss,seq_flds_s2x_fluxes)
      endif
      call map_snos2snox_mct(cdata_ss, s2x_ss, cdata_sx, s2x_sx)
   endif

   if (iamin_CPLOCNID .and. ocn_present) then
      call map_ocnx2ocno_mct( cdata_ox, dom_ox%data, cdata_oo,dom_oo%data)
      if (iamin_OCNID) then
         call domain_areafactinit_mct(cdata_oo,mdl2drv_oo,drv2mdl_oo,'areafact_o')
         call mct_avect_vecmult(o2x_oo,mdl2drv_oo,seq_flds_o2x_fluxes)
      endif
      call map_ocno2ocnx_mct(cdata_oo, o2x_oo, cdata_ox, o2x_ox)
   endif

   if (iamin_CPLICEID .and. ice_present) then
      call map_icex2icei_mct( cdata_ix, dom_ix%data, cdata_ii,dom_ii%data)
      if (iamin_ICEID) then
         call domain_areafactinit_mct(cdata_ii,mdl2drv_ii,drv2mdl_ii,'areafact_i')
         call mct_avect_vecmult(i2x_ii,mdl2drv_ii,seq_flds_i2x_fluxes)
      endif
      call map_icei2icex_mct(cdata_ii, i2x_ii, cdata_ix, i2x_ix)
   endif

   if (iamin_CPLGLCID .and. glc_present) then
      call map_glcx2glcg_mct( cdata_gx, dom_gx%data, cdata_gg,dom_gg%data)
      if (iamin_GLCID) then
         call domain_areafactinit_mct(cdata_gg,mdl2drv_gg,drv2mdl_gg,'areafact_g')
         call mct_avect_vecmult(g2x_gg,mdl2drv_gg,seq_flds_g2x_fluxes)
      endif
      call map_glcg2glcx_mct(cdata_gg, g2x_gg, cdata_gx, g2x_gx)
   endif

   !-----------------------------------------------------------------------------
   ! global sum diagnostics for IC data
   !-----------------------------------------------------------------------------
   if (iamin_CPLID .and. info_debug > 1) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
      if (atm_present) call seq_diag_avect_mct(cdata_ax,a2x_ax,'recv atm IC')
      if (ice_present) call seq_diag_avect_mct(cdata_ix,i2x_ix,'recv ice IC')
      if (lnd_present) call seq_diag_avect_mct(cdata_lx,l2x_lx,'recv lnd IC')
      if (rof_present) call seq_diag_avect_mct(cdata_rx,r2x_rx,'recv rof IC')
      if (sno_present) call seq_diag_avect_mct(cdata_sx,s2x_sx,'recv sno IC')
      if (ocn_present) call seq_diag_avect_mct(cdata_ox,o2x_ox,'recv ocn IC')
      if (glc_present) call seq_diag_avect_mct(cdata_gx,g2x_gx,'recv glc IC')
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   end if

   !-----------------------------------------------------------------------------
   ! Initialize fractions
   !-----------------------------------------------------------------------------

   if (iamin_CPLID) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
      if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Initializing fractions'
      call seq_frac_init(cdata_ax, cdata_ix, cdata_lx, cdata_ox,cdata_gx, &
                         ice_present, ocn_present, lnd_present,glc_present, &
                         dead_comps, &
                         fractions_ax, fractions_ix, fractions_lx,fractions_ox, &
                         fractions_gx)
      if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Setting fractions '
      call seq_frac_set(i2x_ix, &
                        cdata_ax, cdata_ix, cdata_lx, cdata_ox,cdata_gx, &
                        ice_present, ocn_present, lnd_present,glc_present, &
                        fractions_ax, fractions_ix, fractions_lx,fractions_ox, &
                        fractions_gx)

      !-----------------------------------------------------------------------------
      ! Initialize atm/ocn flux component and compute ocean albedos
      !-----------------------------------------------------------------------------
      if (ocn_present) then
         if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Initializing atm/ocn flux component'
         ! note: albedo_only mode doesn't use a2x_ox or o2x_ox or a2x_ax
         ! or o2x_ax
         ! Initialize attribute vector
         call mct_aVect_init(xao_ox, rList=seq_flds_xao_fields,lsize=mct_aVect_lsize(o2x_ox))
         call mct_aVect_zero(xao_ox)
         if (trim(aoflux_grid) == 'ocn') then
            call seq_flux_init_mct(cdata_ox,fractions_ox)
         elseif (trim(aoflux_grid) == 'atm') then
            call seq_flux_init_mct(cdata_ax,fractions_ax)
         elseif (trim(aoflux_grid) == 'exch') then
            call seq_flux_initexch_mct(cdata_ax,cdata_ox)
         endif
         call seq_flux_ocnalb_mct(cdata_ox,xao_ox,fractions_ox)
      endif

      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif

   !-----------------------------------------------------------------------------
   ! Recalculate initial solar. Merge atmosphere input state and run
   ! atmospheric radiation
   ! tcx - for initialization only?
   !-----------------------------------------------------------------------------

   call t_startf('driver_init_atminit')

   if (atm_prognostic) then
      if (iamin_CPLID) then
         if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
         if (lnd_present) then
            if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Calling map_lnd2atm_mct'
            call map_lnd2atm_mct( cdata_lx, l2x_lx, cdata_ax, l2x_ax, &
                                  fractions_l=fractions_lx,fractions_a=fractions_ax, &
                                  fluxlist=seq_flds_l2x_fluxes,statelist=seq_flds_l2x_states )
         endif
         if (ocn_present) then
            if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Calling map_ocn2atm_mct for mapping o2x_ox to o2x_ax'
            call map_ocn2atm_mct( cdata_ox, o2x_ox, cdata_ax, o2x_ax, &
                                  fractions_o=fractions_ox,fractions_a=fractions_ax, &
                                  statelist=seq_flds_o2x_states )
            call map_ocn2atm_mct( cdata_ox, o2x_ox, cdata_ax, o2x_ax, &
                                  fluxlist=seq_flds_o2x_fluxes )
            call map_ocn2atm_mct( cdata_ox, xao_ox, cdata_ax, xao_ax, &
                                  fractions_o=fractions_ox,fractions_a=fractions_ax, &
                                  statelist=seq_flds_xao_albedo )
            if (trim(aoflux_grid) == 'ocn') then
               if ( seq_comm_iamroot(CPLID)) &
                  write(logunit,F00) 'Calling map_ocn2atm_mct for mapping xao_ox to xao_ax'
               call map_ocn2atm_mct( cdata_ox, xao_ox, cdata_ax, xao_ax,&
                                     fractions_o=fractions_ox,fractions_a=fractions_ax, &
                                     fluxlist=seq_flds_xao_fluxes,statelist=seq_flds_xao_states ) 
            endif
            if (trim(aoflux_grid) == 'atm') then
               if ( seq_comm_iamroot(CPLID)) &
                  write(logunit,F00) 'Calling map_atm2ocn_mct for mapping xao_ax to xao_ox'
! tcraig: this mapping has to be done with area overlap mapping for all
! fields 
! due to the masking of the xao_ax data and the fact that states are
! mapped with 
! bilinear mapping currently
!               call map_atm2ocn_mct( cdata_ax, xao_ax, cdata_ox,
!               xao_ox, &
!                                     fluxlist=seq_flds_xao_fluxes,
!                                     statelist=seq_flds_xao_states )
               call map_atm2ocn_mct( cdata_ax, xao_ax, cdata_ox, xao_ox,&
                                     fluxlist=seq_flds_xao_states//":"//seq_flds_xao_fluxes)
            endif
         endif
         if (ice_present) then
            if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Calling map_ice2atm_mct for mapping i2x_ix to i2x_ax'
            call map_ice2atm_mct( cdata_ix, i2x_ix, cdata_ax, i2x_ax, &
                                  fractions_i=fractions_ix,fractions_a=fractions_ax, &
                                  fluxlist=seq_flds_i2x_fluxes,statelist=seq_flds_i2x_states ) 
         endif
         if (lnd_present .or. ocn_present) then
            if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Calling mrg_x2a_run_mct'
            call mrg_x2a_run_mct( cdata_ax, l2x_ax, o2x_ax, xao_ax,i2x_ax, fractions_ax, x2a_ax )
         endif
   
         if (info_debug > 1) call seq_diag_avect_mct(cdata_ax,x2a_ax,'send atm IC2')
         if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Calling atm_init_mct'
   
         if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
      endif

      if (iamin_CPLATMID) then
         call map_atmx2atma_mct( cdata_ax, x2a_ax, cdata_aa, x2a_aa)
         call seq_infodata_exchange(infodata,CPLATMID,'cpl2atm_init')
      endif
   endif  

   !-----------------------------------------------------------------------------
   ! Second phase of atmosphere component initialization, recalculate
   ! solar based 
   ! on input albedo's from surface components. Data or dead atmosphere
   ! may just
   ! return on this phase.
   !-----------------------------------------------------------------------------

   if (iamin_ATMID) then
      call t_adj_detailf(+2)
      if (drv_threading) call seq_comm_setnthreads(nthreads_ATMID)
      call seq_infodata_putData(infodata,atm_phase=2)
      call mct_avect_vecmult(x2a_aa,drv2mdl_aa,seq_flds_x2a_fluxes)
      call atm_init_mct( EClock_a, cdata_aa, cdata_cc, cdata_caca,x2a_aa, a2x_aa, &
                         x2c_cc1, x2c_cc2,  c2x_cc1, c2x_cc2, &
                         x2ca_caca1, x2ca_caca2,  ca2x_caca1,ca2x_caca2, &
                         twoway_coupling, twoway_nudging)
      call mct_avect_vecmult(a2x_aa,mdl2drv_aa,seq_flds_a2x_fluxes)
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
      call t_adj_detailf(-2)
   endif

   if (iamin_CPLATMID) then
      call map_atma2atmx_mct( cdata_aa, a2x_aa, cdata_ax, a2x_ax)
      call map_cama2camx_mct(cdata_cc, c2x_cc1, cdata_cx, c2x_cx1)
      call map_cama2camx_mct(cdata_cc, c2x_cc2, cdata_cx, c2x_cx2)
      call map_gcama2camx_mct(cdata_caca, ca2x_caca1, cdata_cax,ca2x_cax1)  
      call map_gcama2camx_mct(cdata_caca, ca2x_caca2, cdata_cax,ca2x_cax2) 
      call seq_infodata_exchange(infodata,CPLATMID,'atm2cpl_init')
    endif

   if (iamin_CPLID) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
      if (info_debug > 1) call seq_diag_avect_mct(cdata_ax,a2x_ax,'recv atm IC2')
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
   endif

   !------------------------------------------------------------------
   ! wrf  and geatm initial, second phase, juanxiong he
   !------------------------------------------------------------------
        
   if (iamin_CPLID) then
            call map_cam2wrf_mct( cdata_cx, c2x_cx1, cdata_mx, c2x_mx1 ,&
                                    statelist=seq_flds_c2x_states ) 
            call map_cam2wrf_mct( cdata_cx, c2x_cx2, cdata_mx, c2x_mx2 ,&
                                    statelist=seq_flds_c2x_states ) 
            call mrg_x2w_run_mct( cdata_mx, c2x_mx1, x2m_mx1)  
            call mrg_x2w_run_mct( cdata_mx, c2x_mx2, x2m_mx2)  
            call map_cam2gea_mct( cdata_cax, ca2x_cax1, cdata_gex,ca2x_chemx1 , &
                                  statelist=seq_flds_ca2x_states ) 
            call map_cam2gea_mct( cdata_cax, ca2x_cax2, cdata_gex,ca2x_chemx2 , & 
                                  statelist=seq_flds_ca2x_states ) 
            call mrg_x2ge_run_mct( cdata_gex, ca2x_chemx1,x2chem_chemx1)  
            call mrg_x2ge_run_mct( cdata_gex, ca2x_chemx2,x2chem_chemx2)  
   endif

   if (iamin_CPLWRFID .and. wrf_prognostic) then
            call t_drvstartf('driver_c2w_wrfx2wrfa',barrier=mpicom_CPLWRFID)
            call map_wrfx2wrfw_mct( cdata_mx, x2m_mx1, cdata_mm,x2m_mm1)   
            call map_wrfx2wrfw_mct( cdata_mx, x2m_mx2, cdata_mm,x2m_mm2)  
            call t_drvstopf  ('driver_c2w_wrfx2wrfa')
   endif

   if (iamin_WRFID) then
      call t_adj_detailf(+2)
      if (drv_threading) call seq_comm_setnthreads(nthreads_WRFID)
      call seq_infodata_putData(infodata,wrf_phase=2)
      call wrf_init_mct( EClock_w, cdata_ww, cdata_mm, x2w_ww, w2x_ww,x2m_mm1, x2m_mm2, m2x_mm, &
                         twoway_coupling, twoway_nudging )    
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
      twoway_coupling = .False.
      call t_adj_detailf(-2)
   endif

   if (iamin_CPLGEAID .and. geatm_prognostic) then
            call t_drvstartf('driver_ca2ge_geax2geaa',barrier=mpicom_CPLGEAID) 
            call map_geax2geaw_mct( cdata_gex, x2chem_chemx1,cdata_gege, x2chem_chemchem1)  
            call map_geax2geaw_mct( cdata_gex, x2chem_chemx2,cdata_gege, x2chem_chemchem2)   
            call t_drvstopf  ('driver_ca2ge_geax2geaa')
   endif

   if (iamin_GEAID) then
      call t_adj_detailf(+2)
      if (drv_threading) call seq_comm_setnthreads(nthreads_GEAID)
      call seq_infodata_putData(infodata,geatm_phase=2)
      call geatm_init_mct( EClock_ge, cdata_gege, &
                           x2chem_chemchem1, x2chem_chemchem2,chem2x_chemchem ) 
      if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
      call t_adj_detailf(-2)
   endif

   call t_stopf  ('driver_init_atminit')

   !-----------------------------------------------------------------------------
   ! Read driver restart file, overwrite anything previously sent or
   ! computed
   !-----------------------------------------------------------------------------

   call t_startf('driver_init_readrestart')
   call seq_diag_zero_mct(mode='all')
   if (read_restart) call seq_rest_read(rest_file)
   call t_stopf  ('driver_init_readrestart')

   if (do_histinit) then
      if (iamin_CPLID) then
         if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
         if (iamroot_CPLID) then
            call seq_timemgr_EClockGetData( EClock_d, curr_ymd=ymd,curr_tod=tod )
            write(logunit,104) ' Write history file at ',ymd,tod
            call shr_sys_flush(logunit)
         endif
         if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
      endif
   endif

   if ( iamroot_CPLID )then
      write(logunit,*) ' '
      write(logunit,F00) 'Model initialization complete '
      write(logunit,*) ' '
      call shr_sys_flush(logunit)
   endif

   call t_stopf  ('DRIVER_INIT')

end subroutine ccsm_init
