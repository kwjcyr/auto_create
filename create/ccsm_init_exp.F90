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
!<list>
!     call seq_comm_setnthreads(nthreads_{CCC}ID)
!     if (iamroot_GLOID) write(logunit,'(2A,2I4)') subname,'nthreads_{CCC}ID = ',nthreads_{CCC}ID,seq_comm_getnthreads()
!     if (iamroot_GLOID) write(logunit,*)
!</list>
      call seq_comm_setnthreads(nthreads_GLOID)
   endif

!{list}
!  call seq_cdata_init(cdata_{c}x, CPLID, dom_{c}x, gsMap_{c}x,infodata,'cdat{c}_{c}x' )

!{list}
!  call seq_cdata_init(cdata_{c}{c}, {CCC}ID, dom_{c}{c}, gsMap_{c}{c},infodata,'cdata_{c}{c}')

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


   call t_adj_detailf(+2)
!<list>
!  !-----------------------------------------------------------------------------
!  ! Initialization {ccc} component
!  !-----------------------------------------------------------------------------
!  if (iamin_CPL{CCC}ID) then
!     call seq_infodata_exchange(infodata,CPL{CCC}ID,'cpl2{ccc}_init')
!  endif
!  if (iamin_{CCC}ID .and. {ccc}_present) then
!     if (seq_comm_iamroot({CCC}ID)) write(logunit,F00) 'Initialize {ccc} component'
!     call shr_sys_flush(logunit)
!     if (drv_threading) call seq_comm_setnthreads(nthreads_{CCC}ID)
!     call {ccc}_init_mct({ccc_init_mct_attr}) 
!     if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
!  endif
!  if (iamin_CPL{CCC}ID) then
!     call seq_infodata_exchange(infodata,CPL{CCC}ID,'{ccc}2cpl_init')
!  endif
!
!</list>

   call t_adj_detailf(-2)

   call t_stopf  ('driver_init_comps')

   !-----------------------------------------------------------------------------
   ! Determine final settings for presence of land, ice and ocean and
   ! the prognostic flags
   !-----------------------------------------------------------------------------
!<list>
!  if (iamin_CPL{CCC}ID) then
!     call seq_infodata_exchange(infodata,CPL{CCC}ID,'cpl2{ccc}_init')
!  endif
!</list>
   if ( iamroot_CPLID) then
      write(logunit,F00) 'Determine final settings for presence of surface components'
      call shr_sys_flush(logunit)
   endif

   call seq_infodata_getData(infodata, &
!{list}
!       {ccc2}_present={ccc2}_present, &        
!{list}
!       {ccc2}_prognostic={ccc2}_prognostic, &        
        dead_comps=dead_comps, &
!{list}
!       {ccc2}_nx={ccc2}_nx, {ccc2}_ny={ccc2}_ny, &        
        cpl_cdf64=cdf64, &
        atm_aero=atm_aero )

!<list>
!  if ({ccc2}_prognostic .and. .not. {ccc2}_present) then
!     call shr_sys_abort('if prognostic {ccc2} & 
!       must also have {ccc2} present')
!  endif
!</list>
   

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
!{list}
!     write(logunit,F0L)'{ccc2} model present     = ',{ccc2}_present
!{list}
!     write(logunit,F0L)'{ccc2} model prognostic  = ',{ccc2}_prognostic
      write(logunit,F0L)'dead components       = ',dead_comps
      write(logunit,F0L)'domain_check          = ',domain_check
!{list}      
!     write(logunit,F0I)'{ccc2}_nx,atm_ny         = ',{ccc2}_nx,{ccc2}_ny
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

!<list>
!  if (iamin_{CCC}ID .and. {ccc2}_present) then
!     if (drv_threading) call seq_comm_setnthreads(nthreads_{CCC}ID)
!     k1 = mct_aVect_indexRa(cdata_{c}{c}%dom%data,"area"  ,perrWith='{c}{c} area ')
!     k2 = mct_aVect_indexRa(cdata_{c}{c}%dom%data,"aream" ,perrWith='{c}{c} aream')
!     k3 = mct_aVect_indexRa(cdata_{c}{c}%dom%data,"ascale",perrWith='{c}{c} ascale')
!     cdata_{c}{c}%dom%data%rAttr(k2,:) = cdata_{c}{c}%dom%data%rAttr(k1,:)
!     cdata_{c}{c}%dom%data%rAttr(k3,:) = 1.0_r8
!     if (drv_threading) call seq_comm_setnthreads(nthreads_GLOID)
!   endif
!</list>


   !-----------------------------------------------------------------------------
   ! Initialize driver rearrangers and AVs on driver
   ! Initialize cdata_*x data
   ! Zero out x2*_** in case it never gets used then it'll produce zeros
   ! in diags
   !-----------------------------------------------------------------------------
   call t_startf('driver_init_xxx2xxx')
!<list>
!   if (iamin_CPL{CCC}ID .and. {ccc}_present) then
!      call map_{ccc}2{ccc}_init_mct(cdata_{c}{c}, x2{c}_{c}{c}, {c}2x_{c}{c}, {CCC}ID, &
!                                cdata_{c}x, x2{c}_{c}x, {c}2x_{c}x, CPLID,CPL{CCC}ID)
!      call mct_avect_zero(x2{c}_{c}{c})
!      call map_{ccc}{c}2{ccc}x_mct(cdata_{c}{c}, x2{c}_{c}{c}, cdata_{c}x, x2{c}_{c}x)
!   endif
!</list>
   
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
                       
!<list>
!!   if ({ccc}_present) 
!               call mrg_x2{c}_init_mct( cdata_{c1}x 
!                                         ,{c3}2x_{c2}x{d}
!                                       {other})
!</list>

      !-----------------------------------------------------------------------------
      ! Initialize mapping
      ! Read aream into domains!
      !-----------------------------------------------------------------------------

      call t_startf('driver_init_maps')
!<list>
!    if ({segment1}) then
!        if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Initializing {ccc1}/{ccc2} mapping'
!!       call map_{ccc1}2{ccc2}_init_mct(cdata_{c1}x, cdata_{c2}x)
!!       call map_{ccc2}2{ccc1}_init_mct(cdata_{c2}x, cdata_{c1}x)
!     endif
!</list>
      call t_stopf  ('driver_init_maps')

      !-----------------------------------------------------------------------------
      ! Check domains if appropriate
      ! This must be done after the mappers are initialized since
      ! checking is done on each processor and not with a global gather
      !-----------------------------------------------------------------------------

      if (domain_check) then
         if ( seq_comm_iamroot(CPLID)) write(logunit,F00) 'Performing domain checking'
         call seq_domain_check_mct( 
!{list}
!                                    cdata_{c0}x, &
                                   )
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

!<list>
!  if (iamin_CPL{CCC}ID .and. {ccc}_present) then
!     call map_{ccc}x2{ccc}{c}_mct( cdata_{c}x, dom_{c}x%data, cdata_{c}{c},dom_{c}{c}%data)
!     if (iamin_{CCC}ID) then
!        call domain_areafactinit_mct(cdata_{c}{c},mdl2drv_{c}{c},drv2mdl_{c}{c},'areafact_{c}')
!        call mct_avect_vecmult({c}2x_{c}{c},mdl2drv_{c}{c},seq_flds_{c}2x_fluxes)
!     endif
!     call map_{ccc}{c}2{ccc}x_mct(cdata_{c}{c}, {c}2x_{c}{c}, cdata_{c}x, {c}2x_{c}x)
!  endif
!
!</list>

   !-----------------------------------------------------------------------------
   ! global sum diagnostics for IC data
   !-----------------------------------------------------------------------------
   if (iamin_CPLID .and. info_debug > 1) then
      if (drv_threading) call seq_comm_setnthreads(nthreads_CPLID)
!{list}
!     if ({ccc0}_present) call seq_diag_avect_mct(cdata_{c}x,{c}2x_{c}x,'recv {ccc0} IC')
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
