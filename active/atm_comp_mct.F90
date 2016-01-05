module atm_comp_mct

  use pio              , only: file_desc_t, io_desc_t, var_desc_t,pio_double, pio_def_dim, &
                               pio_put_att, pio_enddef, pio_initdecomp,pio_read_darray, pio_freedecomp, &
                               pio_closefile, pio_write_darray,pio_def_var, pio_inq_varid, &
                               pio_noerr, pio_bcast_error,pio_internal_error, pio_seterrorhandling 
  use mct_mod
  use esmf_mod
  use seq_flds_mod
  use seq_cdata_mod
  use seq_infodata_mod
  use seq_timemgr_mod

  use shr_kind_mod     , only: r8 => shr_kind_r8, cl=>shr_kind_cl
  use shr_file_mod     , only: shr_file_getunit, shr_file_freeunit, &
                               shr_file_setLogUnit,shr_file_setLogLevel, &
                               shr_file_getLogUnit,shr_file_getLogLevel, &
                               shr_file_setIO
  use shr_sys_mod      , only: shr_sys_flush, shr_sys_abort

  use cam_cpl_indices
  use cam_comp
  use cam_control_mod  , only: nsrest, adiabatic, ideal_phys,aqua_planet, eccen, obliqr, lambm0, mvelpp
  use radiation        , only: radiation_get, radiation_do,radiation_nextsw_cday
  use phys_grid        , only: get_ncols_p, get_gcol_all_p, & 
                               ngcols, get_gcol_p, get_rlat_all_p, &
                               get_rlon_all_p, get_area_all_p
  use ppgrid           , only: pcols, begchunk, endchunk       
  use dyn_grid         , only: get_horiz_grid_dim_d
  use camsrfexch_types , only: cam_out_t, cam_in_t     
  use cam_restart      , only: get_restcase, get_restartdir
  use cam_history      , only: outfld, ctitle
  use abortutils       , only: endrun
  use fiGlenames        , only: interpret_filename_spec, caseid,brnch_retain_casename
#ifdef SPMD
  use spmd_utils       , only: spmdinit, masterproc, iam
  use mpishorthand     , only: mpicom
#else
  use spmd_utils       , only: spmdinit, masterproc, mpicom, iam
#endif
  use time_manager     , only: get_curr_calday, advance_timestep,get_curr_date, get_nstep, &
                               is_first_step, get_step_size,timemgr_init, timemgr_check_restart
  use ioFileMod             
  use perf_mod
  use cam_logfile      , only: iulog
  use co2_cycle        , only: c_i, co2_readFlux_ocn, co2_readFlux_fuel,co2_transport, &
                               co2_time_interp_ocn,co2_time_interp_fuel, data_flux_ocn, data_flux_fuel
  use physconst       ,  only: mwco2
  use runtime_opts     , only: read_namelist
  use phys_control     , only: cam_chempkg_is
!
! !PUBLIC TYPES:
  implicit none
  save
  private ! except

!--------------------------------------------------------------------------
! Public interfaces
!--------------------------------------------------------------------------

  public :: atm_init_mct
  public :: atm_run_mct
  public :: atm_final_mct

!--------------------------------------------------------------------------
! Private interfaces
!--------------------------------------------------------------------------

  private :: atm_SetgsMap_mct
  private :: atm_import_mct
  private :: atm_export_mct
  private :: atm_domain_mct
  private :: atm_read_srfrest_mct
  private :: atm_write_srfrest_mct

!--------------------------------------------------------------------------
! Private data
!--------------------------------------------------------------------------

  type(cam_in_t) , pointer :: cam_in(:)
  type(cam_out_t), pointer :: cam_out(:)

  type(mct_aVect)   :: a2x_a_SNAP
  type(mct_aVect)   :: a2x_a_SUM

  integer, parameter  :: nlen = 256     ! Length of character strings
  character(len=nlen) :: fname_srf_cam  ! surface restart filename
  character(len=nlen) :: pname_srf_cam  ! surface restart full pathname
!
! Filename specifier for restart surface file
! (%c = caseid, $y = year, $m = month, $d = day, $s = seconds in day, %t = tape number)
!
  character(len=*), parameter :: rsfilename_spec_cam = '%c.cam2.rs.%y-%m-%d-%s.nc' ! cam srf restarts
  integer :: nrg = -1   ! Logical unit number for cam srf restart dataset
!
! Time averaged counter for flux fields
!
  integer :: avg_count
!
! Time averaged flux fields
!  
  character(*), parameter :: a2x_avg_flds = "Faxa_rainc:Faxa_rainl:Faxa_snowc:Faxa_snowl"  
!
! Are all surface types present   
!
  logical :: lnd_present ! if true => land is present
  logical :: ocn_present ! if true => ocean is present

  integer  :: id_isop, id_c10h16
!
!================================================================================
CONTAINS
!================================================================================

  subroutine atm_init_mct( EClock, cdata_a, x2a_a, a2x_a,NLFilename )
    
    use constituents,  only: cnst_get_ind

    !-----------------------------------------------------------------------
    !
    ! Arguments
    !
    type(ESMF_Clock),intent(in)                 :: EClock
    type(seq_cdata), intent(inout)              :: cdata_a
    type(mct_aVect), intent(inout)              :: x2a_a
    type(mct_aVect), intent(inout)              :: a2x_a   
    character(len=*), optional,   intent(IN)    :: NLFilename ! Namelist filename
    !
    ! Locals
    !
    type(mct_gsMap), pointer   :: gsMap_atm
    type(mct_gGrid), pointer   :: dom_a
    type(seq_infodata_type),pointer :: infodata
    integer :: lsize
    integer :: ATMID
    integer :: mpicom_atm
    integer :: iradsw
    logical :: exists           ! true if file exists
    real(r8):: nextsw_cday      ! calendar of next atm shortwave
    integer :: stepno           ! time step          
    integer :: dtime_sync       ! integer timestep size
    integer :: currentymd       ! current year-month-day
    integer :: dtime            ! time step increment (sec)
    integer :: atm_cpl_dt       ! driver atm coupling time step 
    integer :: nstep            ! CAM nstep
    real(r8):: caldayp1         ! CAM calendar day for for next cam time step
    integer :: dtime_cam        ! Time-step increment (sec)
    integer :: ymd              ! CAM current date (YYYYMMDD)
    integer :: yr               ! CAM current year
    integer :: mon              ! CAM current month
    integer :: day              ! CAM current day
    integer :: tod              ! CAM current time of day (sec)
    integer :: start_ymd        ! Start date (YYYYMMDD)
    integer :: start_tod        ! Start time of day (sec)
    integer :: ref_ymd          ! Reference date (YYYYMMDD)
    integer :: ref_tod          ! Reference time of day (sec)
    integer :: stop_ymd         ! Stop date (YYYYMMDD)
    integer :: stop_tod         ! Stop time of day (sec)
    logical :: perpetual_run    ! If in perpetual mode or not
    integer :: perpetual_ymd    ! Perpetual date (YYYYMMDD)
    logical :: single_column
    real(r8):: scmlat,scmlon
    integer :: shrlogunit,shrloglev ! old values
    logical :: first_time = .true.
    character(len=SHR_KIND_CS) :: calendar  ! Calendar type
    character(len=SHR_KIND_CS) :: starttype ! infodata start type
    integer :: lbnum
    integer :: hdim1_d, hdim2_d ! dimensions of rectangular horizontal grid
                                ! data structure, If 1D data structure, then
                                ! hdim2_d == 1.
    !-----------------------------------------------------------------------
    !
    ! Determine cdata points
    !
#if (defined _MEMTRACE)
    if(masterproc) then
      lbnum=1
      call memmon_dump_fort('memmon.out','atm_init_mct:start::',lbnum)
    endif                      
#endif  
                     
    !--------------------------------------------------------------------------
    ! Determine attribute vector indices
    !--------------------------------------------------------------------------

    call atm_cpl_indices_set()
    
  ! Redirect share output to cam log
       
       call spmdinit(mpicom_atm)
       
       if (masterproc) then
          inquire(file='atm_modelio.nml',exist=exists)
          if (exists) then
             iulog = shr_file_getUnit()
             call shr_file_setIO('atm_modelio.nml',iulog)
          endif
          write(iulog,*) "CAM atmosphere model initialization"
       endif
       
       call shr_file_getLogUnit (shrlogunit)
       call shr_file_getLogLevel(shrloglev)
       call shr_file_setLogUnit (iulog)
       ! 
       ! Consistency check                              
       !
       if (co2_readFlux_ocn .and. index_x2a_Faxx_fco2_ocn /= 0) then
          write(iulog,*)'error co2_readFlux_ocn and index_x2a_Faxx_fco2_ocn cannot both be active'
          call shr_sys_abort()
       end if
       ! 
       ! Get data from infodata object
       !
       call seq_infodata_GetData( infodata, &
            case_name=caseid, case_desc=ctitle, &
            start_type=starttype, &
            atm_adiabatic=adiabatic, &
            atm_ideal_phys=ideal_phys, &
            aqua_planet=aqua_planet, &
            brnch_retain_casename=brnch_retain_casename, &
            single_column=single_column, scmlat=scmlat, scmlon=scmlon, &
            orb_eccen=eccen, orb_mvelpp=mvelpp, orb_lambm0=lambm0, orb_obliqr=obliqr, &
            lnd_present=lnd_present, ocn_present=ocn_present, & 
            perpetual=perpetual_run, perpetual_ymd=perpetual_ymd)
       !
       ! Get nsrest from startup type methods
       !

       if (     trim(starttype) == trim(seq_infodata_start_type_start)) then
          nsrest = 0
       else if (trim(starttype) == trim(seq_infodata_start_type_cont) ) then
          nsrest = 1
       else if (trim(starttype) == trim(seq_infodata_start_type_brnch)) then
          nsrest = 3
       else
          write(iulog,*) 'atm_comp_mct: ERROR: unknown starttype'
          call shr_sys_abort()
       end if
!
       ! Initialize time manager.
       !
       call seq_timemgr_EClockGetData(EClock, &
                                      start_ymd=start_ymd, start_tod=start_tod, &
                                      ref_ymd=ref_ymd, ref_tod=ref_tod, &
                                      stop_ymd=stop_ymd, stop_tod=stop_tod, &
                                      calendar=calendar )
       !
       ! Read namelist
       !
       call read_namelist(single_column_in=single_column, scmlat_in=scmlat, scmlon_in=scmlon)
       !
       ! Initialize cam time manager
       !
       if ( nsrest == 0 )then
          call timemgr_init( calendar_in=calendar, start_ymd=start_ymd, &
                             start_tod=start_tod, ref_ymd=ref_ymd,      &
                             ref_tod=ref_tod, stop_ymd=stop_ymd,        &
                             stop_tod=stop_tod,                         &
                             perpetual_run=perpetual_run,               &
                             perpetual_ymd=perpetual_ymd )
       end if
       !
       ! First phase of cam initialization 
       ! Initialize mpicom_atm, allocate cam_in and cam_out and determine 
       ! atm decomposition (needed to initialize gsmap) 
       ! for an initial run, cam_in and cam_out are allocated in cam_initial
       ! for a restart/branch run, cam_in and cam_out are allocated in restart 
       ! Set defaults then override with user-specified input and initialize
       ! time manager
       ! Note that the following arguments are needed to cam_init for
       ! timemgr_restart only
       !
       call cam_init( cam_out, cam_in, mpicom_atm, &
                      start_ymd, start_tod, ref_ymd, ref_tod, stop_ymd,stop_tod, &
                      perpetual_run, perpetual_ymd, calendar)
       !
       ! Check consistency of restart time information with input clock
       !
       if (nsrest /= 0) then
          dtime_cam = get_step_size()
          call timemgr_check_restart( calendar, start_ymd, start_tod, ref_ymd, &
                                      ref_tod, dtime_cam, perpetual_run, perpetual_ymd)
       end if
       !
       ! Initialize MCT gsMap, domain and attribute vectors
       !
       call atm_SetgsMap_mct( mpicom_atm, {CCC}ID, gsMap_atm )
       lsize = mct_gsMap_lsize(gsMap_atm, mpicom_atm)
       !
       ! Initialize MCT domain 
       !
       call atm_domain_mct( lsize, gsMap_atm, dom_a )
       !
       ! Initialize MCT attribute vectors
       !
       call mct_aVect_init(a2x_a, rList=seq_flds_a2x_fields, lsize=lsize)
       call mct_aVect_zero(a2x_a)
       
       call mct_aVect_init(x2a_a, rList=seq_flds_x2a_fields, lsize=lsize) 
       call mct_aVect_zero(x2a_a)
       
       call mct_aVect_init(a2x_a_SNAP, rList=a2x_avg_flds, lsize=lsize)
       call mct_aVect_zero(a2x_a_SNAP)
       
       call mct_aVect_init(a2x_a_SUM , rList=a2x_avg_flds, lsize=lsize)
       call mct_aVect_zero(a2x_a_SUM )
       !
       ! Initialize averaging counter
       !
       avg_count = 0
       !
       ! Create initial atm export state
       !
       call atm_export_mct( cam_out, a2x_a )
       !
       ! Set flag to specify that an extra albedo calculation is to be done
       ! (i.e. specify active)
       !
       call seq_infodata_PutData(infodata, atm_prognostic=.true.)
       call get_horiz_grid_dim_d(hdim1_d, hdim2_d)
       call seq_infodata_PutData(infodata, atm_nx=hdim1_d, atm_ny=hdim2_d)

       ! Set flag to indicate that CAM will provide carbon and dust deposition
       ! fluxes.
       ! This is now hardcoded to .true. since the ability of CICE to read these
       ! fluxes from a file has been removed.
       call seq_infodata_PutData(infodata, atm_aero=.true.)

       !
       ! Set time step of radiation computation as the current calday
       ! This will only be used on the first timestep of an initial run
       !
       if (nsrest == 0) then
          nextsw_cday = get_curr_calday()
          call seq_infodata_PutData( infodata, nextsw_cday=nextsw_cday )
       end if
       
       ! End redirection of share output to cam log
       
       call shr_file_setLogUnit (shrlogunit)
       call shr_file_setLogLevel(shrloglev)

       first_time = .false.

    else
       
       ! For initial run, run cam radiation/clouds and return
       ! For restart run, read restart x2a_a
       ! Note - a2x_a is computed upon the completion of the previous run -
       ! cam_run1 is called
       ! only for the purposes of finishing the flux averaged calculation to
       ! compute a2x_a
       ! Note - cam_run1 is called on restart only to have cam internal state
       ! consistent with the 
       ! a2x_a state sent to the coupler

       ! Redirect share output to cam log

       call shr_file_getLogUnit (shrlogunit)
       call shr_file_getLogLevel(shrloglev)
      call shr_file_setLogUnit (iulog)

       call seq_timemgr_EClockGetData(EClock,curr_ymd=CurrentYMD, StepNo=StepNo, dtime=DTime_Sync )
       if (StepNo == 0) then
          call atm_import_mct( x2a_a, cam_in )
          call cam_run1 ( cam_in, cam_out ) 
          call atm_export_mct( cam_out, a2x_a )
       else
          call atm_read_srfrest_mct( EClock, cdata_a, x2a_a, a2x_a )
          call atm_import_mct( x2a_a, cam_in )
          call cam_run1 ( cam_in, cam_out ) 
       end if

       ! Compute time of next radiation computation, like in run method for
       ! exact restart

! tcx was
!       nextsw_cday = radiation_nextsw_cday() 

       call seq_timemgr_EClockGetData(Eclock,dtime=atm_cpl_dt)
       dtime = get_step_size()          
       nstep = get_nstep()
       if (nstep < 1 .or. dtime < atm_cpl_dt) then
          nextsw_cday = radiation_nextsw_cday() 
       else if (dtime == atm_cpl_dt) then
          caldayp1 = get_curr_calday(offset=int(dtime))
          nextsw_cday = radiation_nextsw_cday() 
          if (caldayp1 /= nextsw_cday) nextsw_cday = -1._r8
       else
          call shr_sys_abort('dtime must be less than or equal to atm_cpl_dt')
       end if
       call seq_infodata_PutData( infodata, nextsw_cday=nextsw_cday ) 

       ! End redirection of share output to cam log
       
       call shr_file_setLogUnit (shrlogunit)
       call shr_file_setLogLevel(shrloglev)
       
    end if

#if (defined _MEMTRACE )
    if(masterproc) then
      lbnum=1
      call memmon_dump_fort('memmon.out','atm_init_mct:end::',lbnum)
      call memmon_reset_addr()
    endif
#endif

    call cnst_get_ind ( 'ISOP', id_isop, abort=.false.)
    call cnst_get_ind ( 'C10H16', id_c10h16, abort=.false.)

    call shr_sys_flush(iulog)

 end subroutine atm_init_mct

!================================================================================

  subroutine atm_run_mct( EClock, cdata_a, x2a_a, a2x_a)






















