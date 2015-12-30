module ice_comp_mct

!---------------------------------------------------------------------------
!BOP
!
! !MODULE: ice_comp_mct
!
! !DESCRIPTION:
! CICE interface routine for the ccsm cpl7 mct system
!
! !USES:

  use shr_kind_mod, only : r8 => shr_kind_r8
  use shr_sys_mod,  only : shr_sys_abort, shr_sys_flush
! use shr_mem_mod,  only : shr_get_memusage, shr_init_memusage
  use shr_file_mod, only : shr_file_getlogunit, shr_file_getloglevel,  &
                   shr_file_setloglevel, shr_file_setlogunit
  use mct_mod
#ifdef USE_ESMF_LIB
  use esmf_mod
#else
  use esmf_mod, only: ESMF_clock
#endif

  use seq_flds_mod
  use seq_cdata_mod,   only : seq_cdata, seq_cdata_setptrs
  use seq_infodata_mod,only : seq_infodata_type, seq_infodata_getdata,
&
                      seq_infodata_putdata,
seq_infodata_start_type_cont, &
                      seq_infodata_start_type_brnch,
seq_infodata_start_type_start
  use seq_timemgr_mod, only : seq_timemgr_eclockgetdata, &
                              seq_timemgr_restartalarmison, &
                      seq_timemgr_eclockdateinsync, &
                              seq_timemgr_stopalarmison
  use perf_mod,        only : t_startf, t_stopf

  use ice_cpl_indices
  use ice_flux,        only : strairxt, strairyt, strocnxt, strocnyt,
&
                  alvdr, alidr, alvdf, alidf, Tref, Qref, Uref, &
                              flat, fsens, flwout, evap, fswabs, fhocn,
&
                              fswthru,     &
                      fresh, fsalt, zlvl, uatm, vatm, potT, Tair, Qa,  &
                      rhoa, swvdr, swvdf, swidr, swidf, flw, frain,    &
                      fsnow, uocn, vocn, sst, ss_tltx, ss_tlty, frzmlt,&
                      sss, tf, wind, fsw, init_flux_atm, init_flux_ocn,&
                              faero
  use ice_state,       only : vice, vsno, aice, trcr, filename_aero,
filename_iage, &
                              filename_volpn, filename_FY, filename_lvl,
&
                              tr_aero, tr_iage, tr_FY, tr_pond, tr_lvl
  use ice_domain_size, only : nx_global, ny_global, block_size_x,
block_size_y, max_blocks
  use ice_domain,      only : nblocks, blocks_ice, halo_info,
distrb_info, profile_barrier
  use ice_blocks,      only : block, get_block, nx_block, ny_block
  use ice_grid,        only : tlon, tlat, tarea, tmask, anglet, hm,
ocn_gridcell_frac, &
                      grid_type, t2ugrid_vector
  use ice_constants,   only : c0, c1, puny, tffresh, spval_dbl,
rad_to_deg, radius, &
                      field_loc_center, field_type_scalar,
field_type_vector, c100
  use ice_communicate, only : my_task, master_task, lprint_stats,
MPI_COMM_ICE
  use ice_calendar,    only : idate, mday, time, month, daycal, secday,
&
                      sec, dt, dt_dyn, xndt_dyn, calendar,      &
                              calendar_type, nextsw_cday,
days_per_year,&
                              get_daycal, leap_year_count
  use ice_orbital,     only : eccen, obliqr, lambm0, mvelpp
  use ice_timers
  use ice_probability, only : init_numIceCells, print_numIceCells,  &
                  write_numIceCells, accum_numIceCells2

  use ice_kinds_mod,   only : int_kind, dbl_kind, char_len_long,
log_kind
!  use ice_init
  use ice_boundary,    only : ice_HaloUpdate 
  use ice_scam,        only : scmlat, scmlon, single_column
  use ice_fileunits,   only : nu_diag
  use ice_dyn_evp,     only :  kdyn
  use ice_prescribed_mod
  use ice_step_mod
  use CICE_RunMod
  use ice_global_reductions
  use ice_broadcast



!--------------------------------------------------------------------------
! Public interfaces
!--------------------------------------------------------------------------

  public :: ice_init_mct
  public :: ice_run_mct
  public :: ice_final_mct

!--------------------------------------------------------------------------
! Private interfaces
!--------------------------------------------------------------------------

  private :: ice_SetgsMap_mct
  private :: ice_import_mct
  private :: ice_export_mct
  private :: ice_domain_mct

!
!================================================================================
CONTAINS
!================================================================================

  subroutine ice_init_mct( EClock, cdata_i, x2i_i, i2x_i,
NLFilename )

    !-----------------------------------------------------------------------
    !
    ! Arguments
    !
    type(ESMF_Clock),intent(in)                 :: EClock
    type(seq_cdata), intent(inout)              :: cdata_i
    type(mct_aVect), intent(inout)              :: x2i_i
    type(mct_aVect), intent(inout)              :: i2x_i   
    character(len=*), optional,   intent(IN)    :: NLFilename ! Namelist
filename
    !
    ! Locals
    !
    type(mct_gsMap), pointer   :: gsMap_ice
    type(mct_gGrid), pointer   :: dom_i
    type(seq_infodata_type),pointer :: infodata
    integer :: lsize
        
    !--------------------------------------------------------------------------
    ! Determine attribute vector indices
    !--------------------------------------------------------------------------

    call {ccc0}_cpl_indices_set()
    call seq_infodata_GetData( infodata,  &

    call seq_timemgr_EClockGetData(EClock, &


