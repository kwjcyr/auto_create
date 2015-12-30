module ocn_comp_mct


!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!BOP
! !MODULE: ocn_comp_mct
! !INTERFACE:

! !DESCRIPTION:
!  This is the main driver for the Parallel Ocean Program (POP).
!
! !REVISION HISTORY:
!  SVN:$Id:
!
! !USES:
   use POP_KindsMod
   use POP_ErrorMod
   use POP_CommMod
   use POP_FieldMod
   use POP_GridHorzMod
   use POP_HaloMod
   use POP_IOUnitsMod
   use POP_MCT_vars_mod

   use mct_mod
   use esmf_mod
   use seq_flds_mod
   use seq_cdata_mod
   use seq_infodata_mod
   use seq_timemgr_mod
   use shr_file_mod 
   use shr_cal_mod, only : shr_cal_date2ymd
   use shr_sys_mod
   use perf_mod
   use ocn_communicator,  only: mpi_communicator_ocn

   use kinds_mod,         only: int_kind, r8
   use POP_CplIndices
   use POP_KindsMod
   use POP_ErrorMod
   use POP_InitMod,       only: POP_Initialize1, POP_Initialize2, &
                                timer_total, cpl_ts 
   use communicate,       only: my_task, master_task
   use constants
   use blocks
   use domain,            only: distrb_clinic, POP_haloClinic
   use exit_mod
   use forcing_shf,       only: SHF_QSW
   use forcing_sfwf,      only: lsend_precip_fact, precip_fact
   use forcing_fields
   use forcing_coupled,   only: ncouple_per_day,  &
                                update_ghost_cells_coupler_fluxes, &
                                rotate_wind_stress,
pop_set_coupled_forcing, &
                                pop_init_coupled,  &
                                orb_eccen, orb_obliqr, orb_lambm0,
orb_mvelpp
   use ice,               only: tfreez, tmelt, liceform,QFLUX, QICE,
AQICE, &
                                tlast_ice
   use grid,              only: TLAT, TLON, KMT
   use global_reductions, only: global_sum_prod
   use io_tools,          only: document
   use named_field_mod,   only: named_field_register,
named_field_get_index, &
                                named_field_set, named_field_get
   use prognostic
   use timers,            only: get_timer, timer_start, timer_stop
   use diagnostics,       only: check_KE
   use output,            only: output_driver
   use step_mod,          only: step
   use time_management
   use registry
!
! !PUBLIC MEMBER FUNCTIONS:
  implicit none

!--------------------------------------------------------------------------
! Public interfaces
!--------------------------------------------------------------------------

  public :: ocn_init_mct
  public :: ocn_run_mct
  public :: ocn_final_mct

!--------------------------------------------------------------------------
! Private interfaces
!--------------------------------------------------------------------------

  private :: ocn_SetgsMap_mct
  private :: ocn_import_mct
  private :: ocn_export_mct
  private :: ocn_domain_mct

!
!================================================================================
CONTAINS
!================================================================================

  subroutine ocn_init_mct( EClock, cdata_o, x2o_o, o2x_o,
NLFilename )

    !-----------------------------------------------------------------------
    !
    ! Arguments
    !
    type(ESMF_Clock),intent(in)                 :: EClock
    type(seq_cdata), intent(inout)              :: cdata_o
    type(mct_aVect), intent(inout)              :: x2o_o
    type(mct_aVect), intent(inout)              :: o2x_o   
    character(len=*), optional,   intent(IN)    :: NLFilename ! Namelist
filename
    !
    ! Locals
    !
    type(mct_gsMap), pointer   :: gsMap_ocn
    type(mct_gGrid), pointer   :: dom_o
    type(seq_infodata_type),pointer :: infodata
    integer :: lsize
        
    !--------------------------------------------------------------------------
    ! Determine attribute vector indices
    !--------------------------------------------------------------------------

    call {ccc0}_cpl_indices_set()
    call seq_infodata_GetData( infodata,  &

    call seq_timemgr_EClockGetData(EClock, &


