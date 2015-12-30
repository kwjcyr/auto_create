module glc_comp_mct


! !USES:

  use shr_sys_mod
  use shr_kind_mod     , only: IN=>SHR_KIND_IN, R8=>SHR_KIND_R8, &
                               CS=>SHR_KIND_CS, CL=>SHR_KIND_CL
  use shr_file_mod     , only: shr_file_getunit, shr_file_getlogunit,
shr_file_getloglevel, &
                               shr_file_setlogunit,
shr_file_setloglevel, shr_file_setio, &
                               shr_file_freeunit
  use shr_mpi_mod      , only: shr_mpi_bcast
  use mct_mod
  use esmf_mod

  use seq_flds_mod
  use seq_cdata_mod
  use seq_infodata_mod
  use seq_timemgr_mod

  use glc_cpl_indices
  use glc_constants,   only : verbose, stdout, stderr, nml_in, &
                              radius,  radian, tkfrz,  glc_nec
  use glc_errormod,    only : glc_success
  use glc_InitMod,     only : glc_initialize
  use glc_RunMod,      only : glc_run
  use glc_FinalMod,    only : glc_final
  use glc_communicate, only : init_communicate
  use glc_io,          only : glc_io_write_restart, &
                              glc_io_write_history
  use glc_time_management, only:
iyear,imonth,iday,ihour,iminute,isecond, runtype
  use glc_global_fields,   only: ice_sheet
  use glc_global_grid,     only: glc_grid, glc_landmask, glc_landfrac

! !PUBLIC TYPES:
  implicit none
  save
  private ! except

!--------------------------------------------------------------------------
! Public interfaces
!--------------------------------------------------------------------------

  public :: glc_init_mct
  public :: glc_run_mct
  public :: glc_final_mct

!--------------------------------------------------------------------------
! Private interfaces
!--------------------------------------------------------------------------

  private :: glc_SetgsMap_mct
  private :: glc_import_mct
  private :: glc_export_mct
  private :: glc_domain_mct

!
!================================================================================
CONTAINS
!================================================================================

  subroutine glc_init_mct( EClock, cdata_g, x2g_g, g2x_g,
NLFilename )

    !-----------------------------------------------------------------------
    !
    ! Arguments
    !
    type(ESMF_Clock),intent(in)                 :: EClock
    type(seq_cdata), intent(inout)              :: cdata_g
    type(mct_aVect), intent(inout)              :: x2g_g
    type(mct_aVect), intent(inout)              :: g2x_g   
    character(len=*), optional,   intent(IN)    :: NLFilename ! Namelist
filename
    !
    ! Locals
    !
    type(mct_gsMap), pointer   :: gsMap_glc
    type(mct_gGrid), pointer   :: dom_g
    type(seq_infodata_type),pointer :: infodata
    integer :: lsize
        
    !--------------------------------------------------------------------------
    ! Determine attribute vector indices
    !--------------------------------------------------------------------------

    call {ccc0}_cpl_indices_set()
    call seq_infodata_GetData( infodata,  &

    call seq_timemgr_EClockGetData(EClock, &


