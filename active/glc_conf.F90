[start 1]

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
[end 1]
