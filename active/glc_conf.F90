[start 1]

! !USES:

  use shr_sys_mod
  use shr_kind_mod     , only: IN=>SHR_KIND_IN, R8=>SHR_KIND_R8, &
                               CS=>SHR_KIND_CS, CL=>SHR_KIND_CL
  use shr_file_mod     , only: shr_file_getunit, shr_file_getlogunit,shr_file_getloglevel, &
                               shr_file_setlogunit,shr_file_setloglevel, shr_file_setio, &
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
  use glc_time_management, only: iyear,imonth,iday,ihour,iminute,isecond, runtype
  use glc_global_fields,   only: ice_sheet
  use glc_global_grid,     only: glc_grid, glc_landmask, glc_landfrac

! !PUBLIC TYPES:
  implicit none
  save
  private ! except
[end 1]
[start 4]
    integer(IN)              :: ierr        ! error code
    integer(IN)              :: i,j,n,nxg,nyg
    integer(IN)              :: COMPID
    integer(IN)              :: mpicom
    type(seq_infodata_type), pointer :: infodata   ! Input init object
    integer(IN)              :: shrlogunit, shrloglev  
    character(CL)            :: starttype

    !--- formats ---
    character(*), parameter :: F00   = "('(glc_init_mct) ',8a)"
    character(*), parameter :: F01   = "('(glc_init_mct) ',a,8i8)"
    character(*), parameter :: F02   = "('(glc_init_mct) ',a,4es13.6)"
    character(*), parameter :: F03   = "('(glc_init_mct) ',a,i8,a)"
    character(*), parameter :: F90   = "('(glc_init_mct) ',73('='))"
    character(*), parameter :: F91   = "('(glc_init_mct) ',73('-'))"
    character(*), parameter :: subName = "(glc_init_mct) "
!EOP
!-------------------------------------------------------------------------------
[end 4]
[start 5]
    !----------------------------------------------------------------------------
    ! Set cdata pointers
    !----------------------------------------------------------------------------

    call seq_cdata_setptrs(cdata, ID=COMPID, mpicom=mpicom, &
         gsMap=gsMap, dom=dom, infodata=infodata)

    call mpi_comm_rank(mpicom, my_task, ierr)

    !---------------------------------------------------------------------------
    ! use infodata to determine type of run
    !---------------------------------------------------------------------------

    call seq_infodata_GetData( infodata, start_type=starttype)
[end 5]
[start 6.1]
       runtype = "initial"
[end 6.1]
[start 6.2]
       runtype = "continue"
[end 6.2]
[start 6.3]
       runtype = "branch"
[end 6.3]
[start 7]
    !----------------------------------------------------------------------------
    ! Reset shr logging to my log file
    !----------------------------------------------------------------------------
    !--- open log file ---
    if (my_task == master_task) then
       stdout = shr_file_getUnit()
       call shr_file_setIO('glc_modelio.nml',stdout)
    else
       stdout = 6
    endif
    stderr = stdout
    nml_in = shr_file_getUnit()

    call shr_file_getLogUnit (shrlogunit)
    call shr_file_getLogLevel(shrloglev)
    call shr_file_setLogUnit (stdout)

    errorCode = glc_Success
    if (verbose .and. my_task == master_task) then
       write(stdout,F00) ' Starting'
       call shr_sys_flush(stdout)
    endif
    call init_communicate(mpicom)
    call glc_initialize(errorCode)
    if (verbose .and. my_task == master_task) then
       write(stdout,F01) ' GLC Initial Date ',iyear,imonth,iday,ihour,iminute,isecond
       write(stdout,F01) ' Initialize Done', errorCode
       call shr_sys_flush(stdout)
    endif

    nxg = glc_grid%nx
    nyg = glc_grid%ny
    lsize = nxg*nyg
[end 7]
