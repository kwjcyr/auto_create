[start 1]

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
                                rotate_wind_stress,pop_set_coupled_forcing, &
                                pop_init_coupled,  &
                                orb_eccen, orb_obliqr, orb_lambm0,orb_mvelpp
   use ice,               only: tfreez, tmelt, liceform,QFLUX, QICE,AQICE, &
                                tlast_ice
   use grid,              only: TLAT, TLON, KMT
   use global_reductions, only: global_sum_prod
   use io_tools,          only: document
   use named_field_mod,   only: named_field_register,named_field_get_index, &
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
[end 1]
[start 2]
!
! !PRIVATE MODULE VARIABLES

  logical (log_kind) ::   &
       ldiag_cpl = .false.

  integer (int_kind), private ::   &
      cpl_write_restart,   &! flag id for write restart
      cpl_write_history,   &! flag id for write history
      cpl_write_tavg,      &! flag id for write tavg      
      cpl_diag_global,     &! flag id for computing diagnostics
      cpl_diag_transp       ! flag id for computing diagnostics

  real (r8),   &
      dimension(:,:,:,:), allocatable ::  &
      SBUFF_SUM           ! accumulated sum of send buffer quantities
                          ! for averaging before being sent
   real (r8) ::  &
     tlast_coupled

   integer (int_kind)  ::   &
      nsend, nrecv

   character(char_len) :: &
      runtype         

   type(seq_infodata_type), pointer :: &
      infodata   

[end 2]
[start 3]
!
! !DESCRIPTION:
! Initialize POP 
!
! !INPUT/OUTPUT PARAMETERS:
[end 3]
[start 4]
    integer(int_kind) ::  &
       OCNID,       &
       mpicom_o,    &
       start_ymd,   &
       start_tod,   &
       start_year,  &
       start_day,   &
       start_month, &
       start_hour,  &
       iyear,       &
       ocn_cpl_dt,  &
       pop_cpl_dt,  &
       shrlogunit,  &  ! old values
       shrloglev       ! old values


    integer (POP_i4) :: &
       errorCode         ! error code

    integer (int_kind) :: &
       nThreads

    real (r8) ::  &
       precadj

    integer (int_kind) :: iam,ierr 
    character(len=32)  :: starttype          ! infodata start type

#ifdef _OPENMP
   integer, external :: omp_get_max_threads  ! max number of threads that can execute
                                             ! concurrently in a single parallel region
#endif

     integer :: lbnum

!-----------------------------------------------------------------------
!
!  set cdata pointers
!
!-----------------------------------------------------------------------

   errorCode = POP_Success

#ifdef _OPENMP
   nThreads = omp_get_max_threads()
#endif
   call seq_cdata_setptrs(cdata_o, ID=OCNID, mpicom=mpicom_o, &
        gsMap=gsMap_o, dom=dom_o, infodata=infodata)

   POP_MCT_OCNID   =  OCNID
   POP_MCT_gsMap_o => gsMap_o
   POP_MCT_dom_o   => dom_o

#if (defined _MEMTRACE)
    call MPI_comm_rank(mpicom_o,iam,ierr)
    if(iam == 0) then
        lbnum=1
        call memmon_dump_fort('memmon.out','ocn_init_mct:start::',lbnum) 
    endif
#endif


   ! The following communicator module variable will be utilize in init_communicate that
   ! is called by initial - this is done to make the code backwards compatible

   mpi_communicator_ocn = mpicom_o

!-----------------------------------------------------------------------
!
!  initialize the model run 
!
!-----------------------------------------------------------------------

[end 4]
[start 5]
    call seq_infodata_GetData( infodata, case_name=runid )
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

   !TODO: check for consistency of pop runid and runtype with seq_infodata
   
!-----------------------------------------------------------------------
!
!  first initializaiton phase of pop2
!  initialize pop2 because grid information is needed for
!  creation of GSMap_ocn.
!  call pop initialization routines in two stages (needed for backwards
!  compatiblity with cpl6 concurrent system
!
!-----------------------------------------------------------------------

   call t_startf ('pop_init')
   call POP_Initialize1(errorCode)

!-----------------------------------------------------------------------
!
!  register non-standard incoming fields
!
!-----------------------------------------------------------------------

   if (index_x2o_Sa_co2prog > 0) then
      call named_field_register('ATM_CO2_PROG', ATM_CO2_PROG_nf_ind)
   endif
   if (index_x2o_Sa_co2diag > 0) then
      call named_field_register('ATM_CO2_DIAG', ATM_CO2_DIAG_nf_ind)
   endif
   call register_string('pop_init_coupled')
   call flushm (stdout)

!-----------------------------------------------------------------------
!
!  second initialization phase of pop2
!
!-----------------------------------------------------------------------

   call POP_Initialize2(errorCode)

!-----------------------------------------------------------------------
!
!  initialize time-stamp information
!
!-----------------------------------------------------------------------

   call ccsm_char_date_and_time

   call t_stopf ('pop_init')

!----------------------------------------------------------------------------
!
! reset shr logging to my log file
!
!----------------------------------------------------------------------------

    call shr_file_getLogUnit (shrlogunit)
    call shr_file_getLogLevel(shrloglev)
    call shr_file_setLogUnit (stdout)
   
!-----------------------------------------------------------------------
!
!  check for consistency of pop and sync clock initial time
!
!-----------------------------------------------------------------------

   if (runtype == 'initial') then
      call seq_timemgr_EClockGetData(EClock, &
           start_ymd=start_ymd, start_tod=start_tod)
      call shr_cal_date2ymd(start_ymd,start_year,start_month,start_day)

      if (iyear0 /= start_year) then
     if(master_task == my_task)   then
            call document ('ocn_init_mct', 'iyear0     ', iyear0)
            call document ('ocn_init_mct', 'start_year ', start_year)
         endif
         call exit_POP(sigAbort,' iyear0 does not match start_year')
      end if
      if (imonth0 /= start_month) then
     if(master_task == my_task)   then
            call document ('ocn_init_mct', 'imonth0     ', imonth0)
            call document ('ocn_init_mct', 'start_month ', start_month)
         endif
         call exit_POP(sigAbort,' imonth0 does not match start_year')
      end if
      if (iday0 /= start_day) then
     if(master_task == my_task)   then
            call document ('ocn_init_mct', 'iday0     ', iday0)
            call document ('ocn_init_mct', 'start_day ', start_day)
         endif
      end if
#ifndef _HIRES 
      if (seconds_this_day /= start_tod) then
         call document ('ocn_init_mct', 'sec0     ', seconds_this_day)
         call document ('ocn_init_mct', 'start_tod ', start_tod)
         call exit_POP(sigAbort,' sec0 does not start_tod')
      end if
#endif
   end if

!-----------------------------------------------------------------------
!
!  initialize MCT attribute vectors and indices
!
!-----------------------------------------------------------------------

   call t_startf ('pop_mct_init')

[end 7]
[start 8]
   
   nsend = mct_avect_nRattr(o2x_o)
   nrecv = mct_avect_nRattr(x2o_o)
   allocate (SBUFF_SUM(nx_block,ny_block,max_blocks_clinic,nsend))

!-----------------------------------------------------------------------
!
!  Initialize flags and shortwave absorption profile
!  Note that these cpl_write_xxx flags have no freqency options
!  set; therefore, they will retain a default value of .false.
!  unless they are explicitly set .true.  at the appropriate times
!
!-----------------------------------------------------------------------

   call init_time_flag('cpl_write_restart',cpl_write_restart, owner = 'ocn_init_mct')
   call init_time_flag('cpl_write_history',cpl_write_history, owner = 'ocn_init_mct')
   call init_time_flag('cpl_write_tavg'   ,cpl_write_tavg,    owner = 'ocn_init_mct')
   call init_time_flag('cpl_diag_global'  ,cpl_diag_global,   owner = 'ocn_init_mct')
   call init_time_flag('cpl_diag_transp'  ,cpl_diag_transp,   owner = 'ocn_init_mct')

   lsmft_avail = .true.
   tlast_coupled = c0

!-----------------------------------------------------------------------
!
!   initialize necessary  coupling info
!
!-----------------------------------------------------------------------

    call seq_timemgr_EClockGetData(EClock, dtime=ocn_cpl_dt)
    pop_cpl_dt = seconds_in_day / ncouple_per_day
    if (pop_cpl_dt /= ocn_cpl_dt) then
       write(stdout,*)'pop_cpl_dt= ',pop_cpl_dt, &
                     ' ocn_cpl_dt= ',ocn_cpl_dt   
       call exit_POP(sigAbort,'ERROR pop_cpl_dt and ocn_cpl_dt must be identical')
    end if

!-----------------------------------------------------------------------
!
!  send intial state to driver
!
!-----------------------------------------------------------------------

   if ( lsend_precip_fact )  then
      precadj = precip_fact * 1.0e6_r8  
      call seq_infodata_PutData( infodata, precip_fact=precadj)
   end if
   call pop_sum_buffer

   call ocn_export_mct(o2x_o, errorCode)  
   if (errorCode /= POP_Success) then
      call POP_ErrorPrint(errorCode)
      call exit_POP(sigAbort, 'ERROR in ocn_export_mct')
   endif

   call t_stopf ('pop_mct_init')

   call seq_infodata_PutData( infodata, &
        ocn_nx = nx_global , ocn_ny = ny_global)
   call seq_infodata_PutData( infodata, &
    ocn_prognostic=.true., ocnrof_prognostic=.true.)

!----------------------------------------------------------------------------
!
! Reset shr logging to original values
!
!----------------------------------------------------------------------------

[end 8]
[start 9]
#if (defined _MEMTRACE)
    if(iam  == 0) then
!        write(6,*) 'ocn_init_mct:end::'
        lbnum=1
        call memmon_dump_fort('memmon.out','ocn_init_mct:end::',lbnum) 
        call memmon_reset_addr()
    endif
#endif

!-----------------------------------------------------------------------
!
!  document orbital parameters
!
!-----------------------------------------------------------------------

   if (registry_match('qsw_distrb_iopt_cosz')) then
   call seq_infodata_GetData(infodata, &
      orb_eccen=orb_eccen, orb_mvelpp=orb_mvelpp, orb_lambm0=orb_lambm0, orb_obliqr=orb_obliqr)

     write(stdout,*) ' '
     call document ('ocn_import_mct', 'orb_eccen   ',  orb_eccen)
     call document ('ocn_import_mct', 'orb_mvelpp  ',  orb_mvelpp)
     call document ('ocn_import_mct', 'orb_lambm0  ',  orb_lambm0)
     call document ('ocn_import_mct', 'orb_obliqr  ',  orb_obliqr)
    endif

!-----------------------------------------------------------------------
!
!  Now document all time flags, because this is the last step of pop2 
!    initialization
!
!-----------------------------------------------------------------------

   call document_time_flags

!-----------------------------------------------------------------------
!
!  output delimiter to log file
!
!-----------------------------------------------------------------------

   if (my_task == master_task) then
      write(stdout,blank_fmt)
      write(stdout,'(" End of initialization")')
      write(stdout,blank_fmt)
      write(stdout,ndelim_fmt)
      call POP_IOUnitsFlush(POP_stdout)
#ifdef CCSMCOUPLED
      call POP_IOUnitsFlush(stdout)
#endif
   endif

!-----------------------------------------------------------------------
!EOC
[end 9]

