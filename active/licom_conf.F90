[start 0]
#define LOGMSG()
[end 0]
[start 1]
   use mct_mod
   use esmf_mod
   use seq_flds_mod
   use seq_cdata_mod
   use seq_infodata_mod
   use seq_timemgr_mod
   use shr_file_mod
   use shr_cal_mod, only : shr_cal_date2ymd
   use shr_sys_mod
   use shr_const_mod,only:SHR_CONST_SPVAL !linpf 2012Jul26
   use perf_mod
   use fluxcpl
   use POP_CplIndices
   use shr_dmodel_mod

#include <def-undef.h>
use param_mod
use pconst_mod

use shr_msg_mod
use shr_sys_mod
use control_mod
use constant_mod, only : LATVAP
use shr_cal_mod,       only: shr_cal_date2ymd


#if ( defined SPMD ) || ( defined COUP)
use msg_mod, only: tag_1d,tag_2d,tag_3d,tag_4d,nproc,status,mpi_comm_ocn
#endif
use tracer_mod
use pmix_mod
use forc_mod
#ifdef USE_OCN_CARBON
use carbon_mod
use cforce_mod
#endif


  implicit none
#include <netcdf.inc>
[end 1]
[start 2]
  type(seq_infodata_type), pointer :: infodata
[end 2]
[start 4]

    integer (kind(1)) :: nThreads

    integer (kind(1)) :: OCNID

    real (r8) ::  precadj

    integer (kind(1)) :: iam,ierr
    character(len=32)  :: starttype          ! infodata start type

    integer :: i_temp, j_temp, i_comp, j_comp ! used in specifying the grid
number in each process

  !------------------------------------------------------
  ! ROUTINE BEGIN
  !------------------------------------------------------
  mpi_comm_ocn=0
!wangty bug
   ISB = 0
   ISC = 0
   IST = 0
!wangty
  ! lihuimin 2012.7.16
  ! OCNID, errcode
  call seq_cdata_setptrs(cdata_o, ID=OCNID, mpicom=mpi_comm_ocn, &
       gsMap=gsMap_o, dom=dom_o, infodata=infodata)

  ! five parameters initialized in msg_pass('init') in CPL6 coupled version
  ! get infodata from drv
  cdate = 0
  sec = 0
  ierr = 0
  info_time = 0
  call seq_infodata_GetData( infodata, info_debug=info_dbug)

  ! send initial state to drv
  call seq_infodata_PutData( infodata, ocn_nx=(imt_global-2), ocn_ny=jmt_global)
!  call seq_infodata_PutData( infodata, ocn_prognostic=.true.) !LPF 20120829
  ! lihuimin, 2012.7.25, not use ocnrof_p
  call seq_infodata_PutData(infodata, ocn_prognostic=.true.,
ocnrof_prognostic=.true., rof_present=.true.)
!open ocnrof. !LPF 20120829

  !-----------------------------
  ! namelist & logfile setup
  !-----------------------------
  ! TODO: redirect the log file


  mytid=0

! He - 2010-10-08 | NDay: Number of days since the first day when the simulation
! begins.
! lihuimin cancle , 2012.6.14
!     NDAY = 0



!
! mpicom_o <-> mpi_comm_ocn
! TODO
      if (mytid==0) write(6,*)"Begin mpi_comm_rank"
      call mpi_comm_rank (mpi_comm_ocn, mytid, ierr)
      if (mytid==0)  write(6,*)"End mpi_comm_rank"
      call mpi_comm_size (mpi_comm_ocn, nproc, ierr)
      write(6,*) "MYTID=",mytid,"Number of Processors is",nproc
[end 4]

