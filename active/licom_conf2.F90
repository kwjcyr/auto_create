[start before_mod]
#define LOGMSG()
[end before_mod]
[start use]
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
[end use]
[start before_init]
type(seq_infodata_type), pointer :: infodata
[end before_init]
[start init_define_attribute]
    integer (kind(1)) :: nThreads
    integer (kind(1)) :: OCNID,lsize
    real (r8) ::  precadj

    integer (kind(1)) :: iam,ierr
    character(len=32)  :: starttype
    integer :: i_temp, j_temp, i_comp, j_comp
   mpi_comm_ocn=0
   ISB = 0
   ISC = 0
   IST = 0
[end init_define_attribute]
[start getinfo]
  cdate = 0
  sec = 0
  ierr = 0
  info_time = 0
[end getinfo]
