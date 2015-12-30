module {ccc}_comp_mct

!xmlinsert(list[1])

!--------------------------------------------------------------------------
! Public interfaces
!--------------------------------------------------------------------------

  public :: {ccc}_init_mct
  public :: {ccc}_run_mct
  public :: {ccc}_final_mct

!--------------------------------------------------------------------------
! Private interfaces
!--------------------------------------------------------------------------

  private :: {ccc}_SetgsMap_mct
  private :: {ccc}_import_mct
  private :: {ccc}_export_mct
  private :: {ccc}_domain_mct

!
!================================================================================
CONTAINS
!================================================================================

  subroutine {ccc}_init_mct( EClock, cdata_{c}, x2{c}_{c}, {c}2x_{c},
NLFilename )

    !-----------------------------------------------------------------------
    !
    ! Arguments
    !
    type(ESMF_Clock),intent(in)                 :: EClock
    type(seq_cdata), intent(inout)              :: cdata_{c}
    type(mct_aVect), intent(inout)              :: x2{c}_{c}
    type(mct_aVect), intent(inout)              :: {c}2x_{c}   
    character(len=*), optional,   intent(IN)    :: NLFilename ! Namelist
filename
    !
    ! Locals
    !
    type(mct_gsMap), pointer   :: gsMap_{ccc}
    type(mct_gGrid), pointer   :: dom_{c}
    type(seq_infodata_type),pointer :: infodata
    integer :: lsize
        
    !--------------------------------------------------------------------------
    ! Determine attribute vector indices
    !--------------------------------------------------------------------------

    call {ccc0}_cpl_indices_set()
    call seq_infodata_GetData( infodata,  &

    call seq_timemgr_EClockGetData(EClock, &


