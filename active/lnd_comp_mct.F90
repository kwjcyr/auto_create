module lnd_comp_mct

!---------------------------------------------------------------------------
!BOP
!
! !MODULE: lnd_comp_mct
!
!  Interface of the active land model component of CESM the CLM (Community Land Model)
!  with the main CESM driver. This is a thin interface taking CESM driver information
!  in MCT (Model Coupling Toolkit) format and converting it to use by CLM.
!
! !DESCRIPTION:
!
! !USES:
  use shr_kind_mod     , only : r8 => shr_kind_r8
  use mct_mod          , only : mct_aVect

!
! !PUBLIC MEMBER FUNCTIONS:
  implicit none
  SAVE
  private                              ! By default make data private
!
! !PUBLIC MEMBER FUNCTIONS:
!

!--------------------------------------------------------------------------
! Public interfaces
!--------------------------------------------------------------------------

  public :: lnd_init_mct
  public :: lnd_run_mct
  public :: lnd_final_mct

!--------------------------------------------------------------------------
! Private interfaces
!--------------------------------------------------------------------------

  private :: lnd_SetgsMap_mct
  private :: lnd_import_mct
  private :: lnd_export_mct
  private :: lnd_domain_mct

!
!================================================================================
CONTAINS
!================================================================================

  subroutine lnd_init_mct( EClock, cdata_l, x2l_l, l2x_l,
NLFilename )

    !-----------------------------------------------------------------------
    !
    ! Arguments
    !
    type(ESMF_Clock),intent(in)                 :: EClock
    type(seq_cdata), intent(inout)              :: cdata_l
    type(mct_aVect), intent(inout)              :: x2l_l
    type(mct_aVect), intent(inout)              :: l2x_l   
    character(len=*), optional,   intent(IN)    :: NLFilename ! Namelist
filename
    !
    ! Locals
    !
    type(mct_gsMap), pointer   :: gsMap_lnd
    type(mct_gGrid), pointer   :: dom_l
    type(seq_infodata_type),pointer :: infodata
    integer :: lsize
        
    !--------------------------------------------------------------------------
    ! Determine attribute vector indices
    !--------------------------------------------------------------------------

    call {ccc0}_cpl_indices_set()
    call seq_infodata_GetData( infodata,  &

    call seq_timemgr_EClockGetData(EClock, &


