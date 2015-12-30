[start 1] 
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
[end 1]
