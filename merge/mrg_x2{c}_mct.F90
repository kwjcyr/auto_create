module mrg_x2{c}_mct

  use shr_kind_mod
  use mct_mod
  use seq_flds_mod
  use seq_flds_indices
  use seq_comm_mct
  use seq_cdata_mod

  implicit none
  save
  private ! except

!--------------------------------------------------------------------------
! Public interfaces
!--------------------------------------------------------------------------

  public :: mrg_x2{c}_init_mct
  public :: mrg_x2{c}_run_mct
  public :: mrg_x2{c}_final_mct

!--------------------------------------------------------------------------
! Private data
!--------------------------------------------------------------------------

!===========================================================================================
contains
!===========================================================================================

  subroutine mrg_x2{c}_init_mct( cdata_{c} &
!<list> key="cother"
                                , {co}2x_{c} &
!</list>
                                )

    type(seq_cdata) ,intent(in)     :: cdata_{c}
!<list> key="cother"
    type(mct_aVect), intent(inout)  :: {co}2x_{c}
!</list>
    type(mct_GsMap), pointer        :: GSMap_{ccc}
    integer                         :: mpicom

    ! Set gsMap
    call seq_cdata_setptrs(cdata_{c}, gsMap=gsMap_{ccc}, mpicom=mpicom)

!<list> key="cother"
    ! Initialize av for {ccco} export state on lnd decomp

    call mct_aVect_init({co}2x_{c}, rList=seq_flds_{co}2x_fields, &
         lsize=mct_gsMap_lsize(gsMap_{ccc}, mpicom))
    call mct_aVect_zero({co}2x_{c})
!</list>

  end subroutine mrg_x2{c}_init_mct

!===========================================================================================

  subroutine mrg_x2{c}_run_mct( cdata_{c} &
!<list> key="cother"
                                , {co}2x_{c} & 
!</list>
                                , x2{c}_{c} )

    !----------------------------------------------------------------------- 
    !
    ! Arguments
    !
    type(seq_cdata), intent(in)     :: cdata_{c}
!<list> key="cother"
    type(mct_aVect), intent(inout)  :: {co}2x_{c}
!</list>
    type(mct_aVect), intent(inout)  :: x2{c}_{c}  ! output
    !
    ! Local variables
    !
    logical :: usevector    ! use vector-friendly mct_copy
    !----------------------------------------------------------------------- 
    ! 
    ! Create input land state directly from atm output state
    !
#ifdef CPP_VECTOR
   usevector = .true.
#else
   usevector = .false.
#endif

!<list> key="cother"
    call mct_aVect_copy(aVin={co}2x_{c}, aVout=x2{c}_{c}, vector=usevector)
!</list>

  end subroutine mrg_x2{c}_run_mct
!
!===========================================================================================
!
  subroutine mrg_x2{c}_final_mct
    ! ******************
    ! Do nothing for now
    ! ******************
  end subroutine mrg_x2{c}_final_mct

end module mrg_x2{c}_mct
