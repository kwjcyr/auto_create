module mrg_x2i_mct

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

  public :: mrg_x2i_init_mct
  public :: mrg_x2i_run_mct
  public :: mrg_x2i_final_mct

!--------------------------------------------------------------------------
! Private data
!--------------------------------------------------------------------------

!===========================================================================================
contains
!===========================================================================================

  subroutine mrg_x2i_init_mct( cdata_i &
                                , a2x_i &
                                , o2x_i &
                                )

    type(seq_cdata) ,intent(in)     :: cdata_i
    type(mct_aVect), intent(inout)  :: a2x_i
    type(mct_aVect), intent(inout)  :: o2x_i
    type(mct_GsMap), pointer        :: GSMap_ice
    integer                         :: mpicom

    ! Set gsMap
    call seq_cdata_setptrs(cdata_i, gsMap=gsMap_ice, mpicom=mpicom)

    ! Initialize av for atm export state on lnd decomp

    call mct_aVect_init(a2x_i, rList=seq_flds_a2x_fields, &
         lsize=mct_gsMap_lsize(gsMap_ice, mpicom))
    call mct_aVect_zero(a2x_i)
    ! Initialize av for ocn export state on lnd decomp

    call mct_aVect_init(o2x_i, rList=seq_flds_o2x_fields, &
         lsize=mct_gsMap_lsize(gsMap_ice, mpicom))
    call mct_aVect_zero(o2x_i)

  end subroutine mrg_x2i_init_mct

!===========================================================================================

  subroutine mrg_x2i_run_mct( cdata_i &
                                , a2x_i & 
                                , o2x_i & 
                                , x2i_i )

    !----------------------------------------------------------------------- 
    !
    ! Arguments
    !
    type(seq_cdata), intent(in)     :: cdata_i
    type(mct_aVect), intent(inout)  :: a2x_i
    type(mct_aVect), intent(inout)  :: o2x_i
    type(mct_aVect), intent(inout)  :: x2i_i  ! output
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

    call mct_aVect_copy(aVin=a2x_i, aVout=x2i_i, vector=usevector)
    call mct_aVect_copy(aVin=o2x_i, aVout=x2i_i, vector=usevector)

  end subroutine mrg_x2i_run_mct
!
!===========================================================================================
!
  subroutine mrg_x2i_final_mct
    ! ******************
    ! Do nothing for now
    ! ******************
  end subroutine mrg_x2i_final_mct

end module mrg_x2i_mct
