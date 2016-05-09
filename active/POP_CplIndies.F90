module ocn_CplIndices
  
  use seq_flds_mod
  use mct_mod

  implicit none

  SAVE
  public                               ! By default make data private

  ocn -> drv
!{list} key="attr_c2x"  
!  integer :: index_o2x_{attr_c2x}

  drv -> ocn
!{list} key="attr_x2c"  
!  integer :: index_x2o_{attr_x2c}


contains

  subroutine POP_CplIndicesSet( )

    type(mct_aVect) :: o2x      ! temporary
    type(mct_aVect) :: x2o      ! temporary

    ! Determine attribute vector indices

    ! create temporary attribute vectors
    call mct_aVect_init(x2o, rList=seq_flds_x2o_fields, lsize=1)
    call mct_aVect_init(o2x, rList=seq_flds_o2x_fields, lsize=1)

!{list} key="attr_x2c"  
!   index_o2x_{attr_c2x}  = mct_avect_indexra(o2x,'{attr_x2x}')

!{list} key="attr_x2c"  
!   index_x2o_{attr_x2c}  = mct_avect_indexra(x2o,'{attr_x2c}')

    call mct_aVect_clean(x2o)
    call mct_aVect_clean(o2x)

  end subroutine ocn_CplIndicesSet

end module ocn_CplIndices

