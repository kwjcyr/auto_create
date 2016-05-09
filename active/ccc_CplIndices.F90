module {ccc}_CplIndices
  
  use seq_flds_mod
  use mct_mod

  implicit none

  SAVE
  public                               ! By default make data private

  {ccc} -> drv
!{list} key="attr_c2x"  
!  integer :: index_{c}2x_{attr_c2x}

  drv -> {ccc}
!{list} key="attr_x2c"  
!  integer :: index_x2{c}_{attr_x2c}


contains

  subroutine {CCC}_CplIndicesSet( )

    type(mct_aVect) :: {c}2x      ! temporary
    type(mct_aVect) :: x2{c}      ! temporary

    ! Determine attribute vector indices

    ! create temporary attribute vectors
    call mct_aVect_init(x2{c}, rList=seq_flds_x2{c}_fields, lsize=1)
    call mct_aVect_init({c}2x, rList=seq_flds_{c}2x_fields, lsize=1)

!{list} key="attr_x2c"  
!   index_{c}2x_{attr_c2x}  = mct_avect_indexra({c}2x,'{attr_x2x}')

!{list} key="attr_x2c"  
!   index_x2{c}_{attr_x2c}  = mct_avect_indexra(x2{c},'{attr_x2c}')

    call mct_aVect_clean(x2{c})
    call mct_aVect_clean({c}2x)

  end subroutine {ccc}_CplIndicesSet

end module {ccc}_CplIndices

