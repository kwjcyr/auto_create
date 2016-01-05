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
!xmlinsert(list[2])
!
!================================================================================
CONTAINS
!================================================================================

  subroutine {ccc}_init_mct( EClock, cdata_{c}, x2{c}_{c}, {c}2x_{c},NLFilename )
!xmlinsert(list[3])
    !-----------------------------------------------------------------------
    !
    ! Arguments
    !
    type(ESMF_Clock),intent(in)                 :: EClock
    type(seq_cdata), intent(inout)              :: cdata_{c}
    type(mct_aVect), intent(inout)              :: x2{c}_{c}
    type(mct_aVect), intent(inout)              :: {c}2x_{c}   
    character(len=*), optional,   intent(IN)    :: NLFilename ! Namelist filename
    !
    ! Locals
    !
    type(mct_gsMap), pointer   :: gsMap_{ccc}
    type(mct_gGrid), pointer   :: dom_{c}
    type(seq_infodata_type),pointer :: infodata
    integer :: lsize
!xmlinsert(list[4])
    !--------------------------------------------------------------------------
    ! Determine attribute vector indices
    !--------------------------------------------------------------------------

    call {ccc}_cpl_indices_set()
    
!xmlinsert(list[5])

       if (     trim(starttype) == trim(seq_infodata_start_type_start)) then
!xmlinsert(list[6.1])
       else if (trim(starttype) == trim(seq_infodata_start_type_cont) ) then
!xmlinsert(list[6.2])       
       else if (trim(starttype) == trim(seq_infodata_start_type_brnch)) then
!xmlinsert(list[6.3])
       else
          write(iulog,*) '{ccc}_comp_mct: ERROR: unknown starttype'
          call shr_sys_abort()
       end if
!xmlinsert(list[7])
       !
       ! Initialize MCT gsMap, domain and attribute vectors
       !
       call {ccc}_SetgsMap_mct( mpicom_{ccc}, {CCC}ID, gsMap_{ccc} )
       lsize = mct_gsMap_lsize(gsMap_{ccc}, mpicom_{ccc})
       !
       ! Initialize MCT domain 
       !
       call atm_domain_mct( lsize, gsMap_atm, dom_a )
       !
       ! Initialize MCT attribute vectors
       !
       call mct_aVect_init(a2x_a, rList=seq_flds_a2x_fields, lsize=lsize)
       call mct_aVect_zero(a2x_a)
       
       call mct_aVect_init(x2a_a, rList=seq_flds_x2a_fields, lsize=lsize) 
       call mct_aVect_zero(x2a_a)
!xmlinsert(list[8])
       call shr_file_getLogUnit (shrlogunit)
       call shr_file_getLogLevel(shrloglev)
!xmlinsert(list[9])

 end subroutine {ccc}_init_mct

!================================================================================

  subroutine {ccc}_run_mct( EClock, cdata_{c}, x2{c}_{c}, {c}2x_{c})






















