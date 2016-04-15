!xmlinsert(list[before_mod])
module {ccc}_comp_mct
!xmlinsert(list[use])
  public :: {ccc}_init_mct
  public :: {ccc}_run_mct
  public :: {ccc}_final_mct
  SAVE
  private

  private :: {ccc}_export_mct
  private :: {ccc}_import_mct
  private :: {ccc}_SetGSMap_mct
  private :: {ccc}_domain_mctbefore_mod]
!xmlinsert(list[before_init])
CONTAINS
 subroutine {ccc}_init_mct( EClock, cdata_{c}, x2{c}_{c}, {c}2x_{c},NLFilename )
    type(ESMF_Clock),intent(in)                 :: EClock
    type(seq_cdata), intent(inout)              :: cdata_{c}
    type(mct_aVect), intent(inout)              :: x2{c}_{c}
    type(mct_aVect), intent(inout)              :: {c}2x_{c}
    character(len=*), optional,   intent(IN)    :: NLFilename

    type(mct_gsMap), pointer   :: gsMap_{ccc}
    type(mct_gGrid), pointer   :: dom_{c}
!xmlinsert(list[seq_infodata_type])
    call seq_cdata_setptrs(cdata_{c}, ID={CCC}ID, mpicom=mpi_comm_{ccc}, &
       gsMap=gsMap_{c}, dom=dom_{c}, infodata=infodata)
!xmlinsert(list[getinfo])
    call seq_infodata_GetData( infodata, info_debug=info_dbug)

