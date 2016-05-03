module map_snoglc_mct

!---------------------------------------------------------------------
!
! Purpose:
!
! Collect coupling routines for sequential coupling of glc-sno.
!       
!
! Author: R. Jacob, M. Vertenstein
! Revision History:
! 30Mar06 - P. Worley - added optional arguments to MCT_Rearrange call
! 13Apr06 - M. Vertenstein - cleaned up interfaces 
!
!---------------------------------------------------------------------
 
  use shr_kind_mod      ,only: R8 => SHR_KIND_R8, IN=>SHR_KIND_IN
  use shr_sys_mod
  use shr_const_mod
  use shr_mct_mod, only: shr_mct_sMatPInitnc
  use mct_mod

  use seq_comm_mct, only : logunit, loglevel
  use seq_cdata_mod
  use seq_flds_indices
  use seq_infodata_mod
  use seq_map_mod
  use m_die

  implicit none
  save
  private  ! except

!--------------------------------------------------------------------------
! Public interfaces
!--------------------------------------------------------------------------

  public :: map_glc2sno_init_mct
  public :: map_sno2glc_init_mct
  public :: map_glc2sno_mct
  public :: map_sno2glc_mct

!--------------------------------------------------------------------------
! Public data
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
! Private data
!--------------------------------------------------------------------------

  type(mct_rearr), private :: Re_glc2sno
  type(mct_rearr), private :: Re_sno2glc

#ifdef CPP_VECTOR
    logical :: usevector = .true.
#else
    logical :: usevector = .false.
#endif

#ifdef SYSUNICOS
    logical :: usealltoall = .true.
#else
    logical :: usealltoall = .false.
#endif
  logical, private :: samegrid_maps2g

!=======================================================================
   contains
!=======================================================================

  subroutine map_sno2glc_init_mct( cdata_s, cdata_g)

    !--------------------------------------------------
    ! 
    ! Arguments
    !
    type(seq_cdata),intent(in) :: cdata_s
    type(seq_cdata),intent(in) :: cdata_g
    ! 
    ! Local variables
    !
    integer                  :: snosize, glcsize  ! global grid sizes
    type(mct_gsMap), pointer :: gsMap_s           ! sno gsMap
    type(mct_gsMap), pointer :: gsMap_g           ! glc gsMap
    type(mct_gGrid), pointer :: dom_s             ! sno domain
    type(mct_gGrid), pointer :: dom_g             ! glc domain
    integer                  :: mpicom            ! communicator spanning atm and lnd
    integer                  :: ka, km            ! indices
    integer                  :: lsize             ! size of attribute vector
    type(mct_aVect)          :: areasrc           ! atm areas from mapping file
    type(mct_aVect)          :: areadst           ! lnd areas set to atm areas
    character(*),parameter :: subName = '(map_sno2glc_init_mct) '
    !--------------------------------------------------
    call seq_cdata_setptrs(cdata_s, gsMap=gsMap_s, dom=dom_s)
    call seq_cdata_setptrs(cdata_g, gsMap=gsMap_g, dom=dom_g)
    call seq_cdata_setptrs(cdata_s, mpicom=mpicom)

    snosize = mct_gsMap_gsize(gsMap_s) 
    glcsize = mct_gsMap_gsize(gsMap_g) 
    if (snosize /= glcsize) then 
      write(logunit,*) "(map_sno2glc_init_mct) sno and glc are different." 
      write(logunit,*) "(map_sno2glc_init_mct) Must be same size. Exiting." 
      call shr_sys_abort(subName // "different size") 
    endif 


       call mct_rearr_init(gsMap_s, gsMap_g, mpicom, Re_sno2glc)
       
       ! --- copy sno area into glc aream
 
       ka = mct_aVect_indexRA(areasrc   , "aream")
       km = mct_aVect_indexRa(dom_a%data, "aream" )
       areasrc%rAttr(ka,:) = dom_a%data%rAttr(km,:)
       call mct_rearr_rearrange(areasrc, areadst, Re_atm2lnd, VECTOR=usevector, &
          ALLTOALL=usealltoall)



  end subroutine map_sno2glc_init_mct

!=======================================================================

  subroutine map_glc2sno_init_mct( cdata_g, cdata_s )

    !--------------------------------------------------
    ! 
    ! Arguments
    !
    type(seq_cdata),intent(in) :: cdata_g
    type(seq_cdata),intent(in) :: cdata_s
    ! 
    ! Local variables
    !
    type(seq_infodata_type), pointer :: infodata
    type(mct_gsMap), pointer :: gsMap_s           ! sno gsMap
    type(mct_gsMap), pointer :: gsMap_g           ! glc gsMap
    type(mct_gGrid), pointer :: dom_g             ! glc domain
    integer                  :: kf,iam,ierr,lsize
    integer                  :: mpicom            ! communicator spanning sno and glc
    character(*),parameter :: subName = '(map_lnd2atm_init_mct) '
    !--------------------------------------------------

    call seq_cdata_setptrs(cdata_g, gsMap=gsMap_g, dom=dom_g)
    call seq_cdata_setptrs(cdata_s, gsMap=gsMap_s)
    call seq_cdata_setptrs(cdata_s, mpicom=mpicom,infodata=infodata)
    call mpi_comm_rank(mpicom,iam,ierr)
    
    call seq_infodata_GetData( infodata, samegrid_sg=samegrid_maps2g)

    ! Initialize glc -> sno mapping or rearranger

       call mct_rearr_init(gsMap_g, gsMap_s, mpicom, Re_glc2sno)
  end subroutine map_glc2sno_init_mct

!=======================================================================

  subroutine map_sno2glc_mct( cdata_s, av_s, cdata_g, av_g, fluxlist, statelist )

    !--------------------------------------------------
    ! 
    ! Arguments
    !
    type(seq_cdata), intent(in)  :: cdata_s
    type(mct_aVect), intent(in)  :: av_s
    type(seq_cdata), intent(in)  :: cdata_g
    type(mct_aVect), intent(out) :: av_g
    character(len=*),intent(in), optional :: statelist
    character(len=*),intent(in), optional :: fluxlist

       if (present(fluxlist) .or. present(statelist)) then
          if (present(fluxlist)) then
              call mct_rearr_rearrange_fldlist(av_s, av_g, Re_sno2glc, VECTOR=usevector, &
                  ALLTOALL=usealltoall, fldlist=fluxlist)
          endif
          if (present(statelist)) then
              call mct_rearr_rearrange_fldlist(av_s, av_g, Re_sno2glc, VECTOR=usevector, &
                  ALLTOALL=usealltoall, fldlist=statelist)
          endif
       else
          call mct_rearr_rearrange(av_s, av_g, Re_sno2glc, VECTOR=usevector, ALLTOALL=usealltoall)
       end if
  end subroutine map_sno2glc_mct

!=======================================================================

  subroutine map_glc2sno_mct( cdata_g, av_g, cdata_s, av_s, &
                              fluxlist, statelist )

    !--------------------------------------------------
    ! 
    ! Arguments
    !
    type(seq_cdata) ,intent(in)  :: cdata_g
    type(mct_aVect) ,intent(in)  :: av_g
    type(seq_cdata) ,intent(in)  :: cdata_s
    type(mct_aVect) ,intent(out) :: av_s
    character(len=*),intent(in), optional :: fluxlist
    character(len=*),intent(in), optional :: statelist
    !
    ! Local
    !
    integer  :: i,j,kl,lsize,numats,ier
    type(mct_aVect)          :: av_a_f     ! temporary flux attribute vector
    type(mct_aVect)          :: av_a_s     ! temporary state attribute vector
    character(*),parameter :: subName = '(map_glc2sno_mct) '
       if (present(fluxlist) .or. present(statelist)) then
          if (present(fluxlist)) then
             call mct_rearr_rearrange_fldlist(av_g, av_s, Re_glc2sno, VECTOR=usevector, &
                  ALLTOALL=usealltoall, fldlist=fluxlist)
          endif
          if (present(statelist)) then
             call mct_rearr_rearrange_fldlist(av_g, av_s, Re_glc2sno, VECTOR=usevector, &
                  ALLTOALL=usealltoall, fldlist=statelist)
          endif
       else
          call mct_rearr_rearrange(av_g, av_s, Re_glc2sno, VECTOR=usevector, ALLTOALL=usealltoall)
       end if

          if (present(fluxlist) .or. present(statelist)) then
             if (present (fluxlist)) then
                call seq_map_avNorm(av_g, av_s, sMatp_Fg2s, rList=fluxlist, donorm=.false.)
             end if
             if (present(statelist)) then
                call seq_map_avNorm(av_g, av_s, sMatp_Sg2s, rList=statelist, donorm=.false.)
             end if
          else
             ! --- default is flux mapping
             call seq_map_avNorm(av_g, av_s, sMatp_Fg2s, donorm=.false.)
          endif
    endif

  end subroutine map_glc2sno_mct

end module map_snoglc_mct
