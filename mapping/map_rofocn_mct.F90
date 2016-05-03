module map_rofocn_mct

!---------------------------------------------------------------------
!
! Purpose:
!
! Collect coupling routines for sequential coupling of ocn-rof.
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

  public :: map_ocn2rof_init_mct
  public :: map_rof2ocn_init_mct
  public :: map_ocn2rof_mct
  public :: map_rof2ocn_mct

!--------------------------------------------------------------------------
! Public data
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
! Private data
!--------------------------------------------------------------------------

  type(mct_rearr), private :: Re_ocn2rof
  type(mct_rearr), private :: Re_rof2ocn
  type(mct_sMatp), private :: sMatp_Fr2o
  type(mct_sMatp), private :: sMatp_Sr2o
  type(mct_sMatp), private :: sMatp_Fo2r
  type(mct_sMatp), private :: sMatp_So2r

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
  logical, private :: samegrid_mapr2o

!=======================================================================
   contains
!=======================================================================

  subroutine map_rof2ocn_init_mct( cdata_r, cdata_o)

    !--------------------------------------------------
    ! 
    ! Arguments
    !
    type(seq_cdata),intent(in) :: cdata_r
    type(seq_cdata),intent(in) :: cdata_o
    ! 
    ! Local variables
    !
    type(seq_infodata_type), pointer :: infodata
    type(mct_gsMap), pointer :: gsMap_r           ! rof gsMap
    type(mct_gsMap), pointer :: gsMap_o           ! ocn gsMap
    type(mct_gGrid), pointer :: dom_r             ! rof domain
    type(mct_gGrid), pointer :: dom_o             ! ocn domain
    integer                  :: mpicom            ! communicator spanning atm and lnd
    integer                  :: ka, km            ! indices
    integer                  :: lsize             ! size of attribute vector
    type(mct_aVect)          :: areasrc           ! atm areas from mapping file
    type(mct_aVect)          :: areadst           ! lnd areas set to atm areas
    character(*),parameter :: subName = '(map_rof2ocn_init_mct) '
    !--------------------------------------------------
    call seq_cdata_setptrs(cdata_r, gsMap=gsMap_r, dom=dom_r)
    call seq_cdata_setptrs(cdata_o, gsMap=gsMap_o, dom=dom_o)
    call seq_cdata_setptrs(cdata_r, mpicom=mpicom, infodata=infodata)
    call seq_infodata_GetData( infodata, samegrid_ro=samegrid_mapr2o)



    if (samegrid_mapr2o) then
       call mct_rearr_init(gsMap_r, gsMap_o, mpicom, Re_rof2ocn)
       
       ! --- copy rof area into ocn aream
 
       ka = mct_aVect_indexRA(areasrc   , "aream")
       km = mct_aVect_indexRa(dom_a%data, "aream" )
       areasrc%rAttr(ka,:) = dom_a%data%rAttr(km,:)
       call mct_rearr_rearrange(areasrc, areadst, Re_atm2lnd, VECTOR=usevector, &
          ALLTOALL=usealltoall)

    else

       call shr_mct_sMatPInitnc(sMatp_Fr2o,gsMap_r,gsMap_o,"seq_maps.rc", &
          "rof2ocnFmapname:","rof2ocnFmaptype:",mpicom, &
          areasrc=areasrc, areadst=areadst)

       call shr_mct_sMatPInitnc(sMatp_Sr2o,gsMap_r,gsMap_o,"seq_maps.rc", &
          "rof2ocnSmapname:","rof2ocnSmaptype:",mpicom)

    endif


  end subroutine map_rof2ocn_init_mct

!=======================================================================

  subroutine map_ocn2rof_init_mct( cdata_o, cdata_r )

    !--------------------------------------------------
    ! 
    ! Arguments
    !
    type(seq_cdata),intent(in) :: cdata_o
    type(seq_cdata),intent(in) :: cdata_r
    ! 
    ! Local variables
    !
    type(seq_infodata_type), pointer :: infodata
    type(mct_gsMap), pointer :: gsMap_r           ! rof gsMap
    type(mct_gsMap), pointer :: gsMap_o           ! ocn gsMap
    type(mct_gGrid), pointer :: dom_o             ! ocn domain
    integer                  :: kf,iam,ierr,lsize
    integer                  :: mpicom            ! communicator spanning rof and ocn
    character(*),parameter :: subName = '(map_lnd2atm_init_mct) '
    !--------------------------------------------------

    call seq_cdata_setptrs(cdata_o, gsMap=gsMap_o, dom=dom_o)
    call seq_cdata_setptrs(cdata_r, gsMap=gsMap_r)
    call seq_cdata_setptrs(cdata_r, mpicom=mpicom,infodata=infodata)
    call mpi_comm_rank(mpicom,iam,ierr)
    
    call seq_infodata_GetData( infodata, samegrid_ro=samegrid_mapr2o)

    ! Initialize ocn -> rof mapping or rearranger

    if (samegrid_mapr2o) then
       call mct_rearr_init(gsMap_o, gsMap_r, mpicom, Re_ocn2rof)
    else
       call shr_mct_sMatPInitnc(sMatp_Fo2r, gsMap_o, gsMap_r, "seq_maps.rc", &
            "ocn2rofFmapname:", "ocn2rofFmaptype:", mpicom)

       call shr_mct_sMatPInitnc(sMatp_So2r, gsMap_o, gsMap_r, "seq_maps.rc", &
            "ocn2rofSmapname:", "ocn2rofSmaptype:", mpicom)

    endif
  end subroutine map_ocn2rof_init_mct

!=======================================================================

  subroutine map_rof2ocn_mct( cdata_r, av_r, cdata_o, av_o, fluxlist, statelist )

    !--------------------------------------------------
    ! 
    ! Arguments
    !
    type(seq_cdata), intent(in)  :: cdata_r
    type(mct_aVect), intent(in)  :: av_r
    type(seq_cdata), intent(in)  :: cdata_o
    type(mct_aVect), intent(out) :: av_o
    character(len=*),intent(in), optional :: statelist
    character(len=*),intent(in), optional :: fluxlist

    if (samegrid_mapr2o) then
       if (present(fluxlist) .or. present(statelist)) then
          if (present(fluxlist)) then
              call mct_rearr_rearrange_fldlist(av_r, av_o, Re_rof2ocn, VECTOR=usevector, &
                  ALLTOALL=usealltoall, fldlist=fluxlist)
          endif
          if (present(statelist)) then
              call mct_rearr_rearrange_fldlist(av_r, av_o, Re_rof2ocn, VECTOR=usevector, &
                  ALLTOALL=usealltoall, fldlist=statelist)
          endif
       else
          call mct_rearr_rearrange(av_r, av_o, Re_rof2ocn, VECTOR=usevector, ALLTOALL=usealltoall)
       end if
    else
       if (present(fluxlist) .or. present(statelist)) then
          if (present(fluxlist)) then
            call seq_map_avNorm(av_r, av_o, sMatp_Fr2o, rList=fluxlist, donorm=.false.)
          end if
          if (present(statelist)) then
            call seq_map_avNorm(av_r, av_o, sMatp_Sr2o, rList=statelist, donorm=.false.)
          end if
       else
          call seq_map_avNorm(av_r, av_o, sMatp_Fr2o, donorm=.false.)
       endif
    endif
  end subroutine map_rof2ocn_mct

!=======================================================================

  subroutine map_ocn2rof_mct( cdata_o, av_o, cdata_r, av_r, &
                              fluxlist, statelist )

    !--------------------------------------------------
    ! 
    ! Arguments
    !
    type(seq_cdata) ,intent(in)  :: cdata_o
    type(mct_aVect) ,intent(in)  :: av_o
    type(seq_cdata) ,intent(in)  :: cdata_r
    type(mct_aVect) ,intent(out) :: av_r
    character(len=*),intent(in), optional :: fluxlist
    character(len=*),intent(in), optional :: statelist
    !
    ! Local
    !
    integer  :: i,j,kl,lsize,numats,ier
    type(mct_aVect)          :: av_a_f     ! temporary flux attribute vector
    type(mct_aVect)          :: av_a_s     ! temporary state attribute vector
    character(*),parameter :: subName = '(map_ocn2rof_mct) '
    if (samegrid_mapr2o) then
       if (present(fluxlist) .or. present(statelist)) then
          if (present(fluxlist)) then
             call mct_rearr_rearrange_fldlist(av_o, av_r, Re_ocn2rof, VECTOR=usevector, &
                  ALLTOALL=usealltoall, fldlist=fluxlist)
          endif
          if (present(statelist)) then
             call mct_rearr_rearrange_fldlist(av_o, av_r, Re_ocn2rof, VECTOR=usevector, &
                  ALLTOALL=usealltoall, fldlist=statelist)
          endif
       else
          call mct_rearr_rearrange(av_o, av_r, Re_ocn2rof, VECTOR=usevector, ALLTOALL=usealltoall)
       end if
    else

          if (present(fluxlist) .or. present(statelist)) then
             if (present (fluxlist)) then
                call seq_map_avNorm(av_o, av_r, sMatp_Fo2r, rList=fluxlist, donorm=.false.)
             end if
             if (present(statelist)) then
                call seq_map_avNorm(av_o, av_r, sMatp_So2r, rList=statelist, donorm=.false.)
             end if
          else
             ! --- default is flux mapping
             call seq_map_avNorm(av_o, av_r, sMatp_Fo2r, donorm=.false.)
          endif
    endif

  end subroutine map_ocn2rof_mct

end module map_rofocn_mct
