module map_atmocn_mct

!---------------------------------------------------------------------
!
! Purpose:
!
! Collect coupling routines for sequential coupling of ocn-atm.
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

  public :: map_ocn2atm_init_mct
  public :: map_atm2ocn_init_mct
  public :: map_ocn2atm_mct
  public :: map_atm2ocn_mct

!--------------------------------------------------------------------------
! Public data
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
! Private data
!--------------------------------------------------------------------------

  type(mct_rearr), private :: Re_ocn2atm
  type(mct_rearr), private :: Re_atm2ocn
  type(mct_sMatp), private :: sMatp_Fa2o
  type(mct_sMatp), private :: sMatp_Sa2o
  type(mct_sMatp), private :: sMatp_Fo2a
  type(mct_sMatp), private :: sMatp_So2a

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
  logical, private :: samegrid_mapa2o

!=======================================================================
   contains
!=======================================================================

  subroutine map_atm2ocn_init_mct( cdata_a, cdata_o)

    !--------------------------------------------------
    ! 
    ! Arguments
    !
    type(seq_cdata),intent(in) :: cdata_a
    type(seq_cdata),intent(in) :: cdata_o
    ! 
    ! Local variables
    !
    type(seq_infodata_type), pointer :: infodata
    type(mct_gsMap), pointer :: gsMap_a           ! atm gsMap
    type(mct_gsMap), pointer :: gsMap_o           ! ocn gsMap
    type(mct_gGrid), pointer :: dom_a             ! atm domain
    type(mct_gGrid), pointer :: dom_o             ! ocn domain
    integer                  :: mpicom            ! communicator spanning atm and lnd
    integer                  :: ka, km            ! indices
    integer                  :: lsize             ! size of attribute vector
    type(mct_aVect)          :: areasrc           ! atm areas from mapping file
    type(mct_aVect)          :: areadst           ! lnd areas set to atm areas
    character(*),parameter :: subName = '(map_atm2ocn_init_mct) '
    !--------------------------------------------------
    call seq_cdata_setptrs(cdata_a, gsMap=gsMap_a, dom=dom_a)
    call seq_cdata_setptrs(cdata_o, gsMap=gsMap_o, dom=dom_o)
    call seq_cdata_setptrs(cdata_a, mpicom=mpicom, infodata=infodata)
    call seq_infodata_GetData( infodata, samegrid_ao=samegrid_mapa2o)



    if (samegrid_mapa2o) then
       call mct_rearr_init(gsMap_a, gsMap_o, mpicom, Re_atm2ocn)
       
       ! --- copy atm area into ocn aream
 
       ka = mct_aVect_indexRA(areasrc   , "aream")
       km = mct_aVect_indexRa(dom_a%data, "aream" )
       areasrc%rAttr(ka,:) = dom_a%data%rAttr(km,:)
       call mct_rearr_rearrange(areasrc, areadst, Re_atm2lnd, VECTOR=usevector, &
          ALLTOALL=usealltoall)

    else

       call shr_mct_sMatPInitnc(sMatp_Fa2o,gsMap_a,gsMap_o,"seq_maps.rc", &
          "atm2ocnFmapname:","atm2ocnFmaptype:",mpicom, &
          areasrc=areasrc, areadst=areadst)

       call shr_mct_sMatPInitnc(sMatp_Sa2o,gsMap_a,gsMap_o,"seq_maps.rc", &
          "atm2ocnSmapname:","atm2ocnSmaptype:",mpicom)

    endif


  end subroutine map_atm2ocn_init_mct

!=======================================================================

  subroutine map_ocn2atm_init_mct( cdata_o, cdata_a )

    !--------------------------------------------------
    ! 
    ! Arguments
    !
    type(seq_cdata),intent(in) :: cdata_o
    type(seq_cdata),intent(in) :: cdata_a
    ! 
    ! Local variables
    !
    type(seq_infodata_type), pointer :: infodata
    type(mct_gsMap), pointer :: gsMap_a           ! atm gsMap
    type(mct_gsMap), pointer :: gsMap_o           ! ocn gsMap
    type(mct_gGrid), pointer :: dom_o             ! ocn domain
    integer                  :: kf,iam,ierr,lsize
    integer                  :: mpicom            ! communicator spanning atm and ocn
    character(*),parameter :: subName = '(map_lnd2atm_init_mct) '
    !--------------------------------------------------

    call seq_cdata_setptrs(cdata_o, gsMap=gsMap_o, dom=dom_o)
    call seq_cdata_setptrs(cdata_a, gsMap=gsMap_a)
    call seq_cdata_setptrs(cdata_a, mpicom=mpicom,infodata=infodata)
    call mpi_comm_rank(mpicom,iam,ierr)
    
    call seq_infodata_GetData( infodata, samegrid_ao=samegrid_mapa2o)

    ! Initialize ocn -> atm mapping or rearranger

    if (samegrid_mapa2o) then
       call mct_rearr_init(gsMap_o, gsMap_a, mpicom, Re_ocn2atm)
    else
       call shr_mct_sMatPInitnc(sMatp_Fo2a, gsMap_o, gsMap_a, "seq_maps.rc", &
            "ocn2atmFmapname:", "ocn2atmFmaptype:", mpicom)

       call shr_mct_sMatPInitnc(sMatp_So2a, gsMap_o, gsMap_a, "seq_maps.rc", &
            "ocn2atmSmapname:", "ocn2atmSmaptype:", mpicom)

    endif
  end subroutine map_ocn2atm_init_mct

!=======================================================================

  subroutine map_atm2ocn_mct( cdata_a, av_a, cdata_o, av_o, fluxlist, statelist )

    !--------------------------------------------------
    ! 
    ! Arguments
    !
    type(seq_cdata), intent(in)  :: cdata_a
    type(mct_aVect), intent(in)  :: av_a
    type(seq_cdata), intent(in)  :: cdata_o
    type(mct_aVect), intent(out) :: av_o
    character(len=*),intent(in), optional :: statelist
    character(len=*),intent(in), optional :: fluxlist

    if (samegrid_mapa2o) then
       if (present(fluxlist) .or. present(statelist)) then
          if (present(fluxlist)) then
              call mct_rearr_rearrange_fldlist(av_a, av_o, Re_atm2ocn, VECTOR=usevector, &
                  ALLTOALL=usealltoall, fldlist=fluxlist)
          endif
          if (present(statelist)) then
              call mct_rearr_rearrange_fldlist(av_a, av_o, Re_atm2ocn, VECTOR=usevector, &
                  ALLTOALL=usealltoall, fldlist=statelist)
          endif
       else
          call mct_rearr_rearrange(av_a, av_o, Re_atm2ocn, VECTOR=usevector, ALLTOALL=usealltoall)
       end if
    else
       if (present(fluxlist) .or. present(statelist)) then
          if (present(fluxlist)) then
            call seq_map_avNorm(av_a, av_o, sMatp_Fa2o, rList=fluxlist, donorm=.false.)
          end if
          if (present(statelist)) then
            call seq_map_avNorm(av_a, av_o, sMatp_Sa2o, rList=statelist, donorm=.false.)
          end if
       else
          call seq_map_avNorm(av_a, av_o, sMatp_Fa2o, donorm=.false.)
       endif
    endif
  end subroutine map_atm2ocn_mct

!=======================================================================

  subroutine map_ocn2atm_mct( cdata_o, av_o, cdata_a, av_a, &
                              fractions_o, fractions_a, &
                              fluxlist, statelist )

    !--------------------------------------------------
    ! 
    ! Arguments
    !
    type(seq_cdata) ,intent(in)  :: cdata_o
    type(mct_aVect) ,intent(in)  :: av_o
    type(seq_cdata) ,intent(in)  :: cdata_a
    type(mct_aVect) ,intent(out) :: av_a
    type(mct_aVect) ,intent(in), optional :: fractions_o
    type(mct_aVect) ,intent(in), optional :: fractions_a
    character(len=*),intent(in), optional :: fluxlist
    character(len=*),intent(in), optional :: statelist
    !
    ! Local
    !
    integer  :: i,j,kl,lsize,numats,ier
    type(mct_aVect)          :: av_a_f     ! temporary flux attribute vector
    type(mct_aVect)          :: av_a_s     ! temporary state attribute vector
    character(*),parameter :: subName = '(map_ocn2atm_mct) '
    if (samegrid_mapa2o) then
       if (present(fluxlist) .or. present(statelist)) then
          if (present(fluxlist)) then
             call mct_rearr_rearrange_fldlist(av_o, av_a, Re_ocn2atm, VECTOR=usevector, &
                  ALLTOALL=usealltoall, fldlist=fluxlist)
          endif
          if (present(statelist)) then
             call mct_rearr_rearrange_fldlist(av_o, av_a, Re_ocn2atm, VECTOR=usevector, &
                  ALLTOALL=usealltoall, fldlist=statelist)
          endif
       else
          call mct_rearr_rearrange(av_o, av_a, Re_ocn2atm, VECTOR=usevector, ALLTOALL=usealltoall)
       end if
    else

        if (present(fractions_l)) then
          if (present(fluxlist) .or. present(statelist)) then
             if (present(fluxlist)) then
                call seq_map_avNorm(av_o, av_a, sMatp_Fo2a, fractions_o, 'lfrin', fractions_a, 'lfrin', rList=fluxlist)
             end if
             if (present(statelist)) then
                call seq_map_avNorm(av_o, av_a, sMatp_So2a, fractions_o, 'lfrin', fractions_a, 'lfrin', rList=statelist)
             end if
          else
             call seq_map_avNorm(av_o, av_a, sMatp_Fo2a, fractions_o, 'lfrin', fractions_a, 'lfrin')
          endif
       else
          if (present(fluxlist) .or. present(statelist)) then
             if (present (fluxlist)) then
                call seq_map_avNorm(av_o, av_a, sMatp_Fo2a, rList=fluxlist, donorm=.false.)
             end if
             if (present(statelist)) then
                call seq_map_avNorm(av_o, av_a, sMatp_So2a, rList=statelist, donorm=.false.)
             end if
          else
             ! --- default is flux mapping
             call seq_map_avNorm(av_o, av_a, sMatp_Fo2a, donorm=.false.)
          endif
       endif
    endif

  end subroutine map_ocn2atm_mct

end module map_atmocn_mct
