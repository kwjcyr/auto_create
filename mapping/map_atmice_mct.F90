module map_atmice_mct

!---------------------------------------------------------------------
!
! Purpose:
!
! Collect coupling routines for sequential coupling of ice-atm.
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

  public :: map_ice2atm_init_mct
  public :: map_atm2ice_init_mct
  public :: map_ice2atm_mct
  public :: map_atm2ice_mct

!--------------------------------------------------------------------------
! Public data
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
! Private data
!--------------------------------------------------------------------------

  type(mct_rearr), private :: Re_ice2atm
  type(mct_rearr), private :: Re_atm2ice
  type(mct_sMatp), private :: sMatp_Fa2i
  type(mct_sMatp), private :: sMatp_Sa2i
  type(mct_sMatp), private :: sMatp_Fi2a
  type(mct_sMatp), private :: sMatp_Si2a

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
  logical, private :: samegrid_mapa2i

!=======================================================================
   contains
!=======================================================================

  subroutine map_atm2ice_init_mct( cdata_a, cdata_i)

    !--------------------------------------------------
    ! 
    ! Arguments
    !
    type(seq_cdata),intent(in) :: cdata_a
    type(seq_cdata),intent(in) :: cdata_i
    ! 
    ! Local variables
    !
    type(seq_infodata_type), pointer :: infodata
    type(mct_gsMap), pointer :: gsMap_a           ! atm gsMap
    type(mct_gsMap), pointer :: gsMap_i           ! ice gsMap
    integer                  :: mpicom            ! communicator spanning atm and lnd
    character(*),parameter :: subName = '(map_atm2ice_init_mct) '
    !--------------------------------------------------
    call seq_cdata_setptrs(cdata_a, gsMap=gsMap_a)
    call seq_cdata_setptrs(cdata_i, gsMap=gsMap_i)
    call seq_cdata_setptrs(cdata_a, mpicom=mpicom, infodata=infodata)
    call seq_infodata_GetData( infodata, samegrid_ai=samegrid_mapa2i)



    if (samegrid_mapa2i) then
       call mct_rearr_init(gsMap_a, gsMap_i, mpicom, Re_atm2ice)
       
       ! --- copy atm area into ice aream
 
       ka = mct_aVect_indexRA(areasrc   , "aream")
       km = mct_aVect_indexRa(dom_a%data, "aream" )
       areasrc%rAttr(ka,:) = dom_a%data%rAttr(km,:)
       call mct_rearr_rearrange(areasrc, areadst, Re_atm2lnd, VECTOR=usevector, &
          ALLTOALL=usealltoall)

    else

       call shr_mct_sMatPInitnc(sMatp_Fa2i,gsMap_a,gsMap_i,"seq_maps.rc", &
          "atm2iceFmapname:","atm2iceFmaptype:",mpicom, &
          areasrc=areasrc, areadst=areadst)

       call shr_mct_sMatPInitnc(sMatp_Sa2i,gsMap_a,gsMap_i,"seq_maps.rc", &
          "atm2iceSmapname:","atm2iceSmaptype:",mpicom)

    endif


  end subroutine map_atm2ice_init_mct

!=======================================================================

  subroutine map_ice2atm_init_mct( cdata_i, cdata_a )

    !--------------------------------------------------
    ! 
    ! Arguments
    !
    type(seq_cdata),intent(in) :: cdata_i
    type(seq_cdata),intent(in) :: cdata_a
    ! 
    ! Local variables
    !
    type(seq_infodata_type), pointer :: infodata
    type(mct_gsMap), pointer :: gsMap_a           ! atm gsMap
    type(mct_gsMap), pointer :: gsMap_i           ! ice gsMap
    type(mct_gGrid), pointer :: dom_i             ! ice domain
    integer                  :: kf,iam,ierr,lsize
    integer                  :: mpicom            ! communicator spanning atm and ice
    character(*),parameter :: subName = '(map_lnd2atm_init_mct) '
    !--------------------------------------------------

    call seq_cdata_setptrs(cdata_i, gsMap=gsMap_i, dom=dom_i)
    call seq_cdata_setptrs(cdata_a, gsMap=gsMap_a)
    call seq_cdata_setptrs(cdata_a, mpicom=mpicom,infodata=infodata)
    call mpi_comm_rank(mpicom,iam,ierr)
    
    call seq_infodata_GetData( infodata, samegrid_ai=samegrid_mapa2i)

    ! Initialize ice -> atm mapping or rearranger

    if (samegrid_mapa2i) then
       call mct_rearr_init(gsMap_i, gsMap_a, mpicom, Re_ice2atm)
    else
       call shr_mct_sMatPInitnc(sMatp_Fi2a, gsMap_i, gsMap_a, "seq_maps.rc", &
            "ice2atmFmapname:", "ice2atmFmaptype:", mpicom)

       call shr_mct_sMatPInitnc(sMatp_Si2a, gsMap_i, gsMap_a, "seq_maps.rc", &
            "ice2atmSmapname:", "ice2atmSmaptype:", mpicom)

    endif
  end subroutine map_ice2atm_init_mct

!=======================================================================

  subroutine map_atm2ice_mct( cdata_a, av_a, cdata_i, av_i, fluxlist, statelist )

    !--------------------------------------------------
    ! 
    ! Arguments
    !
    type(seq_cdata), intent(in)  :: cdata_a
    type(mct_aVect), intent(in)  :: av_a
    type(seq_cdata), intent(in)  :: cdata_i
    type(mct_aVect), intent(out) :: av_i
    character(len=*),intent(in), optional :: statelist
    character(len=*),intent(in), optional :: fluxlist

    if (samegrid_mapa2i) then
       if (present(fluxlist) .or. present(statelist)) then
          if (present(fluxlist)) then
              call mct_rearr_rearrange_fldlist(av_a, av_i, Re_atm2ice, VECTOR=usevector, &
                  ALLTOALL=usealltoall, fldlist=fluxlist)
          endif
          if (present(statelist)) then
              call mct_rearr_rearrange_fldlist(av_a, av_i, Re_atm2ice, VECTOR=usevector, &
                  ALLTOALL=usealltoall, fldlist=statelist)
          endif
       else
          call mct_rearr_rearrange(av_a, av_i, Re_atm2ice, VECTOR=usevector, ALLTOALL=usealltoall)
       end if
    else
       if (present(fluxlist) .or. present(statelist)) then
          if (present(fluxlist)) then
            call seq_map_avNorm(av_a, av_i, sMatp_Fa2i, rList=fluxlist, donorm=.false.)
          end if
          if (present(statelist)) then
            call seq_map_avNorm(av_a, av_i, sMatp_Sa2i, rList=statelist, donorm=.false.)
          end if
       else
          call seq_map_avNorm(av_a, av_i, sMatp_Fa2i, donorm=.false.)
       endif
    endif
  end subroutine map_atm2ice_mct

!=======================================================================

  subroutine map_ice2atm_mct( cdata_i, av_i, cdata_a, av_a, &
                              fractions_i, fractions_a, &
                              fluxlist, statelist )

    !--------------------------------------------------
    ! 
    ! Arguments
    !
    type(seq_cdata) ,intent(in)  :: cdata_i
    type(mct_aVect) ,intent(in)  :: av_i
    type(seq_cdata) ,intent(in)  :: cdata_a
    type(mct_aVect) ,intent(out) :: av_a
    type(mct_aVect) ,intent(in), optional :: fractions_i
    type(mct_aVect) ,intent(in), optional :: fractions_a
    character(len=*),intent(in), optional :: fluxlist
    character(len=*),intent(in), optional :: statelist
    !
    ! Local
    !
    integer  :: i,j,kl,lsize,numats,ier
    type(mct_aVect)          :: av_a_f     ! temporary flux attribute vector
    type(mct_aVect)          :: av_a_s     ! temporary state attribute vector
    character(*),parameter :: subName = '(map_ice2atm_mct) '
    if (samegrid_mapa2i) then
       if (present(fluxlist) .or. present(statelist)) then
          if (present(fluxlist)) then
             call mct_rearr_rearrange_fldlist(av_i, av_a, Re_ice2atm, VECTOR=usevector, &
                  ALLTOALL=usealltoall, fldlist=fluxlist)
          endif
          if (present(statelist)) then
             call mct_rearr_rearrange_fldlist(av_i, av_a, Re_ice2atm, VECTOR=usevector, &
                  ALLTOALL=usealltoall, fldlist=statelist)
          endif
       else
          call mct_rearr_rearrange(av_i, av_a, Re_ice2atm, VECTOR=usevector, ALLTOALL=usealltoall)
       end if
    else

        if (present(fractions_l)) then
          if (present(fluxlist) .or. present(statelist)) then
             if (present(fluxlist)) then
                call seq_map_avNorm(av_i, av_a, sMatp_Fi2a, fractions_i, 'lfrin', fractions_a, 'lfrin', rList=fluxlist)
             end if
             if (present(statelist)) then
                call seq_map_avNorm(av_i, av_a, sMatp_Si2a, fractions_i, 'lfrin', fractions_a, 'lfrin', rList=statelist)
             end if
          else
             call seq_map_avNorm(av_i, av_a, sMatp_Fi2a, fractions_i, 'lfrin', fractions_a, 'lfrin')
          endif
       else
          if (present(fluxlist) .or. present(statelist)) then
             if (present (fluxlist)) then
                call seq_map_avNorm(av_i, av_a, sMatp_Fi2a, rList=fluxlist, donorm=.false.)
             end if
             if (present(statelist)) then
                call seq_map_avNorm(av_i, av_a, sMatp_Si2a, rList=statelist, donorm=.false.)
             end if
          else
             ! --- default is flux mapping
             call seq_map_avNorm(av_i, av_a, sMatp_Fi2a, donorm=.false.)
          endif
       endif
    endif

  end subroutine map_ice2atm_mct

end module map_atmice_mct
