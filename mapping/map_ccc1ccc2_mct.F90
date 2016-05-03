module map_{ccc1}{ccc2}_mct

!---------------------------------------------------------------------
!
! Purpose:
!
! Collect coupling routines for sequential coupling of {ccc2}-{ccc1}.
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

  public :: map_{ccc2}2{ccc1}_init_mct
  public :: map_{ccc1}2{ccc2}_init_mct
  public :: map_{ccc2}2{ccc1}_mct
  public :: map_{ccc1}2{ccc2}_mct

!--------------------------------------------------------------------------
! Public data
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
! Private data
!--------------------------------------------------------------------------

  type(mct_rearr), private :: Re_{ccc2}2{ccc1}
  type(mct_rearr), private :: Re_{ccc1}2{ccc2}
!<list> key="infodata"
  type(mct_sMatp), private :: sMatp_F{c1}2{c2}
  type(mct_sMatp), private :: sMatp_S{c1}2{c2}
  type(mct_sMatp), private :: sMatp_F{c2}2{c1}
  type(mct_sMatp), private :: sMatp_S{c2}2{c1}
!</list>

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
  logical, private :: samegrid_map{c1}2{c2}

!=======================================================================
   contains
!=======================================================================

  subroutine map_{ccc1}2{ccc2}_init_mct( cdata_{c1}, cdata_{c2})

    !--------------------------------------------------
    ! 
    ! Arguments
    !
    type(seq_cdata),intent(in) :: cdata_{c1}
    type(seq_cdata),intent(in) :: cdata_{c2}
    ! 
    ! Local variables
    !
!{list} key="infodata"
!    type(seq_infodata_type), pointer :: infodata
!
!{list} key="global_grid_sizes"
!    integer                  :: {ccc1}size, {ccc2}size  ! global grid sizes
!
    type(mct_gsMap), pointer :: gsMap_{c1}           ! {ccc1} gsMap
    type(mct_gsMap), pointer :: gsMap_{c2}           ! {ccc2} gsMap
!{list} key="dom"
!    type(mct_gGrid), pointer :: dom_{c1}             ! {ccc1} domain
!
!{list} key="dom"
!    type(mct_gGrid), pointer :: dom_{c2}             ! {ccc2} domain
!
    integer                  :: mpicom            ! communicator spanning atm and lnd
!<list> key="areasrc_dst" 
    integer                  :: ka, km            ! indices
    integer                  :: lsize             ! size of attribute vector
    type(mct_aVect)          :: areasrc           ! atm areas from mapping file
    type(mct_aVect)          :: areadst           ! lnd areas set to atm areas
!</list>
    character(*),parameter :: subName = '(map_{ccc1}2{ccc2}_init_mct) '
    !--------------------------------------------------
!{list} key="dom"
!    call seq_cdata_setptrs(cdata_{c1}, gsMap=gsMap_{c1}, dom=dom_{c1})
!    call seq_cdata_setptrs(cdata_{c1}, gsMap=gsMap_{c1})
!{list} key="dom"
!    call seq_cdata_setptrs(cdata_{c2}, gsMap=gsMap_{c2}, dom=dom_{c2})
!    call seq_cdata_setptrs(cdata_{c2}, gsMap=gsMap_{c2})
!{list} key="infodata"
!    call seq_cdata_setptrs(cdata_{c1}, mpicom=mpicom, infodata=infodata)
!    call seq_cdata_setptrs(cdata_{c1}, mpicom=mpicom)
!{list} key="infodata"
!    call seq_infodata_GetData( infodata, samegrid_{c1}{c2}=samegrid_map{c1}2{c2})
!

!<list> key="without_samegrid"
    {ccc1}size = mct_gsMap_gsize(gsMap_{c1}) 
    {ccc2}size = mct_gsMap_gsize(gsMap_{c2}) 
    if ({ccc1}size /= {ccc2}size) then 
      write(logunit,*) "(map_{ccc1}2{ccc2}_init_mct) {ccc1} and {ccc2} are different." 
      write(logunit,*) "(map_{ccc1}2{ccc2}_init_mct) Must be same size. Exiting." 
      call shr_sys_abort(subName // "different size") 
    endif 
!</list>

!<list> key="before_samegrid" 
    lsize = mct_gsMap_lsize(gsMap_{c1}, mpicom)
    call mct_aVect_init( areasrc, rList="aream", lsize=lsize )      
    lsize = mct_gsMap_lsize(gsMap_{c2}, mpicom)
    call mct_aVect_init( areadst, rList="aream", lsize=lsize )
!</list>

!{list} key="samegrid"    
!    if (samegrid_map{c1}2{c2}) then
!       
       call mct_rearr_init(gsMap_{c1}, gsMap_{c2}, mpicom, Re_{ccc1}2{ccc2})
       
       ! --- copy {ccc1} area into {ccc2} aream
 
       ka = mct_aVect_indexRA(areasrc   , "aream")
       km = mct_aVect_indexRa(dom_a%data, "aream" )
       areasrc%rAttr(ka,:) = dom_a%data%rAttr(km,:)
       call mct_rearr_rearrange(areasrc, areadst, Re_atm2lnd, VECTOR=usevector, &
          ALLTOALL=usealltoall)

!<list> key="samegrid"
    else

       call shr_mct_sMatPInitnc(sMatp_F{c1}2{c2},gsMap_{c1},gsMap_{c2},"seq_maps.rc", &
          "{ccc1}2{ccc2}Fmapname:","{ccc1}2{ccc2}Fmaptype:",mpicom, &
          areasrc=areasrc, areadst=areadst)

       call shr_mct_sMatPInitnc(sMatp_S{c1}2{c2},gsMap_{c1},gsMap_{c2},"seq_maps.rc", &
          "{ccc1}2{ccc2}Smapname:","{ccc1}2{ccc2}Smaptype:",mpicom)

    endif
!</list>

!<list> key="before_samegrid" 
    ka = mct_aVect_indexRA(areadst   ,"aream")
    km = mct_aVect_indexRA(dom_l%data,"aream")
    dom_l%data%rAttr(km,:) = areadst%rAttr(ka,:)
    call mct_aVect_clean(areasrc)      
    call mct_aVect_clean(areadst)      
!</list>

  end subroutine map_{ccc1}2{ccc2}_init_mct

!=======================================================================

  subroutine map_{ccc2}2{ccc1}_init_mct( cdata_{c2}, cdata_{c1} )

    !--------------------------------------------------
    ! 
    ! Arguments
    !
    type(seq_cdata),intent(in) :: cdata_{c2}
    type(seq_cdata),intent(in) :: cdata_{c1}
    ! 
    ! Local variables
    !
    type(seq_infodata_type), pointer :: infodata
    type(mct_gsMap), pointer :: gsMap_{c1}           ! {ccc1} gsMap
    type(mct_gsMap), pointer :: gsMap_{c2}           ! {ccc2} gsMap
    type(mct_gGrid), pointer :: dom_{c2}             ! {ccc2} domain
    integer                  :: kf,iam,ierr,lsize
    integer                  :: mpicom            ! communicator spanning {ccc1} and {ccc2}
    character(*),parameter :: subName = '(map_lnd2atm_init_mct) '
    !--------------------------------------------------

    call seq_cdata_setptrs(cdata_{c2}, gsMap=gsMap_{c2}, dom=dom_{c2})
    call seq_cdata_setptrs(cdata_{c1}, gsMap=gsMap_{c1})
    call seq_cdata_setptrs(cdata_{c1}, mpicom=mpicom,infodata=infodata)
    call mpi_comm_rank(mpicom,iam,ierr)
    
    call seq_infodata_GetData( infodata, samegrid_{c1}{c2}=samegrid_map{c1}2{c2})

    ! Initialize {ccc2} -> {ccc1} mapping or rearranger

!{list} key="samegrid"
!    if (samegrid_map{c1}2{c2}) then

       call mct_rearr_init(gsMap_{c2}, gsMap_{c1}, mpicom, Re_{ccc2}2{ccc1})
!<list> key="samegrid"
    else
       call shr_mct_sMatPInitnc(sMatp_F{c2}2{c1}, gsMap_{c2}, gsMap_{c1}, "seq_maps.rc", &
            "{ccc2}2{ccc1}Fmapname:", "{ccc2}2{ccc1}Fmaptype:", mpicom)

       call shr_mct_sMatPInitnc(sMatp_S{c2}2{c1}, gsMap_{c2}, gsMap_{c1}, "seq_maps.rc", &
            "{ccc2}2{ccc1}Smapname:", "{ccc2}2{ccc1}Smaptype:", mpicom)

    endif
!</list>
  end subroutine map_{ccc2}2{ccc1}_init_mct

!=======================================================================

  subroutine map_{ccc1}2{ccc2}_mct( cdata_{c1}, av_{c1}, cdata_{c2}, av_{c2}, fluxlist, statelist )

    !--------------------------------------------------
    ! 
    ! Arguments
    !
    type(seq_cdata), intent(in)  :: cdata_{c1}
    type(mct_aVect), intent(in)  :: av_{c1}
    type(seq_cdata), intent(in)  :: cdata_{c2}
    type(mct_aVect), intent(out) :: av_{c2}
    character(len=*),intent(in), optional :: statelist
    character(len=*),intent(in), optional :: fluxlist

!<list> key="samegrid"
    if (samegrid_map{c1}2{c2}) then
!</list>
       if (present(fluxlist) .or. present(statelist)) then
          if (present(fluxlist)) then
              call mct_rearr_rearrange_fldlist(av_{c1}, av_{c2}, Re_{ccc1}2{ccc2}, VECTOR=usevector, &
                  ALLTOALL=usealltoall, fldlist=fluxlist)
          endif
          if (present(statelist)) then
              call mct_rearr_rearrange_fldlist(av_{c1}, av_{c2}, Re_{ccc1}2{ccc2}, VECTOR=usevector, &
                  ALLTOALL=usealltoall, fldlist=statelist)
          endif
       else
          call mct_rearr_rearrange(av_{c1}, av_{c2}, Re_{ccc1}2{ccc2}, VECTOR=usevector, ALLTOALL=usealltoall)
       end if
!<list> key="samegrid"
    else
       if (present(fluxlist) .or. present(statelist)) then
          if (present(fluxlist)) then
            call seq_map_avNorm(av_{c1}, av_{c2}, sMatp_F{c1}2{c2}, rList=fluxlist, donorm=.false.)
          end if
          if (present(statelist)) then
            call seq_map_avNorm(av_{c1}, av_{c2}, sMatp_S{c1}2{c2}, rList=statelist, donorm=.false.)
          end if
       else
          call seq_map_avNorm(av_{c1}, av_{c2}, sMatp_F{c1}2{c2}, donorm=.false.)
       endif
    endif
!</list>
  end subroutine map_{ccc1}2{ccc2}_mct

!=======================================================================

  subroutine map_{ccc2}2{ccc1}_mct( cdata_{c2}, av_{c2}, cdata_{c1}, av_{c1}, &
!{list} key="fraction"                              
!                              fractions_{c2}, fractions_{c1}, &
!
                              fluxlist, statelist )

    !--------------------------------------------------
    ! 
    ! Arguments
    !
    type(seq_cdata) ,intent(in)  :: cdata_{c2}
    type(mct_aVect) ,intent(in)  :: av_{c2}
    type(seq_cdata) ,intent(in)  :: cdata_{c1}
    type(mct_aVect) ,intent(out) :: av_{c1}
!<list> key="fraction"
    type(mct_aVect) ,intent(in), optional :: fractions_{c2}
    type(mct_aVect) ,intent(in), optional :: fractions_{c1}
!</list>    
    character(len=*),intent(in), optional :: fluxlist
    character(len=*),intent(in), optional :: statelist
    !
    ! Local
    !
    integer  :: i,j,kl,lsize,numats,ier
    type(mct_aVect)          :: av_a_f     ! temporary flux attribute vector
    type(mct_aVect)          :: av_a_s     ! temporary state attribute vector
    character(*),parameter :: subName = '(map_{ccc2}2{ccc1}_mct) '
!<list> key="samegrid"
    if (samegrid_map{c1}2{c2}) then
!</list>
       if (present(fluxlist) .or. present(statelist)) then
          if (present(fluxlist)) then
             call mct_rearr_rearrange_fldlist(av_{c2}, av_{c1}, Re_{ccc2}2{ccc1}, VECTOR=usevector, &
                  ALLTOALL=usealltoall, fldlist=fluxlist)
          endif
          if (present(statelist)) then
             call mct_rearr_rearrange_fldlist(av_{c2}, av_{c1}, Re_{ccc2}2{ccc1}, VECTOR=usevector, &
                  ALLTOALL=usealltoall, fldlist=statelist)
          endif
       else
          call mct_rearr_rearrange(av_{c2}, av_{c1}, Re_{ccc2}2{ccc1}, VECTOR=usevector, ALLTOALL=usealltoall)
       end if
!<list> key="samegrid"
    else
!</list>

!<list> key="fraction"
        if (present(fractions_l)) then
          if (present(fluxlist) .or. present(statelist)) then
             if (present(fluxlist)) then
                call seq_map_avNorm(av_{c2}, av_{c1}, sMatp_F{c2}2{c1}, fractions_{c2}, 'lfrin', fractions_{c1}, 'lfrin', rList=fluxlist)
             end if
             if (present(statelist)) then
                call seq_map_avNorm(av_{c2}, av_{c1}, sMatp_S{c2}2{c1}, fractions_{c2}, 'lfrin', fractions_{c1}, 'lfrin', rList=statelist)
             end if
          else
             call seq_map_avNorm(av_{c2}, av_{c1}, sMatp_F{c2}2{c1}, fractions_{c2}, 'lfrin', fractions_{c1}, 'lfrin')
          endif
       else
!</list>
          if (present(fluxlist) .or. present(statelist)) then
             if (present (fluxlist)) then
                call seq_map_avNorm(av_{c2}, av_{c1}, sMatp_F{c2}2{c1}, rList=fluxlist, donorm=.false.)
             end if
             if (present(statelist)) then
                call seq_map_avNorm(av_{c2}, av_{c1}, sMatp_S{c2}2{c1}, rList=statelist, donorm=.false.)
             end if
          else
             ! --- default is flux mapping
             call seq_map_avNorm(av_{c2}, av_{c1}, sMatp_F{c2}2{c1}, donorm=.false.)
          endif
!<list> key="fraction"
       endif
!</list>
    endif

  end subroutine map_{ccc2}2{ccc1}_mct

end module map_{ccc1}{ccc2}_mct
