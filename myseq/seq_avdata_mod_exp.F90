!===============================================================================
! SVN $Id: seq_avdata_mod.F90 18516 2009-09-25 22:54:10Z kauff $
! SVN $URL: https://svn-ccsm-models.cgd.ucar.edu/drv/seq_mct/branch_tags/t3148b_tags/t3148b02_drvseq3_1_48/driver/seq_avdata_mod.F90 $
!===============================================================================
!BOP ===========================================================================
!
! !MODULE: seq_avdata_mod -- provides use access to public cpl7 aVect, domain,
!     and fraction data.
!
! !DESCRIPTION:
!
!    provides use access to public cpl7 aVect, domain, and fraction data.
!
! !REMARKS:
!
!    use access to public cpl7 aVect, domain, and fraction info is to avoid 
!    excessively long routine arg lists, eg. for history & restart modules.
!    Note: while cpl7's non-main program ("driver") routines CAN access this
!    data by use'ing this module, they SHOULD access it via agrument lists 
!    if it is reasonable to do so.  Do the right thing.
!
! !REVISION HISTORY:
!     2009-Sep-25 - B. Kauffman - initial version
!
! !INTERFACE: ------------------------------------------------------------------

module seq_avdata_mod

! !USES:

   use shr_kind_mod  ,only: IN => SHR_KIND_IN
   use mct_mod           ! mct_ wrappers for mct lib

   use seq_cdata_mod     ! "cdata" type & methods (domain + decomp + infodata in one datatype)
   use seq_infodata_mod  ! "infodata" gathers various control flags into one datatype

   implicit none

   public  ! default is public

! !PUBLIC DATA MEMBERS:

   !----------------------------------------------------------------------------
   ! Infodata: inter-model control flags, domain info
   !----------------------------------------------------------------------------

   type (seq_infodata_type) :: infodata ! single instance for cpl and all comps

   !----------------------------------------------------------------------------
   ! cdata types: contains pointers to domain info + component ID + infobuffer
   !----------------------------------------------------------------------------

!{list} key="listc"
!  type (seq_cdata) :: cdata_{c}{c}

!{list} key="listc"
!  type (seq_cdata) :: cdata_{c}x

   !----------------------------------------------------------------------------
   ! domain info: coords, fractions, decomps, area correction factors
   !----------------------------------------------------------------------------

   !--- domain coords, area, mask  (MCT General Grids) --

!{list} key="listc"
!  type(mct_gGrid) :: dom_{c}{c}

!{list} key="listc"
!  type(mct_gGrid) :: dom_{c}x

   !--- domain fractions (only defined on cpl pes) ---

!{list} key="fractionc"
!  type(mct_aVect) :: fractions_{c}x   

   !----------------------------------------------------------------------------
   ! State/flux field bundles (MCT attribute vectors)
   !----------------------------------------------------------------------------

!{list} key="cimport"
!  type(mct_aVect) :: x2{c}_{c}{c}

!{list} key="listc"
!  type(mct_aVect) :: {c}2x_{c}{c}

!{list} key="cimport"
!  type(mct_aVect) :: x2{c}_{c}x

!{list} key="listc"
!  type(mct_aVect) :: {c}2x_{c}x

!{list} key="mergec"
!  type(mct_aVect) :: {c}2x_{co}x

   type(mct_aVect) :: xao_ox    ! Atm-ocn fluxes, ocn grid, cpl pes - defined in flux_ao gc 
   type(mct_aVect) :: xao_ax    ! Atm-ocn fluxes, atm grid, cpl pes - defined in flux_ao gc 
   type(mct_accum) :: r2xacc_rx ! Rof export, rof grid, cpl pes - defined in driver
   type(mct_accum) :: x2oacc_ox ! Ocn import, ocn grid, cpl pes - defined in driver

   integer(IN)     :: r2xacc_rx_cnt ! r2xacc_rx: number of time samples accumulated
   integer(IN)     :: x2oacc_ox_cnt ! x2oacc_ox: number of time samples accumulated

! !PUBLIC MEMBER FUNCTIONS

   ! no public routines

! !PUBLIC TYPES:

   ! no public types

end module seq_avdata_mod

!===============================================================================
