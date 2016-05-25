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

   type (seq_cdata) :: cdata_aa
   type (seq_cdata) :: cdata_ll
   type (seq_cdata) :: cdata_oo
   type (seq_cdata) :: cdata_ii
   type (seq_cdata) :: cdata_rr
   type (seq_cdata) :: cdata_gg
   type (seq_cdata) :: cdata_ss

   type (seq_cdata) :: cdata_ax
   type (seq_cdata) :: cdata_lx
   type (seq_cdata) :: cdata_ox
   type (seq_cdata) :: cdata_ix
   type (seq_cdata) :: cdata_rx
   type (seq_cdata) :: cdata_gx
   type (seq_cdata) :: cdata_sx

   !----------------------------------------------------------------------------
   ! domain info: coords, fractions, decomps, area correction factors
   !----------------------------------------------------------------------------

   !--- domain coords, area, mask  (MCT General Grids) --

   type(mct_gGrid) :: dom_aa
   type(mct_gGrid) :: dom_ll
   type(mct_gGrid) :: dom_oo
   type(mct_gGrid) :: dom_ii
   type(mct_gGrid) :: dom_rr
   type(mct_gGrid) :: dom_gg
   type(mct_gGrid) :: dom_ss

   type(mct_gGrid) :: dom_ax
   type(mct_gGrid) :: dom_lx
   type(mct_gGrid) :: dom_ox
   type(mct_gGrid) :: dom_ix
   type(mct_gGrid) :: dom_rx
   type(mct_gGrid) :: dom_gx
   type(mct_gGrid) :: dom_sx

   !--- domain fractions (only defined on cpl pes) ---

   type(mct_aVect) :: fractions_ax   
   type(mct_aVect) :: fractions_lx   
   type(mct_aVect) :: fractions_ix   
   type(mct_aVect) :: fractions_ox   
   type(mct_aVect) :: fractions_gx   

   !----------------------------------------------------------------------------
   ! State/flux field bundles (MCT attribute vectors)
   !----------------------------------------------------------------------------

   type(mct_aVect) :: x2a_aa
   type(mct_aVect) :: x2l_ll
   type(mct_aVect) :: x2o_oo
   type(mct_aVect) :: x2i_ii
   type(mct_aVect) :: x2g_gg
   type(mct_aVect) :: x2s_ss

   type(mct_aVect) :: a2x_aa
   type(mct_aVect) :: l2x_ll
   type(mct_aVect) :: o2x_oo
   type(mct_aVect) :: i2x_ii
   type(mct_aVect) :: r2x_rr
   type(mct_aVect) :: g2x_gg
   type(mct_aVect) :: s2x_ss

   type(mct_aVect) :: x2a_ax
   type(mct_aVect) :: x2l_lx
   type(mct_aVect) :: x2o_ox
   type(mct_aVect) :: x2i_ix
   type(mct_aVect) :: x2g_gx
   type(mct_aVect) :: x2s_sx

   type(mct_aVect) :: a2x_ax
   type(mct_aVect) :: l2x_lx
   type(mct_aVect) :: o2x_ox
   type(mct_aVect) :: i2x_ix
   type(mct_aVect) :: r2x_rx
   type(mct_aVect) :: g2x_gx
   type(mct_aVect) :: s2x_sx

   type(mct_aVect) :: a2x_lx
   type(mct_aVect) :: a2x_ix
   type(mct_aVect) :: a2x_ox
   type(mct_aVect) :: g2x_sx
   type(mct_aVect) :: i2x_ax
   type(mct_aVect) :: i2x_ox
   type(mct_aVect) :: l2x_ax
   type(mct_aVect) :: o2x_ax
   type(mct_aVect) :: o2x_ix
   type(mct_aVect) :: s2x_gx
   type(mct_aVect) :: r2x_ox

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
