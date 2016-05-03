
module ccsm_comp_mod

#define NEW_BUDGET
#define DEBUG

!-------------------------------------------------------------------------------
!
! Purpose: Main program for NCAR CCSM4/cpl7. Can have different
!          land, sea-ice, and ocean models plugged in at compile-time.
!          These models can be either: stub, dead, data, or active
!          components or some combination of the above.
!
!               stub -------- Do nothing.
!               dead -------- Send analytic data back.
!               data -------- Send data back interpolated from input files.
!               prognostic -- Prognostically simulate the given component.
!
! Method: Call appropriate initialization, run (time-stepping), and 
!         finalization routines.
! 
!-------------------------------------------------------------------------------

   !----------------------------------------------------------------------------
   ! share code & libs
   !----------------------------------------------------------------------------
   use shr_kind_mod,      only: r8 => SHR_KIND_R8 
   use shr_kind_mod,      only: cs => SHR_KIND_CS
   use shr_kind_mod,      only: cl => SHR_KIND_CL
   use shr_sys_mod,       only: shr_sys_abort, shr_sys_flush
   use shr_const_mod,     only: shr_const_cday
   use shr_file_mod,      only: shr_file_setLogLevel, shr_file_setLogUnit
   use shr_file_mod,      only: shr_file_setIO, shr_file_getUnit
   use shr_scam_mod,      only: shr_scam_checkSurface
   use shr_map_mod,       only: shr_map_setDopole
   use shr_mpi_mod,       only: shr_mpi_min, shr_mpi_max
   use shr_mem_mod,       only: shr_mem_init, shr_mem_getusage
   use shr_cal_mod,       only: shr_cal_date2ymd
   use shr_orb_mod,       only: shr_orb_params
   use mct_mod            ! mct_ wrappers for mct lib
   use perf_mod
   use ESMF_Mod

   !----------------------------------------------------------------------------
   ! component model interfaces (init, run, final methods)
   !----------------------------------------------------------------------------
    use atm_comp_mct, only: atm_init_mct, atm_run_mct, atm_final_mct   
    use wrf_comp_mct, only: wrf_init_mct, wrf_run_mct, wrf_final_mct   
    use geatm_comp_mct, only: geatm_init_mct, geatm_run_mct, geatm_final_mct   
    use lnd_comp_mct, only: lnd_init_mct, lnd_run_mct, lnd_final_mct   
    use ocn_comp_mct, only: ocn_init_mct, ocn_run_mct, ocn_final_mct   
    use ice_comp_mct, only: ice_init_mct, ice_run_mct, ice_final_mct   
    use glc_comp_mct, only: glc_init_mct, glc_run_mct, glc_final_mct   
#ifdef ESMF_INTERFACE
   use esmfshr_attribute_mod
    use atm_comp_mct, only: atm_register
    use lnd_comp_mct, only: lnd_register
    use ocn_comp_mct, only: ocn_register
    use ice_comp_mct, only: ice_register
    use glc_comp_mct, only: glc_register
#endif

   !----------------------------------------------------------------------------
   ! cpl7 modules
   !----------------------------------------------------------------------------

   !--- modules with public read/write data ---
   use seq_avdata_mod    ! drv aVects & associated domain, fraction, cdata
   use seq_diag_mct      ! diagnostic routines

   !--- other ---
   use seq_flds_indices  ! drv aVect field indicies, routines
   use seq_comm_mct      ! mpi comm data & routines, plus logunit and loglevel
   use seq_timemgr_mod   ! clock & alarm routines 
   use seq_infodata_mod  ! "infodata" gathers various control flags into one datatype
   use seq_cdata_mod     ! "cdata" type & methods (domain + decomp + infodata in one datatype)
   use seq_domain_mct    ! domain related routines
   use seq_flux_mct      ! flux calc routines
   use seq_frac_mct      ! domain fraction routines
   use seq_rest_mod      ! restart file routines
   use seq_hist_mod      ! history file routines
   use seq_io_mod        ! i/o subroutins

   !--- merging routines ---
    use mrg_x2w_mct      ! w merge gridded component
    use mrg_x2c_mct      ! c merge gridded component
    use mrg_x2ca_mct      ! ca merge gridded component
    use mrg_x2ge_mct      ! ge merge gridded component
    use mrg_x2a_mct      ! a merge gridded component
    use mrg_x2l_mct      ! l merge gridded component
    use mrg_x2o_mct      ! o merge gridded component
    use mrg_x2i_mct      ! i merge gridded component
    use mrg_x2g_mct      ! g merge gridded component
    use mrg_x2s_mct      ! s merge gridded component

   !--- mapping routines ---
    use map_camcam_mct
    use map_wrfwrf_mct
    use map_gcamcam_mct
    use map_geagea_mct
    use map_iceice_mct
    use map_lndlnd_mct
    use map_rofrof_mct
    use map_atmatm_mct
    use map_glcglc_mct
    use map_snosno_mct
    use map_ocnocn_mct

    use map_atmlnd_mct
    use map_atmice_mct
    use map_atmocn_mct
    use map_iceocn_mct
    use map_rofocn_mct
    use map_snoglc_mct
    use map_wrfcam_mct
    use map_geacam_mct
   implicit none

   private

   public ccsm_pre_init, ccsm_init, ccsm_run, ccsm_final
#ifdef ESMF_INTERFACE
   public ccsm_comp_register
#endif
   public timing_dir, mpicom_GLOID

#include <mpif.h>

   !----------------------------------------------------------------------------
   ! domains & related
   !----------------------------------------------------------------------------

   !--- domain decomps (MCT Global Seg Maps) ---
    type(mct_gsMap)  :: gsMap_aa
    type(mct_gsMap)  :: gsMap_ll
    type(mct_gsMap)  :: gsMap_oo
    type(mct_gsMap)  :: gsMap_ii
    type(mct_gsMap)  :: gsMap_rr
    type(mct_gsMap)  :: gsMap_gg
    type(mct_gsMap)  :: gsMap_ss
    type(mct_gsMap)  :: gsMap_ww
    type(mct_gsMap)  :: gsMap_cc
    type(mct_gsMap)  :: gsMap_mm
    type(mct_gsMap)  :: gsMap_gege
    type(mct_gsMap)  :: gsMap_caca

    type(mct_gsMap)  :: gsMap_ax
    type(mct_gsMap)  :: gsMap_lx
    type(mct_gsMap)  :: gsMap_ox
    type(mct_gsMap)  :: gsMap_ix
    type(mct_gsMap)  :: gsMap_rx
    type(mct_gsMap)  :: gsMap_gx
    type(mct_gsMap)  :: gsMap_sx
    type(mct_gsMap)  :: gsMap_wx
    type(mct_gsMap)  :: gsMap_cx
    type(mct_gsMap)  :: gsMap_mx
    type(mct_gsMap)  :: gsMap_gex
    type(mct_gsMap)  :: gsMap_cax

   !--- domain area correction factors (only defined on cpl pes) ---
    real(r8),pointer :: drv2mdl_aa(:), mdl2drv_aa(:)
    real(r8),pointer :: drv2mdl_ll(:), mdl2drv_ll(:)
    real(r8),pointer :: drv2mdl_ii(:), mdl2drv_ii(:)
    real(r8),pointer :: drv2mdl_oo(:), mdl2drv_oo(:)
    real(r8),pointer :: drv2mdl_rr(:), mdl2drv_rr(:)
    real(r8),pointer :: drv2mdl_gg(:), mdl2drv_gg(:)
    real(r8),pointer :: drv2mdl_ss(:), mdl2drv_ss(:)
    real(r8),pointer :: drv2mdl_ww(:), mdl2drv_ww(:)
    real(r8),pointer :: drv2mdl_ge(:), mdl2drv_ge(:)

   !--- domain equivalent 2d grid size ---
    integer          :: aa_nx, aa_ny
    integer          :: ll_nx, ll_ny
    integer          :: ii_nx, ii_ny
    integer          :: oo_nx, oo_ny
    integer          :: rr_nx, rr_ny
    integer          :: gg_nx, gg_ny
    integer          :: ss_nx, ss_ny
    integer          :: ww_nx, ww_ny
    integer          :: ge_nx, ge_ny

   !----------------------------------------------------------------------------
   ! time management
   !----------------------------------------------------------------------------

   type (seq_timemgr_type), SAVE :: seq_SyncClock ! array of all clocks & alarm
   type (ESMF_Clock), SAVE       :: EClock_d      ! driver clock
   type (ESMF_Clock), SAVE       :: EClock_a
   type (ESMF_Clock), SAVE       :: EClock_w
   type (ESMF_Clock), SAVE       :: EClock_ge
   type (ESMF_Clock), SAVE       :: EClock_l
   type (ESMF_Clock), SAVE       :: EClock_o
   type (ESMF_Clock), SAVE       :: EClock_i
   type (ESMF_Clock), SAVE       :: EClock_g

   logical  :: restart_alarm          ! restart alarm
   logical  :: history_alarm          ! history alarm
   logical  :: histavg_alarm          ! history alarm
   logical  :: stop_alarm             ! stop alarm
   logical  :: atmrun_alarm          ! atm run alarm
   logical  :: wrfrun_alarm          ! wrf run alarm
   logical  :: gearun_alarm          ! gea run alarm
   logical  :: lndrun_alarm          ! lnd run alarm
   logical  :: ocnrun_alarm          ! ocn run alarm
   logical  :: icerun_alarm          ! ice run alarm
   logical  :: glcrun_alarm          ! glc run alarm
   logical  :: ocnnext_alarm          ! ocn run alarm on next timestep
   logical  :: tprof_alarm            ! timing profile alarm
   logical  :: t1hr_alarm             ! alarm every hour
   logical  :: t2hr_alarm             ! alarm every two hours 
   logical  :: t3hr_alarm             ! alarm every three hours 
   logical  :: t6hr_alarm             ! alarm every six hours 
   logical  :: t12hr_alarm            ! alarm every twelve hours 
   logical  :: t24hr_alarm            ! alarm every twentyfour hours 

   real(r8) :: days_per_year = 365.0  ! days per year

   integer  :: dtime                  ! dt of one coupling interval
   integer  :: ncpl                   ! number of coupling intervals per day
   integer  :: ymd                    ! Current date (YYYYMMDD)
   integer  :: year                   ! Current date (YYYY)
   integer  :: month                  ! Current date (MM)
   integer  :: day                    ! Current date (DD)
   integer  :: tod                    ! Current time of day (seconds)
   character(CL) :: orb_mode          ! orbital mode
   integer  :: orb_iyear              ! orbital year
   integer  :: orb_iyear_align        ! associated with model year
   integer  :: orb_cyear              ! orbital year for current orbital computation
   integer  :: orb_nyear              ! orbital year associated with currrent model year
   real(r8) :: orb_eccen              ! orbital eccentricity
   real(r8) :: orb_obliq              ! obliquity in degrees
   real(r8) :: orb_mvelp              ! moving vernal equinox long
   real(r8) :: orb_obliqr             ! Earths obliquity in rad
   real(r8) :: orb_lambm0             ! Mean long of perihelion at vernal equinox (radians)
   real(r8) :: orb_mvelpp             ! moving vernal equinox long

   !--- for documenting speed of the model ---
   character( 8) :: dstr              ! date string
   character(10) :: tstr              ! time string
   integer       :: begStep, endStep  ! Begining and ending step number
   real(r8)      :: simDays           ! Number of simulated days
   real(r8)      :: SYPD              ! Simulated years per day
   real(r8)      :: Time_begin        ! Start time
   real(r8)      :: Time_end          ! Ending time
   real(r8)      :: Time_bstep        ! Start time
   real(r8)      :: Time_estep        ! Ending time
   real(r8)      :: dtstep            ! delta time
   real(r8)      :: dtstep_acc        ! dtstep accumulator
   integer       :: dtstep_cnt        ! dtstep counter
   character(CL) :: timing_file       ! Local path to tprof filename
   character(CL) :: timing_dir        ! timing directory
   character(CL) :: tchkpt_dir        ! timing checkpoint directory

   !----------------------------------------------------------------------------
   ! control flags
   !----------------------------------------------------------------------------
   logical  :: atm_present            ! .true.  => atm is present
   logical  :: lnd_present            ! .true.  => lnd is present
   logical  :: ice_present            ! .true.  => ice is present
   logical  :: ocn_present            ! .true.  => ocn is present
   logical  :: rof_present            ! .true.  => rof is present
   logical  :: glc_present            ! .true.  => glc is present
   logical  :: sno_present            ! .true.  => sno is present
   logical  :: wrf_present            ! .true.  => wrf is present
   logical  :: geatm_present            ! .true.  => geatm is present

   logical  :: atm_prognostic         ! .true.  => atm comp expects input
   logical  :: lnd_prognostic         ! .true.  => lnd comp expects input
   logical  :: ice_prognostic         ! .true.  => ice comp expects input
   logical  :: ocn_prognostic         ! .true.  => ocn comp expects input
   logical  :: ocnrof_prognostic         ! .true.  => ocnrof comp expects input
   logical  :: glc_prognostic         ! .true.  => glc comp expects input
   logical  :: sno_prognostic         ! .true.  => sno comp expects input
   logical  :: wrf_prognostic         ! .true.  => wrf comp expects input
   logical  :: geatm_prognostic         ! .true.  => geatm comp expects input

   logical  :: dead_comps             ! .true.  => dead components 

   logical  :: single_column          ! scm mode logical
   real(r8) :: scmlon                 ! single column lon
   real(r8) :: scmlat                 ! single column lat
   logical  :: aqua_planet            ! aqua planet mode
   real(r8) :: nextsw_cday            ! radiation control
   logical  :: atm_aero               ! atm provides aerosol data
   real(r8) :: flux_epbalfact         ! precip factor

   logical  :: ocean_tight_coupling   ! couple ocn as frequently as lnd & ice
   logical  :: skip_ocean_run         ! skip the ocean model first pass
   logical  :: cpl2ocn_first          ! use to call initial cpl2ocn timer
   character(CS) :: aoflux_grid       ! grid for a/o flux calc: atm xor ocn 
   logical  :: run_barriers           ! barrier the component run calls

   logical       :: read_restart      ! local read restart flag
   character(CL) :: rest_file         ! restart file path + filename

   logical  :: domain_check           ! .true.  => check consistency of domains
   logical  :: shr_map_dopole         ! logical for dopole in shr_map_mod

   !--- history & budgets ---
   logical       :: do_budgets        ! heat/water budgets on
   logical       :: do_histinit       ! initial hist file
   logical       :: do_histavg        ! histavg on or off
   logical       :: do_hist_r2x       ! create aux files: r2x
   logical       :: do_hist_l2x       ! create aux files: l2x
   logical       :: do_hist_a2x24hr   ! create aux files: a2x
   logical       :: do_hist_a2x       ! create aux files: a2x
   logical       :: do_hist_a2x3hrp   ! create aux files: a2x 3hr precip
   logical       :: do_hist_a2x3hr    ! create aux files: a2x 3hr states
!  character(CL) :: hist_r2x_flds     = 'all'
!  character(CL) :: hist_l2x_flds     = 'all'
   character(CL) :: hist_a2x_flds     = 'Faxa_swndr:Faxa_swvdr:Faxa_swndf:Faxa_swvdf'
!  character(CL) :: hist_a2x24hr_flds = 'all'
   character(CL) :: hist_a2x3hrp_flds = 'Faxa_rainc:Faxa_rainl:Faxa_snowc:Faxa_snowl'
   character(CL) :: hist_a2x3hr_flds  = 'Sa_z:Sa_u:Sa_v:Sa_tbot:Sa_ptem:Sa_shum:Sa_dens:Sa_pbot:Sa_pslv:Faxa_lwdn'
   integer  :: budget_inst            ! instantaneous budget flag
   integer  :: budget_daily           ! daily budget flag
   integer  :: budget_month           ! monthly budget flag
   integer  :: budget_ann             ! annual budget flag
   integer  :: budget_ltann           ! long term budget flag for end of year writing
   integer  :: budget_ltend           ! long term budget flag for end of run writing

   ! --- other ---
   integer  :: k1,k2,k3               ! aVect field indices
   integer  :: ocnrun_count           ! number of times ocn run alarm went on
   logical  :: exists                 ! true if file exists
   integer  :: ierr                   ! MPI error return
   integer  :: rc                     ! return code
   logical  :: cdf64                  ! true => use 64 bit addressing in netCDF files

   character(*), parameter :: NLFileName = "drv_in"  ! input namelist filename

   integer  :: info_debug = 0         ! local info_debug level

   !----------------------------------------------------------------------------
   ! memory monitoring
   !----------------------------------------------------------------------------
   real(r8) :: msize,msize0,msize1     ! memory size (high water)
   real(r8) :: mrss ,mrss0 ,mrss1      ! resident size (current memory use)

   !----------------------------------------------------------------------------
   ! threading control
   !----------------------------------------------------------------------------
   integer  :: nthreads_GLOID         ! OMP global number of threads
   integer  :: nthreads_CPLID         ! OMP cpl number of threads
    integer  :: nthreads_ATMID         ! OMP ATM number of threads
    integer  :: nthreads_LNDID         ! OMP LND number of threads
    integer  :: nthreads_ICEID         ! OMP ICE number of threads
    integer  :: nthreads_OCNID         ! OMP OCN number of threads
    integer  :: nthreads_GLCID         ! OMP GLC number of threads
    integer  :: nthreads_WRFID         ! OMP WRF number of threads
    integer  :: nthreads_GEAID         ! OMP GEA number of threads

   integer  :: pethreads_GLOID        ! OMP number of threads per task

    integer  :: nthreads_CPLATMID      ! OMP cpl-ATM number of threads
    integer  :: nthreads_CPLLNDID      ! OMP cpl-LND number of threads
    integer  :: nthreads_CPLICEID      ! OMP cpl-ICE number of threads
    integer  :: nthreads_CPLOCNID      ! OMP cpl-OCN number of threads
    integer  :: nthreads_CPLGLCID      ! OMP cpl-GLC number of threads
    integer  :: nthreads_CPLWRFID      ! OMP cpl-WRF number of threads
    integer  :: nthreads_CPLGEAID      ! OMP cpl-GEA number of threads

   logical  :: drv_threading          ! driver threading control

   !----------------------------------------------------------------------------
   ! communicator groups and related
   !----------------------------------------------------------------------------
   integer  :: Global_Comm
   integer  :: mpicom_GLOID           ! MPI global communicator
   integer  :: mpicom_CPLID           ! MPI cpl communicator
   integer  :: mpicom_ATMID           ! MPI ATM communicator
   integer  :: mpicom_LNDID           ! MPI LND communicator
   integer  :: mpicom_ICEID           ! MPI ICE communicator
   integer  :: mpicom_OCNID           ! MPI OCN communicator
   integer  :: mpicom_GLCID           ! MPI GLC communicator
   integer  :: mpicom_WRFID           ! MPI WRF communicator
   integer  :: mpicom_GEAID           ! MPI GEA communicator

   integer  :: mpicom_CPLATMID        ! MPI cpl-ATM communicator
   integer  :: mpicom_CPLLNDID        ! MPI cpl-LND communicator
   integer  :: mpicom_CPLICEID        ! MPI cpl-ICE communicator
   integer  :: mpicom_CPLOCNID        ! MPI cpl-OCN communicator
   integer  :: mpicom_CPLGLCID        ! MPI cpl-GLC communicator
   integer  :: mpicom_CPLWRFID        ! MPI cpl-WRF communicator
   integer  :: mpicom_CPLGEAID        ! MPI cpl-GEA communicator
   
   logical  :: iamroot_GLOID          ! GLOID masterproc
   logical  :: iamroot_CPLID          ! CPLID masterproc
   logical  :: iamroot_ATMID          ! ATMID masterproc
   logical  :: iamroot_LNDID          ! LNDID masterproc
   logical  :: iamroot_ICEID          ! ICEID masterproc
   logical  :: iamroot_OCNID          ! OCNID masterproc
   logical  :: iamroot_GLCID          ! GLCID masterproc
   logical  :: iamroot_WRFID          ! WRFID masterproc
   logical  :: iamroot_GEAID          ! GEAID masterproc

   logical  :: iamin_CPLID            ! pe associated with CPLID
   logical  :: iamin_ATMID            ! pe associated with {CCC}ID
   logical  :: iamin_LNDID            ! pe associated with {CCC}ID
   logical  :: iamin_ICEID            ! pe associated with {CCC}ID
   logical  :: iamin_OCNID            ! pe associated with {CCC}ID
   logical  :: iamin_GLCID            ! pe associated with {CCC}ID
   logical  :: iamin_WRFID            ! pe associated with {CCC}ID
   logical  :: iamin_GEAID            ! pe associated with {CCC}ID

   logical  :: iamin_CPLATMID         ! pe associated with CPLATMID
   logical  :: iamin_CPLLNDID         ! pe associated with CPLLNDID
   logical  :: iamin_CPLICEID         ! pe associated with CPLICEID
   logical  :: iamin_CPLOCNID         ! pe associated with CPLOCNID
   logical  :: iamin_CPLGLCID         ! pe associated with CPLGLCID
   logical  :: iamin_CPLWRFID         ! pe associated with CPLWRFID
   logical  :: iamin_CPLGEAID         ! pe associated with CPLGEAID

   character(CL) :: complist          ! list of comps on this pe
   integer  :: iam_GLOID              ! pe number in global id
   integer, pointer :: atm_petlist(:)
   integer, pointer :: wrf_petlist(:)
   integer, pointer :: gea_petlist(:)
   integer, pointer :: lnd_petlist(:)
   integer, pointer :: ocn_petlist(:)
   integer, pointer :: ice_petlist(:)
   integer, pointer :: glc_petlist(:)

   !----------------------------------------------------------------------------
   ! formats
   !----------------------------------------------------------------------------
   character(*), parameter :: subname = '(seq_mct_drv)'
   character(*), parameter :: F00 = "('"//subname//" : ', 4A )"
   character(*), parameter :: F0L = "('"//subname//" : ', A, L6 )"
   character(*), parameter :: F0I = "('"//subname//" : ', A, 2i8 )"
   character(*), parameter :: F0R = "('"//subname//" : ', A, 2g23.15 )"
   character(*), parameter :: FormatA = '(A,": =============== ", A41,          " ===============")'
   character(*), parameter :: FormatD = '(A,": =============== ", A20,2I8,5x,   " ===============")'
   character(*), parameter :: FormatR = '(A,": =============== ", A31,F9.3,1x,  " ===============")'
   character(*), parameter :: FormatQ = '(A,": =============== ", A20,2F10.2,1x," ===============")'

   logical :: twoway_coupling ! juanxiong he
   integer :: twoway_nudging ! juanxiong he
   integer :: integration_phase ! juanxiong he

!===============================================================================
contains
!===============================================================================

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

