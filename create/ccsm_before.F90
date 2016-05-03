
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
!{list} key="ccc1" 
!   use {ccc}_comp_mct, only: {ccc}_init_mct, {ccc}_run_mct, {ccc}_final_mct   
#ifdef ESMF_INTERFACE
   use esmfshr_attribute_mod
!{list} key="ccc2"
!   use {ccc}_comp_mct, only: {ccc}_register
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
!{list} key="c3" key="ccc3"
!   use mrg_x2{c}_mct      ! {ccc} merge gridded component

   !--- mapping routines ---
!{list} key="ccc4"
!   use map_{ccc}{ccc}_mct

!{list} key="map1"
!   use map_{ccc1}{ccc2}_mct
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
!{list} key="c5"
!   type(mct_gsMap)  :: gsMap_{c}{c}

!{list} key="c5"
!   type(mct_gsMap)  :: gsMap_{c}x

   !--- domain area correction factors (only defined on cpl pes) ---
!{list} key="cc6"
!   real(r8),pointer :: drv2mdl_{cc}(:), mdl2drv_{cc}(:)

   !--- domain equivalent 2d grid size ---
!{list} key="cc6"
!   integer          :: {ccc}_nx, {ccc}_ny

   !----------------------------------------------------------------------------
   ! time management
   !----------------------------------------------------------------------------

   type (seq_timemgr_type), SAVE :: seq_SyncClock ! array of all clocks & alarm
   type (ESMF_Clock), SAVE       :: EClock_d      ! driver clock
!{list} key="c1"   
!  type (ESMF_Clock), SAVE       :: EClock_{c}

   logical  :: restart_alarm          ! restart alarm
   logical  :: history_alarm          ! history alarm
   logical  :: histavg_alarm          ! history alarm
   logical  :: stop_alarm             ! stop alarm
!{list} key="ccc1a1"   
!  logical  :: {ccc}run_alarm          ! {ccc} run alarm
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
!{list} key="ccc6"
!  logical  :: {ccc}_present            ! .true.  => {ccc} is present

!{list} key="ccc7"
!  logical  :: {ccc}_prognostic         ! .true.  => {ccc} comp expects input

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
!{list} key="ccc8"   
!   integer  :: nthreads_{ccc}ID         ! OMP {ccc} number of threads

   integer  :: pethreads_GLOID        ! OMP number of threads per task

!{list} key="ccc8"
!   integer  :: nthreads_CPL{ccc}ID      ! OMP cpl-{ccc} number of threads

   logical  :: drv_threading          ! driver threading control

   !----------------------------------------------------------------------------
   ! communicator groups and related
   !----------------------------------------------------------------------------
   integer  :: Global_Comm
   integer  :: mpicom_GLOID           ! MPI global communicator
   integer  :: mpicom_CPLID           ! MPI cpl communicator
!{list} key="ccc8"   
!  integer  :: mpicom_{ccc}ID           ! MPI {ccc} communicator

!{list} key="ccc8"
!  integer  :: mpicom_CPL{ccc}ID        ! MPI cpl-{ccc} communicator
   
   logical  :: iamroot_GLOID          ! GLOID masterproc
   logical  :: iamroot_CPLID          ! CPLID masterproc
!{list} key="ccc8"
!  logical  :: iamroot_{ccc}ID          ! {ccc}ID masterproc

   logical  :: iamin_CPLID            ! pe associated with CPLID
!{list} key="ccc8"
!  logical  :: iamin_{ccc}ID            ! pe associated with {CCC}ID

!{list} key="ccc8"
!  logical  :: iamin_CPL{ccc}ID         ! pe associated with CPL{ccc}ID

   character(CL) :: complist          ! list of comps on this pe
   integer  :: iam_GLOID              ! pe number in global id
!{list} key="ccc1a1"
!  integer, pointer :: {ccc}_petlist(:)

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

