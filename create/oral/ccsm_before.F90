
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
   use mrg_x2w_mct      ! wrf merge gridded component
   use mrg_x2c_mct      ! cam merge gridded component
   use mrg_x2ca_mct      ! cam merge gridded component
   use mrg_x2ge_mct      ! wrf merge gridded component
   use mrg_x2a_mct      ! atm merge gridded component
   use mrg_x2l_mct      ! lnd merge gridded component
   use mrg_x2o_mct      ! ocn merge gridded component
   use mrg_x2i_mct      ! ice merge gridded component
   use mrg_x2g_mct      ! glc merge gridded component
   use mrg_x2s_mct      ! sno merge gridded component

   !--- mapping routines ---
   use map_camcam_mct
   use map_wrfwrf_mct
   use map_wrfcam_mct
   use map_gcamcam_mct
   use map_geagea_mct
   use map_geacam_mct

   use map_atmlnd_mct   ! atm to lnd coupler component
   use map_atmice_mct   ! atm to ice coupler component
   use map_atmocn_mct   ! atm to ocn coupler component
   use map_iceocn_mct   ! ice to ocn coupler component
   use map_rofocn_mct   ! roff to ocn coupler component
   use map_snoglc_mct   ! lnd/sno to glc coupler component
   use map_ocnocn_mct   ! ocn to ocn coupler component
   use map_iceice_mct   ! ice to ice coupler component
   use map_lndlnd_mct   ! lnd to lnd coupler component
   use map_rofrof_mct   ! lnd to lnd coupler component
   use map_atmatm_mct   ! atm to atm coupler component
   use map_glcglc_mct   ! glc to glc coupler component
   use map_snosno_mct   ! lnd/sno to lnd/sno coupler component

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
   type(mct_gsMap)  :: gsMap_aa    ! on component pes
   type(mct_gsMap)  :: gsMap_ll
   type(mct_gsMap)  :: gsMap_oo
   type(mct_gsMap)  :: gsMap_ii
   type(mct_gsMap)  :: gsMap_rr
   type(mct_gsMap)  :: gsMap_gg
   type(mct_gsMap)  :: gsMap_ss
   type(mct_gsMap)  :: gsMap_ww   !juanxiong he
   type(mct_gsMap)  :: gsMap_cc   !juanxiong he
   type(mct_gsMap)  :: gsMap_mm   !juanxiong he
   type(mct_gsMap)  :: gsMap_gege   !juanxiong he
   type(mct_gsMap)  :: gsMap_caca   !juanxiong he

   type(mct_gsMap)  :: gsMap_ax    ! on cpl pes
   type(mct_gsMap)  :: gsMap_lx
   type(mct_gsMap)  :: gsMap_ox
   type(mct_gsMap)  :: gsMap_ix
   type(mct_gsMap)  :: gsMap_rx
   type(mct_gsMap)  :: gsMap_gx
   type(mct_gsMap)  :: gsMap_sx
   type(mct_gsMap)  :: gsMap_cx  !juanxiong he 
   type(mct_gsMap)  :: gsMap_wx  !juanxiong he
   type(mct_gsMap)  :: gsMap_mx   !juanxiong he
   type(mct_gsMap)  :: gsMap_cax  !juanxiong he 
   type(mct_gsMap)  :: gsMap_gex  !juanxiong he

   !--- domain area correction factors (only defined on cpl pes) ---
   real(r8),pointer :: drv2mdl_aa(:), mdl2drv_aa(:)
   real(r8),pointer :: drv2mdl_ll(:), mdl2drv_ll(:)
   real(r8),pointer :: drv2mdl_ii(:), mdl2drv_ii(:)
   real(r8),pointer :: drv2mdl_oo(:), mdl2drv_oo(:)
   real(r8),pointer :: drv2mdl_rr(:), mdl2drv_rr(:)
   real(r8),pointer :: drv2mdl_gg(:), mdl2drv_gg(:)
   real(r8),pointer :: drv2mdl_ss(:), mdl2drv_ss(:)
   real(r8),pointer :: drv2mdl_ww(:), mdl2drv_ww(:)   ! juanxiong he
   real(r8),pointer :: drv2mdl_ge(:), mdl2drv_ge(:)   ! juanxiong he

   !--- domain equivalent 2d grid size ---
   integer          :: atm_nx, atm_ny  ! nx, ny of 2d grid, if known
   integer          :: lnd_nx, lnd_ny
   integer          :: ice_nx, ice_ny
   integer          :: ocn_nx, ocn_ny
   integer          :: rof_nx, rof_ny
   integer          :: glc_nx, glc_ny
   integer          :: sno_nx, sno_ny
   integer          :: wrf_nx, wrf_ny  ! juanxiong he
   integer          :: geatm_nx, geatm_ny  ! juanxiong he

   !----------------------------------------------------------------------------
   ! time management
   !----------------------------------------------------------------------------

   type (seq_timemgr_type), SAVE :: seq_SyncClock ! array of all clocks & alarm
   type (ESMF_Clock), SAVE       :: EClock_d      ! driver clock
   type (ESMF_Clock), SAVE       :: EClock_a
   type (ESMF_Clock), SAVE       :: EClock_l
   type (ESMF_Clock), SAVE       :: EClock_o
   type (ESMF_Clock), SAVE       :: EClock_i
   type (ESMF_Clock), SAVE       :: EClock_g
   type (ESMF_Clock)       :: EClock_w  ! juanxiong he for wrf/ccsm4 coupling
   type (ESMF_Clock)       :: EClock_ge  ! juanxiong he for wrf/ccsm4 coupling

   logical  :: restart_alarm          ! restart alarm
   logical  :: history_alarm          ! history alarm
   logical  :: histavg_alarm          ! history alarm
   logical  :: stop_alarm             ! stop alarm
   logical  :: atmrun_alarm           ! atm run alarm
   logical  :: wrfrun_alarm           ! wrf run alarm, juanxiong he
   logical  :: gearun_alarm           ! gea run alarm, juanxiong he
   logical  :: lndrun_alarm           ! lnd run alarm
   logical  :: icerun_alarm           ! ice run alarm
   logical  :: ocnrun_alarm           ! ocn run alarm
   logical  :: ocnnext_alarm          ! ocn run alarm on next timestep
   logical  :: glcrun_alarm           ! glc run alarm
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
   logical  :: lnd_present            ! .true.  => land is present
   logical  :: ice_present            ! .true.  => ice is present
   logical  :: ocn_present            ! .true.  => ocn is present
   logical  :: rof_present            ! .true.  => land runoff is present
   logical  :: glc_present            ! .true.  => glc is present
   logical  :: wrf_present            ! .true.  => wrf is present, juanxiong he
   logical  :: geatm_present            ! .true.  => gea is present, juanxiong he
   logical  :: sno_present            ! .true.  => land sno is present

   logical  :: atm_prognostic         ! .true.  => atm comp expects input
   logical  :: lnd_prognostic         ! .true.  => lnd comp expects input
   logical  :: ice_prognostic         ! .true.  => ice comp expects input
   logical  :: ocn_prognostic         ! .true.  => ocn comp expects input
   logical  :: ocnrof_prognostic      ! .true.  => ocn comp expects runoff input
   logical  :: glc_prognostic         ! .true.  => glc comp expects input
   logical  :: wrf_prognostic         ! .true.  => wrf comp expects input, juanxiong he
   logical  :: geatm_prognostic         ! .true.  => gea comp expects input, juanxiong he
   logical  :: sno_prognostic         ! .true.  => sno comp expects input

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
   integer  :: nthreads_ATMID         ! OMP atm number of threads
   integer  :: nthreads_LNDID         ! OMP lnd number of threads
   integer  :: nthreads_ICEID         ! OMP ice number of threads
   integer  :: nthreads_OCNID         ! OMP ocn number of threads
   integer  :: nthreads_GLCID         ! OMP glc number of threads
   integer  :: nthreads_WRFID         ! OMP wrf number of threads, juanxiong he
   integer  :: nthreads_GEAID         ! OMP wrf number of threads, juanxiong he

   integer  :: pethreads_GLOID        ! OMP number of threads per task

   integer  :: nthreads_CPLATMID      ! OMP cpl-atm number of threads
   integer  :: nthreads_CPLLNDID      ! OMP cpl-lnd number of threads
   integer  :: nthreads_CPLICEID      ! OMP cpl-ice number of threads
   integer  :: nthreads_CPLOCNID      ! OMP cpl-ocn number of threads
   integer  :: nthreads_CPLGLCID      ! OMP cpl-glc number of threads
   integer  :: nthreads_CPLWRFID      ! OMP cpl-glc number of threads, juanxiong he
   integer  :: nthreads_CPLGEAID      ! OMP cpl-glc number of threads, juanxiong he

   logical  :: drv_threading          ! driver threading control

   !----------------------------------------------------------------------------
   ! communicator groups and related
   !----------------------------------------------------------------------------
   integer  :: Global_Comm
   integer  :: mpicom_GLOID           ! MPI global communicator
   integer  :: mpicom_CPLID           ! MPI cpl communicator
   integer  :: mpicom_ATMID           ! MPI atm communicator
   integer  :: mpicom_LNDID           ! MPI lnd communicator
   integer  :: mpicom_ICEID           ! MPI ice communicator
   integer  :: mpicom_OCNID           ! MPI ocn communicator
   integer  :: mpicom_GLCID           ! MPI glc communicator
   integer  :: mpicom_WRFID           ! MPI wrf communicator, juanxiong he
   integer  :: mpicom_GEAID           ! MPI wrf communicator, juanxiong he

   integer  :: mpicom_CPLATMID        ! MPI cpl-atm communicator
   integer  :: mpicom_CPLLNDID        ! MPI cpl-lnd communicator
   integer  :: mpicom_CPLICEID        ! MPI cpl-ice communicator
   integer  :: mpicom_CPLOCNID        ! MPI cpl-ocn communicator
   integer  :: mpicom_CPLGLCID        ! MPI cpl-glc communicator
   integer  :: mpicom_CPLWRFID        ! MPI cpl-wrf communicator, juanxiong he
   integer  :: mpicom_CPLGEAID        ! MPI cpl-wrf communicator, juanxiong he

   logical  :: iamroot_GLOID          ! GLOID masterproc
   logical  :: iamroot_CPLID          ! CPLID masterproc
   logical  :: iamroot_ATMID          ! ATMID masterproc
   logical  :: iamroot_LNDID          ! LNDID masterproc
   logical  :: iamroot_ICEID          ! ICEID masterproc
   logical  :: iamroot_OCNID          ! OCNID masterproc
   logical  :: iamroot_GLCID          ! GLCID masterproc
   logical  :: iamroot_WRFID          ! WRFID masterproc, juanxiong he
   logical  :: iamroot_GEAID          ! GEAID masterproc, juanxiong he

   logical  :: iamin_CPLID            ! pe associated with CPLID
   logical  :: iamin_ATMID            ! pe associated with ATMID
   logical  :: iamin_LNDID            ! pe associated with LNDID
   logical  :: iamin_ICEID            ! pe associated with ICEID
   logical  :: iamin_OCNID            ! pe associated with OCNID
   logical  :: iamin_GLCID            ! pe associated with GLCID
   logical  :: iamin_WRFID            ! pe associated with WRFID, juanxiong he
   logical  :: iamin_GEAID            ! pe associated with WRFID, juanxiong he

   logical  :: iamin_CPLATMID         ! pe associated with CPLATMID
   logical  :: iamin_CPLLNDID         ! pe associated with CPLLNDID
   logical  :: iamin_CPLICEID         ! pe associated with CPLICEID
   logical  :: iamin_CPLOCNID         ! pe associated with CPLOCNID
   logical  :: iamin_CPLGLCID         ! pe associated with CPLGLCID
   logical  :: iamin_CPLWRFID         ! pe associated with CPLWRFID, juanxiong he
   logical  :: iamin_CPLGEAID         ! pe associated with CPLWRFID, juanxiong he

   character(CL) :: complist          ! list of comps on this pe
   integer  :: iam_GLOID              ! pe number in global id
   integer, pointer :: atm_petlist(:), lnd_petlist(:), ice_petlist(:), ocn_petlist(:), glc_petlist(:), &
                       wrf_petlist(:), gea_petlist(:) ! juanxiong he

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

