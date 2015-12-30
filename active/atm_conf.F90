[start 0]
  cyrcyrcyr00
[end 0]
[start 1]
  use pio              , only: file_desc_t, io_desc_t, var_desc_t,
pio_double, pio_def_dim, &
                               pio_put_att, pio_enddef, pio_initdecomp,
pio_read_darray, pio_freedecomp, &
                               pio_closefile, pio_write_darray,
pio_def_var, pio_inq_varid, &
                               pio_noerr, pio_bcast_error,
pio_internal_error, pio_seterrorhandling 
  use mct_mod
  use esmf_mod
  use seq_flds_mod
  use seq_cdata_mod
  use seq_infodata_mod
  use seq_timemgr_mod

  use shr_kind_mod     , only: r8 => shr_kind_r8, cl=>shr_kind_cl
  use shr_file_mod     , only: shr_file_getunit, shr_file_freeunit, &
                               shr_file_setLogUnit,
shr_file_setLogLevel, &
                               shr_file_getLogUnit,
shr_file_getLogLevel, &
                               shr_file_setIO
  use shr_sys_mod      , only: shr_sys_flush, shr_sys_abort

  use cam_cpl_indices
  use cam_comp
  use cam_control_mod  , only: nsrest, adiabatic, ideal_phys,
aqua_planet, eccen, obliqr, lambm0, mvelpp
  use radiation        , only: radiation_get, radiation_do,
radiation_nextsw_cday
  use phys_grid        , only: get_ncols_p, get_gcol_all_p, & 
                               ngcols, get_gcol_p, get_rlat_all_p, &
                               get_rlon_all_p, get_area_all_p
  use ppgrid           , only: pcols, begchunk, endchunk       
  use dyn_grid         , only: get_horiz_grid_dim_d
  use camsrfexch_types , only: cam_out_t, cam_in_t     
  use cam_restart      , only: get_restcase, get_restartdir
  use cam_history      , only: outfld, ctitle
  use abortutils       , only: endrun
  use fiGlenames        , only: interpret_filename_spec, caseid,
brnch_retain_casename
#ifdef SPMD
  use spmd_utils       , only: spmdinit, masterproc, iam
  use mpishorthand     , only: mpicom
#else
  use spmd_utils       , only: spmdinit, masterproc, mpicom, iam
#endif
  use time_manager     , only: get_curr_calday, advance_timestep,
get_curr_date, get_nstep, &
                               is_first_step, get_step_size,
timemgr_init, timemgr_check_restart
  use ioFileMod             
  use perf_mod
  use cam_logfile      , only: iulog
  use co2_cycle        , only: c_i, co2_readFlux_ocn, co2_readFlux_fuel,
co2_transport, &
                               co2_time_interp_ocn,
co2_time_interp_fuel, data_flux_ocn, data_flux_fuel
  use physconst       ,  only: mwco2
  use runtime_opts     , only: read_namelist
  use phys_control     , only: cam_chempkg_is
!
! !PUBLIC TYPES:
  implicit none
  save
  private ! except
[end 1]
[start 2]
        cyrcyrcyr2
[end 2]

