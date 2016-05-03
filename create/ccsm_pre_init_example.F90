subroutine ccsm_pre_init()

   !--------------------------------------------------------------------------
   ! Initialize MCT and MPI communicators and IO
   !--------------------------------------------------------------------------

    call mpi_init(ierr)        
    call shr_mpi_chkerr(ierr,subname//' mpi_init') 

    Global_Comm=MPI_COMM_WORLD  
    call seq_io_init1(NLFileName, Global_Comm)  

    if (Global_Comm /= MPI_COMM_NULL) then
     call seq_comm_init(Global_Comm, NLFileName,atm_petlist=atm_petlist, lnd_petlist=lnd_petlist, &
           ice_petlist=ice_petlist, ocn_petlist=ocn_petlist,glc_petlist=glc_petlist, &
           wrf_petlist=wrf_petlist, gea_petlist=gea_petlist) 
    end if

    call seq_io_init2()
 
   !--- set task based threading counts ---
   call seq_comm_setptrs(GLOID,pethreads=pethreads_GLOID,iam=iam_GLOID)
   call seq_comm_setnthreads(pethreads_GLOID)

   !--- get some general data ---
   call seq_comm_setptrs(GLOID,mpicom=mpicom_GLOID,iamroot=iamroot_GLOID,nthreads=nthreads_GLOID)
!{lists}
!call seq_comm_setptrs({CCC}ID,mpicom=mpicom_{CCC}ID,iamroot=iamroot_{CCC}ID,nthreads=nthreads_{CCC}ID)
   
!{lists}
!call seq_comm_setptrs(CPL{CCC}ID,mpicom=mpicom_CPL{CCC}ID,nthreads=nthreads_CPL{CCC}ID)

!{lists}
!iamin_{CCC}ID  = seq_comm_iamin({CCC})

!{lists}
!iamin_CPL{CCC}ID = seq_comm_iamin({CCC}ID)

   complist = " "
!{lists}
!if (iamin_{CCC}ID) complist = trim(complist)//' {ccc}'

   !--------------------------------------------------------------------------
   ! Set logging parameters both for shr code and locally
   !--------------------------------------------------------------------------

   if (iamroot_CPLID) then
      inquire(file='cpl_modelio.nml',exist=exists)      
      if (exists) then
         logunit = shr_file_getUnit()
         call shr_file_setIO('cpl_modelio.nml',logunit)
         call shr_file_setLogUnit(logunit)
         loglevel = 1
         call shr_file_setLogLevel(loglevel)
      endif
   else
      loglevel = 0
      call shr_file_setLogLevel(loglevel)
   endif

   !--------------------------------------------------------------------------
   ! Log info about the environment settings
   !--------------------------------------------------------------------------

   if (iamroot_CPLID) then
#ifdef USE_ESMF_LIB
      write(logunit,'(2A)') subname,' USE_ESMF_LIB is set'
#else
      write(logunit,'(2A)') subname,' USE_ESMF_LIB is NOT set, using
esmf_wrf_timemgr'
#endif
#ifdef MCT_INTERFACE
      write(logunit,'(2A)') subname,' MCT_INTERFACE is set'
#endif
#ifdef ESMF_INTERFACE
      write(logunit,'(2A)') subname,' ESMF_INTERFACE is set'
#endif
   endif


end subroutine ccsm_pre_init


