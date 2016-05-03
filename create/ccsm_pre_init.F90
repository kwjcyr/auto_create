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
   call seq_comm_setptrs(CPLID,mpicom=mpicom_CPLID,iamroot=iamroot_CPLID,nthreads=nthreads_CPLID)
   call seq_comm_setptrs(OCNID,mpicom=mpicom_OCNID,iamroot=iamroot_OCNID,nthreads=nthreads_OCNID)
   call seq_comm_setptrs(ATMID,mpicom=mpicom_ATMID,iamroot=iamroot_ATMID,nthreads=nthreads_ATMID)
   call seq_comm_setptrs(LNDID,mpicom=mpicom_LNDID,iamroot=iamroot_LNDID,nthreads=nthreads_LNDID)
   call seq_comm_setptrs(ICEID,mpicom=mpicom_ICEID,iamroot=iamroot_ICEID,nthreads=nthreads_ICEID)
   call seq_comm_setptrs(GLCID,mpicom=mpicom_GLCID,iamroot=iamroot_GLCID,nthreads=nthreads_GLCID)
   call seq_comm_setptrs(WRFID,mpicom=mpicom_WRFID,iamroot=iamroot_WRFID,nthreads=nthreads_WRFID)
   call seq_comm_setptrs(GEAID,mpicom=mpicom_GEAID,iamroot=iamroot_GEAID,nthreads=nthreads_GEAID)
   
   call seq_comm_setptrs(CPLOCNID,mpicom=mpicom_CPLOCNID,nthreads=nthreads_CPLOCNID)
   call seq_comm_setptrs(CPLATMID,mpicom=mpicom_CPLATMID,nthreads=nthreads_CPLATMID)
   call seq_comm_setptrs(CPLLNDID,mpicom=mpicom_CPLLNDID,nthreads=nthreads_CPLLNDID)
   call seq_comm_setptrs(CPLICEID,mpicom=mpicom_CPLICEID,nthreads=nthreads_CPLICEID)
   call seq_comm_setptrs(CPLGLCID,mpicom=mpicom_CPLGLCID,nthreads=nthreads_CPLGLCID)
   call seq_comm_setptrs(CPLWRFID,mpicom=mpicom_CPLWRFID,nthreads=nthreads_CPLWRFID)
   call seq_comm_setptrs(CPLGEAID,mpicom=mpicom_CPLGEAID,nthreads=nthreads_CPLGEAID)

   iamin_CPLID  = seq_comm_iamin(CPL)
   iamin_OCNID  = seq_comm_iamin(OCN)
   iamin_ATMID  = seq_comm_iamin(ATM)
   iamin_LNDID  = seq_comm_iamin(LND)
   iamin_ICEID  = seq_comm_iamin(ICE)
   iamin_GLCID  = seq_comm_iamin(GLC)
   iamin_WRFID  = seq_comm_iamin(WRF)
   iamin_GEAID  = seq_comm_iamin(GEA)

   iamin_CPLOCNID = seq_comm_iamin(OCNID)
   iamin_CPLATMID = seq_comm_iamin(ATMID)
   iamin_CPLLNDID = seq_comm_iamin(LNDID)
   iamin_CPLICEID = seq_comm_iamin(ICEID)
   iamin_CPLGLCID = seq_comm_iamin(GLCID)
   iamin_CPLWRFID = seq_comm_iamin(WRFID)
   iamin_CPLGEAID = seq_comm_iamin(GEAID)

   complist = " "
   if (iamin_CPLID) complist = trim(complist)//' cpl'
   if (iamin_OCNID) complist = trim(complist)//' ocn'
   if (iamin_ATMID) complist = trim(complist)//' atm'
   if (iamin_LNDID) complist = trim(complist)//' lnd'
   if (iamin_ICEID) complist = trim(complist)//' ice'
   if (iamin_GLCID) complist = trim(complist)//' glc'
   if (iamin_WRFID) complist = trim(complist)//' wrf'
   if (iamin_GEAID) complist = trim(complist)//' gea'

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


