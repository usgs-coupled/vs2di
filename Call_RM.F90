Module vs2dt_rm
    integer rm_id
    integer nthreads 
    integer, dimension(:), allocatable :: forward1
    
    CONTAINS  
    
    SUBROUTINE CreateRM 
    USE PhreeqcRM
    !USE COMPNAM
    IMPLICIT NONE
    INTEGER NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
    COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
    CHARACTER*80 CHEMFILE,DATABASEFILE,PREFIX
    COMMON/SOLCHAR/CHEMFILE,DATABASEFILE,PREFIX
    LOGICAL HEAT,SOLUTE
    COMMON/TRANSTYPE/HEAT,SOLUTE
    SAVE 
    INTEGER i, status
    CHARACTER*32 string
    
    ! ... make a reaction module, makes instances of IPhreeqc and IPhreeqcPhast with same rm_id
    nthreads = 1
    rm_id = RM_Create(NNODES, nthreads)
    status = RM_SetFilePrefix(rm_id, PREFIX)
    status = RM_OpenFiles(rm_id)  
    IF (solute) THEN         
        IF (rm_id.LT.0) THEN
            STOP
        END IF
        status = RM_SetComponentH2O(rm_id, 1)
        nthreads = RM_GetThreadCount(rm_id)
        status = RM_SetErrorHandlerMode(rm_id, 2)   ! exit
        status = RM_SetPrintChemistryOn(rm_id, 0, 1, 0) 
        status = RM_UseSolutionDensityVolume(rm_id, 0)
        status = RM_LoadDatabase(rm_id, DATABASEFILE)
        !... Call phreeqc, find number of components, f1name, chem.dat, f2name, database, f3name, prefix
        status = RM_LogMessage(rm_id, "Initial PHREEQC run.") 
        status = RM_ScreenMessage(rm_id, "Initial PHREEQC run.")  
        status = RM_RunFile(rm_id, 1, 1, 1, CHEMFILE) 
        string = 'DELETE; -all' 
        status = RM_RunString(rm_id, 1, 0, 1, trim(string))
        if (status .ne. 0) stop "Failed DELETE in CreateRM"
        status = RM_FindComponents(rm_id)   
        nSol = RM_GetComponentCount(rm_id)
        status = RM_LogMessage(rm_id, "Done with Initial PHREEQC run.")
        status = RM_ScreenMessage(rm_id, "Done with Initial PHREEQC run.")
    ENDIF
END SUBROUTINE CreateRM
    
SUBROUTINE InitializeRM
    use SOLINDEX, only: cmixfarc, indsol1, indsol2, ic1_reordered
    use RPROPSH, only: HK
    use JTXX, only: jtex
    use TRXX, only: cc
    USE PhreeqcRM
    IMPLICIT NONE
    integer :: insol1, insol2
    double precision :: Solcomp
    common/solind/ INSOL1(7),INSOL2(7),Solcomp(50)
    LOGICAL HEAT,SOLUTE
    COMMON/TRANSTYPE/HEAT,SOLUTE
    SAVE
    INTEGER a_err, i, j, status
    INTEGER ipartition_uz_solids
    INTEGER, DIMENSION(:,:), ALLOCATABLE :: ic2_reordered
    DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: f1_reordered
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: temp
    DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: c
    INTEGER :: nxyz, ncomps
    integer, dimension(:), allocatable :: ibc
    double precision, dimension(:,:), allocatable :: bc_comps
    
    nxyz = RM_GetGridCellCount(rm_id)
    ncomps = RM_GetComponentCount(rm_id)
 
    IF(solute) THEN

        ! ... Send data to threads or workers
        ! 1 mg/L, 2 mol/L, 3 kg/kgs
        status = RM_SetUnitsSolution(rm_id, 2)
        
        ! 0 mol/L rv, 1 mol/L water, 2 mol/L rock
        status = RM_SetUnitsExchange(rm_id, 1)
        status = RM_SetUnitsGasPhase(rm_id, 1)
        status = RM_SetUnitsKinetics(rm_id, 1)
        status = RM_SetUnitsPPassemblage(rm_id, 1)
        status = RM_SetUnitsSSassemblage(rm_id, 1)
        status = RM_SetUnitsSurface(rm_id, 1)            
        status = RM_SetTimeConversion(rm_id, 1.0d0)

        ! set rv
        allocate (temp(nxyz))
        temp = 1.0
        status = RM_SetRepresentativeVolume(rm_id, temp)
        deallocate(temp)
        
        ! partition
        status = RM_SetPartitionUZSolids(rm_id, 0)
        
        ! set printing?
        !status = RM_SetPrintChemistryMask(rm_id, iprint_chem)
        status = RM_SetSelectedOutputOn(rm_id, 1)
 
        ! Set porosity
        allocate (temp(nxyz))
        do i = 1, nxyz            
            temp(i) = HK(jtex(i),3)
        enddo
        status = RM_SetPorosity(rm_id, temp) 
        deallocate(temp)   

        ! rebalance
        status = RM_SetRebalanceFraction(rm_id, 0.0d0)
        status = RM_SetRebalanceByCell(rm_id, 1)

        ! ... Define mapping from 3D domain to chemistry
        status = RM_CreateMapping(rm_id, forward1)      
        
        
        ! ... Make arrays in the correct order
        ALLOCATE(ic2_reordered(nxyz,7), f1_reordered(nxyz,7),   &
        STAT = a_err)
        IF (a_err /= 0) THEN
            PRINT *, "Array allocation failed: InitializeRM"  
            STOP
        ENDIF
        
        ! indsol1, indsol2, cmixfarc are (7,nxyz)
        DO i = 1, nxyz
            do j = 1, 7
                ic1_reordered(i,j) = indsol1(j,i)
                ic2_reordered(i,j) = indsol2(j,i)
                f1_reordered(i,j) = cmixfarc(j,i)
            enddo
        enddo
          
        ! ... Distribute chemistry initial conditions
        status = RM_InitialPhreeqc2Module(rm_id, &
            ic1_reordered,           & ! Fortran nxyz x 7 end-member 1 
            ic2_reordered,           & ! Fortran nxyz x 7 end-member 2
            f1_reordered)              ! Fortran nxyz x 7 fraction of end-member 1 
                        
        DEALLOCATE (ic2_reordered, f1_reordered, &
            STAT = a_err)
        IF (a_err /= 0) THEN
            PRINT *, "Array deallocation failed: InitializeRM"  
            STOP
        ENDIF
        
#ifdef SKIP
        CALL process_restart_files()
#endif       
        CALL GetConcentrationsRM(cc)

        ! Set SolComp
        allocate(ibc(1), bc_comps(1,ncomps))
        ibc(1) = INSOL1(1)
        status = RM_InitialPhreeqc2Concentrations(rm_id, bc_comps, 1, ibc)  
        do i = 1, ncomps
            Solcomp(i) = bc_comps(1,i)
        enddo
    ENDIF        ! ... solute
END SUBROUTINE InitializeRM
#ifdef SKIP  
SUBROUTINE InitialEquilibrationRM 
    USE machine_constants, ONLY: kdp
    USE mcc, ONLY:               iprint_xyz, prcphrqi, prf_chem_phrqi, prhdfci, rm_id, solute, steady_flow
    USE mcg, ONLY:               grid2chem, nxyz
    USE mcn, ONLY:               x_node, y_node, z_node, phreeqc_density, pv0, por, volume
    USE mcp, ONLY:               pv
    USE mcv, ONLY:               c, frac, sat, time_phreeqc
    USE hdf_media_m, ONLY:       pr_hdf_media
    USE PhreeqcRM
    IMPLICIT NONE
    SAVE
    DOUBLE PRECISION :: deltim_dummy
    CHARACTER(LEN=130) :: logline1
    INTEGER :: stop_msg, status, i !, imedia
    
    ! ...  Initial equilibrate
    IF (solute) THEN
        deltim_dummy = 0._kdp
        ! ... Equilibrate the initial conditions for component concentrations
        WRITE(logline1,'(a)') 'Equilibration of cells for initial conditions.'
        status = RM_LogMessage(rm_id, logline1)
        status = RM_ScreenMessage(rm_id, logline1)
        stop_msg = 0
        deltim_dummy = 0._kdp
        ! Set porosity
        do i = 1, nxyz
            if (volume(i) .ne. 0.0d0) then
                por(i) = pv0(i)/volume(i)
            else
                por(i) = 1.0d0
            endif
        enddo
        status = RM_SetPorosity(rm_id, por)

        sat = 1.0
        do i = 1, nxyz
            if (frac(i) <= 0.0) then
                sat(i) = 0.0
            endif
        enddo
        status = RM_SetSaturation(rm_id, sat)
        status = RM_SetPrintChemistryOn(rm_id, prf_chem_phrqi, 0, 0)
	    status = 0
	    if (prhdfci .ne. 0 .or. prcphrqi .ne. 0) status = 1
        status = RM_SetSelectedOutputOn(rm_id, status)
        status = RM_SetTime(rm_id, time_phreeqc) 
        status = RM_SetTimeStep(rm_id, deltim_dummy) 
        status = RM_SetConcentrations(rm_id, c)
        status = RM_RunCells(rm_id)     
        !status = RM_GetConcentrations(rm_id, c(1,1))
        status = RM_GetConcentrations(rm_id, c)
        !status = RM_GetDensity(rm_id, phreeqc_density(1))
        !status = RM_SetDensity(rm_id, phreeqc_density(1))
    ENDIF  
    !imedia = 0
    !if (pr_hdf_media) imedia = 1
    !CALL FH_WriteFiles(rm_id, prhdfci,  imedia, prcphrqi, &
	   ! iprint_xyz(1), 0)       
END SUBROUTINE InitialEquilibrationRM
#endif    

#ifdef SKIP    
SUBROUTINE TimeStepRM    
    USE mcb, ONLY:               fresur
    USE mcc, ONLY:               iprint_xyz, rm_id, solute, steady_flow
    USE mcc_m, ONLY:             prcphrq, prhdfc
    USE mcg, ONLY:               grid2chem, nxyz
    USE mcn, ONLY:               x_node, y_node, z_node, phreeqc_density, volume, por
    USE mcp, ONLY:               pv
    USE mcv,  ONLY:              c, deltim, frac, indx_sol1_ic, sat, time, ns
    USE hdf_media_m, ONLY:       pr_hdf_media
    USE print_control_mod, ONLY: print_force_chemistry, print_hdf_chemistry, print_restart
    USE PhreeqcRM
    IMPLICIT NONE
    SAVE
    INTEGER stop_msg, status, i, j !, ihdf, ixyz, imedia
    CHARACTER(LEN=130) :: logline1
    
    stop_msg = 0
    IF (solute) THEN
        CALL time_parallel(9)                                     ! 9 new time
        WRITE(logline1,'(a)') '     Beginning chemistry calculation.'
        status = RM_LogMessage(rm_id, logline1)
        status = RM_ScreenMessage(rm_id, logline1)
        if (.not.steady_flow) then
          
            do i = 1, nxyz
                if (volume(i) .ne. 0.0d0) then
                    por(i) = pv(i)/volume(i)
                else
                    por(i) = 1.0d0
                endif
            enddo
            status = RM_SetPorosity(rm_id, por)            
        endif
        if (fresur.and.(.not.steady_flow)) then
            sat = frac
            do i = 1, nxyz
                if (frac(i) <= 0.0) then
                    sat(i) = 0.0
                else if (frac(i) > 1.0) then
                    sat(i) = 1.0
                endif
            enddo
            status = RM_SetSaturation(rm_id, sat)
        endif
        status = RM_SetPrintChemistryOn(rm_id, print_force_chemistry%print_flag_integer, 0, 0)
	    status = 0
        if (prhdfc .or. prcphrq) status = 1
        status = RM_SetSelectedOutputOn(rm_id, status)
        
        status = RM_SetTime(rm_id, time) 
        status = RM_SetTimeStep(rm_id, deltim) 
        status = RM_SetConcentrations(rm_id, c)
        CALL time_parallel(10)                                    ! 10 - 9 chemistry communication
        status = RM_RunCells(rm_id)  
        CALL time_parallel(11)                                    ! 11 - 10 run cells
        status = RM_GetConcentrations(rm_id, c)
        !status = RM_GetDensity(rm_id, phreeqc_density(1))
        !status = RM_SetDensity(rm_id, phreeqc_density(1))
        CALL time_parallel(12)                                    ! 12 - 11 chemistry communication
    ENDIF    ! ... Done with chemistry    
    !ihdf = 0
    !if (prhdfc) ihdf = 1
    !imedia = 0
    !if (pr_hdf_media) imedia = 1 
    !ixyz = 0
    !if (prcphrq) ixyz = 1        
    !CALL FH_WriteFiles(rm_id, ihdf, imedia, ixyz, &
    !    iprint_xyz(1), print_restart%print_flag_integer) 
    CALL time_parallel(13)                                    ! 13 - 12 chemistry files
END SUBROUTINE TimeStepRM   
    
INTEGER FUNCTION set_components()
    USE mcc, ONLY:               mpi_myself, rm_id, solute
    USE mcch, ONLY:              comp_name
    USE mcv, ONLY:               ns
    USE mpi_mod
    USE PhreeqcRM
    IMPLICIT NONE
    SAVE
    integer method, a_err, i, status
    ! makes the list of components on the Fortran side.
    
    !ns = RM_FindComponents(rm_id)
    ns = RM_GetComponentCount(rm_id)
    ALLOCATE(comp_name(ns),  & 
    STAT = a_err)
    IF (a_err /= 0) THEN
        PRINT *, "Array allocation failed: phast_manager, point 0"  
        STOP
    ENDIF
    DO i = 1, ns
        comp_name(i) = ' '
        status = RM_GetComponent(rm_id, i, comp_name(i))
    ENDDO  
    set_components = 0
END FUNCTION set_components 
SUBROUTINE process_restart_files()
    USE mcc, ONLY: mpi_myself, rm_id
    USE mcch
    USE mcg
    USE mcn
    USE mcv
    USE mpi_mod
    IMPLICIT NONE 
    INTEGER :: i

    DO i = 1, num_restart_files
        CALL FH_SetRestartName(restart_files(i))
    ENDDO
    CALL FH_SetPointers(x_node(1), y_node(1), z_node(1), indx_sol1_ic(1,1), frac(1), grid2chem(1))
    CALL FH_ProcessRestartFiles(rm_id, &
	        indx_sol1_ic(1,1),            &
	        indx_sol2_ic(1,1),            & 
	        ic_mxfrac(1,1))
    END SUBROUTINE process_restart_files 
#endif    

subroutine CreateMappingRM(initial_conditions, axes, nx, nz)
    USE PhreeqcRM
    implicit none
    integer, dimension(:,:), allocatable, intent(in) :: initial_conditions
    logical, dimension(2), intent(in) :: axes
    integer, intent(in) :: nx, nz
	integer :: i, n, ii, jj, ix, iz, ixz, count_chem, status
    logical :: success
	! calculate mapping from full set of cells to subset needed for chemistry

	count_chem = 1
	ix = nx;
	iz = nz;
	ixz = ix*iz;
	if (axes(1) == .false. .and. axes(2) == .false. ) then
		status = RM_ErrorMessage(rm_id, "No active coordinate direction in DIMENSIONS keyword.")
		STOP "No active coordinate direction in DIMENSIONS keyword."
	endif

	count_chem = ixz
    
	!   allocate space
    allocate(forward1(ixz))

	n = 0;
	! x and y
	if ((axes(1) == .true.) .and. (axes(2) == .true.)) then
		n = 0;
        do i = 1, ixz
			if (initial_conditions(1,i) .ge. 0 .or. initial_conditions(1,i) .le. -100) then
				forward1(i) = n
				n = n + 1
			else
				forward1(i) = -1
            endif
            write(*,*) "XZforward [",i-1,"] =", forward1(i)
		enddo
		count_chem = n;
	! x only
	else if ((axes(1) == .true.) .and. (axes(2) == .false.)) then
		if (iz .ne. 2) then
			status = RM_ErrorMessage(rm_id, "z direction should contain only three nodes for this 1D problem.")
			STOP "z direction should contain only three nodes for this 1D problem."
		endif

		n = 0
        do i = 1, ixz
			if (initial_conditions(i,1) .lt. 0 .and. initial_conditions(1,i) .gt. -100) then
				status = RM_ErrorMessage(rm_id, "Can not have inactive cells in a 1D simulation.")
				STOP "Can not have inactive cells in a 1D simulation."
            endif
			if (jj == 0) then
				forward1(i) = n
				n = n + 1
			else
				forward1(i) = -1
            endif
			write(*,*) "Xforward [",i-1,"] =",forward1(i)
		enddo
		count_chem = n;
	!  Copy z line
	else if ((axes(1) == .false.) .and. (axes(2) == .true.)) then
		n = 0
        do i = 1, ixz
			if (initial_conditions(1,i) .lt. 0 .and. initial_conditions(1,i) > -100) then
				status = RM_ErrorMessage(rm_id, "Can not have inactive cells in a 1D simulation.")
				STOP "Can not have inactive cells in a 1D simulation."
			endif
			success = n_to_ij(i, ii, jj, nx, nz)
			if (ii .eq. 0) then
				forward1(i) = n
				n = n + 1
			else
				forward1(i) = -1
			endif
			write(*,*) "Zforward [",i-1,"] =",forward1(i)
		enddo
		count_chem = n
	endif
    ! pass mapping to RM
    status = RM_CreateMapping(rm_id, forward1)
	write(*,*) "count_chem =", count_chem
	return
end subroutine CreateMappingRM

logical function n_to_ij(n, i, j, ix, iz)
    USE PhreeqcRM
    implicit none
	integer, intent(in) :: n, ix, iz
	integer, intent(inout) :: i, j
    integer :: status
	logical :: return_value

	return_value = .true.

	i = mod(n, ix)
	j = n / ix
	
	if (i .lt. 1 .or. i .gt. ix) then
		status = RM_ErrorMessage(rm_id, "X index out of range")
		return_value = .false.
    endif
    
	if (j .lt. 1 .or. j .gt. iz) then
		status = RM_ErrorMessage(rm_id, "z index out of range")
		return_value = .false.
    endif
    n_to_ij = return_value
	return 
end function n_to_ij

subroutine GetConcentrationsRM(cc)
    use PhreeqcRM
    implicit none
    double precision, dimension(:,:), intent(out) :: cc
    double precision, dimension(:,:), allocatable :: c
    integer :: i, j, nxyz, ncomps, status

    nxyz = RM_GetGridCellCount(rm_id)
    ncomps = RM_GetComponentCount(rm_id)
    allocate(c(nxyz,ncomps))
    status = RM_GetConcentrations(rm_id, c)     
    DO i = 1, nxyz
        do j = 1, ncomps
            cc(j,i) = c(i,j)
        enddo
    enddo  
    deallocate(c)
end subroutine GetConcentrationsRM

subroutine SetConcentrationsRM(cc)
    use PhreeqcRM
    implicit none
    double precision, dimension(:,:), intent(in) :: cc
    double precision, dimension(:,:), allocatable :: c
    double precision, dimension(:), allocatable :: gfw
    integer :: i, j, nxyz, ncomps, status

    nxyz = RM_GetGridCellCount(rm_id)
    ncomps = RM_GetComponentCount(rm_id)
    allocate(c(nxyz,ncomps))  
    allocate(gfw(ncomps))
    status = RM_GetGfw(rm_id, gfw)
    DO i = 1, nxyz
        do j = 1, ncomps
            c(i,j) = cc(j,i)
            if (j .eq. 1) then
                c(i,j) = 1000.0 / gfw(1)
            endif
        enddo
    enddo  
    status = RM_SetConcentrations(rm_id, c)  
    deallocate(c)
end subroutine SetConcentrationsRM

end module