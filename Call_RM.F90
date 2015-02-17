Module vs2dt_rm
    integer :: rm_id
    integer :: nthreads, nxyz, ncomps
    integer, dimension(:), allocatable :: forward1
    logical :: solute_rm
    
    CONTAINS  
    
    SUBROUTINE CreateRM(solute, nnodes,  prefix, databasefile, chemfile, nsol)
    USE PhreeqcRM
    IMPLICIT NONE
    logical, intent(in) :: solute
    integer, intent(in) :: nnodes
    character(*), intent(in) :: prefix, databasefile, chemfile
    integer, intent(out) :: nsol

    SAVE 
    INTEGER i, status
    CHARACTER*32 string
    
    ! ... make a reaction module, makes instances of IPhreeqc and IPhreeqcPhast with same rm_id
    solute_rm = solute
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
    
SUBROUTINE InitializeRM(cmixfarc, indsol1, indsol2, ic1_reordered)
    USE PhreeqcRM
    IMPLICIT NONE
    DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE, intent(in) :: cmixfarc
    INTEGER, DIMENSION(:,:), ALLOCATABLE, intent(in) :: indsol1, indsol2
    INTEGER, DIMENSION(:,:), ALLOCATABLE, intent(out) :: ic1_reordered
    SAVE
    INTEGER a_err, i, j, status
    INTEGER ipartition_uz_solids
    INTEGER, DIMENSION(:,:), ALLOCATABLE :: ic2_reordered
    DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: f1_reordered
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: temp
    DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: c
    
    nxyz = RM_GetGridCellCount(rm_id)
    ncomps = RM_GetComponentCount(rm_id)
 
    IF(solute_rm) THEN

        ! ... Send data to threads or workers
        ! 1 mg/L, 2 mol/L, 3 kg/kgs
        status = RM_SetUnitsSolution(rm_id, 1)
        
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

        ! rebalance
        status = RM_SetRebalanceFraction(rm_id, 0.0d0)
        status = RM_SetRebalanceByCell(rm_id, 1)

        ! ... Define mapping from 3D domain to chemistry
        status = RM_CreateMapping(rm_id, forward1)      

        ! ... Make arrays in the correct order
        ALLOCATE(ic1_reordered(nxyz,7), ic2_reordered(nxyz,7), f1_reordered(nxyz,7),   &
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
    ENDIF        ! ... solute
END SUBROUTINE InitializeRM
   

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
    integer :: i, j, nxyz, ncomps, status

    nxyz = RM_GetGridCellCount(rm_id)
    ncomps = RM_GetComponentCount(rm_id)
    allocate(c(nxyz,ncomps))  
    DO i = 1, nxyz
        do j = 1, ncomps
            c(i,j) = cc(j,i)
        enddo
    enddo  
    status = RM_SetConcentrations(rm_id, c)  
    deallocate(c)
end subroutine SetConcentrationsRM

end module