Module vs2dt_rm
#ifdef USE_MPI    
  INCLUDE 'mpif.h'
#endif
    integer :: rm_id
    integer :: nthreads = -1, nxyz, ncomps
    integer :: status, mpi_myself, mpi_tasks
    integer, dimension(:), allocatable :: forward1
    logical :: solute_rm
    logical :: RM_OK = .TRUE. 
 
        INTERFACE
            SUBROUTINE FH_SetPointers(x, z, x_index, z_index, ic1_reordered, theta, forward1) &
                BIND(C, NAME='FH_SetPointers')
                USE ISO_C_BINDING
                IMPLICIT NONE
                REAL(KIND=C_DOUBLE), INTENT(in) :: x, z, theta
                INTEGER(KIND=C_INT), INTENT(in) :: x_index, z_index, ic1_reordered, forward1
            END SUBROUTINE FH_SetPointers
                
            SUBROUTINE FH_WriteFiles(rm_id, xz_on, obs_on, xz_mask, obs_mask) &
                BIND(C, NAME='FH_WriteFiles')
                USE ISO_C_BINDING
                IMPLICIT NONE
                INTEGER(KIND=C_INT), INTENT(in) :: rm_id, xz_on, obs_on, xz_mask, obs_mask
                END SUBROUTINE FH_WriteFiles
                
            SUBROUTINE FH_FinalizeFiles() &
                BIND(C, NAME='FH_FinalizeFiles')
                USE ISO_C_BINDING
                IMPLICIT NONE
            END SUBROUTINE FH_FinalizeFiles 

            INTEGER(KIND=C_INT) FUNCTION FH_GetProcessorCount() &
                BIND(C, NAME='FH_GetProcessorCount')
                USE ISO_C_BINDING
                IMPLICIT NONE
            END FUNCTION FH_GetProcessorCount 
                
        END INTERFACE  
        
    CONTAINS  
    
    SUBROUTINE CreateRM(solute, nnodes,  prefix, databasefile, chemfile, nsol)
    USE PhreeqcRM
    IMPLICIT NONE
    COMMON/ISPAC/NLY,NLYY,NXR,NXRR,XXXNNODES
    integer NLY,NLYY,NXR,NXRR,XXXNNODES
    logical, intent(in) :: solute
    integer, intent(in) :: nnodes
    character(*), intent(in) :: prefix, databasefile, chemfile
    integer, intent(out) :: nsol

    SAVE 
    INTEGER i, status, nproc, ncells
    CHARACTER*32 string
    
    ! ... make a reaction module, makes instances of IPhreeqc and IPhreeqcPhast with same rm_id
    solute_rm = solute
    nsol = 0
    !nthreads = 1
#ifdef USE_MPI
    rm_id = RM_Create(NNODES, MPI_COMM_WORLD)
#else
    ncells = (NLY-2)*(NXR-2)
    nproc = FH_GetProcessorCount()
    IF (nthreads < 1) THEN
        nthreads = MIN(ncells, nproc)
    ELSE
        nthreads = MIN(ncells, nthreads)
    END IF
    rm_id = RM_Create(NNODES, nthreads)
#endif    
    status = RM_SetFilePrefix(rm_id, PREFIX)
    status = RM_OpenFiles(rm_id)  
    IF (solute) THEN         
        IF (rm_id.LT.0) THEN
            return
        END IF
        status = RM_SetComponentH2O(rm_id, 1)
        if (status .NE. 0) goto 1000  

        nthreads = RM_GetThreadCount(rm_id)
        !!!status = RM_SetErrorHandlerMode(rm_id, 2)   ! exit
        status = RM_SetPrintChemistryOn(rm_id, 0, 1, 0) 
        if (status .NE. 0) goto 1000 

        status = RM_UseSolutionDensityVolume(rm_id, 1)
        if (status .NE. 0) goto 1000 

        status = RM_LoadDatabase(rm_id, DATABASEFILE)
        if (status .NE. 0) goto 1000

        !... Call phreeqc, find number of components, f1name, chem.dat, f2name, database, f3name, prefix
        status = RM_LogMessage(rm_id, "Initial PHREEQC run.") 
        if (status .NE. 0) goto 1000  

        status = RM_ScreenMessage(rm_id, "Initial PHREEQC run.")  
        if (status .NE. 0) goto 1000   

        status = RM_RunFile(rm_id, 1, 1, 1, CHEMFILE)
        if (status .NE. 0) goto 1000

        string = 'DELETE; -all' 
        status = RM_RunString(rm_id, 1, 0, 1, trim(string))
        if (status .NE. 0) goto 1000  

        status = RM_FindComponents(rm_id)   
        nSol = RM_GetComponentCount(rm_id)
        status = RM_LogMessage(rm_id, "Done with Initial PHREEQC run.")

        status = RM_ScreenMessage(rm_id, "Done with Initial PHREEQC run.")
        return 
1000    CONTINUE
#ifdef USE_MPI
      status = RM_MpiWorkerBreak(rm_id)
#endif
      status = RM_CloseFiles(rm_id)
      status = RM_Destroy(rm_id)
#ifdef USE_MPI
      call MPI_FINALIZE(status)
#endif
        rm_id = -1
    ENDIF
END SUBROUTINE CreateRM
    
SUBROUTINE InitializeRM(cmixfarc, indsol1, indsol2, ic1_reordered)
    USE PhreeqcRM
    IMPLICIT NONE
    INTEGER JSTOP,JFLAG,jflag1
    COMMON/JCON/JSTOP,JFLAG,jflag1
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
        status = RM_SetScreenOn(rm_id, 0)

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
        if (status .ne. 0) goto 1000

        ! ... Make arrays in the correct order
        if (allocated(ic1_reordered)) deallocate(ic1_reordered)
        if (allocated(ic2_reordered)) deallocate(ic2_reordered)
        if (allocated(f1_reordered)) deallocate(f1_reordered)
        ALLOCATE(ic1_reordered(nxyz,7), ic2_reordered(nxyz,7), f1_reordered(nxyz,7),   &
        STAT = a_err)
        IF (a_err /= 0) THEN
            write(6,*) "Array allocation failed: InitializeRM"  
            goto 1000
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
        if (status .ne. 0) goto 1000

        DEALLOCATE (ic2_reordered, f1_reordered, &
        STAT = a_err)
        IF (a_err /= 0) THEN
            write(6,*) "Array deallocation failed: InitializeRM"  
            goto 1000
        ENDIF
    ENDIF        ! ... solute
    return

1000 continue
    JSTOP = 13
    write(6,*) "InitializeRM failed"
    return
END SUBROUTINE InitializeRM
   

subroutine CreateMappingRM(initial_conditions, axes, nx, nz)
    USE PhreeqcRM
    implicit none
    INTEGER JSTOP,JFLAG,jflag1
    COMMON/JCON/JSTOP,JFLAG,jflag1
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
    if ((.not. axes(1)) .and. (.not. axes(2))) then
        status = RM_ErrorMessage(rm_id, "No active coordinate direction.")
        write(6,*)  "No active coordinate direction."
        write(6,*) "Error in CreateMappingRM"
        JSTOP = 14
        return
    endif

    count_chem = ixz
    
    !   allocate space
    allocate(forward1(ixz))

    n = 0;
    ! x and y
    if ((axes(1) .eqv. .true.) .and. (axes(2) .eqv. .true.)) then
        n = 0;
        do i = 1, ixz
            if (initial_conditions(1,i) .ge. 0 .or. initial_conditions(1,i) .le. -100) then
                forward1(i) = n
                n = n + 1
            else
                forward1(i) = -1
            endif
            !write(*,*) "XZforward [",i-1,"] =", forward1(i)
        enddo
        count_chem = n;
    ! x only
    else if ((axes(1) .eqv. .true.) .and. (axes(2) .eqv. .false.)) then
        if (iz .ne. 2) then
            status = RM_ErrorMessage(rm_id, "z direction should contain only three nodes for this 1D problem.")
            write(6,*) "z direction should contain only three nodes for this 1D problem."
            write(6,*) "Error in CreateMappingRM"
            JSTOP = 15
            return
        endif

        n = 0
        do i = 1, ixz
            if (initial_conditions(i,1) .lt. 0 .and. initial_conditions(1,i) .gt. -100) then
                status = RM_ErrorMessage(rm_id, "Can not have inactive cells in a 1D simulation.")
                write(6,*) "Can not have inactive cells in a 1D simulation."
                write(6,*) "Error in CreateMappingRM"
                JSTOP = 16
                return
            endif
            if (jj == 0) then
                forward1(i) = n
                n = n + 1
            else
                forward1(i) = -1
            endif
            !write(*,*) "Xforward [",i-1,"] =",forward1(i)
        enddo
        count_chem = n;
    !  Copy z line
    else if ((axes(1) .eqv. .false.) .and. (axes(2) .eqv. .true.)) then
        n = 0
        do i = 1, ixz
            if (initial_conditions(1,i) .lt. 0 .and. initial_conditions(1,i) > -100) then
                status = RM_ErrorMessage(rm_id, "Can not have inactive cells in a 1D simulation.")
                write(6,*) "Can not have inactive cells in a 1D simulation."
                write(6,*) "Error in CreateMappingRM"
                JSTOP = 16
                return
            endif
            success = n_to_ij(i, ii, jj, nx, nz)
            if (.not. success) then
                write(6,*) "Error in CreateMappingRM: n_to_ij failed."
                JSTOP = 17
                return                
            endif

            if (ii .eq. 0) then
                forward1(i) = n
                n = n + 1
            else
                forward1(i) = -1
            endif
            !write(*,*) "Zforward [",i-1,"] =",forward1(i)
        enddo
        count_chem = n
    endif
    ! pass mapping to RM
    status = RM_CreateMapping(rm_id, forward1)
    if (status .ne. 0) then
        write(6,*) "Error in CreateMappingRM: RM_CreateMapping failed."
        JSTOP = 18
        return
    endif
    !write(*,*) "count_chem =", count_chem
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
        write(6,*) "X index out of range"
        return_value = .false.
    endif
    
    if (j .lt. 1 .or. j .gt. iz) then
        status = RM_ErrorMessage(rm_id, "Z index out of range")
        write(6,*) "Z index out of range"
        return_value = .false.
    endif
    n_to_ij = return_value
    return 
end function n_to_ij

subroutine GetConcentrationsRM(cc)
    use PhreeqcRM
    use trxx, only: nctyp
    implicit none
    INTEGER JSTOP,JFLAG,jflag1
    COMMON/JCON/JSTOP,JFLAG,jflag1
    double precision, dimension(:,:), intent(inout) :: cc
    double precision, dimension(:,:), allocatable :: c
    integer :: i, j, nxyz, ncomps, status

    nxyz = RM_GetGridCellCount(rm_id)
    ncomps = RM_GetComponentCount(rm_id)
    allocate(c(nxyz,ncomps))
    status = RM_GetConcentrations(rm_id, c)                
    if (status .ne. 0) then
        write(6,*) "Error in GetConcentrationsRM: RM_GetConcentrations failed."
        JSTOP = 19
        if (allocated(c)) deallocate(c)
        return                
    endif
    DO i = 1, nxyz
        do j = 1, ncomps
            if (nctyp(i) .ne. 1) then
                cc(j,i) = c(i,j)
            endif
        enddo
    enddo  
    deallocate(c)
end subroutine GetConcentrationsRM

subroutine SetConcentrationsRM(cc)
    use PhreeqcRM
    implicit none
    INTEGER JSTOP,JFLAG,jflag1
    COMMON/JCON/JSTOP,JFLAG,jflag1
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
    if (status .ne. 0) then
        write(6,*) "Error in SetConcentrationsRM: RM_SetConcentrations failed."
        JSTOP = 20                      
    endif
    if (allocated(c)) deallocate(c)
    return 
end subroutine SetConcentrationsRM

end module
