!-----------------------------------------------------------------------
!     Module:        stelltran_input_mod
!     Authors:       S. Lazerson (lazerson@pppl.gov)
!     Date:          08/21/2015
!     Description:   This module contains the STELLTRAN input namelist
!                    and subroutine which initializes and reads the
!                    STELLTRAN input namelist.
!-----------------------------------------------------------------------
      MODULE stelltran_input_mod
!-----------------------------------------------------------------------
!     Libraries
!-----------------------------------------------------------------------
      USE stel_kinds, ONLY: rprec
      USE stellopt_runtime
      USE safe_open_mod, ONLY: safe_open
      USE vmec_params, ONLY: version_vmec=> version_
!-----------------------------------------------------------------------
!     Module Variables
!         
!-----------------------------------------------------------------------
      IMPLICIT NONE
!DEC$ IF DEFINED (MPI_OPT)
      INCLUDE 'mpif.h'                                                          ! MPI
!DEC$ ENDIF        
!-----------------------------------------------------------------------
!     Input Namelists
!         &optimum
!            equil_type         Equilibrium Code:
!                                  'VMEC2000' (default)
!                                  'SPEC'
!            db_file            Path to timeslice database file
!
!   NOTE:  All varaibles which start with target have an similar
!          varaible starting with sigma which defines the error bars.
!-----------------------------------------------------------------------
      NAMELIST /stelltran_input/ equil_type, db_file, fit_tol
      
!-----------------------------------------------------------------------
!     Subroutines
!         read_stellopt_input:   Reads optimum namelist
!-----------------------------------------------------------------------
      CONTAINS
      
      SUBROUTINE read_stelltran_input(filename, istat, ithread)
      CHARACTER(*), INTENT(in) :: filename
      INTEGER, INTENT(out) :: istat
      INTEGER, INTENT(in) :: ithread
      LOGICAL :: lexist
      INTEGER :: i, iunit, local_master
      ! Initializations
      equil_type      = 'VMEC2000'
      db_file         = 'empty'
      fit_tol         = 0.01
      ! Read name list
      lexist            = .false.
      istat=0
      iunit=12
      INQUIRE(FILE=TRIM(filename),EXIST=lexist)
      IF (.not.lexist) CALL handle_err(FILE_EXIST_ERR,TRIM(filename),istat)
      CALL safe_open(iunit,istat,TRIM(filename),'old','formatted')
      IF (istat /= 0) CALL handle_err(FILE_OPEN_ERR,TRIM(filename),istat)
      READ(iunit,NML=stelltran_input,IOSTAT=istat)
      IF (istat /= 0) CALL handle_err(NAMELIST_READ_ERR,'OPTIMUM in: '//TRIM(filename),istat)
      CALL FLUSH(iunit)
      CLOSE(iunit)
      ! Fix String vars
      equil_type=TRIM(equil_type)
      equil_type=ADJUSTL(equil_type)
      db_file=TRIM(db_file)
      db_file=ADJUSTL(db_file)
      ! Handle Some things
      CALL tolower(equil_type)
      IF ((myid == master) .and. (TRIM(equil_type(1:4)) == 'vmec') ) THEN
         WRITE(6,*)        " Equilibrium calculation provided by: "
         WRITE(6,"(2X,A)") "================================================================================="
         WRITE(6,"(2X,A)") "=========       Variational Moments Equilibrium Code (v "//TRIM(version_vmec)//")           ========="
         WRITE(6,"(2X,A)") "=========                (S. Hirshman, J. Whitson)                      ========="
         WRITE(6,"(2X,A)") "=========         http://vmecwiki.pppl.wikispaces.net/VMEC              ========="
         WRITE(6,"(2X,A)") "================================================================================="
         WRITE(6,*)        "    "
      END IF
      END SUBROUTINE read_stelltran_input
      
      SUBROUTINE write_stelltran_namelist(iunit,istat)
      INTEGER, INTENT(in) :: iunit
      INTEGER, INTENT(in) :: istat
      INTEGER :: ik, n, m, u, v
      CHARACTER(LEN=*), PARAMETER :: outboo  = "(2X,A,1X,'=',1X,L1)"
      CHARACTER(LEN=*), PARAMETER :: outint  = "(2X,A,1X,'=',1X,I0)"
      CHARACTER(LEN=*), PARAMETER :: outflt  = "(2X,A,1X,'=',1X,ES22.12E3)"
      CHARACTER(LEN=*), PARAMETER :: outexp  = "(2X,A,1X,'=',1X,ES22.12E3)"
      CHARACTER(LEN=*), PARAMETER :: outcmp  = "(2x,A,1X,'=','(',i3,',',i3,')')"
      CHARACTER(LEN=*), PARAMETER :: outstr  = "(2X,A,1X,'=',1X,'''',A,'''')"
      CHARACTER(LEN=*), PARAMETER :: onevar  = "(2X,A,1X,'=',1X,L1,2(2X,A,1X,'=',1X,ES22.12E3))"
      CHARACTER(LEN=*), PARAMETER :: vecvar  = "(2X,A,'(',I3.3,')',1X,'=',1X,L1,2(2X,A,'(',I3.3,')',1X,'=',1X,ES22.12E3))"
      WRITE(iunit,'(A)') '&STELLTRAN_INPUT'
      WRITE(iunit,'(A)') '!----------------------------------------------------------------------'
      WRITE(iunit,'(A)') '!       Transport Run Control Parameters'
      WRITE(iunit,'(A)') '!----------------------------------------------------------------------'
      WRITE(iunit,outstr) 'EQUIL_TYPE',TRIM(equil_type)
      WRITE(iunit,outstr) 'EQUIL_TYPE',TRIM(db_file)
      WRITE(iunit,outflt) 'FIT_TOL',fit_tol
      WRITE(iunit,'(A)') '/'
      RETURN
      END SUBROUTINE write_stelltran_namelist
      
      END MODULE stelltran_input_mod
