!-----------------------------------------------------------------------
!     Subroutine:    stelltran_read_database
!     Authors:       S. Lazerson (lazerson@pppl.gov)
!     Date:          08/21/2015
!     Description:   This routine sets up the arrays of timeslice
!                    profile info.
!-----------------------------------------------------------------------
      SUBROUTINE stelltran_read_database
!-----------------------------------------------------------------------
!     Libraries
!-----------------------------------------------------------------------
      USE stel_kinds, ONLY: rprec
      USE stelltran_runtime
      USE stelltran_data
!-----------------------------------------------------------------------
!     Local Variables
!          lexist         Logical flag for file existance
!          ier            Local Error flag
!          iunit          Iunit for database file
!-----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL :: lexist
      INTEGER :: ier, iunit
      
      LOGICAL :: ldebug = .TRUE.
!-----------------------------------------------------------------------
!     Begin Subroutine
!-----------------------------------------------------------------------
      
      ! Check for database file
      INQUIRE(FILE=TRIM(db_file),EXIST=lexist)
      IF (.not.lexist) CALL handle_err(FILE_EXIST_ERR,TRIM(db_file),istat)
      
      ! Open the database file
      CALL safe_open(iunit,ier,TRIM(db_file),'old','formatted')
      
      ! Close the database file
      CLOSE(iunit)
      
      ! For now we mock up some profiles
      IF (ldebug) THEN
         IF (ALLOCATED(te_s)) DEALLOCATE(te_s,te_f)
         IF (ALLOCATED(ne_s)) DEALLOCATE(ne_s,ne_f)
         IF (ALLOCATED(ti_s)) DEALLOCATE(ti_s,ti_f)
         IF (ALLOCATED(zeff_s)) DEALLOCATE(zeff_s,zeff_f)
         ntimesteps = 5
         nte_spline = 5; nne_spline=5; nti_spline=5; nzeff_spline=5;
         ALLOCATE(te_s(nte_spline), ti_s(nti_spline), &
                  ne_s(nne_spline), zeff_s(nzeff_spline))
         ALLOCATE(te_f(ntimesteps,nte_spline),& 
                  ti_f(ntimesteps,nti_spline),&
                  ne_f(ntimesteps,nne_spline),&
                  zeff_f(ntimesteps,nzeff_spline))
         te_s(1) = 0
         te_s(2) = 0.25
         te_s(3) = 0.50
         te_s(4) = 0.75
         te_s(5) = 1
         ne_s    = te_s
         ti_s    = te_s
         zeff_s  = te_s
         te_f(1,1)=0; te_f(1,2)=0.3; te_f(1,3)=0.6; te_f(1,4)=0.8; te_f(1,5)=1;
         ti_f(1,1)=0; ti_f(1,2)=0.3; ti_f(1,3)=0.6; ti_f(1,4)=0.8; ti_f(1,5)=1;
         ne_f(1,1)=0; ne_f(1,2)=0.7; ne_f(1,3)=0.8; ne_f(1,4)=0.9; ne_f(1,5)=1;
         DO i = 2, ntimesteps
            te_f(i,:) = 1.5*te_f(i-1,:);
            ne_f(i,:) = 1.5*ne_f(i-1,:);
            ti_f(i,:) = 1.5*ti_f(i-1,:);
         END DO
         zeff_f(:,:) = 1.0
         
      END IF
      
      RETURN
!-----------------------------------------------------------------------
!     End Subroutine
!-----------------------------------------------------------------------    
      END SUBROUTINE stelltran_read_database
