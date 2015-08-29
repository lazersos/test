!-----------------------------------------------------------------------
!     Subroutine:    stelltran_cycle
!     Authors:       S. Lazerson (lazerson@pppl.gov)
!     Date:          08/21/2015
!     Description:   This is the main cycle routine for the transport
!                    simulation.
!-----------------------------------------------------------------------
      SUBROUTINE stelltran_cycle
!-----------------------------------------------------------------------
!     Libraries
!-----------------------------------------------------------------------
      USE stel_kinds, ONLY: rprec
      USE stelltran_runtime
!-----------------------------------------------------------------------
!     Local Variables
!          lconsistent    Logical to determine equilibrium consistency
!          itime          Timeslice index
!-----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL :: lconsistent
      INTEGER :: itime
!-----------------------------------------------------------------------
!     Begin Subroutine
!-----------------------------------------------------------------------
      DO itime=1, ntimesteps
         ! Update the equilibrium
         CALL stelltran_updateeq(itime)
         
         ! Set some pass parameters
         lconsistent = .FALSE.
         lfirst_pass = .TRUE.
         
         ! Iterate till the profiles are consistent
         DO WHILE (.not. lconsistent)
            ! Calculate an equilibrium
            CALL stelltran_runeq
            
            ! Calculate basic stuff (volume, etc.)
            !CALL stelltran_basic
            
            ! Calculate and check current
            !CALL stelltran_current
            	  
            ! Calculate and check species profiles
            !CALL stelltran_thermal
            	    
            ! Check the consistency of the equilibirum
            !CALL stelltran_eqconsistency(lconsistent)
         END DO
         
         ! Save the timestep
         ! CALL stelltran_save
         	 
         ! Update the timestep
         ! CALL stelltran_timestep(itime)
         	 
      END DO
!-----------------------------------------------------------------------
!     End Subroutine
!-----------------------------------------------------------------------    
      END SUBROUTINE stelltran_cycle
