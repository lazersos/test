!-----------------------------------------------------------------------
!     Subroutine:    stelltran_init
!     Authors:       S. Lazerson (lazerson@pppl.gov)
!     Date:          08/21/2015
!     Description:   This routine is for reading in the input files.
!-----------------------------------------------------------------------
      SUBROUTINE stelltran_init
!-----------------------------------------------------------------------
!     Libraries
!-----------------------------------------------------------------------
      USE stel_kinds, ONLY: rprec
      USE stelltran_input_mod
      USE stelltran_runtime
!-----------------------------------------------------------------------
!     Local Variables
!          ier            Local Error flag
!-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER :: ier
!-----------------------------------------------------------------------
!     Begin Subroutine
!-----------------------------------------------------------------------
      ! First read the STELLTRAN_INPUT namelist from the input file
      ier = 0
      CALL read_stelltran_input(id_string,ier)
      
      ! Now initalize the equilibrium calculations
      SELECT CASE(TRIM(equil_type))
         CASE('vmec2000')
            id_string = id_string(7:LEN(id_string))
            ictrl(1) = restart_flag + readin_flag + reset_jacdt_flag
            ictrl(2) = 0
            ictrl(3) = 50
            ictrl(4) = 0
            ictrl(5) = myid
            CALL runvmec(ictrl,id_string,.false.,'')
      END SELECT
      
      ! Now read the timeslice info
      CALL stelltran_read_database
      	      
      RETURN
!-----------------------------------------------------------------------
!     End Subroutine
!-----------------------------------------------------------------------    
      END SUBROUTINE stelltran_init
