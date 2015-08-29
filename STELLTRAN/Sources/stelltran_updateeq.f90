!-----------------------------------------------------------------------
!     Subroutine:    stelltran_updateeq
!     Authors:       S. Lazerson (lazerson@pppl.gov)
!     Date:          08/22/2015
!     Description:   This routine updates the equilibrium
!-----------------------------------------------------------------------
      SUBROUTINE stelltran_updateeq(itime)
!-----------------------------------------------------------------------
!     Libraries
!-----------------------------------------------------------------------
      USE stel_kinds, ONLY: rprec
      USE stelltran_runtime
      USE stelltran_data
      USE vmec_input, ONLY: am_type, ac_type, 
      USE EZspline_obj
!-----------------------------------------------------------------------
!     Local Variables
!        ier          Error flag
!-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: itime
      INTEGER ::  ier
!-----------------------------------------------------------------------
!     Begin Subroutine
!-----------------------------------------------------------------------
      ! Setup the profiles
      IF (EZspline_allocated(te_spl)) CALL EZspline_free(te_spl,ier)
      IF (EZspline_allocated(ne_spl)) CALL EZspline_free(ne_spl,ier)
      IF (EZspline_allocated(ti_spl)) CALL EZspline_free(ti_spl,ier)
      IF (EZspline_allocated(zeff_spl)) CALL EZspline_free(zeff_spl,ier)
      CALL EZspline_init(te_spl,nte_spline,bcs0,ier)
      CALL EZspline_init(ti_spl,nti_spline,bcs0,ier)
      CALL EZspline_init(ne_spl,nne_spline,bcs0,ier)
      CALL EZspline_init(zeff_spl,zeff_spline,bcs0,ier)
      te_spl%x1 = te_s
      ti_spl%x1 = ti_s
      ne_spl%x1 = ne_s
      zeff_spl%x1 = zeff_s
      te_spl%isHermite = 1
      ti_spl%isHermite = 1
      ne_spl%isHermite = 1
      zeff_spl%isHermite = 1
      CALL EZspline_setup(te_spl,te_f(itime,:),ier)
      CALL EZspline_setup(ne_spl,ne_f(itime,:),ier)
      CALL EZspline_setup(ti_spl,ti_f(itime,:),ier)
      CALL EZspline_setup(zeff_spl,zeff_f(itime,:),ier)
	
      ! Handle setting up each equilibrium
      SELECT CASE (TRIM(equil_type))
         CASE('vmec2000')
            ! Update the input vars
            !CALL update_vmec_input
            
            !Update the internal vars 
            !CALL stelltran_reinit_vmec
      END SELECT
!-----------------------------------------------------------------------
!     End Subroutine
!-----------------------------------------------------------------------    
      END SUBROUTINE stelltran_updateeq
