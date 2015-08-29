!-----------------------------------------------------------------------
!     Module:        stelltran_data
!     Authors:       S. Lazerson (lazerson@pppl.gov)
!     Date:          08/21/2015
!     Description:   This module contains various parameters related to
!                    the code parameters.
!-----------------------------------------------------------------------
      MODULE stelltran_data
!-----------------------------------------------------------------------
!     Libraries
!-----------------------------------------------------------------------
      USE stel_kinds, ONLY: rprec
      USE EZspline_obj
!-----------------------------------------------------------------------
!     Module Variables
!          ntimesteps   Number of timesteps in database file
!          
!          te_s         Electron temperature spline knot locations
!          ti_s         Ion temperature spline knot locations
!          ne_s         Electron density spline knot locations
!          zeff_s       Z-effective spline knot locations
!          te_f         Electron temperautre spline knots values
!          ti_f         Ion temperautre spline knots values
!          ne_f         Electron density spline knots values
!          zeff_f       Z-effective spline knots values
!----------------------------------------------------------------------
      IMPLICIT NONE
      
      INTEGER :: ntimesteps, nte_spline, nti_spline, nne_spline, nzeff_spline
      REAL(rprec), ALLOCATABLE :: te_s(:), ti_s(:), ne_s(:), zeff_s(:)
      REAL(rprec), ALLOCATABLE :: te_f(:,:), ti_f(:,:), ne_f(:,:), zeff_f(:,:)
      TYPE(EZspline1_r8) :: te_spl, ti_spl, ne_spl, zeff_spl
      
      END MODULE stelltran_data
