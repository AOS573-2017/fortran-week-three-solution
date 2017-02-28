MODULE thermo
CONTAINS

SUBROUTINE pottemp(TK, pPa, thetaK)
IMPLICIT NONE
! by Bucky Badger
! This subroutine calculates the potential temperature at
!  a pressure level with a given temperature.
!
! Inputs:
!  TK - temperature at a level in Kelvin
!  pPa - pressure at a level in Pascals
!
! Outputs:
!  thetaK - potential temperature in Kelvin
REAL :: TK, pPA, thetaK
REAL, PARAMETER :: Rd=287.0      ! Gas constant
REAL, PARAMETER :: cp=1004.0     ! Specific heat isobaric
REAL, PARAMETER :: p0=1000000.0  ! Reference pressure

thetaK = Tk * ( p0 / pPa ) ** ( Rd / cp )

END SUBROUTINE pottemp


SUBROUTINE evap(rhov, Tk, e)
IMPLICIT NONE
! by Bucky Badger
! This subroutine Calculates the vapor pressure for a
!  given temperature and density of water vapor.
!
! Inputs:
!  rhov - density of water vapor in kg/m**3
!  Tk - air temperature in Kelvin
!
! Outputs:
!  e - vapor pressure in Pascals
REAL :: rhov, Tk, e
REAL, PARAMETER :: Rv=461.5     ! Gas constant

e = rhov * Rv * Tk

END SUBROUTINE evap


SUBROUTINE evapsat(Tk, es)
IMPLICIT NONE
! by Bucky Badger
! This subroutine calculates the *saturation* vapor
!  pressure for a given temperature.
!
! Inputs:
!  Tk - air temperature in Kelvin
!
! Outputs:
!  es - saturation vapor pressure in Pascals
REAL :: Tk, Tc, es
REAL, PARAMETER :: a=611.2
REAL, PARAMETER :: b=17.67
REAL, PARAMETER :: c=243.5

Tc = Tk - 273.15 ! Converting input Kelvin to Celsius

es = a * EXP( (b * Tc) / (Tc + c) )

END SUBROUTINE evapsat


SUBROUTINE stdatmo(Tklevel, pstd)
IMPLICIT NONE
! by Bucky Badger
! This subroutine calculates the pressure at a
!  given temperature level for the standard atmosphere.
!
! Inputs:
!  Tklevel - temperature at a given level
!
! Outputs:
!  pstd - standard pressure at that level
REAL :: Tklevel, pstd
REAL, PARAMETER :: Tk0=288      ! Reference temperature in Kelvin
REAL, PARAMETER :: p0=1013200   ! Reference pressure in Pascals
REAL, PARAMETER :: g=9.81       ! Gravitational acceleration in m/s**2
REAL, PARAMETER :: Rd=287.0     ! Gas constant
REAL, PARAMETER :: Lapse=6.5E-3 ! Environmental lapse rate

pstd = p0 * (Tklevel / Tk0) ** ( g / (Rd * Lapse) )

END SUBROUTINE stdatmo

SUBROUTINE rh(e, es, rhpt)
IMPLICIT NONE
! by Bucky Badger
! This subroutine calculates relative humidity.
!
! Inputs:
!  e - environmental vapor pressure in Pascals
!  es - saturation vapor pressure in Pascals
!
! Outputs:
!  rhpt - relative humidity in percent
REAL :: e, es, rhpt

rhpt = (e / es) * 100.0

END SUBROUTINE rh


END MODULE thermo
