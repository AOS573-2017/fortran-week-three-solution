PROGRAM homework
! by ______
! This program will use your thermo module, call a few subroutines
! or functions, and expect them to be compatible.
USE THERMO
IMPLICIT NONE

REAL :: temp1K, press1hPa, theta1pa
REAL :: rhokgmmm, temp2K, e2Pa
REAL :: temp3K, press3Pa
REAL :: temp4K, press4Pa
REAL :: e5Pa, es5Pa, rh5per

! Converted variables
REAL :: press1Pa

temp1K = 230.0       ! Temperature in Kelvin
press1hPa = 800.0    ! Pressure in hectoPascals
press1Pa = press1hpa * 1000.0  !! Converted pressure to Pa for function
CALL POTTEMP(temp1K, press1Pa, theta1pa)
PRINT *, 'The potential temperature with ', temp1K, ' and ', press1hPa, &
 'is: ', theta1pa

rhokgmmm = 0.4       ! Density in kg per cubic meter
temp2K = 200.0       ! Temperature in Kelvin
CALL EVAP(rhokgmmm, temp2K, e2Pa)
PRINT *, 'The vapor pressure with ', rhokgmmm, ' and ', temp2K, &
 ' is: ', e2Pa

temp3K = 200.0       ! Temperature in Kelvin
CALL EVAPSAT(temp3K, press3Pa)
PRINT *, 'The saturation vapor pressure with ', temp3K, ' is: ', press3Pa

temp4K = 230.0       ! Temperature in Kelvin
CALL STDATMO(temp4K, press4Pa)
PRINT *, 'The standard atmospheric pressure with ', temp4K ,' is: ', press4Pa

e5Pa = 36920.0       ! Vapor pressure in Pascals
es5Pa = 50000.0      ! Saturation vapor pressure in Pascals
CALL RH(e5Pa, es5Pa, rh5per)
PRINT *, 'The relative humidity with ', e5Pa, ' and ', es5Pa, ' is: ', rh5per

END PROGRAM
