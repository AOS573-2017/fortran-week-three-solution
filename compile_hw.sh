#!/bin/sh

gfortran -c thermo.f90
gfortran thermo.o homework.f90 -o hw
echo "Compilation script completed." 
