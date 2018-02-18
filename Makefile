#### edition de liens

prog : pb_reel.o mod_TER.o 
	gfortran -o exe pb_reel.o mod_TER.o 

#### compilation

pb_reel.o : pb_reel.f90 mod_TER.o 
	gfortran -c pb_reel.f90
mod_TER.o : mod_TER.f90 
	gfortran -c mod_TER.f90
