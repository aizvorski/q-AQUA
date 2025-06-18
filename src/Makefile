#Generic makefile for fortran90

FC := ifort 
# serial
FFLAGS := -r8 -O
# parallel  
#LIBS := -mkl=parallel
#FFLAGS := -r8 -O3 -qopenmp 

OBJ := \
constants.o\
pot_monomer_mod.o\
pot_monomer.o\
ltp2011_chg.o\
ltp2011_dms.o\
bemsa2b_v8.o\
bemsa3b_v1.o\
bemsa3b_v9.o\
bemsa4b_v3.o\
pes_shell.o\

OBJPOT := \
getpot.o


%.o : %.f90
	$(FC) -c $(FFLAGS) $< 

getpot.x :  $(OBJ) $(OBJPOT)
	$(FC) -o $@  $^ $(FFLAGS) 
clean :
	rm *.o *:.mod 
