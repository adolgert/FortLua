# module swap pgi intel
# module add lua
FFLAGS=-g
INCLUDES=-I$(LUA_INC)
LIBS=-L$(LUA_LIB) -llua


# This cute snippet stops the makefile if the variables listed in MODVARS aren't defined.
# MODVARS := IFC_DIR TACC_LUA_INC
LOSTMODS := $(foreach modvar,$(MODVARS),$(if $(subst undefined,,$(origin $(value modvar))),,$(modvar)) )
UNLOADCNT := $(words $(LOSTMODS))
ifneq ($(UNLOADCNT),0)
  $(error Necessary modules not loaded: $(LOSTMODS))
endif



default: calculate

calculate: config.f90 program.f90
	ifort $(FFLAGS) $^ $(INCLUDES) $(LIBS) -o $@

clean:
	rm -f *.o interp
	rm -f calculate
	rm -f *~
