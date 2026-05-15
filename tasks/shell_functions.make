SHELL_FUNCTIONS_MAKE := $(lastword $(MAKEFILE_LIST))
TASKS_ROOT := $(patsubst %/,%,$(dir $(SHELL_FUNCTIONS_MAKE)))

FUNCTIONS = $(shell cat $(TASKS_ROOT)/shell_functions.sh)
STATA = @$(FUNCTIONS); stata_with_flag
R = @$(FUNCTIONS); R_pc_and_slurm

ifneq (,$(findstring n,$(MAKEFLAGS)))
STATA := STATA
R := R
endif
