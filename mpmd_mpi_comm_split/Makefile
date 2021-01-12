F90     = mpiifort
RM      = rm -f
FCFLAGS = -O2 -fp-model=precise

BINFAC = factorial.ex
BINSUM = sum.ex
OBJCOM = para_range.o mpmd.o
OBJSUM = $(OBJCOM) sum.o
OBJFAC = $(OBJCOM) factorial.o
INC = mpmd.mod

all: $(BINSUM) $(BINFAC)

%.o: %.f90
	$(F90) $(FCFLAGS) -c $^

$(BINSUM): $(OBJSUM)
	$(F90) $(FCFLAGS) -o $@ $(OBJSUM) -module $(INC)

$(BINFAC): $(OBJFAC)
	$(F90) $(FCFLAGS) -o $@ $(OBJFAC) -module $(INC)

clean:
	$(RM) $(BINFAC) $(BINSUM) $(OBJSUM) $(OBJFAC) $(INC)
