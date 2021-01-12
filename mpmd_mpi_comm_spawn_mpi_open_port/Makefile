F90     = mpiifort
RM      = rm -f
FCFLAGS = -O2 -fp-model=precise

BINFAC = factorial.ex
BINSUM = sum.ex 
BINMAS = master.ex
OBJCOM = para_range.o
OBJFAC = $(OBJCOM) factorial.o
OBJSUM = $(OBJCOM) sum.o 
OBJMAS = $(OBJCOM) master.o

all: $(BINFAC) $(BINSUM) $(BINMAS)

%.o: %.f90
	$(F90) $(FCFLAGS) -c $^

$(BINFAC): $(OBJFAC)
	$(F90) $(FCFLAGS) -o $@ $(OBJFAC)

$(BINSUM): $(OBJSUM)
	$(F90) $(FCFLAGS) -o $@ $(OBJSUM)

$(BINMAS): $(OBJMAS)
	$(F90) $(FCFLAGS) -o $@ $(OBJMAS)

clean:
	$(RM) $(BINFAC) $(BINSUM) $(BINMAS) $(OBJFAC) $(OBJSUM) $(OBJMAS)
