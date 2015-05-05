GHC = ghc
GHC_FLAGS = -O2 -Wall -fno-warn-missing-signatures

SOURCE_FILES := $(foreach d, ./, $(wildcard $(d)*.hs))
OBJECT_FILES := $(patsubst %.hs, %, $(SOURCE_FILES))

all: $(OBJECT_FILES)
	  
%: %.hs
	$(GHC) -o $@ $(GHC_FLAGS) $< 

clean:
	rm *.o
	rm *.hi
	rm $(OBJECT_FILES)
	rm prog.exe
	rm Common/*.o
	rm Common/*.hi

.PHONY: clean
