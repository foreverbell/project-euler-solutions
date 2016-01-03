GHC = ghc
GHC_FLAGS = -threaded -O2 -fllvm -W -fwarn-tabs

SOURCE_FILES := $(wildcard Src/*.hs)
OBJECT_FILES := $(patsubst Src/%.hs, Build/%, $(SOURCE_FILES))

all: $(OBJECT_FILES)
	  
Build/%: Src/%.hs
	$(GHC) -iLib -o $@ $(GHC_FLAGS) $< 

%: Build/%
	

clean:
	rm Src/*.o
	rm Src/*.hi
	rm $(OBJECT_FILES)

.PHONY: clean
