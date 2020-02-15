GHC = ghc
GHC_FLAGS = -threaded -O2 -fllvm -W -fwarn-tabs

SOURCE_FILES := $(wildcard Src/*.hs)
OBJECT_FILES := $(patsubst Src/%.hs, Build/%, $(SOURCE_FILES))

all: $(OBJECT_FILES)
	  
build/%: src/%.hs
	$(GHC) -iLib -o $@ $(GHC_FLAGS) $< 

%: build/%
	

clean:
	rm src/*.o
	rm src/*.hi
	rm $(OBJECT_FILES)

.PHONY: clean
