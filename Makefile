IDRIS := idris

build: .PHONY
	$(IDRIS) --build idrisscript.ipkg

install:
	$(IDRIS) --install idrisscript.ipkg

clean: .PHONY
	$(IDRIS) --clean idrisscript.ipkg

rebuild: clean build

.PHONY:
