HC = ghc

ConsiderGUI: Main.hs LuaParser.hs
	$(HC) --make Main -o $@

clean:
	rm -f *.o *.hi ConsiderGUI.exe ConsiderGUI

dist:
	mkdir -p ConsiderGUI-`cat VERSION`
	cp ConsiderGUI.exe *.dll gui.glade LuaParser.hs Main.hs Makefile VERSION GPL-3 README \
		ConsiderGUI-`cat VERSION`
	zip -r ConsiderGUI-`cat VERSION`.zip ConsiderGUI-`cat VERSION`/
