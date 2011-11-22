QUICKLISP=$(HOME)/quicklisp

ghprojects: package.lisp ghprojects.lisp atom-template.xml
	buildapp --asdf-path .  \
		--output ghprojects \
		--manifest-file $(QUICKLISP)/manifest.txt \
		--manifest-file $(QUICKLISP)/local-projects/system-index.txt \
		--load-system ghprojects \
		--entry ghprojects:main

clean:
	rm -f ghprojects

install: ghprojects
	install -c -m 555 ghprojects /usr/local/bin/ghprojects
