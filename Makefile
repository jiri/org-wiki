FILES = ./server.el        \
	./mousetrap.min.js \
	./style.css        \
	./theme.css        \
	./org-wiki-pkg.el

package:
	mkdir -p "org-wiki-0.1"
	cp $(FILES) org-wiki-0.1/
	tar -cvf org-wiki-0.1.tar org-wiki-0.1/*

clean:
	rm -rf org-wiki-0.1/
	rm -f  org-wiki-0.1.tar
