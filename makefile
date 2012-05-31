all: test.scm html.scm tag-defs.scm
	@rm -f /home/kylcarte/public_html/test.{php,css}
	/home/kylcarte/scripts/scm2html $< /home/kylcarte/public_html/test.php \
		/home/kylcarte/public_html/test.css

edit:
	vim /home/kylcarte/public_html/test.php
