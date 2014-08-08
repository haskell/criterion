bootstrap := bootstrap-3.1.1-dist

files := index.html tutorial.html
deps = bootstrap-custom.css background.jpg fibber-screenshot.png fibber.html \
	report.html
install := $(files) $(deps)
destdir := $(HOME)/public_html/criterion

all: $(files)

install: $(files)
	-mkdir -p $(destdir)
	cp -a $(install) $(destdir)
	cp -a $(bootstrap) $(destdir)
	-chcon -R -t httpd_sys_content_t $(destdir)

%.html: %.md template.html $(deps)
	pandoc $< -o $@ --smart --template template.html \
	  --css $(bootstrap)/css/bootstrap.css \
	  --css bootstrap-custom.css \
	  --toc --toc-depth 2

clean:
	-rm -f $(files)
