slides.pdf: slides.org
	./org-files-to-pdf.el slides.org

force:
	rm slides.pdf
	make slides.pdf

clean:
	rm -f *.tex *.pdf *.tex~
	rm -rf _minted-*
	rm -rf .hypothesis

manual:
	pdflatex -shell-escape slides.tex

skim: slides.org
	touch skim
	./org-files-to-pdf.el slides.org
	cp slides.pdf slides-skim.pdf

watch:
	make skim
	open -a skim slides-skim.pdf
	watch -n 1 make skim
