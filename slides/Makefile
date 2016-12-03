slides.pdf: slides.tex
	xelatex -shell-escape slides.tex


slides.tex: slides.org
	#./org-files-to-pdf.el slides.org
	emacs --script ./org-files-to-tex.el *.org

force:
	rm -f slides.pdf slides.tex
	make slides.pdf
	make manual
	make manual

clean:
	rm -f *.tex *.pdf *.tex~
	rm -rf _minted-*
	rm -rf .hypothesis

manual:
	xelatex -shell-escape slides.tex

skim: slides.org
	touch skim
	make slides.pdf

watch:
	make skim
	open -a skim slides.pdf
	watch -n 1 make skim
