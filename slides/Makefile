slides.pdf: slides.tex
	xelatex -shell-escape slides.tex

slides.tex: slides.org
	emacs --script ./org-files-to-tex.el slides.org

clean:
	rm -f *.tex *.pdf *.tex~ slides.nav slides.log slides.out slides.snm slides.toc slides.aux
	rm -rf _minted-*

manual:
	xelatex -shell-escape slides.tex

watch:
	@# use this to re-run automatically if slides.tex or slides.org change
	latexmk --xelatex --shell-escape -pvc -view=none slides.tex

help:
	@echo "make slides.pdf -- compile org to tex to pdf"
	@echo "make watch      -- update slides.pdf on any change"
	@echo "make clean      -- delete all derived files"
	@echo "make slides.tex -- update slides.tex from slides.org"
	@echo "make help       -- show this command overview"
	@echo
	@echo "Investigate the Makefile for more."
