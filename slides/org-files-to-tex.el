#!/usr/bin/env emacs --script
;; Emacs script for exporting all org files in current directory to pdf
;; using LaTeX and beamer.
;
;; Author: Sam Sinayoko, Hans Fangohr, 27/12/2015, 3/12/2016
;; Email: s.sinayoko@soton.ac.uk
;; Date: 05/10/2014

(require 'ox-beamer)
(require 'ox-latex)

;; Define an interactive function for easy testing
(defun org-beamer-export-to-pdf-directory (files)
  "Export all files to latex"
  (interactive "Export org files to tex")
  (save-excursion
    (let ((org-files-lst ))
      (dolist (org-file files)
        (message "*** Exporting file %s ***" org-file)
        (find-file org-file)
        (org-beamer-export-to-latex)
        (kill-buffer)))))

;; Make the code blocks look nicer
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

;; style decision for source code blocks
(setq org-latex-minted-options
      '(("bgcolor" "white") ("frame" "lines")))


;; the next section allows to add :ignoreheading: to section headers,
;; and the heading will be removed in the latex output, but the section
;; itself be included.
;;
;; This is useful to 'label' paragraphs or sections to draft a document
;; while not wanting to reveal that label/title in the final version to the
;; reader.
(defun sa-ignore-headline (contents backend info)
  "Ignore headlines with tag `ignoreheading'."
  ;;(message "*** debug: working on ignoreheading")
  (when (and (org-export-derived-backend-p backend 'latex 'html 'ascii)
             (string-match "\\(\\`.*\\)ignoreheading\\(.*\n\\)"
                           (downcase contents)))
                                        ;(replace-match "\\1\\2" nil nil contents)  ;; remove only the tag ":ignoreheading:" but keep the rest of the headline
    (replace-match "" nil nil contents)        ;; remove entire headline
    (message "*** replacing header")
    ))
(add-to-list 'org-export-filter-headline-functions 'sa-ignore-headline)
;; Note: This ^doesn't seem to work at the moment; needs fixing. Dec 2016

;; Use utf8x for LaTeX export to access more unicode characters
(setq org-latex-inputenc-alist '(("utf8" . "utf8x")))

;; Export all org files given on the command line
(org-beamer-export-to-pdf-directory argv)



;; Currently unused but potentially useful?
;;
;; Don't evaluate the blocks
;; (all the results must evaluated beforehand and the results stored in the org file)
;; (setq org-export-babel-evaluate nil)

;; Add the environment "onlyenv" to display something only on certain slides
;;(add-to-list 'org-beamer-environments-extra
;;             '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}"))
