;;; tex.el --- LaTeX configuration -*- lexical-binding: t; -*-

;;; ============================================================
;;; AUCTeX (defer until .tex files)
;;; ============================================================

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :custom
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method 'synctex)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-electric-math (cons "$" "$"))
  (LaTeX-electric-left-right-brace t)
  (reftex-plug-into-AUCTeX t)
  (TeX-source-correlate-start-server t)
  (TeX-master nil)
  :config
  (setq TeX-view-program-list
        '(("Skim" "/run/current-system/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  (setq TeX-view-program-selection '((output-pdf "Skim")))
  (setq TeX-show-compilation nil))

(use-package auctex-latexmk
  :after tex
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

;;; ============================================================
;;; Evil-tex (defer until LaTeX-mode)
;;; ============================================================

(use-package evil-tex
  :hook (LaTeX-mode . evil-tex-mode))

;;; ============================================================
;;; Company backends for LaTeX (defer until LaTeX-mode)
;;; ============================================================

(use-package company-auctex
  :after (tex company)
  :config
  (company-auctex-init))

(use-package company-reftex
  :after (tex company))

(use-package company-math
  :after (tex company)
  :hook (LaTeX-mode . my-latex-mode-setup)
  :config
  (defun my-latex-mode-setup ()
    (setq-local company-backends
                (append '((company-math-symbols-latex company-latex-commands))
                        company-backends))))

;;; tex.el ends here
