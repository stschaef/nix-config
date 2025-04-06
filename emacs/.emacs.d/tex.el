(use-package auctex)

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\$" . latex-mode)
  :custom
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method 'synctex)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-electric-math (cons "$" "$"))
  (LaTeX-electric-left-right-brace t)
  (reftex-plug-into-AUCTeX t)
  (setq TeX-view-program-list
      '(("Skim" "/run/current-system/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  (setq TeX-view-program-selection '((output-pdf "Skim")))
  (TeX-source-correlate-start-server t)
  (TeX-master nil)
  )

(use-package auctex-latexmk
  :ensure t
  :config (auctex-latexmk-setup)
        (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package evil-tex)

(add-hook 'LaTeX-mode-hook #'evil-tex-mode)

(use-package company-auctex
  :ensure t
  :init (company-auctex-init))
(use-package company-reftex :ensure t)

(defun my-latex-mode-setup ()
  (setq-local company-backends
              (append '((company-math-symbols-latex company-latex-commands))
                      company-backends)))
(use-package company-math
  :ensure t
  :init (add-hook 'Tex-mode-hook 'my-latex-mode-setup))

(setq TeX-show-compilation nil)
