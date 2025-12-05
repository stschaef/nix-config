;;; early-init.el --- Early initialization

;; Prevent automatic package initialization at startup
;; This stops Emacs from calling package-initialize before init.el
(setq package-enable-at-startup nil)

;; Set network timeouts early
(setq url-queue-timeout 10)
(setq url-http-attempt-keepalives nil)

