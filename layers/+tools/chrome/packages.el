;;; packages.el --- Chrome Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Ben Hayden <hayden767@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq chrome-packages '(
                        edit-server
                        gmail-message-mode
                        flymd
                        ))

(defun chrome/init-edit-server ()
  (use-package edit-server
    :init
    (progn
      (edit-server-start))
    :config
    (when (configuration-layer/layer-used-p 'markdown)
      (spacemacs/set-markdown-keybindings
       'gmail-message-client-mode gmail-message-client-mode-map))))

(defun chrome/init-flymd ()
  (use-package flymd
    :defer t
    :init
    (progn
      (defun start-browser(browser url)
        (let ((process-environment (browse-url-process-environment)))
          (apply 'start-process
                 "flymd" nil
                 browser
                 (list "--new-window" "--allow-file-access-from-files" url))))

      (defun my-flymd-browser-function (url)
               (cond
                (chrome-exec-path (start-browser chrome-exec-path url))
                ((executable-find "chromium") (start-browser (executable-find "chromium") url))
                ((executable-find "google-chrome") (start-browser (executable-find "google-chrome") url))
                ((executable-find "google-chrome-stable") (start-browser (executable-find "google-chrome-stable") url))
                (t (message "no useful browser"))))

(defun chrome/pre-init-markdown-mode ()
  (spacemacs|use-package-add-hook markdown-mode
    :pre-config
    (when (configuration-layer/package-used-p 'gmail-message-mode)
      (add-to-list 'markdown--key-bindings-modes 'gmail-message-client-mode))))
