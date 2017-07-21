;;; packages.el --- Java Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Lukasz Klich <klich.lukasz@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq java-packages
      '(
        company
        (company-emacs-eclim :toggle
                             (configuration-layer/package-used-p 'company))
        eclim
        eldoc
        ensime
        flycheck
        (flycheck-eclim :location local
                        :requires flycheck)
        flyspell
        ggtags
        helm-gtags
        (java-mode :location built-in)
        ))

(defun java/post-init-company ()
  (spacemacs|add-company-hook java-mode))

(defun java/init-company-emacs-eclim ()
  (use-package company-emacs-eclim
    :defer t
    :init (push 'company-emacs-eclim company-backends-java-mode)))

(defun java/init-eclim ()
  (use-package eclim
    :defer t
    :init
    (progn
      (add-hook 'java-mode-hook 'eclim-mode)
      (add-to-list 'spacemacs-jump-handlers-java-mode 'eclim-java-find-declaration))
    :config
    (progn
      (spacemacs|hide-lighter eclim-mode)
      (require 'eclimd)
      (setq help-at-pt-display-when-idle t
            help-at-pt-timer-delay 0.1)
      (help-at-pt-set-timer)

      (add-to-list 'minor-mode-alist
                   '(eclim-mode (:eval (eclim-modeline-string))))
      ;; hack to support Maven multi-modules
      (defun my-eclim-fix-relative-path (path)
        (replace-regexp-in-string "^.*src/" "src/" path))
      (advice-add 'eclim--project-current-file :filter-return
                  #'my-eclim-fix-relative-path)
      ;; key bindings
      (dolist (prefix '(("ma" . "ant")
                        ("mD" . "daemon")
                        ("mg" . "goto")
                        ("mh" . "help/doc")
                        ("mi" . "issues")
                        ("mm" . "maven")
                        ("mp" . "project")
                        ("mr" . "refactor")
                        ("mt" . "test")))
        (spacemacs/declare-prefix-for-mode
         'java-mode (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'java-mode
        ;; ant
        "aa" 'eclim-ant-run
        "ac" 'eclim-ant-clear-cache
        "ar" 'eclim-ant-run
        "av" 'eclim-ant-validate
        ;; daemon
        "Dk" 'stop-eclimd
        "Ds" 'start-eclimd
        ;; errors (problems)
        "ee" 'eclim-problems-correct
        ;; find
        "ff" 'eclim-java-find-generic
        ;; goto
        "gt" 'eclim-java-find-type
        ;; help/doc
        "hc" 'eclim-java-call-hierarchy
        "hh" 'eclim-java-show-documentation-for-current-element
        "hi" 'eclim-java-hierarchy
        "hu" 'eclim-java-find-references
        ;; maven
        "mi" 'spacemacs/java-maven-clean-install
        "mI" 'spacemacs/java-maven-install
        "mp" 'eclim-maven-lifecycle-phases
        "mr" 'eclim-maven-run
        "mR" 'eclim-maven-lifecycle-phase-run
        "mt" 'spacemacs/java-maven-test
        ;; project
        "pb" 'eclim-project-build
        "pc" 'eclim-project-create
        "pd" 'eclim-project-delete
        "pg" 'eclim-project-goto
        "pi" 'eclim-project-import
        "pj" 'eclim-project-info-mode
        "pk" 'eclim-project-close
        "po" 'eclim-project-open
        "pp" 'eclim-project-mode
        "pr" 'eclim-java-run-run
        "pu" 'eclim-project-update
        ;; refactor
        "rc" 'eclim-java-constructor
        "rf" 'eclim-java-format
        "rg" 'eclim-java-generate-getter-and-setter
        "ri" 'eclim-java-import-organize
        "rj" 'eclim-java-implement
        "rn" 'eclim-java-new
        "rr" 'eclim-java-refactor-rename-symbol-at-point
        ;; test
        "tt" 'eclim-run-junit)
      (evil-define-key 'insert java-mode-map
        (kbd ".") 'spacemacs/java-completing-dot
        (kbd ":") 'spacemacs/java-completing-double-colon
        (kbd "M-.") 'eclim-java-find-declaration
        (kbd "M-,") 'pop-tag-mark
        (kbd "M-<mouse-3>") 'eclim-java-find-declaration
        (kbd "<mouse-8>") 'pop-tag-mark)

      (evil-define-key 'normal java-mode-map
        (kbd "M-.") 'eclim-java-find-declaration
        (kbd "M-,") 'pop-tag-mark
        (kbd "M-<mouse-3>") 'eclim-java-find-declaration
        (kbd "<mouse-8>") 'pop-tag-mark)

      (evil-define-key 'normal eclim-problems-mode-map
        (kbd "a") 'eclim-problems-show-all
        (kbd "e") 'eclim-problems-show-errors
        (kbd "g") 'eclim-problems-buffer-refresh
        (kbd "q") 'eclim-quit-window
        (kbd "w") 'eclim-problems-show-warnings
        (kbd "f") 'eclim-problems-toggle-filefilter
        (kbd "c") 'eclim-problems-correct
        (kbd "RET") 'eclim-problems-open-current)

      (evil-define-key 'normal eclim-project-mode-map
        (kbd "N") 'eclim-project-create
        (kbd "m") 'eclim-project-mark-current
        (kbd "M") 'eclim-project-mark-all
        (kbd "u") 'eclim-project-unmark-current
        (kbd "U") 'eclim-project-unmark-all
        (kbd "o") 'eclim-project-open
        (kbd "c") 'eclim-project-close
        (kbd "i") 'eclim-project-info-mode
        (kbd "I") 'eclim-project-import
        (kbd "RET") 'eclim-project-goto
        (kbd "D") 'eclim-project-delete
        (kbd "p") 'eclim-project-update
        (kbd "g") 'eclim-project-mode-refresh
        (kbd "R") 'eclim-project-rename
        (kbd "q") 'eclim-quit-window))))

(defun java/post-init-eldoc ()
  (add-hook 'java-mode-local-vars-hook #'spacemacs//java-setup-eldoc))

(defun java/init-ensime ()
  (use-package ensime
    :defer t
    :commands ensime-mode
    :init
    (progn
      (setq ensime-startup-dirname (concat spacemacs-cache-directory "ensime/"))
      (spacemacs/register-repl 'ensime 'ensime-inf-switch "ensime"))
    :config
    (progn
      ;; This function was renamed in ensime. Usually we don't need to do this,
      ;; but documentation recommends the stable version of ensime, so we must
      ;; try to support it, too.
      (unless (fboundp 'ensime-type-at-point)
        (defalias 'ensime-type-at-point 'ensime-print-type-at-point))

      ;; key bindings
      (dolist (mode java--ensime-modes)
        (dolist (prefix '(("mb" . "build")
                          ("mc" . "check")
                          ("md" . "debug")
                          ("mD" . "daemon")
                          ("me" . "errors")
                          ("mg" . "goto")
                          ("mh" . "docs")
                          ("mi" . "inspect")
                          ("mr" . "refactor")
                          ("mt" . "test")
                          ("ms" . "repl")
                          ("my" . "yank")))
          (spacemacs/declare-prefix-for-mode mode (car prefix) (cdr prefix)))
        (spacemacs/set-leader-keys-for-major-mode mode
          "/"      'ensime-search
          "'"      'ensime-inf-switch

          "bc"     'ensime-sbt-do-compile
          "bC"     'ensime-sbt-do-clean
          "bi"     'ensime-sbt-switch
          "bp"     'ensime-sbt-do-package
          "br"     'ensime-sbt-do-run

          "ct"     'ensime-typecheck-current-buffer
          "cT"     'ensime-typecheck-all

          "dA"     'ensime-db-attach
          "db"     'ensime-db-set-break
          "dB"     'ensime-db-clear-break
          "dC"     'ensime-db-clear-all-breaks
          "dc"     'ensime-db-continue
          "di"     'ensime-db-inspect-value-at-point
          "dn"     'ensime-db-next
          "do"     'ensime-db-step-out
          "dq"     'ensime-db-quit
          "dr"     'ensime-db-run
          "ds"     'ensime-db-step
          "dt"     'ensime-db-backtrace

          "Df"     'ensime-reload-open-files
          "Dr"     'spacemacs/ensime-gen-and-restart
          "Ds"     'ensime

          "ee"     'ensime-print-errors-at-point
          "el"     'ensime-show-all-errors-and-warnings
          "es"     'ensime-stacktrace-switch

          "gp"     'ensime-pop-find-definition-stack
          "gi"     'ensime-goto-impl
          "gt"     'ensime-goto-test

          "hh"     'ensime-show-doc-for-symbol-at-point
          "hT"     'ensime-type-at-point-full-name
          "ht"     'ensime-type-at-point
          "hu"     'ensime-show-uses-of-symbol-at-point

      (spacemacs/set-leader-keys-for-major-mode 'java-mode
        "ea" 'eclim-problems-show-all
        "eb" 'eclim-problems
        "ec" 'eclim-problems-correct
        "ee" 'eclim-problems-show-errors
        "ef" 'eclim-problems-toggle-filefilter
        "en" 'eclim-problems-next-same-window
        "eo" 'eclim-problems-open
        "ep" 'eclim-problems-previous-same-window
        "ew" 'eclim-problems-show-warnings

        "ds" 'start-eclimd
        "dk" 'stop-eclimd

        "ff" 'eclim-java-find-generic

        "gt" 'eclim-java-find-type

        "rc" 'eclim-java-constructor
        "rg" 'eclim-java-generate-getter-and-setter
        "rf" 'eclim-java-format
        "ri" 'eclim-java-import-organize
        "rj" 'eclim-java-implement
        "rr" 'eclim-java-refactor-rename-symbol-at-point

        "hc" 'eclim-java-call-hierarchy
        "hh" 'eclim-java-show-documentation-for-current-element
        "hi" 'eclim-java-hierarchy
        "hu" 'eclim-java-find-references

;; (defun java/post-init-ensime ()
;;   (when (eq 'ensime java-backend)
;;     (use-package ensime
;;       :defer t
;;       :init
;;       (progn
;;         (spacemacs//ensime-init 'java-mode t nil)
;;         (when (configuration-layer/package-used-p 'company)
;;           (push 'ensime-company company-backends-java-mode)))
;;       :config
;;       (progn
;;         (spacemacs/ensime-configure-keybindings 'java-mode)))))

        "aa" 'eclim-ant-run
        "ac" 'eclim-ant-clear-cache
        "ar" 'eclim-ant-run
        "av" 'eclim-ant-validate

        "pb" 'eclim-project-build
        "pc" 'eclim-project-create
        "pd" 'eclim-project-delete
        "pg" 'eclim-project-goto
        "pi" 'eclim-project-import
        "pj" 'eclim-project-info-mode
        "pk" 'eclim-project-close
        "po" 'eclim-project-open
        "pp" 'eclim-project-mode
        "pu" 'eclim-project-update

        "tt" 'eclim-run-junit))))

(defun java/post-init-ggtags ()
  (add-hook 'java-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun java/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'java-mode))

(defun java/init-java-mode ()
  (setq java/key-binding-prefixes '(("me" . "errors")
                                    ("md" . "eclimd")
                                    ("mf" . "find")
                                    ("mg" . "goto")
                                    ("mr" . "refactor")
                                    ("mh" . "documentation")
                                    ("mm" . "maven")
                                    ("ma" . "ant")
                                    ("mp" . "project")
                                    ("mt" . "test")))
  (mapc (lambda(x) (spacemacs/declare-prefix-for-mode
                    'java-mode (car x) (cdr x)))
        java/key-binding-prefixes))
