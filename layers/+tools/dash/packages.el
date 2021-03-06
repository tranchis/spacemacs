 ;; see conditional package inclusion
(setq dash-packages
      '(
        (dash-at-point :toggle (spacemacs/system-is-mac))
        (helm-dash :requires helm)
        (counsel-dash :requires ivy)
        (zeal-at-point :toggle (or (spacemacs/system-is-linux)
                                   (spacemacs/system-is-mswindows)))))

(defun dash/init-helm-dash ()
  (use-package helm-dash
    :defer t
    :init
    (spacemacs/set-leader-keys
      "dh" 'helm-dash-at-point
      "dH" 'helm-dash)
    :config
    (dash//activate-package-docsets helm-dash-docset-newpath)))

(defun dash/init-counsel-dash ()
  (use-package counsel-dash
    :defer t
    :init
    (spacemacs/set-leader-keys
      "dh" 'counsel-dash-at-point
      "dH" 'counsel-dash)
    (defun counsel-dash-at-point ()
      "Counsel dash with selected point"
      (interactive)
      (counsel-dash
       (if (use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end))
         (substring-no-properties (or (thing-at-point 'symbol) "")))))
    :config
    (dash//activate-package-docsets helm-dash-docset-newpath)))

(defun dash/init-dash-at-point ()
  (use-package dash-at-point
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "dd" 'dash-at-point)
      (spacemacs/set-leader-keys "dD" 'dash-at-point-with-docset))))

(defun dash/init-zeal-at-point ()
  (use-package zeal-at-point
    :defer t
    :init
    (spacemacs/set-leader-keys
      "dd" 'zeal-at-point
      "dD" 'zeal-at-point-set-docset)
    :config
    ;; This lets users seach in multiple docsets
    (push '(web-mode . "html,css,javascript") zeal-at-point-mode-alist)
    ))
