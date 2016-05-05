;; This is the list of directories that the source browser displays
;; on the left.
(setq ecb-source-path `("~/"
                        ,portacl-root
                        ,(concat portacl-root "/sbcl/sources/")))

;; This is the default font that is used globally. Adjust it to
;; one that you like. Consolas is nice, for example.
(set-frame-font "Monospace-10" nil t)
