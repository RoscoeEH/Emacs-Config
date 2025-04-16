;;; Code:
;; If using make sure to update the for your machines locations

(defconst STANDARDS_PATH "c:/Users/Roscoe/Documents/FIPS markdown standards/")

(defconst md-standard-location-map
  '(("IG" . (concat STANDARDS_PATH "FIPS 140-3 IG.md"))
    ("SP" . (concat STANDARDS_PATH "test-doc.md"))
    )
  "Alist mapping special keywords to their Markdown reference files.")


;;; xref-standard-paths.el ends here
