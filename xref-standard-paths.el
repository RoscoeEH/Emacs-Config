;;; Code:
;; If using make sure to update the for your machines locations

(defconst STANDARDS_PATH "c:/Users/Roscoe/Documents/FIPS markdown standards/")

(defconst md-standard-location-map
  `(("IG" . ,(concat STANDARDS_PATH "FIPS 140-3 IG.md"))
    ("19790:2012" . ,(concat STANDARDS_PATH "ISO IEC 19790 2012E [2014] {140-3 PUB}.md"))
    ("SP800-38D" . ,(concat STANDARDS_PATH "SP800-38d.md"))
    ("SP800-90Ar1" . ,(concat STANDARDS_PATH "SP800-90Ar1.md"))
    ("SP800-90B" . ,(concat STANDARDS_PATH "SP800-90B.md"))
    ("SP800-90C" . ,(concat STANDARDS_PATH "SP800-90C.md"))
    )
  "Alist mapping special keywords to their Markdown reference files.")
;;; xref-standard-paths.el ends here
