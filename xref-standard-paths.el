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
    ("FIPS 180-4" . ,(concat STANDARDS_PATH "FIPS 180-4.md"))
    ("FIPS 186-5" . ,(concat STANDARDS_PATH "FIPS 186-5.md"))
    ("FIPS 197" . ,(concat STANDARDS_PATH "FIPS 197.md"))
    ("FIPS 198-1" . ,(concat STANDARDS_PATH "FIPS 198-1.md"))
    ("FIPS 202" . ,(concat STANDARDS_PATH "FIPS 202.md"))
    ("FIPS 205" . ,(concat STANDARDS_PATH "FIPS 205.md"))
    ("SP800-131Ar2" . ,(concat STANDARDS_PATH "SP800-131Ar2.md"))
    ("SP800-140Br1" . ,(concat STANDARDS_PATH "SP800-140Br1.md"))
    ("SP800-56Ar3" . ,(concat STANDARDS_PATH "SP800-56Ar3.md"))
    ("SP800-56Br2" . ,(concat STANDARDS_PATH "SP800-56Br2.md"))
    )
  "Alist mapping special keywords to their Markdown reference files.")
;;; xref-standard-paths.el ends here
