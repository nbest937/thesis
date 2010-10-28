(TeX-add-style-hook "thesis"
 (lambda ()
    (LaTeX-add-bibliographies
     "9_backmatter/references")
    (LaTeX-add-labels
     "nom")
    (TeX-run-style-hooks
     "latex2e"
     "Latex/Classes/PhDthesisPSnPDF11"
     "Latex/Classes/PhDthesisPSnPDF"
     "oneside"
     "11pt"
     "Latex/Macros/MacroFile1"
     "0_frontmatter/abstract"
     "0_frontmatter/dedication"
     "0_frontmatter/acknowledgement"
     "0_frontmatter/glossary"
     "1_introduction/introduction"
     "2/aims"
     "3/XYZ"
     "4/XYZ"
     "5/XYZ"
     "6/XYZ"
     "7/discussion"
     "8/materials_methods"
     "9_backmatter/declaration")))

