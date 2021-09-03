;; maxwidth macro comes from:
;; https://tex.stackexchange.com/questions/86350/includegraphics-maximum-width
(defvar en-article ;; this is the default latex class
  '("article"
    "\\documentclass{article}
\\usepackage{xeCJK}
\\usepackage{amsmath,amssymb,amsfonts}
\\usepackage{geometry}
\\usepackage{titlesec}
\\usepackage{enumitem}
\\usepackage{fancyhdr, lastpage}
\\usepackage{hyperref}
\\usepackage{graphicx}
\\usepackage[citestyle=numeric, bibstyle=numeric, hyperref=true,
backref=true, maxcitenames=3, url=true,backend=biber, natbib=true,
sorting=none]{biblatex}
\\addbibresource{../references.bib}
\\makeatletter
\\def\\maxwidth#1{\\ifdim\\Gin@nat@width>#1 #1\\else\\Gin@nat@width\\fi}
\\makeatother
\\hypersetup{hidelinks}
\\hypersetup{colorlinks = true, urlcolor = blue, linkcolor = blue, citecolor = blue}
\\fancypagestyle{plain}{
    \\fancyhf{}
    \\fancyfoot[C]{{\\thepage}/\\pageref*{LastPage}}
    \\renewcommand{\\headrulewidth}{0pt}
}
\\pagestyle{plain}
\\setlength{\\parindent}{0em}
\\setlist[1]{labelindent=\\parindent,nosep,leftmargin= *}
\\geometry{a4paper,scale=0.8}
\\geometry{a4paper,left=2.5cm,right=2.5cm,top=3cm,bottom=3cm}
\\setlength{\\baselineskip}{20pt}
\\setlength{\\parskip}{5pt}"
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(defvar en-article2c
  '("article2c"
    "\\documentclass[twocolumn]{article}
\\usepackage{xeCJK}
\\usepackage{amsmath,amssymb,amsfonts}
\\usepackage{geometry}
\\usepackage{titlesec}
\\usepackage{enumitem}
\\usepackage{fancyhdr, lastpage}
\\usepackage[citestyle=numeric, bibstyle=numeric, hyperref=true,
backref=true, maxcitenames=3, url=true,backend=biber, natbib=true,
sorting=none]{biblatex}
\\addbibresource{../references.bib}
\\fancypagestyle{plain}{
    \\fancyhf{}
    \\fancyfoot[C]{{\\thepage}/\\pageref*{LastPage}}
    \\renewcommand{\\headrulewidth}{0pt}
}
\\usepackage{hyperref}
\\hypersetup{hidelinks}
\\hypersetup{colorlinks = true, urlcolor = blue, linkcolor = blue, citecolor = blue}
\\pagestyle{plain}
\\setlist[1]{labelindent=\\parindent,nosep,leftmargin= *}
\\geometry{a4paper,scale=0.8}
\\geometry{a4paper,left=2.5cm,right=2.5cm,top=2cm,bottom=2cm}
\\setlength{\\baselineskip}{20pt}
\\setlength{\\parskip}{5pt}"
       ("\\section{%s}" . "\\section*{%s}")
       ("\\subsection{%s}" . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}" . "\\paragraph*{%s}")
       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(defvar biz-report
  '("biz-report"
    "\\documentclass[15pt]{ctexart}
\\usepackage{geometry}
\\usepackage{titlesec}
\\usepackage{titling}
\\posttitle{\\par\\end{center}\\vskip -60pt}
\\let\\titleoriginal\\title
\\renewcommand{\\title}[1]{
  \\titleoriginal{\\LARGE{\\heiti{#1}}\\vspace{-4em}}
  }
\\let\\maketitleorig\\maketitle
\\renewcommand{\\maketitle}{
  \\maketitleorig
  \\Large
  }
\\setlength{\\droptitle}{-60pt}
\\usepackage{enumitem}
\\usepackage{abstract}
\\usepackage[backend=biber,style=gb7714-2015]{biblatex}
\\addbibresource{../references.bib}
\\renewcommand{\\abstractname}{摘要}
\\renewcommand\\refname{参考文献}
\\usepackage{fancyhdr, lastpage}
\\fancypagestyle{plain}{
    \\fancyhf{}
    \\fancyfoot[C]{{\\thepage}/\\pageref*{LastPage}}
    \\renewcommand{\\headrulewidth}{0pt}
}
\\usepackage{hyperref}
\\hypersetup{hidelinks}
\\hypersetup{colorlinks = true, urlcolor = blue, linkcolor = blue, citecolor = blue}
\\pagestyle{plain}
\\setlist[1]{labelindent=\\parindent,nosep,leftmargin= *}
\\geometry{a4paper,scale=0.8}
\\geometry{a4paper,left=2.5cm,right=2.5cm,top=3cm,bottom=3cm}
\\setlength{\\baselineskip}{20pt}
\\setlength{\\parskip}{5pt}
\\DeclareRobustCommand\\nobreakspace{\\leavevmode\\nobreak\\ }"
   ("\\section{%s}" . "\\section*{%s}")
   ("\\subsection{%s}" . "\\subsection*{%s}")
   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
   ("\\paragraph{%s}" . "\\paragraph*{%s}")
   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(defvar cn-article
  '("cn-article"
    "\\documentclass[a4paper,UTF8]{ctexart}
\\usepackage{geometry}
\\usepackage{titlesec}
\\usepackage{enumitem}
\\usepackage{abstract}
\\renewcommand{\\abstractname}{摘要}
\\renewcommand\\refname{参考文献}
\\CTEXsetup[format={\\Large\\bfseries}]{section}
\\renewcommand\\thesection{\\chinese{section}、}
\\renewcommand\\thesubsection{\\arabic{section}.\\arabic{subsection}.}
\\renewcommand\\thesubsubsection{\\arabic{section}.\\arabic{subsection}.\\arabic{subsubsection}}
\\usepackage{fancyhdr, lastpage}
\\fancypagestyle{plain}{
    \\fancyhf{}
    \\fancyfoot[C]{{\\thepage}/\\pageref*{LastPage}}
    \\renewcommand{\\headrulewidth}{0pt}
}
\\usepackage[backend=biber,style=gb7714-2015,hyperref=true,
backref=true, maxcitenames=3, url=true]{biblatex}
\\addbibresource{../references.bib}
\\usepackage{hyperref}
\\hypersetup{hidelinks}
\\hypersetup{colorlinks = true, urlcolor = blue, linkcolor = blue, citecolor = blue}
\\setCJKmainfont{微软雅黑} % sets the roman font
\\setCJKsansfont{微软雅黑} % sets the sans font
\\setCJKmonofont{Consolas} % otherwise FangSong is not found
\\pagestyle{plain}
\\setlist[1]{labelindent=\\parindent,nosep,leftmargin= *}
\\geometry{a4paper,scale=0.8}
\\geometry{a4paper,left=2.5cm,right=2.5cm,top=3cm,bottom=3cm}
\\setlength{\\baselineskip}{20pt}
\\setlength{\\parskip}{5pt}
\\DeclareRobustCommand\\nobreakspace{\\leavevmode\\nobreak\\ }"
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(defvar cn-book
  '("cn-book"
    "\\documentclass[a4paper,UTF8]{ctexbook}
\\usepackage{enumitem}
\\usepackage{abstract}
\\usepackage[backend=biber,style=gb7714-2015,hyperref=true,
backref=true, maxcitenames=3, url=true]{biblatex}
\\addbibresource{../references.bib}
\\renewcommand{\\abstractname}{摘要}
\\renewcommand\\refname{参考文献}
\\usepackage{fancyhdr, lastpage}
\\fancypagestyle{plain}{
    \\fancyhf{}
    \\fancyfoot[C]{{\\thepage}/\\pageref*{LastPage}}
    \\renewcommand{\\headrulewidth}{0pt}
}
\\usepackage{hyperref}
\\hypersetup{hidelinks}
\\hypersetup{colorlinks = true, urlcolor = blue, linkcolor = blue, citecolor = blue}
\\pagestyle{plain}
\\setlist[1]{labelindent=\\parindent,nosep,leftmargin= *}
\\setlength{\\baselineskip}{20pt}
\\setlength{\\parskip}{5pt}
\\DeclareRobustCommand\\nobreakspace{\\leavevmode\\nobreak\\ }"
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(defvar IEEEtrans
  '("IEEEtran"
    "\\documentclass[conference]{IEEEtran}
\\IEEEoverridecommandlockouts
\\usepackage{cite}
\\usepackage{amsmath,amssymb,amsfonts}
\\usepackage{algorithmic}
\\usepackage{xcolor}
\\usepackage{hyperref}
\\def\\BibTeX{{\\rm B\\kern-.05em{\\sc i\\kern-.025em b}\\kern-.08em
  T\\kern-.1667em\\lower.7ex\\hbox{E}\\kern-.125emX}}"))

(defvar cn-beamer
  '("ctexbeamer"
    "\\documentclass[presentation, aspectratio=169]{ctexbeamer}
\\usepackage[backend=biber,style=gb7714-2015,hyperref=true,
backref=true, maxcitenames=3, url=true]{biblatex}
\\addbibresource{../references.bib}
\\setlength{\\baselineskip}{20pt}
\\setlength{\\parskip}{5pt}"
    ("\\begin{frame}{%s}" "\\end{frame}" "\\begin{frame}{%s}" "\\end{frame}")))


(defvar en-beamer
 '("beamer"
   "\\documentclass[presentation, aspectratio=169]{beamer}"
   ("\\section{%s}" . "\\section*{%s}")
   ("\\subsection{%s}" . "\\subsection*{%s}")
   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(defvar kimim/latex-classes
      ;; use backquote ` to evaluate element before put to list
      `(
        ,cn-article      ;; Chinese style article
        ,cn-book         ;; Chinese style book
        ,cn-beamer      ;; Chinese beamer slides
        ,en-article      ;; English article
        ,en-article2c    ;; English article in two columns
        ,en-beamer
        ,biz-report      ;; Business report format used in a private owned enterprice
        ,IEEEtrans       ;; IEEE transaction paper
        ))

(provide 'latex-classes)
