;;; ado-font-lock.el --- all the endless font locking
;; Copyright (c) 2003--2017
;; Bill Rising
;; Author:   Bill Rising
;; Maintainer: Same <brising@alum.mit.edu>
;;             URL: http://louabill.org/stata
;; Keywords: ado-mode
;; Version:  1.15.1.4 of June 26, 2018

;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description:
;;
;; This file contains functions for good keyword highlighting, aka
;;   font-locking.
;; The main, huge function is: 
;;   ado-set-font-lock-keywords
;; It is a massively long list of items for the nice highlighting
;;   in ado-mode, nothing more, nothing less
;; Other functions have been added to allow users to include their
;;   own commands in the font locking
;; Known shortcomings: this highlights command names withing
;;   longer commands. One day I'll come up with a good rule for
;;   being smart about the highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; these regexps will still be fooled by line continuations
;;;   or colons in the middle of a line of text
(defconst ado-capture-noisily-regexp
  (concat
   (eval-when-compile
	 (regexp-opt
	  '(
	  "cap" "capt" "captu" "captur" "capture"
	  )))
   "[ /t]+"
   (eval-when-compile
	 (regexp-opt
	  '(
		"n" "no" "noi" "nois" "noisi" "noisil" "noisily"
		)))
   )
  "The regexp for testing for a -capture noisily- prefix. Ugh.")


(defconst ado-prefix-any-regexp
  (concat
   "\\(?:[ \t]*"
   "\\(?:"
   (eval-when-compile
	 (regexp-opt
	  '(
		"cap" "capt" "captu" "captur" "capture"
		"mata"
		"n" "no" "noi" "nois" "noisi" "noisil" "noisily" 
		"qui" "quie" "quiet" "quietl" "quietly"
		)))
   "\\|"
   ado-capture-noisily-regexp
   "\\)"
   "\\(?:[ \t]*:\\)?\\)?"
   )
  "Miserable regexp for those commands which could be thrown in front of any other
command without any colon. Defined so that other command starters can include these
more easily. What a pita.")

(defconst ado-start-cmd-regexp
  (concat
   "^\\(?:\\(?:.*:\\)*"
   "\\|"
   ado-prefix-any-regexp
   "\\)[ \t]*"
   )
  "start-of-command regexp to try to keep mid-line commands from highlighting.
Meant for typical commands which could allow a prefix command.
Not implemented as much more than an experiment. ")

(defconst ado-start-cmd-no-prefix-regexp
  (concat
   "^"
   ado-prefix-any-regexp
   "[ \t]*"
   )
  "start-of-command regexp for commands which do _not_ allow a prefix command.
Not implemented as much more than an experiment. ")

(defconst ado-start-cmd-must-start-line-regexp "^[ \t]*"
  "start-of-command regexp for commands which don't allow capture, noisily, etc.
Meant for commands which do _not_ allow a prefix command.
Not implemented as much more than an experiment. ")

(defconst ado-start-cmd-null-regexp ""
  "empty start-of-command regexp to make clear that there is no leading regexp.
Meant for spurious-higlighting problems which have not been solved yet.")

(defconst ado-end-cmd-regexp "\\([ \t]+\\|,\\|;\\|:\\|$\\)"
  "end-of-command regexp to keep things like -regress(- from highlighting")

(defconst ado-stata-name-regexp "[[:alpha:]_][[:graph:]_]*"
  "regexp for uniform highlighting of Stata names, so that future changes will be easier")

(defconst ado-stata-name-bound-regexp
  (concat "\\(" ado-stata-name-regexp "\\)")
  "same as \\[ado-stata-name-regexp] but bound within \\( and \\) for highlighting")

(defconst ado-stata-local-name-regexp "[[:graph:]_]+"
  "regexp for highlighting local macros")
  
(defconst ado-stata-local-name-bound-regexp
  (concat "\\(" ado-stata-local-name-regexp "\\)")
  "regexp for highlighting local macros, bound within \\( and \\)")

(defconst ado-man-abbrevs '("BAYES" "D" "DSGE" "ERM" "FMM" "FN" "G" "GSM" "GSU" "GSW" "IG" "IRT" "M" "ME" "MI" "MV" "P" "PSS" "R" "SEM" "SP" "ST" "SVY" "TE" "TS" "U" "XT"))



(defun ado-set-font-lock-keywords ()
						  ;  (make-local-variable 'ado-font-lock-keywords)
  (interactive)
  (setq
   ado-font-lock-keywords
   (list
    ;; nested quotes
	(list "\\(`\".*?\"'\\)" '(1 ado-string-face t))
    ;; special highlighting
	;; starting a mata program; not allowing comments, though
	(list "^[ \t]*\\(mata\\)\\(:\\)[ \t]*$" 
		  '(1 ado-builtin-harmful-face)
		  '(2 ado-constant-face))
    ;; ado 'which' comments
	(list "\\(^\\*!.*\\)" '(1 ado-builtin-harmful-face t))
	;; program define or list
	(list
	 (concat
	  "^[ \t]*"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "pr" "pro" "prog" "progr" "progra" "program")
		 'words))
	  "[ \t]+"
	  (eval-when-compile
		(regexp-opt 
		 '(
		   "d" "de" "def" "defi" "defin" "define" "drop"
		   "l" "li" "lis" "list"
		   ) 'words))
	  "[ \t]+"
	  ado-stata-name-bound-regexp
	  )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t) '(3 ado-builtin-harmful-face t))
	;;
	;; program w/o define
	(list
	 (concat
	  ado-start-cmd-must-start-line-regexp
	  (eval-when-compile 
		(regexp-opt
		 '(
		   "pr" "pro" "prog" "progr" "progra" "program")
		 'words))
	  "[ \t]+\\([^ \t]+\\)"
	  )
	 '(1 ado-builtin-harmful-face) '(2 ado-builtin-harmful-face))
	
	(list
	 (concat
	  ado-start-cmd-must-start-line-regexp
	  (eval-when-compile 
		(regexp-opt
		 '(
		   "pr" "pro" "prog" "progr" "progra" "program"
		   )
		 'words))
	  "[ \t]+"
	  (eval-when-compile
		(regexp-opt '("di" "dir")
					'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t))

	;; it appears Stata accepts any version number
	;; this just allows major[.0/max for particular version]
	;; only 0's: 1, 4, 5, 7, 14 (so far)
    ;; .1's: 2, 3, 6, 10, 12, 13
	;; .2's: 8, 9, 11
	(list
	 (concat
	  (eval-when-compile 
		(regexp-opt 
		 '("vers" "versi" "versio" "version")
		 'words))
	  "[ \t]+\\(\\(?:\\(?:[1-9]\\|1[012345]\\)\\(?:[.]0\\)?\\)\\|\\(?:\\(?:[23689]\\|1[012345]\\)[.]1\\)\\|\\(?:[89]\\|1[14]\\)[.]2\\)\\($\\|[ \t]+\\|:\\)"
	  )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	;; pause on/off
	(list
	 (concat
	  (eval-when-compile 
		(regexp-opt 
		 '("pause") 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '("off" "on") 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))
	;; end and pause must start lines
	(list
	 (concat
	  "^[ \t]*"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "end" "pause"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face))

	(list
	 (concat
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "#d" "#de" "#del" "#deli" "#delim" "#delimi" "#delimit" 
		   ) 'words))
	  "[ \t]+\\(cr\\|;\\)[ \t]*$"
	  )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

    ;; obsolete stuff which appears as OK as subcommands
	;; "lfit" // removed entirely, because (lfit ...) is ok
	(list
	 (concat
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "sco" "scor" "score"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-obsolete-face))

	;; various bayes commands
	(list
	 (concat
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "bayesgraph"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "matrix"
		   "name"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	
	(list
	 (concat
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "bayesstats"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "ess"
		   "ic"
		   "summ" "summa" "summar" "summary"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "bayestest"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "int" "inte" "inter" "interv" "interva" "interval"
		   "model"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; churdle commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "churdle"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "exp" "expo" "expon" "expone" "exponen" "exponent" "exponenti" "exponentia" "exponential"
		   "lin" "line" "linea" "linear"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

    ;; the cluster commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt '("cluster") 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt '(
					  "dend" "dendr" "dendro" "dendrog" "dendrogr" "dendrogra" "dendrogram"
					  "dir"
					  "k" "km" "kme" "kmea" "kmean" "kmeans" 
					  "kmed" "kmedi" "kmedia" "kmedian" "kmedians" 
					  "list"
					  "note" "notes"
					  "parsedist" "parsedista" "parsedistan" "parsedistanc" "parsedistance" 
					  "query"
					  "tr" "tre" "tree"
					  ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

    ;; 

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "cluster"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "del" "dele" "delet" "delete" 
		   "drop"
		   "gen" "gene" "gener" "genera" "generat" "generate" 
		   "measures"
		   "rename" "renamevar"
		   "set"
		   "use"
		   ) 'words))
	  ado-end-cmd-regexp ) 
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))


    ;; putting together common cluster and clustermat commands

	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(cluster\\(?:mat\\)?\\)\\>[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "a" "anova"
		   "av" "ave" "aver" "avera" "averag" "average" "averagel" "averageli" 
		   "averagelin" "averagelink" "averagelinka" "averagelinkag" "averagelinkage" 
		   "c" "cen" "cent" "centr" "centro" "centroi" "centroid" "centroidl" "centroidli" 
		   "centroidlin" "centroidlink" "centroidlinka" "centroidlinkag" "centroidlinkage" 
		   "co" "com" "comp" "compl" "comple" "complet" "complete" "completel" "completeli" 
		   "completelin" "completelink" "completelinka" "completelinkag" "completelinkage" 
		   "manova" "med" "medi" "media" "median" "medianl" "medianli" "medianlin" 
		   "medianlink" "medianlinka" "medianlinkag" "medianlinkage" 
		   "s" "si" "sin" "sing" "singl" "single" "singlel" "singleli" "singlelin" 
		   "singlelink" "singlelinka" "singlelinkag" "singlelinkage"
		   "stop"
		   "ward" "wards" "wardsl" "wardsli" "wardslin" "wardslink" "wardslinka" "wardslinkag" "wardslinkage"
		   "wav" "wave" "waver" "wavera" "waverag" "waverage" "waveragel" 
		   "waverageli" "waveragelin" "waveragelink" "waveragelinka" "waveragelinkag" "waveragelinkage" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

    ;; the copyright commands
	(list
	 (concat
	  (eval-when-compile 
		ado-start-cmd-regexp
		(regexp-opt 
		 '(
		   "copyright"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "apache" "autolink"
		   "boost"
		   "flexmark"
		   "hamcrest"
		   "icd10"
		   "icu"
		   "jsoup"
		   "lapack"
		   "libharu"
		   "libpng"
		   "mersennetwister"
		   "miglayout"
		   "scintilla" "slf4j"
		   "ttf2pt1"
		   "zlib"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; dyndoc tags
	;; <<dd_version>> (sloppy for now, because it can only be 1)
	(list "^[ \t]*\\(<<dd_version\\)[ \t]*:[ \t]*\\(1\\)[ \t]*\\(>>\\)"
		  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face) '(3 ado-builtin-harmless-face))
	  

	;; <<dd_do>> with attributes
	;; this nested list construction allows multiple matches on a line
	(list "^[ \t]*\\(<<dd_do\\)[ \t]*:[ \t]*" '(1 ado-builtin-harmless-face)
		  (list
		   (eval-when-compile
			 (regexp-opt
			  '(
				"nocom" "nocomm" "nocomma" "nocomman" "nocommand" "nocommands"
				"noout" "nooutp" "nooutpu" "nooutput"
				"noprom" "nopromp" "noprompt" 
				"qui" "quie" "quiet" "quietl" "quietly" 
				) 'words))
		   "+[ \t]*"
		   "\\(>>\\)"
		   '(0 ado-subcommand-face t) '(1 ado-builtin-harmless-face)
		   ))

	;; <<dd_remove>> and <</dd_remove>> can be anywhere in a line without attributes
	(list
	  "[ \t]*\\(<<\\(?:/\\)?dd_remove>>\\)"
	  '(1 ado-builtin-harmless-face t))
	
	;; <<dd_display>>, <<dd_graph>> can be anywhere in a line, needs : anything afterwards
	(list
	 (concat
	  "[ \t]*"
	  "\\(<<dd_\\(?:display\\|graph\\)\\)"
	  "[ \t]*:.*?"
	  "\\(>>\\)"
	  )
	 '(1 ado-builtin-harmless-face t) '(2 ado-builtin-harmless-face))

	;; dd_ignore /dd_ignore dd_end all start line but have no attributes
	(list
	 (concat
	  "^[ \t]*"
	  "\\(<<dd_\\(?:do\\|end\\|ignore\\|skip_else\\|skip_end\\)\\)"
	  "[ \t]*"
	  "\\(>>\\)"
	  )
	 '(1 ado-builtin-harmless-face t) '(2 ado-builtin-harmless-face))

	;; <</dd_do>> and <</dd_do>> must start a line
	(list
	  "^[ \t]*\\(<</dd_\\(?:do\\|ignore\\)\\)\\(>>\\)"
	  '(1 ado-builtin-harmless-face t) '(2 ado-builtin-harmless-face))

	;; <<dd_include>> must have a colon and must start a line. fug.
	(list
	  "^[ \t]*\\(<<dd_\\(?:include\\|skip_if\\)\\)[ \t]*:.*?\\(>>\\)"
	  '(1 ado-builtin-harmless-face t) '(2 ado-builtin-harmless-face))


	;; this is for highlighting <<dd_xx>> constructs with colon but no atttributes
	(list "<<dd_do.*\\(>>\\)[ \t]*$" '(1 ado-builtin-harmless-face t))

	;; discrim commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "discrim"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "knn"
		   "lda"
		   "logistic"
		   "qda"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; fracreg commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\(\\<fracreg\\>\\)"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "log" "logi" "logit" 
		   "pr" "pro" "prob" "probi" "probit" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	 ;; mgarch commands
	 (list
	  (concat
	   ado-start-cmd-regexp
	   "\\(\\<mgarch\\>\\)"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"ccc"
			"dcc"
			"dvech"
			"vcc"
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	 ;; mswitch commands
	 (list
	  (concat
	  ado-start-cmd-regexp
	   "\\(\\<mswitch\\>\\)"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"ar"
			"dr"
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	 ;; the one an only npregress command
	 (list
	  (concat
	  ado-start-cmd-regexp
	   "\\(\\<npregress\\>\\)"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"kernel"
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	 

	;; power commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "power" 
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "cmh" "cox"
		   "exp" "expo" "expon" "expone" "exponen" "exponent" "exponenti" "exponentia" "exponential"
		   "log" "logr" "logra" "logran" "logrank" 
		   "mcc"
		   "onecorr" "onecorre" "onecorrel" "onecorrela" "onecorrelat" "onecorrelati" "onecorrelatio" "onecorrelation" 
		   "onemean"
		   "oneprop" "onepropo" "onepropor" "oneproport" "oneproporti" "oneproportio" "oneproportion"
		   "oneslope"
		   "onevar" "onevari" "onevaria" "onevarian" "onevarianc" "onevariance"
		   "oneway"
		   "pairedm" "pairedme" "pairedmea" "pairedmean" "pairedmeans" 
		   "pairedpr" "pairedpro" "pairedprop" "pairedpropo" "pairedpropor" "pairedproport" "pairedproporti" "pairedproportio" "pairedproportion" "pairedproportions"
		   "pcorr"
		   "repeated"
		   "rsq" "rsqu" "rsqua" "rsquar" "rsquare" "rsquared"
		   "trend"
		   "twocorr" "twocorre" "twocorrel" "twocorrela" "twocorrelat" "twocorrelati" "twocorrelatio" "twocorrelation" "twocorrelations"
		   "twomeans"
		   "twoprop" "twopropo" "twopropor" "twoproport" "twoproporti" "twoproportio" "twoproportion" "twoproportions" 
		   "twovar" "twovari" "twovaria" "twovarian" "twovarianc" "twovariance" "twovariances" 
		   "twoway"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))


	;; putdocx/putpdf commands
	;; first the harmless one
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "putdocx" "putpdf"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "describe"
		   ) 'words))
	  ado-end-cmd-regexp ) 
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; now, all the rest (pause and resume are not official, but should be)
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "putdocx" "putpdf"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "begin"
		   "clear"
		   "image"
		   "pagebreak"
		   "paragraph"
		   "pause"
		   "resume"
		   "save"
		   "table"
		   "text"
		   ) 'words))
	  ado-end-cmd-regexp ) 
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	;; putdocx only
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "putdocx"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "append"
		   ) 'words))
	  ado-end-cmd-regexp ) 
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	;; st_is
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "st_is"
		   ) 'words))
	  "[ \t]+2[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "analysis"
		   "full"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; stpower commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "stpow" "stpowe" "stpower" 
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "cox"
		   "exp" "expo" "expon" "expone" "exponen" "exponent" "exponenti" "exponentia" "exponential" 
		   "log" "logr" "logra" "logran" "logrank" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-obsolete-face) '(2 ado-subcommand-face t))


	;; set command and its variations
	;; splitting up what was done before

	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "se" "set"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "a" "ad" "ado" "ados" "adosi" "adosiz" "adosize" 
		   "cformat" "clevel"
		   "fredkey"
		   "fvwrap"
		   "haverdir"
		   "httpproxyhost" "httpproxyport" "httpproxypw" "httpproxyuser"
		   "l" "le" "lev" "leve" "level"
		   "li" "lin" "line" 
		   "lineg" "linega" "linegap" 
		   "lines" "linesi" "linesiz" "linesize"
		   "locale_functions" "locale_ui"
		   "mat" "mats" "matsi" "matsiz" "matsize"
		   "maxdb" "maxiter" "max_memory" "maxvar" "min_memory"
		   "niceness" "notifyuser"
		   "ob" "obs"
		   "pa" "pag" "page" "pages" "pagesi" "pagesiz" "pagesize"
		   "pformat"
		   "processors"
		   "reventr" "reventri" "reventrie" "reventries"
		   "rngstate" "rngstream"
		   "scheme" "scrollbufsize"
		   "se" "see" "seed"
		   "segmentsize"
		   "sformat"
		   "timeout1"
		   "timeout2"
		   "traced" "tracede" "tracedep" "tracedept" "tracedepth" 
		   "traceh" "tracehi" "tracehil" "tracehili" "tracehilit" "tracehilite" 
		   "update_interval"
		   "varlab" "varlabe" "varlabel" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; set incomplete commands; must come first for highlighting
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "se" "set"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "autotabgraphs"
		   "checksum" "coeftabresults" "copycolor"
		   "dockable"
		   "dockingg" "dockinggu" "dockinggui" "dockingguid" "dockingguide" "dockingguides"
		   "doublebuffer" "dp"
		   "emptycells" 
		   "fastscroll" "floatresults" "floatwindows"
		   "fvlabel" "fvtrack" "fvwrapon"
		   "g" "gr" "gra" "grap" "graph" "graphi" "graphic" "graphics"
		   "httpproxy" 
		   "httpproxya" "httpproxyau" "httpproxyaut" "httpproxyauth" 
		   "include_bitmap"
		   "locksplit" "locksplitt" "locksplitte" "locksplitter" "locksplitters" 
		   "logt" "logty" "logtyp" "logtype" 
		   "lstretch"
		   "matastrict"
		   "mo" "mor" "more" 
		   "notifyuser"
		   "odbcdriver"
		   "odbcmgr" 
		   "ou" "out" "outp" "outpu" "output"
		   "pinnable" "playsnd" "printcolor"
		   "r" "revkeyboard" "rm" "rms" "rmsg" "rng"
		   "searchdefault"
		   "showbaselevels" "showemptycells" "showomitted"
		   "smoothf" "smoothfo" "smoothfon" "smoothfont" "smoothfonts" 
		   "tr" "tra" "trac" "trace"
		   "tracee" "traceex" "traceexp" "traceexpa" "traceexpan" "traceexpand" 
		   "tracei" "tracein" "traceind" "traceinde" "traceinden" "traceindent" 
		   "tracen" "tracenu" "tracenum" "tracenumb" "tracenumbe" "tracenumber" 
		   "traces" "tracese" "tracesep"
		   "ty" "typ" "type" 
		   "update_prompt" "update_query"
		   "varabbrev" "varkeyboard"
		   "xptheme"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face) '(2 ado-needs-subcommand-face t))

	;; set with odd options---need to be split because of partial highlighting
    ;;;;;; ugh
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "se" "set"
		   ) 'words))
	  "[ \t]+"
	  "\\<\\("
	  "\\(?:conren\\(?:[ \t]+\\(?:clear\\|sf\\|bf\\|it\\|res\\|resu\\|resul\\|result\\|reset\\|txt\\|text\\|inp\\|inpu\\|input\\|err\\|erro\\|error\\|li\\|lin\\|link\\|hi\\|hil\\|hili\\|hilit\\|hilite\\|ulof\\|uloff\\|ulon\\)\\)?\\)"
	  "\\|"
	  "\\(?:copycolor[ \t]+\\(?:auto\\|autom\\|automa\\|automat\\|automati\\|automatic\\|asis\\|gs[123]\\)\\)"
	  "\\|"
	  "\\(?:dp[ \t]+\\(?:com\\|comm\\|comma\\|per\\|peri\\|perio\\|period\\)\\)"
	  "\\|"
	  "\\(?:fvtrack[ \t]+\\(?:term\\|factor\\)\\)"
	  "\\|"
	  "\\(?:log\\(?:t\\|ty\\|typ\\|type\\)[ \t]+\\(?:t\\|te\\|tex\\|text\\|s\\|sm\\|smc\\|smcl\\)\\)"
	  "\\|"
	  "\\(?:odbcdriver[ \t]+\\(?:ansi\\|unicode\\)\\)"
	  "\\|"
	  "\\(?:odbcmgr[ \t]+\\(?:iodbc\\|unixodbc\\)\\)"
	  "\\|"
	  "\\(?:printcolor[ \t]+\\(?:auto\\|autom\\|automa\\|automat\\|automati\\|automatic\\|asis\\|gs[123]\\)\\)"
	  "\\|"
	  "\\(?:search\\(?:d\\|de\\|def\\|defa\\|defau\\|defaul\\|default\\)[ \t]+\\(?:all\\|local\\|net\\)\\)"
	  "\\|"
	  "\\(?:showbaselevels[ \t]+\\(?:o\\(?:ff\\|n\\)\\|all\\)\\)"
	  "\\|"
	  "\\(?:t\\(?:y\\|yp\\|ype\\)[ \t]+\\(?:double\\|float\\)\\)"
	  "\\)\\>" ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t))
	
	;; set <foo> on/off commands

	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "se" "set"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "autotabgraphs"
		   "checksum" "coeftabresults"
		   "dockable"
		   "dockingg" "dockinggu" "dockinggui" "dockingguid" "dockingguide" "dockingguides"
		   "doublebuffer"
		   "fastscroll" "floatresults" "floatwindows" "fvlabel"
		   "g" "gr" "gra" "grap" "graph" "graphi" "graphic" "graphics"
		   "httpproxy" 
		   "httpproxya" "httpproxyau" "httpproxyaut" "httpproxyauth" 
		   "include_bitmap"
		   "locksplit" "locksplitt" "locksplitte" "locksplitter" "locksplitters"
		   "lstretch"
		   "matastrict"
		   "mo" "mor" "more"
		   "notifyuser"
		   "pinnable"
		   "playsnd"
		   "r" "revkeyboard" "rm" "rms" "rmsg"
		   "showemptycells" "showomitted"
		   "smoothf" "smoothfo" "smoothfon" "smoothfont" "smoothfonts" 
		   "tr" "tra" "trac" "trace"
		   "tracee" "traceex" "traceexp" "traceexpa" "traceexpan" "traceexpand" 
		   "tracei" "tracein" "traceind" "traceinde" "traceinden" "traceindent" 
		   "tracen" "tracenu" "tracenum" "tracenumb" "tracenumbe" "tracenumber" 
		   "traces" "tracese" "tracesep" 
		   "update_prompt" "update_query"
		   "varabbrev" "varkeyboard"
		   "xptheme"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "off" "on"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t) '(3 ado-subcommand-face t))
										;
	;; set charset command, with its odd subsubcommands (obsolete in Stata 14)
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "se" "set"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "charset"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "latin1" "mac"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-obsolete-face t) '(3 ado-obsolete-face))

	;; set rng command, with its odd subsubcommands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "se" "set"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "rng"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "default" "kiss32" "mt64" "mt64s"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t) '(3 ado-subcommand-face))

	;; set emptycells command, with its odd subsubcommands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "se" "set"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "emptycells"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "drop" "keep"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t) '(3 ado-subcommand-face))

	;; set fvwrapon command, with its odd subsubcommands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "se" "set"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "fvwrapon"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "width" "word"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t) '(3 ado-subcommand-face))

	;; set output command, with its odd subsubcommands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "se" "set"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "ou" "out" "outp" "outpu" "output"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "e" "er" "err" "erro" "error"
		   "i" "in" "inf" "info" "infor" "inform" 
		   "p" "pr" "pro" "proc" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t) '(3 ado-subcommand-face))

	;; ;; set showbaselevels command with it's odd all subcommand
	;; (list
	;;  (concat
	;;   "^[ \t]*"
	;;    (eval-when-compile 
	;; 	 (regexp-opt 
    ;;    '(
	;; 	 "se" "set"
	;; 	 ) 'words))
	;;    "[ \t]+"
	;;    "\\<\\(showbaselevels\\)\\>"
	;;    "[ \t]+"
	;;    (eval-when-compile 
	;; 	 (regexp-opt 
	;; 	  '(
	;; 		"all" "off" "on"
	;; 		) 'words))
	;;    )
	;;   '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face) '(3 ado-subcommand-face t))

	;; these appear to be obsolete and do not show in the manuals
	;;   some work, however.
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "se" "set"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "ANSI" 
		   "b" "be" "bee" "beep"
		   "charset" "contents" 
		   "d" "di" "dis" "disp" "displ" "displa" "display"
		   "help"
		   "IBM" 
		   "icmap"
		   "log"
		   "macgp" "macgph" "macgphe" "macgphen" "macgpheng" 
		   "macgphengi" "macgphengin" "macgphengine" 	  
		   "maxobs"
		   "mem" "memo" "memor" "memory"
		   "persistfv" "persistvtopic"
		   "piccom" "piccomm" "piccomme" "piccommen" "piccomment" "piccomments" 
		   "revwin" "revwind" "revwindo" "revwindow" 
		   "seed0" "shell" "smalldlg"
		   "smoothsize"
		   "te" "tex" "text" "texts" "textsi" "textsiz" "textsize"
		   "use_atsui_graph" "use_qd_text"
		   "varlabelpos"
		   "varwin" "varwind" "varwindo" "varwindow" "video"
		   "vir" "virt" "virtu" "virtua" "virtual" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-obsolete-face t))

	;; set eolchar command (obsolete)
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "se" "set"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "eolch" "eolcha" "eolchar" 		   
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "mac" "unix"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-obsolete-face t) '(3 ado-obsolete-face))

	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "se" "set"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "printcolor"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile
		(regexp-opt
		 '(
		   "grayscale" "greyscale"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t) 
	 '(3 ado-obsolete-face t))

	;; set_defaults _all
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "set_defaults"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "_all"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

    ;; the stteffects
	(list
	 (concat
	 ado-start-cmd-regexp
	  "\\(\\<stteffects\\>\\)"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "ipw"
		   "ipwra"
		   "ra"
		   "wra"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

    ;; the tebalance
	(list
	 (concat
	 ado-start-cmd-regexp
	  "\\(\\<tebalance\\>\\)"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "box"
		   "density"
		   "overid"
		   "summarize"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

    ;; the teffects
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\(\\<teffects\\>\\)"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "aipw"
		   "ipw"
		   "ipwra"
		   "nnmatch"
		   "overlap"
		   "psmatch"
		   "ra"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; the timer command
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "timer"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "clear" "list"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "timer"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "clear" "list" "off" "on"
		   ) 'words))
	  "[ \t]+"
	  "\\([0-9]+\\|`[[:alnum:]_`']'\\)" 
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t) '(3 ado-subcommand-face))
	
	;; tsfilter commands (all data-changing)
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "tsfilter"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "bk" "bw"
		   "cf"
		   "hp"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))


	;; the args command 

	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\(\\<args\\>\\)"
	  "[ \t]+"
 	  "\\(\\(?:" ado-stata-name-regexp ado-end-cmd-regexp "\\)+\\)"
	  )
	 '(1 ado-builtin-harmful-face) '(2 ado-variable-name-face t))

	;; char with sub commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "char"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "l" "li" "lis" "list" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "char"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "define"
		   "ren" "rena" "renam" "rename"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

    ;; the bcal commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "bcal"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "c" "ch" "che" "chec" "check"
		   "create"
		   "d" "de" "des" "desc" "descr" "descri" "describ" "describe" 
		   "dir"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "bcal"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "load"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	;; the ci commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile
		(regexp-opt
		 '(
		   "ci" "cii"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile
		(regexp-opt
		 '(
		   "mean" "means"
		   "prop" "propo" "propor" "proport" "proporti" "proportio" "proportion" "proportions"
		   "var" "vari" "varia" "varian" "varianc" "variance" "variances"
		   ) 'words))
	  ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))


    ;; the constraint commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "cons" "const" "constr" "constra" "constrai" "constrain" "constraint" 
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "d"
		   "de" "def" "defi" "defin" "define" 
		   "di" "dir"
		   "drop"
		   "free" "get"
		   "l" "li" "lis" "list"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

    ;; the confirm commands - could be a mess!
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "conf" "confi" "confir" "confirm"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "e" "ex" "exi" "exis" "exist" "existe" "existen" "existenc" "existence"
		   "f" "fi" "fil" "file"
		   "fo" "for" "form" "forma" "format" 
		   "mat" "matr" "matri" "matrix" 
		   "n" "name" "names" "nu" "num" "numb" "numbe" "number" 
		   "sca" "scal" "scala" "scalar" 
		   "v" "va" "var" "vari" "varia" "variab" "variabl" "variable"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "conf" "confi" "confir" "confirm"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "integer"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "n" "nu" "num" "numb" "numbe" "number"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t) '(3 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "conf" "confi" "confir" "confirm"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "n" "ne" "new"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "f" "fi" "fil" "file"
		   "v" "va" "var" "vari" "varia" "variab" "variabl" "variable"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t) '(3 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "conf" "confi" "confir" "confirm"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "byte" "double" "float" "int" "long"
		   "numeric"
		   "str" "stri" "strin" "string"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "v" "va" "var" "vari" "varia" "variab" "variabl" "variable"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t) '(3 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "conf" "confi" "confir" "confirm"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "date"
		   "numeric"
		   "str" "stri" "strin" "string"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "fo" "for" "form" "forma" "format" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t) '(3 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "conf" "confi" "confir" "confirm"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "ts"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "fo" "for" "form" "forma" "format" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-obsolete-face t) '(3 ado-subcommand-face t))

    ;;; confirm str#
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "conf" "confi" "confir" "confirm"
		   ) 'words))
	  "[ \t]+"
	  "\\<\\(str"
	  "\\(?:L"
	  "\\|"
	  "\\(?:[1-9][0-9]?[0-9]?\\)" 
	  "\\|"
	  "\\(?:1[0-9][0-9][0-9]\\)" 
	  "\\|"
	  "\\(?:20[0-3][0-9]\\)" 
	  "\\|"
	  "\\(?:204[0-5]\\)" 
	  "\\)\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "v" "va" "var" "vari" "varia" "variab" "variabl" "variable" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t) '(3 ado-subcommand-face t))

	;; confirm incomplete
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "conf" "confi" "confir" "confirm" 
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "byte" "double" "float" "int" "long"
		   "date"
		   "integer"
		   "n" "ne" "new" 
		   "numeric"
		   "str" "stri" "strin" "string"
		   "ts"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face) '(2 ado-needs-subcommand-face))
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "conf" "confi" "confir" "confirm"
		   ) 'words))
	  "[ \t]+"
	  "\\<\\(str"
	  "\\|"
	  "\\(?:str[1-9][0-9]?\\)" 
	  "\\|"
	  "\\(?:str1[0-9][0-9]\\)" 
	  "\\|"
	  "\\(?:str2[0-3][0-9]\\)" 
	  "\\|"
	  "\\(?:str24[0-4]\\)"
	  "\\)\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face) '(2 ado-needs-subcommand-face))

	;; merge
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "mer" "merg" "merge"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "1:1" "1:m" "m:1" "m:m"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face t) '(2 ado-subcommand-face t))

	;; mvtest
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "mvtest"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "corr" "corre" "correl" "correla" "correlat" "correlati" "correlatio" "correlation" "correlations" 
		   "cov" "cova" "covar" "covari" "covaria" "covarian" "covarianc" "covariance" "covariances" 
		   "m" "me" "mea" "mean" "means" 
		   "norm" "norma" "normal" "normali" "normalit" "normality"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "note" "notes" 
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "drop"
		   "renumber" "replace"
		   "search"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "note" "notes" 
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "l" "li" "lis" "list" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

    ;; the duplicates commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "duplicates"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "e" "ex" "exa" "exam" "examp" "exampl" "example" "examples" 
		   "l" "li" "lis" "list" 
		   "r" "re" "rep" "repo" "repor" "report" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "duplicates"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "drop"
		   "t" "ta" "tag" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "duplicates"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "b" "br" "bro" "brow" "brows" "browse" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-obsolete-face))

	;; estimates commands moved to just after estat commands
    ;; the _estimates commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "_est" "_esti" "_estim" "_estima" "_estimat" "_estimate" "_estimates"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "clear"
		   "dir" 
		   "drop"
		   "h" "ho" "hol" "hold"
		   "u" "un" "unh" "unho" "unhol" "unhold"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t))

    ;; the estat commands --- moved to after the obsolete commands

	;; the export/import commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "export"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "exc" "exce" "excel"
		   "dbase"
		   "delim" "delimi" "delimit" "delimite" "delimited" 
		   "hav" "have" "haver" 
		   "sasxport"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "import"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "exc" "exce" "excel"
		   "dbase"
		   "delim" "delimi" "delimit" "delimite" "delimited"
		   "fred"
		   "hav" "have" "haver" 
		   "sasxport"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

    ;; the file commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "file"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "close" "open" 
		   "q" "qu" "que" "quer" "query" 
		   "r" "re" "rea" "read" 
		   "seek" 
		   "set"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "file"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "sersetread" "sersetwrite"
		   "w" "wr" "wri" "writ" "write" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

    ;;
    ;; the gprefs commands
    ;;   (in multiple pieces)
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(gprefs\\)\\>"
	  "[ \t]+"
	  "\\<\\(set\\)\\>"
	  "[ \t]+"
	  "\\<\\(window\\)\\>"
	  "[ \t]+"
	  "\\<\\(scheme\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "black" "blackb" "blackbg" 
		   "custom1" "custom2" "custom3"
		   "mono" "monoc" "monoch" "monochr" "monochro" "monochrom" "monochrome" 
		   "white" "whiteb" "whitebg" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t) '(5 ado-subcommand-face t))

	;;
	;; the other gprefs set window 
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(gprefs\\)\\>"
	  "[ \t]+"
	  "\\<\\(set\\)\\>"
	  "[ \t]+"
	  "\\<\\(window\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "update"
		   "xsize"
		   "ysize"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(gprefs\\)\\>"
	  "[ \t]+"
	  "\\<\\(set\\)\\>"
	  "[ \t]+"
	  "\\<\\(window\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "displaythick"
		   "usegphsize"
		   ) 'words))
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "off" "on"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t) '(5 ado-subcommand-face t))
	;;
	;; the gprefs set scheme commands 
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(gprefs\\)\\>"
	  "[ \t]+"
	  "\\<\\(set\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "custom1" "custom2" "custom3"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile
		(regexp-opt
		 '(
		   "background_color"
		   "pen1_color" "pen2_color" "pen3_color" "pen4_color" "pen5_color" "pen6_color" "pen7_color" "pen8_color" "pen9_color"
		   "pen1_thick" "pen2_thick" "pen3_thick" "pen4_thick" "pen5_thick" "pen6_thick" "pen7_thick" "pen8_thick" "pen9_thick"
		   "symmag_all"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))
	;;
	;; shoulda never started this - the gprefs query scheme layout all by its lonesome
	(list
	 (concat
	  "\\<\\(gprefs\\)\\>"
	  "[ \t]+"
	  "\\<\\(query\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "custom1" "custom2" "custom3"
		   "window"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	 '(3 ado-subcommand-face t))

	;; gprefs incomplete
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gprefs"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "q" "qu" "que" "quer" "query" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face) '(2 ado-needs-subcommand-face))

	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(gprefs\\)\\>"
	  "[ \t]+"
	  "\\<\\(set\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "custom1" "custom2" "custom3"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face) '(2 ado-needs-subcommand-face)
	 '(3 ado-needs-subcommand-face))

	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(gprefs\\)\\>"
	  "[ \t]+"
	  "\\<\\(set\\)\\>"
	  "[ \t]+"
	  "\\<\\(window\\)\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face) '(2 ado-needs-subcommand-face)
	 '(3 ado-needs-subcommand-face))

	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(gprefs\\)\\>"
	  "[ \t]+"
	  "\\<\\(set\\)\\>"
	  "[ \t]+"
	  "\\<\\(window\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "displaythick"
		   "scheme"
		   "usegphsize"
		   "xsize"
		   "ysize"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face) '(2 ado-needs-subcommand-face)
	 '(3 ado-needs-subcommand-face) '(4 ado-needs-subcommand-face))

     ;;; worse than smcl ---- it's graph!
     ;;;  -> will need multiple copies of the subcommands for the () and || and plain versions
     ;;;     argh, what a pain in the rear.
	;; regular graph ... commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gr" "gra" "grap" "graph"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "bar" "box"
		   "close" "combine" "copy"
		   "des" "desc" "descr" "descri" "describ" "describe" 
		   "di" "dir" "dis" "disp" "displ" "displa" "display"
		   "dot"
		   "export"
		   "hbar" "hbox"
		   "matrix"
		   "pie" "play" "print"
		   "q" "qu" "que" "quer" "query"
		   "replay"
		   "save" "set"
		   "tw" "two" "twow" "twowa" "twoway"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t))

	;; the initial graph commands which are destructive
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gr" "gra" "grap" "graph"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "drop"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "_all"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face t) '(2 ado-subcommand-face t) '(3 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gr" "gra" "grap" "graph"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "drop" "rename" "use"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face t) '(2 ado-subcommand-face t))
	;; graph set commands ! seem to introduce all sorts of trouble
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gr" "gra" "grap" "graph"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "set"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "eps" "print" "ps" "svg"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t) '(3 ado-subcommand-face t))
	
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gr" "gra" "grap" "graph"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "set"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "window"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "fontface" "fontfacemono" "fontfacesans"
		   "fontfaceserif" "fontfacesymbol"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t) 
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))

	;; the graph twoway stuff 
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(\\(?:\\(?:gr\\|gra\\|grap\\|graph\\)[ \t]+\\)?\\)"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "tw" "two" "twow" "twowa" "twoway"
		   ) 'words))
	  "[ \t]+\(?[ \t]*"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "area"
		   "bar"
		   "con" "conn" "conne" "connec" "connect" "connecte" "connected"
		   "contour" "contourline"
		   "dot" "dropline"
		   "fpfit" "fpfitci" "function"
		   "hist" "histogram" 
		   "kdensity"
		   "line"
		   "lfit" "lfitci"
		   "lowess" "lpoly" "lpolyci"
		   "mband" "mspline"
		   "pcarrow" "pcarrowi" "pcbarrow"  "pccapsym" "pci" "pcscatter" "pcspike"
		   "qfit" "qfitci"
		   "rarea" "rbar" "rcap" "rcapsym" 
		   "rcon" "rconn" "rconne" "rconnec" "rconnect" "rconnecte" "rconnected" 
		   "rl" "rli" "rlin" "rline" 
		   "rsc" "rsca" "rscat" "rscatt" "rscatte" "rscatter" 
		   "rspike"
		   "sc" "sca" "scat" "scatt" "scatte" "scatter" 
		   "scatteri" "spike"
		   "tsline" "tsrline"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face t) 
	 '(3 ado-subcommand-face t))

	;; even more aggravating: things for which both graph and twoway are optional
	;; this is blank

	;; graph incomplete w/ multiple arguments
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gr" "gra" "grap" "graph"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "set"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face) '(2 ado-needs-subcommand-face))
	
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gr" "gra" "grap" "graph"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "set"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "window"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face) '(2 ado-needs-subcommand-face)
	 '(3 ado-needs-subcommand-face))

	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(\\(?:\\(?:gr\\|gra\\|grap\\|graph\\)[ \t]+\\)?\\)"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "tw" "two" "twow" "twowa" "twoway"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face) '(2 ado-needs-subcommand-face))

	
	;; icd9, icd9p, icd10, icd10cm commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "icd9" "icd9p" "icd10" "icd10cm" "icd10pcs"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "check"
		   "look" "looku" "lookup"
		   "q" "qu" "que" "quer" "query" 
		   "sea" "sear" "searc" "search" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; icd9s with generate
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "icd9" "icd9p" "icd10" "icd10cm" "icd10pcs"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "clean"
		   "gen" "gene" "gener" "genera" "generat" "generate" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	;; irt commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "irt"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "1pl" "2pl" "3pl"
		   "gpcm" "grm"
		   "hybrid"
		   "nrm"
		   "pcm"
		   "rsm"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t))

	;; irtgraph commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "irtgraph"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "icc" "iif"
		   "tcc" "tif"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t))
	
	;; fvset commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(fvset\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "b" "ba" "bas" "base" 
		   "clear"
		   "d" "de" "des" "desi" "desig" "design" 
		   "report"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; log commands, cmdlog also (though cmdlog query is undocumented)
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile
		(regexp-opt
		 '(
		   "cmdlog" "log"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "c" "cl" "clo" "clos" "close" 
		   "of" "off" "on"
		   "query"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; misstable commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(misstable\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "nest" "neste" "nested"
		   "sum" "summ" "summa" "summar" "summari" "summariz" "summarize" 
		   "pat" "patt" "patte" "patter" "pattern" "patterns" 
		   "tree"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; some of the matrix commands
	;; with no matrix arguments - harmless
	;; with one following argument but no subcommand
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "mat" "matname" "mat_put_rr" "matr" "matri" "matrix"
		   ) 'words))
	  "[ \t]+"
	  ado-stata-name-bound-regexp
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-matrix-name-face))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "mat" "matr" "matri" "matrix"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "d" "di" "dir"
		   "post"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t))
	;; with no matrix arguments - harmful
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "mat" "matr" "matri" "matrix"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "sco" "scor" "score"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "mat" "matr" "matri" "matrix"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile
		(regexp-opt
		 '(
		   "dispCns" "makeCns"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face)  '(2 ado-obsolete-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "mat" "matr" "matri" "matrix"
		   ) 'words))
	  "[ \t]+"
	  "\\<\\(drop\\)\\>"
	  "[ \t]+"
	  "\\<\\(_all\\)\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	 '(3 ado-subcommand-face t))
	;; doesn't quite work, because it underlines the spaces
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "mat" "matr" "matri" "matrix"
		   ) 'words))
	  "[ \t]+"
	  "\\<\\(drop\\)\\>"
	  "[ \t]+"
	  "\\(\\(?:" ado-stata-name-regexp "[ \t]*\\)+\\)"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t) '(3 ado-matrix-name-face))

	;; with one following argument 
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "mat" "matr" "matri" "matrix"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "ac" "acc" "accu" "accum" 
		   "cole" "coleq" 
		   "coln" "colna" "colnam" "cloname" "colnames"
		   "def" "defi" "defin" "define"
		   "dis" "diss" "dissi" "dissim" "dissimi" "dissimil" "dissimila" "dissimilar" 
		   "dissimilari" "dissimilarit" "dissimilarity" 
		   "glsa" "glsac" "glsacc" "glsaccu" "glsaccum"
		   "in" "inp" "inpu" "input" 
		   "opaccum"
		   "rowe" "roweq" 
		   "rown" "rowna" "rownam" "rowname" "rownames"
		   "veca" "vecac" "vecacc" "vecaccu" "vecaccum"
		   ) 'words))
	  "[ \t]+"
	  ado-stata-name-bound-regexp
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	 '(3 ado-matrix-name-face t))
	;; with one following arguments -- but harmless (good grief!)
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "mat" "matr" "matri" "matrix"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "l" "li" "lis" "list" 
		   ) 'words))
	  "[ \t]+"
	  ado-stata-name-bound-regexp
	  "\\b"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t)
	 '(3 ado-matrix-name-face t))
	;; with two following arguments
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "mat" "matr" "matri" "matrix"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "eigenval" "eigenvalu" "eigenvalue" "eigenvalues" 
		   "ren" "rena" "renam" "rename" 
		   "syme" "symei" "symeig" "symeige" "symeigen"
		   ) 'words))
	  "[ \t]+"
	  "\\<"
	  ado-stata-name-bound-regexp
	  "\\>"
	  "[ \t]+"
	  "\\<"
	  ado-stata-name-bound-regexp
	  "\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	 '(3 ado-matrix-name-face t) '(4 ado-matrix-name-face t))
	;; with three(!) following arguments
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "mat" "matr" "matri" "matrix"
		   ) 'words))
	  "[ \t]+"
	  "\\<\\(svd\\)\\>"
	  "[ \t]+"
	  "\\<"
	  ado-stata-name-bound-regexp
	  "\\>"
	  "[ \t]+"
	  "\\<"
	  ado-stata-name-bound-regexp
	  "\\>"
	  "[ \t]+"
	  "\\<"
	  ado-stata-name-bound-regexp
	  "\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	 '(3 ado-matrix-name-face t) '(4 ado-matrix-name-face t)
	 '(5 ado-matrix-name-face t))
	;; with three(!) following arguments but no friggin matrix command! 
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(matcproc\\)\\>"
	  "[ \t]+"
	  "\\<"
	  ado-stata-name-bound-regexp
	  "\\>"
	  "[ \t]+"
	  "\\<"
	  ado-stata-name-bound-regexp
	  "\\>"
	  "[ \t]+"
	  "\\<"
	  ado-stata-name-bound-regexp
	  "\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-matrix-name-face t)
	 '(3 ado-matrix-name-face t) '(4 ado-matrix-name-face t))
	;; now for the svmat command
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(svmat\\)\\>"
	  "[ \t]+"
	  "\\<"
	  ado-stata-name-bound-regexp
	  "\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-matrix-name-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(svmat\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "byte" "double" "float" "int" "long"
		   ) 'words))
	  "[ \t]+"
	  "\\<"
	  ado-stata-name-bound-regexp
	  "\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	 '(3 ado-matrix-name-face t))

	;; the ml commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(ml\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "check" "clear" "count"
		   "di" "dis" "disp" "displ" "displa" "display"
		   "foot" "footn" "footno" "footnot" "footnote"
		   "gr" "gra" "grap" "graph"
		   "init"
		   "max" "maxi" "maxim" "maximi" "maximiz" "maximize"
		   "me" "met" "meth" "metho" "method"
		   "mod" "mode" "model"
		   "p" "pl" "plo" "plot" 
		   "q" "qu" "que" "quer" "query"
		   "rep" "repo" "repor" "report"
		   "sea" "sear" "searc" "search"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; the ml commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(ml\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "trace"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face) '(2 ado-needs-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(ml\\)\\>"
	  "[ \t]+"
	  "\\(count\\|trace\\)"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "off" "on"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t)
	 '(3 ado-subcommand-face t))
   
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(ml\\)\\>"
	  "[ \t]+"
	  "\\(count\\)"
	  "[ \t]+"
	  "\\(clear\\)"
	  ado-end-cmd-regexp )
	'(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t)
	'(3 ado-subcommand-face t))


	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(ml\\)\\>"
	  "[ \t]+"
	  "\\<\\(score\\)\\>"
	  )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))
	;; obsolete ml commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(ml\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "b" "be" "beg" "begi" "begin" 
		   "de" "dep" "depn" "depna" "depnam" "depname" "depnames" 
		   "f" "fu" "fun" "func" "funct" "functi" "functio" "function"
		   "ml" "mlo" "mlou" "mlout"
		   "pl" "plo" "plot"
		   "po" "pos" "post" 
		   "sa" "sam" "samp" "sampl" "sample"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-obsolete-face))
	;; the net commands 
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(net\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "cd"
		   "d" "de" "des" "desc" "descr" "descri" "describ" "describe"
		   "from" "get" 
		   "ins" "inst" "insta" "instal" "install" 
		   "link"
		   "q" "qu" "que" "quer" "query"
		   "search" "sj" "stb"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(net\\)\\>"
	  "[ \t]+"
	  "\\<\\(set\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "ado" "other"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	 '(3 ado-subcommand-face t))

	;; net incomplete
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(net\\)\\>"
	  "[ \t]+"
	  "\\<\\(set\\)\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face) '(2 ado-needs-subcommand-face))

	;; ado commands (-ado- by itself is OK)
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(ado\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "d" "de" "des" "desc" "descr" "descri" "describ" "describe"
		   "dir"
		   "uninstall"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	
	;; odbc commands 
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(odbc\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "des" "desc" "descr" "descri" "describ" "describe"
		   "li" "lis" "list"
		   "q" "qu" "que" "quer" "query"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(odbc\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "in" "ins" "inse" "inser" "insert" 
		   "lo" "loa" "load" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))
	;; odbc functional subcommands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(odbc\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "exe" "exec" 
		   "sql" "sqlf" "sqlfi" "sqlfil" "sqlfile" 
		   ) 'words))
	  "(" )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	;; palette commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(palette\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "color"
		   "line" "linep" "linepa" "linepal" "linepale" "linepalet" "linepalett" "linepalette" 
		   "symbol" "symbolp" "symbolpa" "symbolpal" "symbolpale" "symbolpalet" "symbolpalett" "symbolpalette" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; postutil commands - both of them
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(postutil\\)\\>"
	  "[ \t]+"
	  "\\<\\(dir\\)\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(postutil\\)\\>"
	  "[ \t]+"
	  "\\<\\(clear\\)\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	;; query/set_defaults commands 
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "q" "qu" "que" "quer" "query"
		   "set_defaults"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "eff" "effi" "effic" "effici" "efficie" "efficien" "efficienc" "efficiency" 
		   "graph" "graphi" "graphic" "graphics" 
		   "inter" "interf" "interfa" "interfac" "interface"
		   "mata"
		   "mem" "memo" "memor" "memory" 
		   "net" "netw" "netwo" "networ" "network" 
		   "out" "outp" "outpu" "output" 
		   "oth" "othe" "other" 
		   "trace"
		   "unicode"
		   "up" "upd" "upda" "updat" "update" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
    
	;; the reshape commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(reshape\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "clear"
		   "error"
		   "i" "j"
		   "long"
		   "wide"
		   "xi" "xij"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(reshape\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "q" "qu" "que" "quer" "query" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; the snapshot commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(snapshot\\)\\>"
	  "[ \t]+"
	  "\\<\\(restore\\)\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(snapshot\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "erase"
		   "label" "list"
		   "save"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	;; one lonely sysuse subcommand
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(sysuse\\)\\>"
	  "[ \t]+"
	  "\\<\\(dir\\)\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; the _return commands (not the return commands)
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "_ret" "_retu" "_retur" "_return"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "dir" "drop"
		   "hold"
		   "res" "rest" "resto" "restor" "restore" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; the return commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "ret" "retu" "retur" "return"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "add" "clear"
		   "li" "lis" "list" 
		   "loc" "loca" "local" 
		   "mat" "matr" "matri" "matrix" 
		   "sca" "scal" "scala" "scalar" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	;; ereturn
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "eret" "eretu" "eretur" "ereturn"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "clear"
		   "di" "dis" "disp" "displ" "displa" "display" 
		   "li" "lis" "list" 
		   "loc" "loca" "local" 
		   "mat" "matr" "matri" "matrix" 
		   "post" "repost"
		   "sca" "scal" "scala" "scalar" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	;; sreturn
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "sret" "sretu" "sretur" "sreturn"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "clear" 
		   "li" "lis" "list" 
		   "loc" "loca" "local"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; scc commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(ssc\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "copy"
		   "d" "de" "des" "desc" "descr" "descri" "describ" "describe"
		   "hot"
		   "inst" "insta" "instal" "install"
		   "new" "type" "uninstall"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	;; scc commands---obsolete

	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(ssc\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "what" "whats" "whatsn" "whatsne" "whatsnew" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-obsolete-face))

	;; the serset commands
	;; NO subcommands, b/c -serset- is legal by itself
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(serset\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "clear"
		   "cr" "cre" "crea" "creat" "create" "create_cspline" "create_xmedians"
		   "drop"
		   "reset_id"
		   "set" "sort"
		   "use"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(serset\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "dir"
		   "su" "sum" "summ" "summa" "summar" "summari" "summariz" "summarize" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; the spmatrix commands - harmful incomplete
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(spmatrix\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "clear"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

		;; the spmatrix commands - harmless
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(spmatrix\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "dir"
		   "summarize"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	;; spmatrix incomplete with subcommand
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(spmatrix\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "copy" "create"
		   "drop"
		   "export"
		   "fromdata"
		   "import"
		   "matafromsp"
		   "note"
		   "normalize"
		   "save"
		   "spfrommata"
		   "use"
		   "userdefined"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face) '(2 ado-needs-subcommand-face t))

	;; spmatrix incomplete with two subcommands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(spmatrix\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "create"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile
		(regexp-opt
		 '(
		   "cont" "conti" "contig" "contigu" "contigui" "contiguit" "contiguity"
		   "idist" "idista" "idistan" "idistanc" "idistance"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face t) '(2 ado-needs-subcommand-face t)
	 '(3 ado-needs-subcommand-face t))
	;; spmatrix incomplete with subcommand
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(spmatrix\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "copy"
		   "drop"
		   "export"
		   "fromdata"
		   "import"
		   "matafromsp"
		   "note"
		   "normalize"
		   "save"
		   "spfrommata"
		   "use"
		   "userdefined"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face t) '(2 ado-needs-subcommand-face t))
	;; dangerous spmatrix with one matrix
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(spmatrix\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "drop"
		   "export"
		   "fromdata"
		   "import"
		   "note"
		   "normalize"
		   "save"
		   "spfrommata"
		   "use"
		   "userdefined"
		   ) 'words))
	  "[ \t]+"
	  ado-stata-name-bound-regexp
	  "\\b"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face t) '(2 ado-subcommand-face t)
	 '(3 ado-matrix-name-face t)) 
	;; harmless spmatrix with one matrix
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(spmatrix\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "summarize"
		   ) 'words))
	  "[ \t]+"
	  ado-stata-name-bound-regexp
	  "\\b"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t)
	 '(3 ado-matrix-name-face t)) 
	;; harmful spmatrix with two matrices
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(spmatrix\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "copy"
		   "matafromsp"
		   ) 'words))
	  "[ \t]+"
	  ado-stata-name-bound-regexp
	  "\\b"
	  "[ \t]+"
	  ado-stata-name-bound-regexp
	  "\\b"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t)
	 '(3 ado-matrix-name-face t) '(4 ado-matrix-name-face t)) 

	;; harmful spmatrix create with two subcommands and one matrix
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(spmatrix\\)\\>"
	  "[ \t]+"
	  "\\<\\(create\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "cont" "conti" "contig" "contigu" "contigui" "contiguit" "contiguity"
		   "idist" "idista" "idistan" "idistanc" "idistance" 
		   ) 'words))
	  "[ \t]+"
	  ado-stata-name-bound-regexp
	  "\\b"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face t) '(2 ado-subcommand-face t)
	 '(3 ado-subcommand-face t) '(4 ado-matrix-name-face t)) 
	

	
	;; sts commands, NO partial, because -sts- is good by itself
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(sts\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "g" "gr" "gra" "grap" "graph"
		   "l" "li" "lis" "list"
		   "t" "te" "tes" "test"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(sts\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gen" "gene" "gener" "genera" "generat" "generate"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	;; the sw commands
	;; the sw commands are all now obsolete, because of syntax change
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(sw\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "clogit" "cloglog" "cnreg" "cox" 
		   "ereg" 
		   "gamma" "glm" "gompertz" 
		   "hetprob" 
		   "llogistic" "lnormal" "logistic" "logit" "nbreg" "ologit" "oprobit"
		   "poisson" "probit" "qreg" "reg" "regr" "regre" "regres" "regress"
		   "scobit" "stcox" "streg" "tobit" "weibull"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-obsolete-face) '(2 ado-obsolete-face))

    ;; the gph commands (really obsolete)
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gph"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "arc"
		   "box"
		   "clear" "close"
		   "font"
		   "line"
		   "open"
		   "pen" "point"
		   "text"
		   "vline" "vpoint" "vpoly" "vtext"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-obsolete-face t) '(2 ado-obsolete-face t))

	;; mfp arguments --- obsolete in Stata 11
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(mfp\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "clogit" "cnreg" "glm" "logistic" "logit" "mlogit"
		   "nbreg" "ologit" "oprobit" "poisson"
		   "probit" "qreg" "regress" "stcox" "streg" "xtgee"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-obsolete-face))

	;; the sysdir commands
	;; no partial, b/c -sysdir- is legal
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(sysdir\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "l" "li" "lis" "list"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(sysdir\\)\\>"
	  "[ \t]+"
	  "\\(set\\)"
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(sysdir\\)\\>"
	  "[ \t]+"
	  "\\(set\\)"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "BASE"
		   "OLDPLACE"
		   "PERSONAL" "PLUS"
		   "SITE" "STATA"
		   "UPDATES"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t)
	 '(3 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(personal\\)\\>"
	  "[ \t]+"
	  "\\<\\(dir\\)\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; tsunab and unab commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "fvunab" "tsunab" "unab"
		   ) 'words))
	  "[ \t]+"
	  ado-stata-name-bound-regexp 
	  "[ \t]*:[ \t]*"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-variable-name-face))

	;; the tssmooth commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(tssmooth\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "d" "de" "dex" "dexp" "dexpo" "dexpon" "dexpone" "dexponen" "dexponent" "dexponenti" "dexponentia" "dexponential" 
		   "e" "ex" "exp" "expo" "expon" "expone" "exponen" "exponent" "exponenti" "exponentia" "exponential" 
		   "h" "hw" "hwi" "hwin" "hwint" "hwinte" "hwinter" "hwinters" 
		   "ma" "nl"
		   "s" "sh" "shw" "shwi" "shwin" "shwint" "shwinte" "shwinter" "shwinters" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))
	;;
	;; the translator commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(translator\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "q" "qu" "que" "quer" "query" 
		   "reset"
		   "set"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	;;
	;; the transmap commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(transmap\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "def" "defi" "defin" "define" 
		   "q" "qu" "que" "quer" "query" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	;; 
	;; the update commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(update\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "all"
		   "from"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(update\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "q" "qu" "que" "quer" "query" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	;; obsolete update commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(update\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "ado"
		   "executable"
		   "swap"
		   "utilities"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-obsolete-face t))

	;; the xtcointest commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(xtcointest\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "kao"
		   "pedroni"
		   "westerlund"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; the xtunitroot commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(xtunitroot\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "breitung"
		   "fisher"
		   "hadri"
		   "ht"
		   "ips"
		   "llc"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; the fcast commands which leave data alone
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(fcast\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "g" "gr" "gra" "grap" "graph" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	;; fcast commands which alter data
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(fcast\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "c" "co" "com" "comp" "compu" "comput" "compute" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	;; the obsolete varfcast commands with sub commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(varfcast\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "c" "cl" "cle" "clea" "clear" 
		   "co" "com" "comp" "compu" "comput" "compute" 
		   "g" "gr" "gra" "grap" "graph" 
		   "c" "co" "com" "comp" "compu" "comput" "compute" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-obsolete-face) '(2 ado-obsolete-face))

	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(irf\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "cg" "cgr" "cgra" "cgrap" "cgraph" 
		   "ct" "cta" "ctab" "ctabl" "ctable" 
		   "d" "de" "des" "desc" "descr" "descri" "describ" "describe" 
		   "di" "dir"
		   "g" "gr" "gra" "grap" "graph" 
		   "og" "ogr" "ogra" "ograp" "ograph" 
		   "t" "ta" "tab" "tabl" "table" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	;; irf partial commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(irf\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "g" "gr" "gra" "grap" "graph"
		   "t" "ta" "tab" "tabl" "table"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face t) '(2 ado-needs-subcommand-face t))
	;; irf graph/table and their subcommands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(irf\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "g" "gr" "gra" "grap" "graph"
		   "t" "ta" "tab" "tabl" "table"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "cdm" "cirf" "coirf"
		   "dm"
		   "fevd"
		   "irf"
		   "oirf"
		   "sfevd" "sirf"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t)
	 '(3 ado-subcommand-face t))
	;; the irf commands which alter data
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(irf\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "a" "ad" "add" 
		   "cr" "cre" "crea" "creat" "create" 
		   "drop"
		   "ren" "rena" "renam" "rename" 
		   "set"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))
	;; the irf commands which are obsolete
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(irf\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "di" "dir"
		   "erase"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-obsolete-face t))

	;; obsolete varirf commands
	;; the varirf commands which leave data alone
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(varirf\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "a" "ad" "add" 
		   "cg" "cgr" "cgra" "cgrap" "cgraph" 
		   "cr" "cre" "crea" "creat" "create" 
		   "ct" "cta" "ctab" "ctabl" "ctable" 
		   "d" "de" "des" "desc" "descr" "descri" "describ" "describe" 
		   "di" "dir"
		   "drop" "erase"
		   "g" "gr" "gra" "grap" "graph" 
		   "og" "ogr" "ogra" "ograp" "ograph" 
		   "ren" "rena" "renam" "rename" 
		   "set"
		   "t" "ta" "tab" "tabl" "table" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-obsolete-face) '(2 ado-obsolete-face t))
	;;
	;; the view commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(view\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "ado" "ado_d"
		   "browse"
		   "file"
		   "help" "help_d"
		   "net" "net_d" "news"
		   "search" "search_d"
		   "view_d"
		   "update" "update_d"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; the webuse commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(webuse\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "query" "set"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; the window commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "win" "wind" "windo" "window"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "d"
		   "di" "dia" "dial" "dialo" "dialog"
		   "dir" "drop"
		   "fo" "fop" "fope" "fopen"
		   "fs" "fsa" "fsav" "fsave"
		   "l" "list"
		   "push"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "win" "wind" "windo" "window"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "c" "co" "con" "cont" "contr" "contro" "control"
		   ) 'words))
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "button" "check" "clear"
		   "edit"
		   "mcombo" "msimple"
		   "radbegin"
		   "radend"
		   "radio"
		   "scombo"
		   "ssimple"
		   "static"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	 '(3 ado-subcommand-face t))
	;; 
	;; the window manage commands	
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "win" "wind" "windo" "window"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "man" "mana" "manag" "manage"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "associate"
		   "maintitle"
		   "minimize" 
		   "restore"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	 '(3 ado-subcommand-face t))

	;; fix up all the 4-word crap
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "win" "wind" "windo" "window"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "man" "mana" "manag" "manage"
		   ) 'words))
	  "[ \t]+"
	  "\\<\\(close\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "graph" "viewer"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "win" "wind" "windo" "window"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "man" "mana" "manag" "manage"
		   ) 'words))
	  "[ \t]+"
	  "\\<\\(prefs\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "default" "load" "save"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "win" "wind" "windo" "window"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "man" "mana" "manag" "manage"
		   ) 'words))
	  "[ \t]+"
	  "\\<\\(print\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "graph" "viewer"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "win" "wind" "windo" "window"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "man" "mana" "manag" "manage"
		   ) 'words))
	  "[ \t]+"
	  "\\<\\(rename\\)\\>"
	  "[ \t]+"
	  "\\<\\(graph\\)\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "win" "wind" "windo" "window"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "man" "mana" "manag" "manage"
		   ) 'words))
	  "[ \t]+"
	  "\\<\\(update\\)\\>"
	  "[ \t]+"
	  "\\<\\(variable\\)\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	 '(3 ado-obsolete-face t) '(4 ado-obsolete-face t))
	;; platform-specific windows manage
	;; need better way to work with platform-specific commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "win" "wind" "windo" "window"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "man" "mana" "manag" "manage"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "associate"
		   "docklabel"
		   "maintitle"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	 '(3 ado-platform-specific-face t))
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "win" "wind" "windo" "window"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "man" "mana" "manag" "manage"
		   ) 'words))
	  "[ \t]+"
	  "\\<\\(maintitle\\)\\>"
	  "[ \t]+"
	  "\\<\\(reset\\)\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	 '(3 ado-platform-specific-face t) '(4 ado-platform-specific-face t))
	;; the window manage forward commands [sheesh]
	;; not platform specific
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "win" "wind" "windo" "window"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "man" "mana" "manag" "manage"
		   ) 'words))
	  "[ \t]+"
	  "\\<\\(forward\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "command" "doeditor" "graph" "help" "results" "review" "variables" "viewer"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))
	;; the window manage close/print commands [all taken care of above]

	;; the window menu commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "win" "wind" "windo" "window"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "m" "me" "men" "menu"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "add_recentfiles"
		   "clear"
		   "refresh"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	 '(3 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "win" "wind" "windo" "window"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "m" "me" "men" "menu"
		   ) 'words))
	  "[ \t]+"
	  "\\<\\(append\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "item"
		   "separator"
		   "submenu"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "win" "wind" "windo" "window"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "stop" "stopb" "stopbo" "stopbox"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "note"
		   "rusure"
		   "stop"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	 '(3 ado-subcommand-face t))

	;; incomplete window multiword subcommands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "win" "wind" "windo" "window"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "c" "co" "con" "cont" "contr" "contro" "control"
		   "man" "mana" "manag" "manage"
		   "m" "me" "men" "menu"
		   "stop" "stopb" "stopbo" "stopbox"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face) '(2 ado-needs-subcommand-face))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "win" "wind" "windo" "window"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "man" "mana" "manag" "manage"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "close" "forward" "prefs" "print" "rename" "update"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face) '(2 ado-needs-subcommand-face)
	 '(3 ado-needs-subcommand-face))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "win" "wind" "windo" "window"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "m" "me" "men" "menu"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "append"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face) '(2 ado-needs-subcommand-face)
	 '(3 ado-needs-subcommand-face))

	;; the xwindow commands---obsolete from at least Stata 9.1

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "xwin" "xwind" "xwindo" "xwindow"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "de" "def" "defi" "defin" "define" 
		   "di" "dir"
		   "drop"
		   "l" "li" "lis" "list"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-obsolete-face) '(2 ado-obsolete-face))

	;; all the endless Stata keywords (not in a good order)
	;; first those keywords which must start a line
	;; note that these will look like text if preceded by a comment
	;; (but comments shouldn't be before the command, anyway, 'cuz it makes things hard to read)
	(list
	 (concat
	  "^[ \t]*"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "by"
		   "cap" "capt" "captu" "captur" "capture"
		   "char" 
		   "e" "err" "erro" "error" "ex" "exi" "exit" 
		   "for"
		   "n" "no" "noi" "nois" "noisi" "noisil" "noisily"
		   "qui" "quie" "quiet" "quietl" "quietly" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face))

	(list
	 (concat
	  "^[ \t]*"
	  "\\(" ado-capture-noisily-regexp "\\)"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t))

	(list
	 (concat
	  "^[ \t]*"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "mata"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face))

	;; commands
	;; note that the really short abbreviations could make a mess of things
	;;
	;; These are split to allow compiling!
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "_coef_table" "_crcswxx"
		   "_datasig" "_datasign" "_datasigna" "_datasignat" "_datasignatu" "_datasignatur" "_datasignature" 
		   "_rmcoll" "_rmdcoll" "_robust"
		   "#r" "#re" "#rev" "#revi" "#revie" "#review" 
		   "about" "ac" "acprplot" 
		   "ado" "adopath" "adoupdate" "alpha" "ameans" 
		   "an" "ano" "anov" "anova" 
		   "arch" "areg" "arfima" "arima" 
		   "as" 
		   "asclogit"
		   "asmixlogit"
		   "asmprobit"
		   "asroprobit"
		   "ass" "asse" "asser" "assert" 
		   "avplot" "avplots"
		   "bayes"
		   "bayesmh" "bayesgraph"
		   "b" "be" "bee" "beep"
		   "betareg" "binreg" "biprobit" "biplot" "bitest" "bitesti"
		   "bootstrap" "boxcox" "br" "break" "brier" 
		   "bro" "brow" "brows" "browse" 
		   "brr" "bsqreg" "bstat"
		   "ca" "cabiplot" "camat" "candisc" "canon" "caprojection" "cat" 
		   "cc" "cci" "cchart" "centile" "cf" 
		   "changeeol"
		   "checkestimationsample" "checksum" 
		   "clog" "clogi" "clogit" "clogitp" "cloglog"
		   "cls"
		   "close" "cmdlog" "cmdtool" 
		   "cnsreg" "codebook" "compare" 
		   "cons" "const" "constr" "constra" "constrai" "constrain" "constraint"
		   "continue"
		   "contrast"
		   "copy" "copyright" 
		   "cor" "corc" "corr" "corre" "correl" "correla" "correlat" "correlate"
		   "corrgram"
		   "cou" "coun" "count" 
		   "cox" "cpoisson" "cprplot"  "cs" "csi" 
		   "ct" "ctset" 
		   "cumsp" "cumul" "cusum"
		   "command"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face))
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "d"
		   "dataex"
		   "datasig" "datasign" "datasigna" "datasignat" "datasignatu" "datasignatur" "datasignature"
		   "db"
		   "de" "des" "desc" "descr" "descri" "describ" "describe"
		   "dfactor" "dfbeta" "dfgls" "dfuller" 
		   "di" "diflogistic" "difmh" "dir"
		   "dis" "discrim" "disp" "disp_res" "disp_s" 
		   "displ" "displa" "display"
		   "do" 
		   "doed" "doedi" "doedit" 
		   "dotplot"
		   "ds" "dstdize"
		   "dyndoc" "dyntext"
		   "eivreg" "eq" "esizei"
		   "est" "esti" "estim" "estima" "estimat" "estimate" "estimates" 
		   "eteffects" "etpoisson" "etregress"
		   "exlogistic" "expoisson"
		   "fac" "fact" "facto" "factor" "factormat"
		   "findfile" "fit"
		   "fl" "fli" "flis" "flist"
		   "for" "fp" "fpredict" 
		   "fracplot"
		   "freddescribe" "fredsearch"
		   "frontier" "fsl" "fvexpand"
		   "gladder" "gllamm" "glm" "glmpred" 
		   "gmm" "gnbreg"
		   "gphdot" "gphpen" "gr7" "graph7" "grmap" "grmeanby" "gsem"
		   "h"
		   "hadimvo" "hausman"
		   "heckman" "heckoprobit" "heckpoisson" "heckprob" "heckprobit" 
		   "he" "hel" "help" 
		   "hetprob" "hetprobit" "hetregress"
		   "hexdump" "hilite"
		   "hist" "histo" "histog" "histogr" "histogra" "histogram" 
		   "hlu" "hotel" "hotelling"
		   "icc" "include" "ins" "insp" "inspe" "inspec" "inspect" "intreg" 
		   "iqreg" "ir" "iri" 
		   "isid" "istdize" 
		   "ivprobit" "ivregress" "ivtobit"
		   "jackknife" "javacall"
		   "kap" "kappa" "kapwgt" "kdensity" "ksm" "ksmirnov" "ktau"
		   "kwallis"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "l"
		   "labelbook" "ladder"
		   "levelsof"
		   "li" "line"
		   "lincom" "linktest" 
		   "lis" "list"
		   "loadingplot" "log"
		   "logi" "logistic" "logit" 
		   "loneway" "lookfor" "lowess" "lpredict" "lpoly"
		   "lroc" "lrtest" "ls" "lsens" "ltable" "lv" "lvr2plot"
		   "man" "mano" "manov" "manova" "manovatest" 
		   "margins" "marginsplot" "markdown" "matlist"
		   "mca" "mcaplot" "mcaprojection" "mcc" "mcci" 
		   "mds" "mdsconfig" "mdslong" "mdsmat" "mdsshepard"
		   "mean" "mecloglog" "median" "meglm" "meintreg" "memory" 
		   "melogit" "menbreg" "menl" "meologit" "meoprobit" 
		   "mepoisson" "meprobit" "meqrlogit" "meqrpoisson" "mestreg" "metobit"
		   "mfp" "mhodds"
		   "mixed"
		   "mlexp" "mlog" "mlogi" "mlogit"
		   "mor" "more"
		   "mprobit" "mvreg" "mx_param"
		   "nbreg" "nestreg" "net" "newey" "news"
		   "nl" "nlcom" "nlogit" "nlogittree" "nlsur" 
		   "note" "notes" "novarabbrev"
		   "npgraph"
		   "numlabel"
		   "nptrend" "numlist"
		   "olog" "ologi" "ologit"
		   "ologitp" 
		   "on" "one" "onew" "onewa" "oneway"
		   "oprob" "oprobi" "oprobit"
		   "oprobitp"
		   "pac" "pca" "pcamat" "pchart" "pchi" "pcorr" "pergram" "permute" "personal"
		   "pkcross" "pkequiv" "pkexamine" "pksumm"
		   "pl" "plo" "plot"
		   "pnorm" "poisson" "postest" "pperron"
		   "prais" "print"
		   "prob" "probi" "probit"
		   "procoverlay" "procrustes"
		   "projman" "projmana" "projmanag" "projmanage" "projmanager" 
		   "proportion"
		   "prtest" "prtesti"
		   "psdensity"
		   "putexcel"
		   "pwcompare" "pwcorr" "pwd" "pwmean"
		   "q" "qchi" "qnorm" "qqplot" "qreg" "qladder" "quadchk" "quantile" 
		   "qu" "que" "quer" "query"
		   "ranksum" "ratio" "rchart" "regdw" "regph" 
		   "reg" "reg3" "regr" "regre" "regres" "regress"
		   "reshape"
		   "robvar"
		   "roccomp" "rocfit" "rocgold" "rocplot" "rocreg" "rocregplot" "roctab"
		   "rologit" "rolling"
		   "rot" "rota" "rotat" "rotate"
		   "rotatemat"
		   "rreg"
		   "ru" "run" "runtest" "rvfplot" "rvpplot"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "sconfirm" 
		   "sc" "sca" "scat" "scatt" "scatte" "scatter" 
		   "scobit" "scoreplot" "scree" "screeplot"
		   "sdr" "sdtest" "sdtesti" "search" "sem" "serrbar" "serset"
		   "sfrancia" 
		   "sh" "she" "shewhart" "shel" "shell" 
		   "signestimationsample" "signrank" "signtest"
		   "sktest" "sleep" "slog" "slogit"
		   "spdistance" "spearman" "spikeplot" "spivregress" "spregress"
		   "sqreg"
		   "sspace"
		   "st" "st_is" "st_show" "st_ct" "stci"
		   "stcox" "stcoxkm" 
		   "stcrr" "stcrre" "stcrreg"
		   "stcurv" "stcurve" "stdescribe"
		   "stem" "stepwise"
		   "stereg" "stintreg" "stir" "stmc" "stmh" "stphplot" "stptime" 
		   "strate" "streg" "streset"
		   "sts" "stse" "stset" "stsum" "stvary" "stweib"
		   "su" "suest" "sum" "summ" "summa" "summar" "summari" "summariz" "summarize"
		   "sureg" "sunflower" "svar"
		   "svydes" "svydescribe" "svyset"
		   "swilk" "symmetry" "symmi" "symplot" "syntax" "sysdir" 
		   "ta" "tab" 
		   "tab1" "tab2" 
		   "tabdisp"
		   "tabi" 
		   "table" "tabodds" "tabstat"
		   "tabu" "tabul" "tabula" "tabulat" "tabulate"
		   "te" "tes" "test"
		   "testnl" "testparm" "tetrachoric"
		   "threshold"
		   "tnbreg"
		   "tob" "tobi" "tobit"
		   "token" "tokeni" "tokeniz" "tokenize" 
		   "total" "touch" 
		   "tpoisson"
		   "translate"
		   "truncreg"
		   "tsline" "tsreport" "tsrline" "tsset" "tsunab" "ttest" "ttesti"
		   "twoway"
		   "ty" "typ" "type"
		   "ucm" "unab" "unabcmd" "update" "using"
		   "var" "varabbrev" "varbasic" "vargranger"  
		   "varlmar" 
		   "varm" "varma" "varman" "varmana" "varmanag" "varmanage" "varmanager" 
		   "varnorm" "varsoc" "varstable" "varwle" 
		   "vec" "veclmar" "vecnorm" "vecrank" "vecstable"
		   "verinst" "view" "viewsource" "vwls"
		   "which" "who" "wntestb" "wntestq" 
		   "xchart" "xcorr"
		   "xsh" "xshe" "xshel" "xshell" 
		   "xtabond" "xtcloglog" 
		   "xtdes" "xtdesc" "xtdescr" "xtdescri" "xtdescrib" "xtdescribe"
		   "xtdpd" "xtdpdsys"
		   "xtfrontier"
		   "xtgee" "xtgls" "xthtaylor" "xtintreg" "xtivreg"
		   "xtline" "xtlogit"  
		   "xtnbreg" "xtologit" "xtoprobit" "xtpcse" "xtpoisson" "xtprobit"
		   "xtrc" "xtreg" "xtregar" "xtset" "xtstreg" "xtsum" "xttab" "xttest0" "xttobit" "xttrans"
		   "zinb" "zioprobit" "zip" "ztest" "ztesti"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face))

	;; things to use with the svy ... : prefix
	;; can be fooled by svy brr, nothing but garbage:
	;; another problem: don't know how to make the hilighted stuff in the middle
	;;  optional
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(svy\\)\\>"
	  "[ \t]*,?.*?:[ \t]*"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "asmixlog"
		   "betareg" "biprobit"
		   "churdle" "clogit" "cloglog" "cnsreg" "cpoisson"
		   "eintreg" "eoprobit" "eregress" "etpoisson" "etregress"
		   "fracreg" 
		   "glm" "gnbreg" "gsem"
		   "heckman" "heckoprobit" "heckpoisson"
		   "heckprob" "heckprobit" "hetprob" "hetregress" 
		   "intreg" "ivprobit" "ivregress" "ivtobit"
		   "logistic" "logit"
		   "mecloglog" "mean" "meglm" "melogit" "menbreg"
		   "meintreg" "meologit" "meoprobit"
		   "mepoisson" "meprobit" "mestreg" "metobit"
		   "mprobit" "mlogit"
		   "nl"
		   "nbreg" "ologit" "oprobit"
		   "poisson" "probit" "proportion" 
		   "ratio" 
		   "reg" "regr" "regre" "regres" "regress"
		   "scobit" "sem" "slogit" "stcox" "stintreg" "streg"
		   "tab" "tabu" "tabul" "tabula" "tabulat" "tabulate"
		   "tnbreg" "tobit" "total" "tpoisson" "truncreg"
		   "zinb" "zioprobit" "zip"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face))
	;; svy stuff with sub commands (so let the command do its highlighting)
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(svy\\)\\>"
	  "[ \t]*,?.*?:[ \t]*"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "churdle"
		   "fracreg"
		   "irt"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face))
	;; more svy stuff
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(svy\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "bootstrap"
		   "brr"
		   "jack" "jackk" "jackkn" "jackkni" "jackknif" "jackknife" 
		   "linear" "lineari" "lineariz" "linearize" "linearized" 
		   "sdr"
		   ) 'words))
	  ".*?,?.*?:[ \t]*"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gnbreg" 
		   "heckman" "heckprob" 
		   "intreg" "ivreg" 
		   "logistic" "logit"
		   "mean" "mlogit" 
		   "nbreg" "ologit" "oprobit"
		   "poisson" "probit" "proportion" 
		   "ratio" 
		   "reg" "regr" "regre" "regres" "regress" 
		   "tab" "tabu" "tabul" "tabula" "tabulat" "tabulate" 
		   "total"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	 '(3 ado-builtin-harmless-face))
	;; incomplete svy fmm
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(svy\\)\\>"
	  "[ \t]*,?.*?:[ \t]*"
	  "\\<\\(fmm\\)\\>"
	  "[ \t]*,?.*?:[ \t]*"
	  )
	 '(1 ado-needs-subcommand-face) '(2 ado-needs-subcommand-face))
	;; svy fmm
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(svy\\)\\>"
	  "[ \t]*,?.*?:[ \t]*"
	  "\\<\\(fmm\\)\\>"
	  "[ \t]*,?.*?:[ \t]*"
	  (eval-when-compile
		(regexp-opt
		 '(
		   "betareg"
		   "cloglog"
		   "glm"
		   "intreg" "ivregress"
		   "logit"
		   "mlogit"
		   "nbreg"
		   "ologit" "oprobit"
		   "pointmass" "poisson" "probit"
		   "regress"
		   "streg"
		   "tobit"
		   "tpoisson"
		   "truncreg"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-builtin-harmless-face t)
	 '(3 ado-builtin-harmless-face))
	;; haver subcommands ... all obsolete in Stata 13
	
	;; (list
	;;  (concat
	;;   "\\<\\(haver\\)\\>"
	;;   "[ \t]+"
	;;   (eval-when-compile 
	;; 	(regexp-opt 
	;; 	 '(
	;; 	   "des" "desc" "descr" "descri" "describ" "describe"
	;; 	   ) 'words))
	;;   ado-end-cmd-regexp )
	;;  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face))

	;; (list
	;;  (concat
	;;   "\\<\\(haver\\)\\>"
	;;   "[ \t]+"
	;;   "\\<\\(use\\)\\>"
	;;   ado-end-cmd-regexp )
	;;  '(1 ado-builtin-harmful-face) '(2 ado-builtin-harmful-face))

	;; Conditional statements 
	;; if might not work right ('cuz it is also a keyword)
	;; this does not allow missing { single statement ifs and elses
	;;   because of mata. Hmm...
	(list
	 (concat
	  ado-start-cmd-must-start-line-regexp
	  (eval-when-compile
		(regexp-opt
		 '(
		   "if" "else" "while"
		   ) 'words ))
	  "[ \t]+.*?{"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t))
	
	;; at least try to have else if's highlight properly
	(list
	 (concat
	  ado-start-cmd-must-start-line-regexp
	  "\\<\\(else\\)\\>"
	  "[ \t]+"
	  "\\<\\(if\\)\\>"
	  "[ \t]+.*?{?"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-builtin-harmless-face t))

	;; variable types which appear as subcommands often
	;; this leads to some spurious highlighting; will need a fix
	(list
	 (concat
	  ado-start-cmd-null-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "byte" "int" "long" "str" "float" "double"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-subcommand-face t))

	;; string variable types 
	(list
	 (concat
	  ado-start-cmd-null-regexp
	  "\\<\\(str"
	  "\\(L"
	  "\\|"
	  "\\(?:[1-9][0-9]?[0-9]?\\)" 
	  "\\|"
	  "\\(?:1[0-9][0-9][0-9]\\)" 
	  "\\|"
	  "\\(?:20[0-3][0-9]\\)" 
	  "\\|"
	  "\\(?:204[0-5]\\)" 
	  "\\)\\)\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-subcommand-face t))

	;; things used with display
	;; since these are often split across lines, and Stata commands are hard
	;; to delimit, this will highlight even if out of context

	;;
	;; plain display
	;;
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "di" "dis" "disp" "displ" "displa" "display"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "_asis"
		   "_c" "_co" "_con" "_cont" "_conti" "_contin" "_continu" "_continue"
		   "_n" "_ne" "_new" "_newl" "_newli" "_newlin" "_newline" 
		   "_quote"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "di" "dis" "disp" "displ" "displa" "display"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "_r" "_re" "_req" "_requ" "_reque" "_reques" "_request"
		   ) 'words))
	  "([ \t]*"
	  ado-stata-name-bound-regexp
	  "[ \t]*)"
	  )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	 '(3 ado-variable-name-face))
	;; display things which take numbers
	;; some left as is, because they are used in dictionaries...
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "di" "dis" "disp" "displ" "displa" "display"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "_char"
		   "_col" "_colu" "_colum" "_column"
		   "_d" "_du" "_dup"
		   "_n" "_ne" "_new" "_newl" "_newli" "_newlin" "_newline"
		   "_s" "_sk" "_ski" "_skip"
		   ) 'words))
	  "([ \t]*"
	  "\\([1-9]+[0-9]*\\)"
	  "[ \t]*)"
	  )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	 '(3 ado-constant-face))
	;;
	;; other display subcommands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "di" "dis" "disp" "displ" "displa" "display"
		   ) 'words))
	  "[ \t]+"
	  "\\<\\(as\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "err" "erro" "error" 
		   "inp" "inpu" "input"
		   "res" "resu" "resul" "result" 
		   "text" "txt"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	 '(3 ado-subcommand-face t))

	;; trust Stata corp to use different prepositions...
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "di" "dis" "disp" "displ" "displa" "display"
		   ) 'words))
	  "[ \t]+"
	  "\\<\\(in\\)\\>"
	  "[ \t]+"
	  "\\<\\(smcl\\)\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	 '(3 ado-subcommand-face t))

	;;
	;; obsolete coloring
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "di" "dis" "disp" "displ" "displa" "display"
		   ) 'words))
	  "[ \t]+"
	  "\\<\\(in\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "b" "bl" "blu" "blue" 
		   "g" "gr" "gre" "gree" "green"
		   "r" "re" "red"
		   "w" "wh" "whi" "whit" "white"
		   "y" "ye" "yel" "yell" "yello" "yellow"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-obsolete-face)
	 '(3 ado-obsolete-face))
	;;
	;; foreach ... in
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(foreach\\)\\>"
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  "[ \t]+"
	  "\\<\\(in\\)\\>"
	  "[ \t]+"
	  )
	 '(1 ado-builtin-harmless-face) '(2 ado-variable-name-face)
	 '(3 ado-subcommand-face t))

	;; foreach ... of
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(foreach\\)\\>"
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  "[ \t]+"
	  "\\<\\(of\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "glo" "glob" "globa" "global"
		   "loc" "loca" "local" 
		   "new" "newl" "newli" "newlis" "newlist" 
		   "num" "numl" "numli" "numlis" "numlist" 
		   "var" "varl" "varli" "varlis" "varlist" 
		   ) 'words))
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-variable-name-face t)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t)
	 '(5 ado-variable-name-face))

	;; forvalues ... = ??
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "forv" "forva" "forval" "forvalu" "forvalue" "forvalues"
		   ) 'words))
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  "[ \t]*=[ \t]*"
	  )
	 '(1 ado-builtin-harmless-face) '(2 ado-variable-name-face t))

	;; gettoken
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(gettoken\\)\\>"
	  "\\(\\(?:[ \t]+(\\(?:loc\\|glob\\)al)\\)?\\)"
	  "[ \t]+" ado-stata-local-name-bound-regexp
	  "\\(\\(?:[ \t]+(\\(?:loc\\|glob\\)al)\\)?\\)"
	  "[ \t]+" ado-stata-local-name-bound-regexp 
	  "[ \t]*:[ \t]*"
	  ado-stata-local-name-bound-regexp 
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	 '(3 ado-variable-name-face t) '(4 ado-subcommand-face t)
	 '(5 ado-variable-name-face t) '(6 ado-variable-name-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(gettoken\\)\\>"
	  "\\(\\(?:[ \t]+(\\(?:loc\\|glob\\)al)\\)?\\)"
	  "\\(\\(?:[ \t]+\\(?:" ado-stata-local-name-regexp "\\)\\)\\{1,2\\}\\)"
	  "[ \t]*:[ \t]*"
	  ado-stata-local-name-bound-regexp
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	 '(3 ado-variable-name-face t) '(4 ado-variable-name-face t))

	;; labels no longer experimental
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "la" "lab" "labe" "label"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "copy"
		   "da" "dat" "data"
		   "de" "def" "defi" "defin" "define"
		   "di" "dir" 
		   "drop" 
		   "lang" "langu" "langua" "languag" "language" 
		   "l" "li" "lis" "list"
		   "save"
		   "val" "valu" "value" "values"
		   "var" "vari" "varia" "variab" "variabl" "variable"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t))

	;; all Stata data-altering stuff
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "_pctile" "_predict"
		   "ap" "app" "appe" "appen" "append" 
		   "bcskew0" "bs" "bsample" "bstrap"
		   "bys" "byso" "bysor" "bysort" 
		   "cd" "clear" "clonevar" "collapse" "compress" 
		   "contract" "corr2data" "cross" "cttost" 
		   "dec" "deco" "decod" "decode" "destring"
		   "discard" "drawnorm" "drop" "dydx"
		   "ed" "edi" "edit" "egen" 
		   "en" "enc" "enco" "encod" "encode"
		   "erase"
		   "expand" "expandcl"
		   "filef" "filefi" "filefil" "filefilt" "filefilte" "filefilter" 
		   "fillin"
		   "form" "forma" "format"
		   "fracpred"
		   "fvrevar"
		   "g" "ge" "gen" "gene" "gener" "genera" "generat" "generate"
		   "getmata" "gsort"
		   "inf" "infi" "infile" "infix" 
		   "inp" "inpu" "input"
		   "insobs"
		   "integ" "ipolate" 
		   "joinby"
		   "keep" 
		   "lnskew0"
		   "makecns"
		   "mark" "markin" "markout" "mat"
		   "matr" "matri" "matrix"
		   "mkdir" "mkmat" "mkspline"
		   "mleval" "mlmatsum" "mlsum""mlvecsum"
		   "modify" 
		   "mvdecode" "mvencode" 
		   "nlogitgen" "nlpred" "nobreak" 
		   "order" "orthog" "orthpoly"
		   "ou" "out" "outf" "outfi" "outfil" "outfile"
		   "pctile" 
		   "pkcollapse" "pkshape"
		   "post" "postclose" "postfile" 
		   "predict" "predictnl" "preserve" "putmata"
		   "range" "recast" "recode" 
		   "ren" "rena" "renam" "rename"
		   "renpfix" "replace" "restore" "rm" "rmdir"
		   "sappend" 
		   "sa" "sav" "save" "saveold"
		   "sample" "sdrop"
		   "separate"
		   "simul" "simulate" "sinfile" "smerge" 
		   "smooth" "snapspan" 
		   "so" "sor" "sort" "sortpreserve"
		   "spbalance" "spcompress" "spgenerate" "split"
		   "ssave" "ssort" "stack" "statsby"
		   "stbase" "stfill" "stgen" "stjoin" "stsplit" "sttocc" "sttoct"
		   "suse" "svmat" "svymarkout" "sysuse"
		   "tostring"
		   "tsappend" "tsfill" "tsrevar"
		   "u" "unzipfile" "us" "use" "uselabel"
		   "webuse"
		   "xi" "xi:" 
		   "xmlsav" "xmlsave" "xmluse" 
		   "xtile" "xpose" 
		   "xtdata" "xtpred"
		   "zipfile"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face))
	;; clear commmands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  "\\<\\(clear\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "ado" "all"
		   "mata" "matrix"
		   "programs"
		   "results" "rngstream"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	;; assignment of macros
	;;  local macros have different naming conventions (boo)
	;;  marksample added, because need harmful/scalar name
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gl" "glo" "glob" "globa" "global" 
		   "marksample"
		   "sca" "scal" "scala" "scalar" 
		   ) 'words))
	  "[ \t]+`*"
	  ado-stata-name-bound-regexp
	  )
	 '(1 ado-builtin-harmful-face) '(2 ado-variable-name-face t))
	;; local macro definition
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "loc" "loca" "local" 
		   ) 'words))
	  "[ \t]+\\(?:\\(?:++\\|--\\|[`]+\\)?\\)"
	  ado-stata-local-name-bound-regexp
	  )
	 '(1 ado-builtin-harmless-face) '(2 ado-variable-name-face t))

	;; warning for local wrong--
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "loc" "loca" "local" 
		   ) 'words))
	  "[ \t]+"
	  "\\(\\(?:`\\|[[:alnum:]_]\\|'\\)+\\)"
	  "\\(\\(?:++\\|--\\)\\)"
	  )
	 '(1 ado-builtin-harmless-face) '(2 ado-variable-name-face t)
	 '(3 ado-obsolete-face t))

	;; scalars
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "sca" "scal" "scala" "scalar"
		   ) 'words))
	  "\\(\\(?:[ \t]+\\(?:def\\|defi\\|defin\\|define\\)\\)?\\)"
	  "[ \t]+"
	  ado-stata-name-bound-regexp
	  )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	 '(3 ado-variable-name-face t))

	;; scalar/macro drop etc.
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "ma" "mac" "macr" "macro"
		   "sca" "scal" "scala" "scalar" 
		   ) 'words))
	  "[ \t]+"
	  "\\<\\(drop\\)\\>"
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	 '(3 ado-variable-name-face t))

	;; (list
	;;  (concat
	;;   (eval-when-compile 
	;; 	(regexp-opt 
	;; 	 '(
	;; 	   "ma" "mac" "macr" "macro"
	;; 	   "sca" "scal" "scala" "scalar" 
	;; 	   ) 'words))
	;;   "[ \t]+"
	;;   "\\<\\(drop\\)\\>"
	;;   )
	;;  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	;; wtf is this? local drop ...?
	;; (list
	;;  (concat
	;;   ado-start-cmd-no-prefix-regexp
	;;   (eval-when-compile 
	;; 	(regexp-opt 
	;; 	 '(
	;; 	   "loc" "loca" "local" 
	;; 	   ) 'words))
	;;   "[ \t]+"
	;;   "\\<\\(drop\\)\\>"
	;;   "[ \t]+"
	;;   "\\(\\(?:"
	;;   ado-stata-local-name-regexp
	;;   "\\b\\)?\\)"
	;;   )
	;;  '(1 ado-builtin-harmful-face) '(2 ado-obsolete-face)
	;;  '(3 ado-variable-name-face t))

	;;
	;; an attempt to include the extended macro names
	;; single word extended macro names
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gl" "glo" "glob" "globa" "global" 
		   "loc" "loca" "local" 
		   ) 'words))
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  "[ \t]*:[ \t]*"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "adosubdir"
		   "char" "cole" "coleq" "coleqnumb" 
		   "colf" "colfu" "colful" "colfull" "colfulln" "colfullna" "colfullnam" "colfullname" "colfullnames"
		   "colnfreeparms" "collfnames"
		   "coln" "colna" "colnam" "colname" "colnames"
		   "colnlfs" "colnumb" "colsof" "colvarlist"
		   "constraint"
		   "dirsep" 
		   "di" "dir" "dis" "disp" "displ" "displa" "display" 
		   "env" "envi" "envir" "enviro" "environ" "environm" "environme" "environmen" "environment" 
		   "f" "fo" "for" "form" "forma" "format" 
		   "lab" "labe" "label"
		   "list"
		   "permname" "piece" "properties" "pwd"
		   "rowe" "roweq" "roweqnumb" 
		   "rowf" "rowfu" "rowful" "rowfull" "rowfulln" "rowfullna" "rowfullnam" "rowfullname" "rowfullnames"
		   "rowlfnames"
		   "rown" "rowna" "rownam" "rowname" "rownames"
		   "rownfreeparms" "rownlfs" "rownumb" "rowsof" "rowvarlist"
		   "sort" "sorte" "sorted" "sortedb" "sortedby" "sysdir"
		   "tempf" "tempfi" "tempfil" "tempfile" "tempv" "tempva" "tempvar" 
		   "tsnorm" 
		   "ty" "typ" "type"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	 '(3 ado-subcommand-face t))

		;; single word extended macro names
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gl" "glo" "glob" "globa" "global" 
		   "loc" "loca" "local" 
		   ) 'words))
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  "[ \t]*:[ \t]*"
	  "\\(sysdir\\)"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "BASE"
		   "OLDPLACE"
		   "PERSONAL"
		   "PLUS"
		   "SITE"
		   "STATA"
		   "UPDATE"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))


	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gl" "glo" "glob" "globa" "global" 
		   "loc" "loca" "local" 
		   ) 'words))
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  "[ \t]*:[ \t]*"
	  "\\<\\(word\\)\\>"
	  "[ \t]+"
	  "\\<\\(count\\)\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gl" "glo" "glob" "globa" "global" 
		   "loc" "loca" "local" 
		   ) 'words))
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  "[ \t]*:[ \t]*"
	  "\\<\\(word\\)\\>"
	  "[ \t]+\\(?:[0-9]+\\|`[^ \t]*'\\)[ \t]+"
	  "\\<\\(of\\)\\>"
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))

	;; things with parens in them (sheesh)
	;; not included above, incase someone uses a font which 
	;;   has a background color
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gl" "glo" "glob" "globa" "global" 
		   "loc" "loca" "local" 
		   ) 'words))
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  "[ \t]*:[ \t]*"
	  "\\<\\(e\\|r\\)\\>"
	  "([ \t]*"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "functions" "macros" "matrices" "scalars"
		   ) 'words))
	  "[ \t]*)"
	  )
	 '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))

	;; damn s(macros)
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gl" "glo" "glob" "globa" "global" 
		   "loc" "loca" "local" 
		   ) 'words))
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  "[ \t]*:[ \t]*"
	  "\\<\\(s\\)\\>"
	  "([ \t]*"
	  "\\<\\(macros\\)\\>"
	  "[ \t]*)"
	  )
	 '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))

	;; twin word macros length and subinstr
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gl" "glo" "glob" "globa" "global" 
		   "loc" "loca" "local" 
		   ) 'words))
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  "[ \t]*:[ \t]*"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "copy"
		   "strlen" "subinstr"
		   "udstrlen" "ustrlen" 
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gl" "glo" "glob" "globa" "global" 
		   "loc" "loca" "local" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))
	
	;; serset macros
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gl" "glo" "glob" "globa" "global" 
		   "loc" "loca" "local" 
		   ) 'words))
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  "[ \t]*:[ \t]*"
	  "\\<\\(serset\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "N"
		   "format" "k" "id" "max" "min" "type" "varnames" "varnum"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))
	;; sheesh, now there are combined abbreviations!
	;;
	(list
	 (concat
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gl" "glo" "glob" "globa" "global" 
		   "loc" "loca" "local" 
		   ) 'words))
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  "[ \t]*:[ \t]*"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "data"
		   "val" "valu" "value" 
		   "var" "vari" "varia" "variab" "variabl" "variable" 
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "l" "la" "lab" "labe" "label"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))
	
	;; macro list commands start here
	;; single word commands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gl" "glo" "glob" "globa" "global" 
		   "loc" "loca" "local" 
		   ) 'words))
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  "[ \t]*:[ \t]*"
	  "\\<\\(list\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "clean"
		   "dups"
		   "retok" "retoke" "retoken" "retokeni" "retokeniz" "retokenize"
		   "sizeof"
		   "sort"
		   "uniq"
		   ) 'words))
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  )
	 '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t)
	 '(5 ado-variable-name-face t))
	;; operator-like list commands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp 
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gl" "glo" "glob" "globa" "global" 
		   "loc" "loca" "local" 
		   ) 'words))
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  "[ \t]*:[ \t]*"
	  "\\<\\(list\\)\\>"
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  "[ \t]*"
	  "\\(?:[|&-]\\|==\\|===\\)"
	  "[ \t]*"
	  ado-stata-local-name-bound-regexp
	  )
	 '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	 '(3 ado-subcommand-face t) '(4 ado-variable-name-face t)
	 '(5 ado-variable-name-face t))
	;; special highlighting for 'in'---which is pretty whack
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp 
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gl" "glo" "glob" "globa" "global" 
		   "loc" "loca" "local" 
		   ) 'words))
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  "[ \t]*:[ \t]*"
	  "\\<\\(list\\)\\>"
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  "[ \t]*"
	  "\\<\\(in\\)\\>"
	  "[ \t]*"
	  ado-stata-local-name-bound-regexp
	  )
	 '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	 '(3 ado-subcommand-face t) '(4 ado-variable-name-face t)
	 '(5 ado-subcommand-face t) '(6 ado-variable-name-face t))
	;; friggin' posof subcommand
	(list
	 (concat
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gl" "glo" "glob" "globa" "global" 
		   "loc" "loca" "local" 
		   ) 'words))
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp 
	  "[ \t]*:[ \t]*"
	  "\\<\\(list\\)\\>"
	  "[ \t]+"
	  "\\<\\(posof\\)\\>"
	  "[ \t]+\".*?\"[ \t]+"
	  "\\<\\(in\\)\\>"
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp 
	  )
	 '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t)
	 '(5 ado-subcommand-face t) '(6 ado-variable-name-face t))

	;; all subcommand
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp 
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gl" "glo" "glob" "globa" "global" 
		   "loc" "loca" "local" 
		   ) 'words))
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  "[ \t]*:[ \t]*"
	  "\\<\\(all\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "globals" "matrices" "scalars"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))
	;; all numeric/string
	(list
	 (concat
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gl" "glo" "glob" "globa" "global" 
		   "loc" "loca" "local" 
		   ) 'words))
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  "[ \t]*:[ \t]*"
	  "\\<\\(all\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "numeric" "string"
		   ) 'words))
	  "[ \t]+"
	   "\\<\\(scalars\\)\\>"
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t)
	  '(5 ado-subcommand-face t))

	;; obsolete macro extended commands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gl" "glo" "glob" "globa" "global" 
		   "loc" "loca" "local" 
		   ) 'words))
	  "[ \t]+"
	  ado-stata-local-name-bound-regexp
	  "[ \t]*:[ \t]*"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "length"
		   "tempf" "tempfi" "tempfil" "tempfile" "tempv" "tempva" "tempvar" 
		   ) 'words))
	  ado-end-cmd-regexp)
	 '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	 '(3 ado-obsolete-face t))
	
	  ;; choosing temp names
	(list
	 (concat
	   ado-start-cmd-no-prefix-regexp
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "tempfile" "tempname" "tempvar"
		 ) 'words))
	   "[ \t]+`*"
	   "\\(\\(?:"
	   ado-stata-local-name-regexp 
	   "[ \t]*\\)+\\)"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-variable-name-face t))

	  ;; other macro commands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp 
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "ma" "mac" "macr" "macro"
		 "sca" "scal" "scala" "scalar" 
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "di" "dir"
		  "l" "li" "lis" "list"			
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp 
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "ma" "mac" "macr" "macro"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "s" "sh" "shi" "shif" "shift"
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	;; stata functions i.e. things which require () after them 
	;; obsolete functions are after this list
	(list
	 (concat
	  ado-start-cmd-null-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "_byn1"  "_byn2" "_caller" 
		   "abbrev" "abs" "acos" "asin" "atan" "atan2" "atanh" "autocode"
		   "betaden" "binomial" "binomialp" "binomialtail" 
		   "binormal" "bofd" "byteorder"
		   "Cdhms" "Chms" "Clock" "Cmdyhms" "Cofc" "Cofd"
		   "c" "cauchy" "cauchyden" "cauchytail"
		   "ceil" "char" "chi2" "chi2tail" "chi2den" 
		   "cholesky" "chop" "cofC" "cofd"
		   "clip" "cloglog" "clock" 
		   "coleqnumb"
		   "collatorlocale" "collatorversion"
		   "colnumb" "colnfreeparms"
		   "colsof"
		   "comb" 
		   "cond" "corr" "cos" 
		   "daily" "date" "day"
		   "det"
		   "dgammapda" "dgammapdada" "dgammapdadx" "dgammapdx" "dgammapdxdx"
		   "dhms"
		   "diag" "diag0cnt" "digamma" 
		   "dofb" "dofC" "dofc" "dofd" "dofh" "dofm" "dofq" "dofw" "dofy" "dow" "doy"
		   "dunnettprob"
		   "e" "el" "epsdouble" "epsfloat" "exp"
		   "exponential" "exponentialden" "exponentialtail" 
		   "F" "Fden" "Ftail" 
		   "fileexists" "fileread" "filereaderror" "filewrite"
		   "float" "floor" "fmtwidth" 
		   "gammaden" "gammap" "gammaptail" "get"
		   "hadamard" "halfyear" "halfyearly" "has_eprop" "hh" "hhC" "hofd" "hms" "hours" "hypergeometric" "hypergeometricp"
		   "I" "ibeta" "ibetatail" "igaussian" "igaussianden" "igaussiantail"
		   "indexnot" "inlist" "inrange" "int"
		   "invF" "invFtail" 
		   "inv" "invbinomial" "invbinomialtail"
		   "invcauchy" "invcauchytail"
		   "invchi2" "invchi2tail" "invcloglog"
		   "invdunnettprob"
		   "invgammap" "invgammaptail"
		   "invibeta" "invibetatail" "invlogit"
		   "invigaussian" "invigaussiantail"
		   "invlaplace" "invlaplacetail"
		   "invnF" "invnFtail" 
		   "invnbinomial" "invnbinomialtail"
		   "invnchi2" "invnchi2tail"
		   "invexponential" "invexponentialtail" 
		   "invnibeta"
		   "invlogistic" "invlogistictail"
		   "invnormal" "invnt" "invnttail" 
		   "invpoisson" "invpoissontail"
		   "invsym"  "invt" "invttail" "invtukeyprob"
		   "invweibull" "invweibullph" "invweibullphtail" "invweibulltail"
		   "irecode" "issymetric" 
		   "J"
		   "laplace" "laplaceden" "laplacetail"
		   "ln" "lncauchyden" "lnfactorial" "lngamma"
		   "lnigammaden" "lnigaussianden" "lniwishartden" "lnlaplaceden"
		   "lnmvnormalden" "lnwishartden"
		   "log" "log10"
		   "logistic" "logisticden" "logistictail"
		   "logit"  
		   "matmissing" "matrix" "matuniform" 
		   "max" "maxbyte" "maxdouble" "maxfloat" "maxint" "maxlong" 
		   "mdy" "mdyhms" "mi"
		   "min" "minbyte" "mindouble" "minfloat" "minint" "minlong" "minutes"
		   "missing" "mm" "mmC" "mod" "mofd" "month" "monthly" "mreldif"
		   "msofhours" "msofminutes" "msofseconds"
		   "nF" "nFden" "nFtail" 
		   "nbetaden" 
		   "nbinomial" "nbinomialp" "nbinomialtail"
		   "nchi2" "nchi2den" "nchi2tail"
		   "normal" "normalden" 
		   "nibeta" 
		   "npnF" "npnchi2" "npnt"
		   "nt" "ntden" "nttail"
		   "nullmat"
		   "poisson" "poissonp" "poissontail"
		   "plural" 
		   "qofd" "quarter" "quarterly"
		   "r" "rbeta" "rbinomial" "rcauchy" "rchi2" "real" "recode"
		   "regexm" "regexr" "regexs"
		   "reldif" "replay" "return"  
		   "rgamma" "rexponential" "rhypergeometric" "rigaussian"
		   "rlaplace" "rlogistic"
		   "rnbinomial" "rnormal"
		   "round" "roweqnumb" "rownfreeparms" "rownumb" "rowsof" "rpoisson" "rt"
		   "runiform" "runiformint" "rweibull" "rweibullph"
		   "s" "scalar" "seconds" "sign" "sin" "soundex" "soundex_nara"
		   "sqrt" "ss" "ssC"
		   "string"
		   "stritrim" "strlen" "strlower" "strltrim"
		   "strmatch" "strofreal" "strpos" "strproper"
		   "strreverse" "strrtrim" "strtoname" "strrtrim" "strupper"
		   "subinstr" "subinword" "substr" "sum" 
		   "sw" "sweep"
		   "t"
		   "tC"
		   "tan" "tanh" "tc" "td" "tden" "th" "tin" "tm"
		   "tobytes"
		   "tq" "trace" "trigamma" "trunc" "ttail" "tukeyprob" "tw" "twithin"
		   "uchar" "udstrlen" "uisdigit" "uisletter"
		   "ustrcompare" "ustrcompareex"
		   "ustrfix" "ustrfrom"
		   "ustrinvalidcnt"
		   "ustrleft" "ustrlen" "ustrlower" "ustrltrim"
		   "ustrnormalize"
		   "ustrpos"
		   "ustrregexm" "ustrregexra" "ustrregexrf" "ustrregexs"
		   "ustrreverse" "ustrright" "ustrrpos" "ustrrtrim"
		   "ustrsortkey" "ustrsortkeyex"
		   "ustrtitle" "ustrto" "ustrtohex" "ustrtoname" "ustrtrim"
		   "ustrunescape" "ustrupper"
		   "ustrword" "ustrwordcount"
		   "usubinstr" "usubstr"
		   "upper"
		   "vec" "vecdiag"
		   "week" "weekly"
		   "weibull" "weibullden" "weibullph" "weibullphden" "weibullphtail" "weibulltail"
		   "wofd" "word" "wordbreaklocale" "wordcount"
		   "year" "yearly" "yh" "ym" "yofd" "yq" "yw"

		 ) 'words))
	   "("
	   )
	  '(1 ado-function-name-face t))

	  ;;
	;; obsolete functions requiring () after them
	;; -length()- is obsolete in Stata but not in Mata(!)
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"Binomial"
			"binorm" 
			"chiprob"
			"d"
			"fprob"
			"group"
			"h" 
			"index"
			"invchi" "invfprob" "invnchi" "invnorm"
			"itrim"
			"issym"
			"lnfact" "lower" "ltrim"
			"m" "match"
			"nchi" "norm" "normd" "normden" "normprob" "npnchi"
			"proper"
			"q"
			"reverse" "rtrim"
			"syminv"
			"tprob" "trim"
			"uniform" "uniform0"
			"y"
			"w"
			) 'words))
	   "("
	   )
	  '(1 ado-obsolete-face t))

	  ;; things which require [] after them
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "_b" "_coef" "_se"
		 ) 'words))
	   "\\["
	   )
	  '(1 ado-builtin-harmless-face))

	  ;; common Stata options which require a () after them
	;; kind of out-of-place, but left as a historical remnant
	(list
	  (concat
	   ",.*[\ t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "bands" "by" "connect" "density" "gap" "iterate" "ltolerance" "margin"
		  "psize" "saving" "tlabel" "tolerance"
		  "xlabel" "xscale" "ylabel" "yscale"
		 ) 'words))
	   "("
	   )
	  '(1  ado-subcommand-face t))

	  ;; egen 'function' options
	(list
	 (concat
	  ado-start-cmd-regexp
	   "egen[ \t]+.*=[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			  "anycount" "anymatch" "anyvalue"
			  "concat" "count" "cut" 
			  "diff"
			  "ends" 
			  "fill" "group" "iqr"
			  "kurt"
			  "mad" "max" "mdev" "mean" "median" "min" "mode" "mtr" 
			  "pc" "pctile" 
			  "rank" 
			  "rowfirst" "rowlast" "rowmax" "rowmean" "rowmedian" "rowmin" "rowmiss" "rownonmiss" "rowpctile" "rowsd" "rowtotal" 
			  "sd" "seq" "skew" "std" 
			  "tag" "total"
		 ) 'words))
	   "("
	   )
	  '(1 ado-function-name-face t))

	  ;; egen 'function' options -- obsolete
	(list
	  (concat
	   ado-start-cmd-regexp
	   "[ \t]*egen[ \t]+.*=[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			  "any"
			  "eqany"
			  "ma"
			  "neqany"
			  "rfirst" "rlast" "rmax" "rmean" "rmin" "rmiss" "robs" "rsd" "rsum" 
			  "sum"
		 ) 'words))
	   "("
	   )
	  '(1 ado-obsolete-face t))

	  ;; all the endless -mi- commands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			"copy"
			"d" "de" "des" "desc" "descr" "descri" "describ" "describe"
			"est" "esti" "estim" "estima" "estimat" "estimate"
			"fvset"
			"q" "qu" "que" "quer" "query"
			"st" "streset" "stset"  "svyset"
			"test" "testtr" "testtra" "testtran" "testtrans" "testtransf" "testtransfo" "testtransfor" "testtransform"
			"tsset"
			"unreg" "unregi" "unregis" "unregist" "unregiste" "unregister"
			"unset" "update"
			"vary" "varyi" "varyin" "varying"
			"xtset"
		 ) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	  ;; mi harmful commands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			"add" "append"
			"erase" "expand" "extract"
			"predict" "predictnl"
			"ren" "rena" "renam" "rename"
			"replace0"
			"reset"
			"select" "stsplit"
		 ) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	  ;; mi convert commands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   "\\<\\(convert\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			"fl" "flo" "flon" "flong"
			"flongs" "flongse" "flongsep"
			"ml" "mlo" "mlon" "mlong"
			"w" "wi" "wid" "wide"
		 ) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))

	  ;; mi export commands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   "\\<\\(export\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "ice"
		 "nhanes1"
		 ) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))
	  ;; mi import commands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   "\\<\\(import\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "flong" "flongsep"
		 "ice"
		 "nhanes1"
		 "wide"
		 ) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))
	  ;; mi impute commands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  "\\<\\(mi\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "imp" "impu" "imput" "impute"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "chain" "chaine" "chained" 
		   "intreg"
		   "logi" "logit"
			"mlog" "mlogi" "mlogit"
			"mon" "mono" "monot" "monoto" "monoton" "monotone"
			"mvn"
			"nbreg"
			"olog" "ologi" "ologit"
			"pmm"
			"poisson"
			"reg" "regr" "regre" "regres" "regress"
			"truncreg"
		 ) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))
	  ;; mi merge commands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   "\\<\\(merge\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "1:1" "1:m" "m:1" "m:m"
		 ) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))
	  ;; mi misstable commands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "misstab" "misstabl" "misstable"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			"nest" "neste" "nested"
			"pat" "patt" "patte" "patter" "pattern" "patterns"
			"sum" "summ" "summa" "summar" "summari" "summariz" "summarize"
			"tree"
		 ) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))
	  ;; mi passive subcommands
	;; will break with options
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "pas" "pass" "passi" "passiv" "passive"
		 ) 'words))
	   "[ \t]*:"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))
	  ;; mi ptrace subcommands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   "\\<\\(ptrace\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			"d" "de" "des" "desc" "descr" "descri" "describ" "describe"
			"use"
		 ) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))
	  ;; mi reshape subcommands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   "\\<\\(reshape\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "long" "wide"
		 ) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))
	  ;; mi select subcommands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   "\\<\\(select\\)\\>"
	   "[ \t]+"
	   "\\<\\(init\\)\\>"
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))
	  ;; mi set subcommands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   "\\<\\(set\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			"M"
			"fl" "flo" "flon" "flong"
			"flongs" "flongse" "flongsep"
			"m" "ml" "mlo" "mlon" "mlong" 
			"w" "wi" "wid" "wide" 
		 ) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))
	  ;; mi register subcommands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "reg" "regi" "regis" "regist" "registe" "register"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			"imp" "impu" "imput" "impute" "imputed" 
			"pas" "pass" "passi" "passiv" "passive"
			"reg" "regu" "regul" "regula" "regular"
		 ) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))
	  ;; mi xeq subcommands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   "\\<\\(xeq\\)\\>"
	   "[ \t]*:"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; mi multiword incomplete subcommands
	;;  includes mi ...: prefixes (which is probably bad)
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"convert" "export" "import"
			"imp" "impu" "imput" "impute"
			"merge"
			"misstab" "misstabl" "misstable"
			"passive" "ptrace"
			"reg" "regi" "regis" "regist" "registe" "register"
			"reshape" "select" "set"
			"xeq"
			) 'words))
		 ado-end-cmd-regexp )
	  '(1 ado-needs-subcommand-face) '(2 ado-needs-subcommand-face))

	;; now, presenting smcl
	;;
	;; Syntax 1
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "asis"
		  "bf" "break"
		  "cmd" "com"
		  "depvar" "depvars" "depvarlist" "dtype"
		  "err" "error"
		  "hi" "hilite" "hline"
		  "ifin" "inp" "indepvars" "input" "it"
		  "newvar"
		  "p" "p_end" "p2colreset" "p2line" 
		  "phang" "phang2" "phang3" "pin" "pin2" "pin3" "pmore" "pmore2" "pmore3" "psee" "pstd"
		  "res" "reset" "result"
		  "s6hlp"
		  "sf" "smcl" "synopthdr" "synoptline" "synoptset"
		  "tab" "text" "txt"
		  "var" "varlist" "varname" "vars"
		  "weight"
		 ) 'words))
	   "[ \t]*?"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face t) '(2 ado-builtin-harmless-face t) '(3 ado-constant-face t))
	;; Syntax 1 with funny chars (need to build regexp by hand)
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   "\\([.]\\(?:[.][.]\\|-\\)\\)"
	   "[ \t]*?"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t) '(3 ado-constant-face))

	  ;; Syntax 2
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "ado_d"
		  "back" "bf" "bind" 
		  "center" "centre" "clearmore" "cmd"
		  "depvar" "depvars" "depvarlist" "dlgtab"
		  "err" "error"
		  "help_d" "hi" "hilite"
		  "indepvars" "inp" "input" "it"
		  "net_d" "netfrom_d" "news" "newvar"
		  "p2col" "p2coldent"
		  "rcenter" "rcentre" "res" "reset" "result" "right"
		  "search_d"
		  "sf" "synopt" "synopthdr" "syntab"
		  "text" "title" "txt"
		  "ul" "update_d"
		  "var" "varlist" "varname" "vars" "view_d"
		 ) 'words))
	   "[ \t]*"
	   "\\(:\\)"
	   "\\([^}]*?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-constant-face) '(4 ado-subcommand-face t) '(5 ado-constant-face))
	  ;; making smcl comments look like comments
	(list
	 (concat
	  ado-start-cmd-null-regexp
	  "\\({\\)"
	  "[ \t]*"
	  "\\([*]\\)"
	  "[ \t]*"
	  "\\(:\\)"
	  "\\([^}]+\\)"
	  "\\(}\\)"
	  )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-constant-face) '(4 ado-comment-face t) '(5 ado-constant-face))

	  ;; Syntax 2plus for cmdab
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   "\\<\\(cmdab\\)\\>"
	   "\\(:\\)"
	   "\\([a-zA-Z][a-zA_Z_0-9]*\\)"
	   "\\(:\\)"
	   "\\([^}]+\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-constant-face) '(4 ado-subcommand-face t) '(5 ado-constant-face)
	  '(6 ado-subcommand-face t) '(7 ado-constant-face))

	  ;; Syntax 3 with free form args
	(list
	 (concat
	  ado-start-cmd-null-regexp 
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "ado"
		  "browse"
		  "char"
		  "dialog"
		  "help" "helpb"
		  "marker" "matacmd"
		  "net"
		  "opt"
		  "search" "stata"
		  "update"
		  "view"
		 ) 'words))
	   "[ \t]+"
	   "\\([^:]*?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face))
	  ;; Syntax 3 with on/off choices
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   "\\<\\(ul\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "off" "on"
		 ) 'words))
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face))

	;; Syntax 3 manlink, manlinki, mansection
	(list
	 (concat
	  ado-start-cmd-null-regexp 
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
		'(
		  "manlink" "manlinki" "mansection"
		  )
		'words))
	   "[ \t]+"
	   (eval-when-compile
		 (regexp-opt ado-man-abbrevs 'words))
	   "\\([^:]*?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-subcommand-face t) 
	  '(4 ado-subcommand-face t) '(5 ado-constant-face))
	   
	;; Syntax 4 mansection
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
		'(
		  "mansection"
		  )
		'words))
	   "[ \t]+"
	   (eval-when-compile
		 (regexp-opt ado-man-abbrevs 'words))
	   "[ \t]*"
	   "\\(:\\)"
	   "\\(.+?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face) 
	  '(5 ado-subcommand-face t) '(6 ado-constant-face))
	   
	;; Syntax 3 manpage
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
		'(
		  "manpage"
		  ) 'words))
	   "[ \t]+"
	   (eval-when-compile
		 (regexp-opt ado-man-abbrevs 'words))
	   "[ \t]+"
	   "\\([1-9][0-9]*\\)"
	   "[ \t]*"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-subcommand-face t) 
	  '(4 ado-subcommand-face t) '(5 ado-constant-face))
	   
	;; Syntax 4 manpage
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
		'(
		  "manpage"
		  ) 'words))
	   "[ \t]+"
	   (eval-when-compile
		 (regexp-opt ado-man-abbrevs 'words))
	   "[ \t]+"
	   "\\(:\\)"
	   "[ \t]*"
	   "\\([1-9][0-9]*\\)"
	   "[ \t]*"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face)
	  '(5 ado-subcommand-face t) '(6 ado-constant-face))
	   
	  ;; Syntax 3 comments
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   "\\([*]\\)"
	   "[ \t]+"
	   "\\([^:]\\)"
	   "\\(.*?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-comment-face t) '(4 ado-comment-face t)
	  '(5 ado-constant-face))
	  ;; Syntax 3 with a single text args --- allow simple macros, too
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "ccl"
		 ) 'words))
	   "[ \t]+"
	   "\\(\\(?:[[:alnum:]_]*\\|`[[:alnum:]_`']*'\\)\\)"
	   "[ \t]*"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face))
	  ;; Syntax 3 with single numerical args --- allow simple macros, too
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "col" "hline" "space" "synoptset"
		 ) 'words))
	   "[ \t]+"
	   "\\(\\(?:[0-9]+\\|`[[:alnum:]_`']*'\\)\\)"
	   "[ \t]*"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face))
	  ;; Syntax 3 with a single numerical arg and a choice (nice syntax)
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   "\\<\\(synoptset\\)\\>"
	   "\\(\\(?:[ \t]+\\(?:[0-9]+\\|`[[:alnum:]_`']*'\\)\\)?\\)"
	   "\\(\\(?:[ \t]+\\(?:notes\\|tabbed\\)\\)?\\)"
	   "[ \t]*"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t)
	  '(5 ado-constant-face))
	  ;; Syntax 3 with 0 through 4 numerical args --- allow simple macros, too
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   "\\<\\(p\\)\\>"
	   "\\(\\(?:[ \t]+\\(?:[0-9]+\\|`[[:alnum:]_`']*'\\)\\)\\{0,4\\}\\)"
	   "[ \t]*"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face))
	  ;; Syntax 3 with exactly 4 numerical args --- allow simple macros, too
	(list
	 (concat
	  ado-start-cmd-null-regexp 
	   "\\({\\)"
	   "[ \t]*"
	   "\\<\\(p2colset\\)\\>"
	   "\\(\\(?:[ \t]+\\(?:[0-9]+\\|`[[:alnum:]_`']*'\\)\\)\\{4\\}\\)"
	   "[ \t]*"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face))
	  ;; Syntax 3 with exactly 2 numerical arguments. 
	;; Why doesn't SMCL come close to having a clean syntax?
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   "\\<\\(p2line\\)\\>"
	   "\\(\\(?:[ \t]+\\(?:[0-9]+\\|`[[:alnum:]_`']*'\\)\\)\\{2\\}\\)"
	   "[ \t]*"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face))
       
	  ;; Syntax 4
	  ;; those whose subcommands are not easy
	  ;;  on second thought: for all!
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "ado"
		  "browse"
		  "dialog"
		  "help" "helpb"
		  "manpage" "matacmd"
		  "net"
		  "opt"
		  "search" "stata"
		  "update"
		  "view"
		 ) 'words))
	   "[ \t]+"
	   "\\([^}]*?\\)"
	   "\\(:\\)"
	   "\\([^}]+?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face prepend)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face t)  
	  '(5 ado-subcommand-face t) '(6 ado-constant-face))

	  ;; syntax 4 with a numeric first argument (for dup)
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			"center" "centre" 
			"dlgtab"
			"dup"
			"lalign"
			"ralign"
			"rcenter" "rcentre"
		 ) 'words))
	   "[ \t]+"
	   "\\([1-9][0-9]*\\)"
	   "[ \t]*"
	   "\\(:\\)"
	   "\\([^}]+?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face prepend)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face t)  
	  '(5 ado-subcommand-face t) '(6 ado-constant-face))
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			"center" "centre" 
			"dlgtab"
			"dup"
			"lalign"
			"ralign"
			"rcenter" "rcentre"
		 ) 'words))
	   "[ \t]+"
	   "\\(`[[:alnum:]_`']*'\\)"
	   "[ \t]*"
	   "\\(:\\)"
	   "\\([^}]+?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face prepend)
	  '(3 ado-variable-name-face) '(4 ado-constant-face t)  
	  '(5 ado-subcommand-face t) '(6 ado-constant-face))
	  ;; Syntax 4 with exactly 2 numerical args to start with
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   "\\<\\(dlgtab\\)\\>"
	   "\\(\\(?:[ \t]+\\(?:[0-9]+\\|`[[:alnum:]_`']*'\\)\\)\\{2\\}\\)"
	   "[ \t]*"
	   "\\(:\\)"
	   "\\([^}]+?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face prepend)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face t)  
	  '(5 ado-subcommand-face t) '(6 ado-constant-face))
	  ;; Syntax 4 with exactly 4 numerical args to start with
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   "\\<\\(p2col\\)\\>"
	   "\\(\\(?:[ \t]+\\(?:[0-9]+\\|`[[:alnum:]_`']*'\\)\\)\\{4\\}\\)"
	   "[ \t]*"
	   "\\(:\\)"
	   "\\([^}]+?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face prepend)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face t)  
	  '(5 ado-subcommand-face t) '(6 ado-constant-face))

	  ;; Syntax 3
	  ;; for the manhelp and manhelpi exceptions
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "manhelp" "manhelpi"
		 ) 'words))
	   "[ \t]+"
	   "\\(\\w*?\\)"
	   "[ \t]+"
	   "\\(\\w*?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face prepend)
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t) 
	  '(5 ado-constant-face))


	  ;; Syntax 3
	  ;; for the undocumented viewer... quick access bar directives 
	(list
	 (concat
	  ado-start-cmd-null-regexp 
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt
       '(
		  "vieweralsosee" "viewerdialog" "viewerjumpto" ;; really have 2 args
		 ) 'words))
	   "[ \t]+\"?"
	   "\\(.*?\\)"
	   "\"*[ \t]+\"?"
	   "\\(.*?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face prepend)
	  '(3 ado-subcommand-face) '(4 ado-subcommand-face) 
	  '(5 ado-constant-face))
	   


	  ;; Syntax 4 exceptions
	(list
	 (concat
	  ado-start-cmd-null-regexp 
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "manhelp" "manhelpi"
		 ) 'words))
	   "[ \t]+"
	   "\\(\\w*?\\)"
	   "[ \t]+"
	   "\\(\\w*?\\)"
	   "\\(:\\)"
	   "\\([^}]+?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face prepend)
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t) 
	  '(5 ado-constant-face t) '(6 ado-subcommand-face t) 
	  '(7 ado-constant-face t))

	  ;; special help syntax with double-hash
	  ;;  sheesh --- should be able to reuse with optional stuff
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "help" "helpb"
		 ) 'words))
	   "[ \t]+"
	   "\\([^:# \t]*\\)"
	   "\\(##\\)"
	   "\\(\\w*?\\)"
	   "\\(\\(?:|\\w*?\\)?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face t) 
	  '(5 ado-subcommand-face t) '(6 ado-subcommand-face t) 
	  '(7 ado-constant-face t))
	(list
	 (concat
	  ado-start-cmd-null-regexp 
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "help" "helpb"
		 ) 'words))
	   "[ \t]+"
	   "\\([^:# \t]*\\)"
	   "\\(##\\)"
	   "\\(\\w*?\\)"
	   "\\(\\(?:|\\w*?\\)?\\)"
	   "[ \t]*"
	   "\\(:\\)"
	   "\\([^}]*?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face t) 
	  '(5 ado-subcommand-face t) '(6 ado-subcommand-face t) 
	  '(7 ado-constant-face t) '(8 ado-subcommand-face t) 
	  '(9 ado-constant-face t))

	;; special smcl characters... ugh
	(list
	 (concat
	  ado-start-cmd-null-regexp
	  "\\({\\)"
	  "[ \t]*"
	  "\\(c\\)"
	  "[ \t]*"
	  "\\(\\(?:[AEIOUaeiou]['^:]\\|[AEIOUaeiou]'g\\|[ANOano]~\\|[Yy][':=]\\|[Cc],\\|[Oo]/\\|r[?!]\\|E=\\|S?|\\|L?-\\|+\\)\\)"
	  "[ \t]*"
	  "\\(}\\)"
	  )
	 '(1 ado-constant-face) '(2 ado-builtin-harmless-face)
	 '(3 ado-subcommand-face) '(4 ado-constant-face)
	 )
	;; special chars which have wordlike names
	(list
	 (concat
	  ado-start-cmd-null-regexp
	  "\\({\\)"
	  "[ \t]*"
	  "\\(c\\)"
	  "[ \t]*"
	  "[ \t]*"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "AE"
		   "BRC" "BLC" "BT"
		   "LT" "RT"
		   "TLC" "TRC" "TT"
		   "ae" "ss"
		 ) 'words))	  
	  "\\(}\\)"
	  )
	 '(1 ado-constant-face) '(2 ado-builtin-harmless-face)
	 '(3 ado-subcommand-face) '(4 ado-constant-face)
    )
	  ;; class stuff
;;; builtin prefix operators
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\([.]\\)"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "Global" "Local" "Super"
		 ) 'words))
	   "\\(?:[.]" ado-stata-name-regexp "\\)+"
	   )
	  '(1 ado-function-name-face) '(2 ado-function-name-face t))

;;; builtin class functions and modifiers
	(list
	 (concat
	  ado-start-cmd-null-regexp
;;	  "\\(?:[.a-zA-Z][[:alnum:]_]*\\)+" ! will cause a hang
	  "\\([.]\\)"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "Arrdropall" "Arrdropel" "Arrpop" "Arrpush"
		   "Declare"
		   "arrindexof" "arrnels"
		   "classmv" "classname" "copy"
		   "dynamicmv"
		   "instancemv" "isa" "isofclass"
		   "new"
		   "objkey" "objtype"
		   "ref" "ref_n"
		   "superclass"
		  "uname" 
		  ) 'words))
	  )
	 '(1 ado-function-name-face) '(2 ado-function-name-face t))

;;; class command
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  "\\<\\(class\\)\\>"
	  "[ \t]+"
	  "\\<\\(exit\\)\\>"
	  )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	(list
	  (concat
	  "\\<\\(class\\)\\>"
	  "[ \t]+"
	  "\\([_a-zA-Z][_:alnum:]*\\)"
	  )
	  '(1 ado-builtin-harmful-face) '(2 ado-builtin-harmful-face t))

     ;; all the different declarations
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "class" "classw" "classwi" "classwid" "classwide" 
		  "instance" "instances" "instancesp" "instancespe" "instancespec" 
		  "instancespeci" "instancespecif" "instancespecifi" "instancespecific" 
		 ) 'words))
	   "[ \t]*:"
	   )
	  '(1 ado-builtin-harmless-face))

	  ;; classutil stuff
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   "\\<\\(classutil\\)\\>"
	   "[ \t]+"
	   "\\<\\(drop\\)\\>"
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   "\\<\\(classutil\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "cdir"
		  "d" "de" "des" "desc" "descr" "descri" "describ" "describe"
		  "dir"
		  "which"
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

;; AssociativeArray methods
	(list
	 (concat
	  ado-start-cmd-null-regexp
;;	  "\\(?:[.a-zA-Z][[:alnum:]_]*\\)+" ! will cause a hang
	  "\\([.]\\)"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "N"
		   "clear"
		   "exists"
		   "firstloc" "firstval"
		   "get"
		   "key" "keys"
		   "next" "nextval" "notfound"
		   "put"
		   "reinit" "remove"
		   "val"
		   ) 'words))
	  "("
	  )
	 '(1 ado-function-name-face) '(2 ado-function-name-face t))

	  ;; oh my - the creturn info!
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "cret" "cretu" "cretur" "creturn"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"l" "li" "lis" "list" 
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

;;; the system 'constants' (which are not really constant) - the c() thingies
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\(c(\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "ALPHA" "Mons" "Months" "MP" "N" "SE" "Wdays" "Weekdays"
		  "adopath" "adosize" "alpha" 
		  "bit" "born_date" "byteorder"
		  "cformat"
		  "changed" "charlen" "checksum"
		  "clevel" "cmdlen" 
		  "coeftabresults" "console" "copycolor" 
		  "current_time" "current_date"
		  "dirsep" "dp" "dyndoc_version"
		  "emptycells" "eolchar" "epsdouble" "epsfloat" "eqlen"
		  "filedate" "filename" "flavor" "fredkey"
		  "fvlabel" "fvtrack" "fvwrap" "fvwrapon"
		  "graphics"
		  "haverdir" "hostname"
		  "httpproxy" "httpproxyauth" "httpproxyhost" "httpproxyport" "httpproxypw" "httpproxyuser"
		  "k"
		  "level" "linegap" "linesize"
		  "locale_functions" "locale_icudflt" "locale_ui"
		  "logtype" "lstretch"
		  "machine_type" "macrolen" 
		  "matacache" "matafavor" "matalibs" "matalnum" "matamofirst" "mataoptimize" "matastrict"
		  "matsize" 
		  "max_N_theory" "max_cmdlen" 
		  "max_k_theory" 
		  "max_macrolen" "max_matsize" "max_memory" "max_width_theory" 
		  "maxbyte" "maxdb" "maxdouble" "maxfloat" "maxint" "maxiter" 
		  "maxlong" "maxstrvarlen" "maxstrlvarlen" "maxvar" "maxvlabellen" 
		  "memory"
		  "min_matsize" "min_memory"
		  "minbyte" "mindouble" "minfloat" "minint" "minlong"
		  "mode" "more"
		  "namelenbyte" "namelenchar"
		  "niceness" "noisily"
		  "odbcdriver" "odbcmgr" "os" "osdtl"
		  "pagesize" "pformat" "pi" "printcolor" 
		  "processors" "processors_lic" "processors_mach" "processors_max" "pwd"
		  "rc" "reventries" "rmsg" "rmsg_time"
		  "rng" "rng_current" "rngseed_mt64s" "rngstate" "rngstream"
		  "scheme" "scrollbufsize" "searchdefault" "segmentsize" "sformat"
		  "showbaselevels" "showemptycells" "showomitted"
		  "smallestdouble" "stata_version"
		  "sysdir_base" "sysdir_oldplace" "sysdir_personal" "sysdir_plus" "sysdir_site" "sysdir_stata"
		  "sysdir_updates" 
		  "timeout1" "timeout2" "tmpdir"
		  "trace" "tracedepth" "traceexpand" "tracehilite" "traceindent" "tracenumber" "tracesep" "type"
		  "username" "userversion"
		  "varabbrev" "version" 
		  "width" 
			) 'words))
	   "[ \t]*"
	   "\\()\\)"
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-constant-face t)
	  '(3 ado-builtin-harmless-face t))

;;; platform specific c() thingies
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\(c(\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"autotabgraphs"
			"dockable" "dockingguides" "doublebuffer"
			"eolchar"
			"fastscroll"
			"include_bitmap"
			"locksplitters"
			"notifyuser"
			"playsnd"
			"pinnable"
			"revkeyboard"
			"smoothfonts"
			"update_interval" "update_prompt" "update_query"
			"varkeyboard"
			) 'words))
	   "[ \t]*"
	   "\\()\\)"
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-platform-specific-face t)
	  '(3 ado-builtin-harmless-face))
;;; obsolete c() thingies
	(list
	 (concat
	  ado-start-cmd-null-regexp
	  "\\(c(\\)"
	  "[ \t]*"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "charset"
		   "eolchar"
		   "floatresults" "floatwindows"
		   "icmap"
		   "macgphengine"
		   "max_N_current" "max_k_current" "max_width_current"
		   "namelen"
		   "smalldlg"
		   "persistfv" "persistvtopic" "piccomments"
		   "revwindow"
		   "seed"
		   "smalldlg" "smoothsize"
		   "use_atsui_graph" "use_qd_text"
		   "varlabelpos" "varwindow" "version_rng" "virtual"
		   "xptheme"
		   ) 'words))
	  "[ \t]*"
	  "\\()\\)"
	  )
	 '(1 ado-builtin-harmless-face t) '(2 ado-obsolete-face t)
	 '(3 ado-builtin-harmless-face))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stuff for writing dlg files  ;;;
;;; should be its own minor mode ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(list
	 (concat
	  ado-start-cmd-must-start-line-regexp
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "VERSION"
		 ) 'words))
	   "[ \t]+"
	  "\\(\\(?:\\(?:[89]\\|1[012345]\\)\\(?:[.]0\\)?\\)\\|\\(?:\\(?:[89]\\|1[012345]\\)[.]1\\)\\|\\(?:[89]\\|1[14]\\)[.]2\\)\\($\\|[ \t]+\\)"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	  ;; general builtins for dialogs
	  ;; here - the harmless faces define static text 
	  ;;        whereas the harmful face defines dynamic text
	(list
	 (concat
	  ado-start-cmd-must-start-line-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		  "BUTTON"
		  "CANCEL" "CHECKBOX" "COMBOBOX" "COPY"
		  "DEFINE" "DIALOG"
		  "EDIT" 
		  "FILE"
		  "HELP"
		  "INCLUDE"
		  "LISTBOX"
		  "OK"
		  "RADIO" "RESET"
		  "SPINNER" "SUBMIT"
		  "TREEVIEW"
		  "VARLIST" "VARNAME"
		 ) 'words))
	  ado-end-cmd-regexp )
	  '(1 ado-builtin-harmful-face))

	(list
	 (concat
	  ado-start-cmd-must-start-line-regexp
	   "\\<\\(stopbox\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"note"
			"rusure"
			"stop"
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-must-start-line-regexp
	  (eval-when-compile
		 (regexp-opt 
       '(
		  "BEGIN"
		  "COLOR"
		  "END" "EXP"
		  "FRAME"
		  "GROUPBOX"
		  "HLINK"
		  "LIST"
		  "MODAL"
		  "POSITION" "PROGRAM"
		  "SCRIPT" "SYNCHRONOUS_ONLY"
		  "TEXT" "TEXTBOX"
		  "allowxi"
		  "beginoptions" "by" "bysort"
		  "create"
		  "endoptions" "exit"
		  "ifexp" "inrange"
		  "option" "optionarg"
		  "put"
		  "require"
		  "stata"
		  "varlist"
		  "weight"
		 ) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face))

	(list
	 (concat
	  ado-start-cmd-must-start-line-regexp
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "action"
		  "gaction"
		  "script"
		  "view"
		  "program"
		 ) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-function-name-face))

	(list
	 (concat
	  ado-start-cmd-must-start-line-regexp
	   "\\<\\(call\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "action"
		  "gaction"
		  "script"
		  "view"
		  "program"
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; annoying -stata- command
	(list
	 (concat
	  ado-start-cmd-must-start-line-regexp
	   "\\<\\(stata\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "hidden"
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-must-start-line-regexp
	   "\\<\\(stata\\)\\>"
	   "[ \t]+"
	   "\\<\\(hidden\\)\\>"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-must-start-line-regexp
	   "\\<\\(stata\\)\\>"
	   "[ \t]+"
	   "\\<\\(hidden\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "immediate" "queue"
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t) '(3 ado-subcommand-face t))

	;; stata dialog functions i.e. things which require () after them 
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\>"
	   "\\([.]\\)"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"contains"
			"endswith"
			"isNumlistEQ" "isNumlistGE" "isNumlistGT" "isNumlistInRange" "isNumlistLE" "isNumlistLT"
			"isdefault" "isenabled" "iseq" "iseqignorecase" "isge" "isgt" 
			"isle" "islt" "isneq" "isnumlist" 
			"isvalidname" "isvarname" "isvisible"
			"startswith"
			) t))
	   "("
	   )
	  '(1 ado-function-name-face) '(2 ado-function-name-face t))

	;; mata keywords --- won't override, because they are only active in a mata block...
	;;  and mata block checking has not been implemented
	(list
	 (concat
	  ado-start-cmd-null-regexp
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "break"
		   "colvector" "complex" "continue"
		   "do"
		   "external"
		   "for" "function"
		   "goto"
		   "if" 
		   "matrix"
		   "numeric"
		   "pointer" "pragma" 
		   "real" "return" "rowvector"
		   "scalar" "string" 
		   "transmorphic" 
		   "vector" "version" "void"
		   "while"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-mata-keyword-face))

	(list
	 (concat
	  ado-start-cmd-null-regexp
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		  "aggregate" "array"
		  "boolean" "byte" 
		  "case" "catch" "class" "const"
		  "default" "delegate" "delete" "double" 
		  "else" "eltypedef" "end" "enum" "explicit" "export"
		  "float" "friend" 
		  "global"
		  "inline" "int" 
		  "local" "long" 
		  "namespace" "new" "NULL" 
		  "operator" "orgtypedef"
		  "polymorphic" "private" "protected" "public"
		  "quad"
		  "short" "signed" "static" "struct" "super" "switch"
		  "template" "this" "throw" "try" "typedef" "typename"
		  "union" "unsigned" "using" 
		  "virtual" "volatile"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-mata-future-keyword-face))

	(list
	 (concat
	  ado-start-cmd-must-start-line-regexp
	   "\\<\\(pragma\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"unset" "unused"
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-mata-keyword-face) '(2 ado-subcommand-face t))
    
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\<"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"pointer" "return"
			) t))
	   "("
	   )
	  '(1 ado-mata-keyword-face))

	  ;; mata subcommands
	  ;;  does this run into the need for extra harmful and harmless faces?!?!
	(list
	 (concat
	  ado-start-cmd-must-start-line-regexp
	   "\\<\\(mata\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "clear"
		  "d" "de" "des" "desc" "descr" "descri" "describ" "describe" 
		  "drop"
		  "end"
		  "help"
		  "matd" "matde" "matdes" "matdesc" "matdescr" "matdescri" "matdescrib" "matdescribe" 
		  "matsave" "matuse" "memory" "mosave"
		  "query"
		  "rename"
		  "stata"
		  "which"
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-mata-keyword-face t) '(2 ado-mata-keyword-face t))

	  ;; mata mlib subcommands
	  ;;  does this run into the need for extra harmful and harmless faces?!?!
	(list
	 (concat
	  ado-start-cmd-must-start-line-regexp
	   "\\<\\(mata\\)\\>"
	   "[ \t]+"
	   "\\<\\(mlib\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "add"
		  "create"
		  "index"
		  "q" "qu" "que" "quer" "query"
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-mata-keyword-face t) '(2 ado-mata-keyword-face t) 
	  '(3 ado-subcommand-face t))

	  ;; general mata set commands
	(list
	 (concat
	  ado-start-cmd-must-start-line-regexp
	   "\\<\\(mata\\)\\>"
	   "[ \t]+"
	   "\\<\\(set\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"matacache" "matalibs"
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-mata-keyword-face t) '(2 ado-mata-keyword-face t) 
	  '(3 ado-subcommand-face t))

	  ;; general mata set on/off commands
	(list
	 (concat
	  ado-start-cmd-must-start-line-regexp
	  "\\<\\(mata\\)\\>"
	  "[ \t]+"
	  "\\<\\(set\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "matalnum" "mataoptimize" "matastrict" "matamofirst"
		   ) 'words))
	 "[ \t]+"
	 (eval-when-compile 
	   (regexp-opt 
		'(
		  "on" "off"
		  ) 'words))	   
	 ado-end-cmd-regexp )
	'(1 ado-mata-keyword-face t) '(2 ado-mata-keyword-face t) 
	'(3 ado-subcommand-face t) '(4 ado-subcommand-face t))
   ;; mata set matafavor
	(list
	  (concat
	   ado-start-cmd-must-start-line-regexp
	   "\\<\\(mata\\)\\>"
	   "[ \t]+"
	   "\\<\\(set\\)\\>"
	   "[ \t]+"
	   "\\<\\(matafavor\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"space" "speed"
			) 'words))	   
	   ado-end-cmd-regexp )
	  '(1 ado-mata-keyword-face t) '(2 ado-mata-keyword-face t) 
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))

	;; incomplete mata subcommands with multiple parts
	;; weird, because -mata- is a command by itself

	(list
	 (concat
	  ado-start-cmd-must-start-line-regexp
	   "\\<\\(mata\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"mlib" "set"
			) 'words))
		 ado-end-cmd-regexp )
	  '(1 ado-needs-subcommand-face) '(2 ado-needs-subcommand-face))

	(list
	 (concat
	  ado-start-cmd-must-start-line-regexp
	  "\\<\\(mata\\)\\>"
	  "[ \t]+"
	  "\\<\\(set\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "matafavor" "matalnum" "mataoptimize" "matastrict" "matamofirst"
		   ) 'words))
		 ado-end-cmd-regexp )
	  '(1 ado-needs-subcommand-face) '(2 ado-needs-subcommand-face)
	  '(3 ado-needs-subcommand-face))

	  ;; mata functions (durn, this is a pain in the butt)
	  ;; functions which exist for regular stata are NOT included
	  ;; these, too ended up being split

	  ;; _ functions... perhaps the _ should be split off?
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   (eval-when-compile 
		 "\\b"
		 (regexp-opt 
		  '(
		  "_chdir" "_cholesky" "_cholinv" "_cholsolve" "_collate" "_conj" "_corr"
		  "_deriv" "_deriv_result_Hessian" "_deriv_result_Jacobian" "_deriv_result_gradient" "_deriv_result_scores" "_deriv_result_values" "_diag"
		  "_editmissing" "_edittoint" "_edittointtol" "_edittozero" "_edittozerotol" "_editvalue"
		  "_eigensystem" "_eigenvalues"
		  "_equilc" "_equilr" "_equilrc" "_error" 
		  "_fft" "_fillmissing" 
		  "_fclose" "_fget" "_fgetmatrix" "_fgetnl" "_flopin" "_flopout" "_fopen" "_fput" "_fputmatrix" "_fread" 
		  "_fseek" "_ftell" "_ftruncate" "_fullsvd" "_fwrite"
		  "_ghessenbergd"
		  "_gschurd" "_gschurdgroupby"
		  "_halton" "_hessenbergd" "_hqrd" "_hqrdp" "_hqrdp_la"
		  "_invfft" "_invsym"
		  "_jumble"
		  "_lefteigensystem" "_lowertriangle" "_lud" "_lud_la" "_luinv" "_luinv_la" "_lusolve" "_lusolve_la"
		  "_makesymmetric" "_matexpsym" "_matlogsym" "_matpowersym" "_mkdir"
		  "_moptimize" "_moptimize_evaluate"
		  "_negate"
		  "_optimize" "_optimize_evaluate"
		  "_perhapsequilc" "_perhapsequilr" "_perhapsequilrc" "_pinv"
		  "_qrinv" "_qrsolve" "_quadrunningsum"
		  "_rmdir" "_runningsum"
		  "_schurd" "_solvelower" "_solvenl_solve" "_solveupper" "_sort" 
		  "_st_addobs" "_st_addvar" 
		  "_st_data" "_st_macroexpand" 
		  "_st_sdata" "_st_sstore" "_st_store"
		  "_st_tsrevar"
		  "_st_varindex"
		  "_stata" "_strtoreal" "_sublowertriangle" "_substr" "_svd" "_svd_la" "_svdsv" "_svsolve" "_symeigensystem" "_symeigenvalues"
		  "_transpose" "_transposeonly"
		  "_unlink" "_uppertriangle" "_usubstr"
		 ) t))
	   "("
	   )
	  '(1 ado-mata-function-name-face t))
	  ;; _ functions... perhaps the _ should be split off?

	;; docx mata functions
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\b\\(_docx_\\)"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"add_data" "add_mata" "add_matrix"
			"append"
			"cell_set_colspan" "cell_set_rowspan"
			"close" "closeall"
			"image_add"
			"new" "new_table"
			"paragraph_add_text" "paragraph_new" "paragraph_new_styledtext"
			"query" "query_table"
			"save"
			"table_add_cell" "table_add_row" "table_del_cell" "table_del_row"
			"table_mod_cell" "table_mod_cell_image" "table_mod_cell_table"
			"table_query_row"
			"text_add_text"
			) t))
	   "("
	   )
	  '(1 ado-mata-function-name-face t) '(2 ado-mata-function-name-face t))
	;; mata commands
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\<\\(mata\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "clear"
		  "d" "de" "des" "desc" "descr" "descri" "describ" "describe" 
		  "drop"
		  "end"
		  "help"
		  "matd" "matde" "matdes" "matdesc" "matdescr" "matdescri" "matdescrib" "matdescribe" 
		  "matsave" "matuse" "memory" "mosave"
		  "query"
		  "rename"
		  "stata"
		  "which"
			) 'words))
	   )
	  '(1 ado-mata-keyword-face t) '(2 ado-mata-keyword-face t))

	(list
	 (concat
	  ado-start-cmd-null-regexp
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "AssociativeArray"
		  "C" "Corr" "Dmatrix" "Hilbert" "Im" "Kmatrix" 
		  "LA_DGBMV" "LA_DGEBAK" "LA_DGEBAL" "LA_DGEES" "LA_DGEEV" "LA_DGEHRD" 
		  "LA_DGGBAK" "LA_DGGBAL" "LA_DGGHRD" 
		  "LA_DHGEQZ" 
		  "LA_DHSEIN" "LA_DHSEQR" 
		  "LA_DLAMCH" "LA_DORGHR" "LA_DSYEVX" 
		  "LA_DTGSEN" "LA_DTGEVC" "LA_DTREVC" "LA_DTRSEN" 
		  "LA_ZGEBAK" "LA_ZGEBAL" "LA_ZGEES" "LA_ZGEEV" "LA_ZGEHRD" 
		  "LA_ZGGBAK" "LA_ZGGBAL" "LA_ZGGHRD" 
		  "LA_ZHGEQZ" "LA_ZHSEIN" "LA_ZHSEQR" 
		  "LA_ZTGSEN" "LA_ZTGEVC" "LA_ZTREVC" "LA_ZTRSEN" "LA_ZUNGHR"
		  "Lmatrix"
		  "PdfDocument" "PdfParagraph" "PdfTable" "PdfText"
		  "Re" "Toeplitz" "Vandermonde"
		  "acosh" "adosubdir" "all" "allof" "any" "anyof" 
		  "arg" "args"
		  "asarray" "asarray_contains" "asarray_contents" "asarray_create" "asarray_elements"
		  "asarray_first" "asarray_key" "asarray_keys" "asarray_next" 
		  "asarray_notfound" "asarray_remove"
		  "ascii" "asinh" "assert" "asserteq" "atanh"
		  "blockdiag" "breakkey" "breakkeyreset" 
		  "bufbfmtisnum" "bufbfmtlen" "bufbyteorder" "bufget" "bufio" "bufmissingvalue" "bufput"
		  "callersversion" "cat" "chdir" "cholsolve" "cholinv"
		  "classname"
		  "colmax" "colmaxabs" "colmin" "colminmax" "colmissing" "colnonmissing" "cols" "colscalefactors" "colshape" "colsum" 
		  "conj" "convolve" "correlation" "cosh" "crexternal" "cross" "crossdev" "cvpermute" "cvpermutesetup"
		  "deconvolve"
		  "deriv"
		  "deriv_init_argument"
		  "deriv_init"
		  "deriv_init_bounds"
		  "deriv_init_evaluator" "deriv_init_evaluatortype"
		  "deriv_init_h"
		  "deriv_init_narguments"
		  "deriv_init_params"
		  "deriv_init_scale" "deriv_init_search"
		  "deriv_init_verbose"
		  "deriv_init_weights"
		  "deriv_query"
		  "deriv_result_Hessian" "deriv_result_Jacobian"
		  "deriv_result_delta"
		  "deriv_result_errorcode" "deriv_result_errortext"
		  "deriv_result_gradient"
		  "deriv_result_h"
		  "deriv_result_returncode"
		  "deriv_result_scale" "deriv_result_scores"
		  "deriv_result_value" "deriv_result_values"
		  "designmatrix" "dettriangular"
		  "diag" "diagonal" "dir" "direxists" "direxternal" "display" "displayas" "displayflush" "dsign"
		  "editmissing" "edittoint" "edittointtol" "edittozero" "edittozerotol" "editvalue" 
		  "eigensystem" "eigensystemselectf" "eigensystemselecti" "eigensystemselectr" "eigenvalues" 
		  "eltype" "epsilon" "error" "errprintf" "exit"
		  "factorial" "favorspeed" "fbufget" "fbufput" "fclose" 
		  "ferrortext" "fft" "fget" "fgetnl" "fgetmatrix" 
		  "fileexists" "findexternal" "findfile" "floatround" "fopen" "fput" "fputmatrix" 
		  "fread" "freturncode" "frombase" "fseek" "fstatus" 
		  "ftell" "ftfreqs" "ftpad" "ftperiodogram" "ftretime" "ftruncate" "ftunwrap" "ftwrap" 
		  "fullsdiag" "fullsvd" "fwrite"
		  "gamma"
		  "geigensystem" "geigensystemelectf" "geigensystemelecti" "geigensystemelectr"
		  "ghalton" "ghessenbergd" 
		  "ghk" "ghk_init" "ghk_init_antithetics" "ghk_init_method" "ghk_init_pivot" "ghk_init_start" 
		  "ghk_query_npts"
		  "ghkfast"
		  "ghkfast_i" 
		  "ghkfast_init" "ghkfast_init_antithetics" "ghkfast_init_pivot"
		  "ghkfast_query_dim"
		  "ghkfast_query_method"
		  "ghkfast_query_n"
		  "ghkfast_query_npts"
		  "ghkfast_query_pointset_i"
		  "ghkfast_query_rseed"
		  "gschurd" "gschurdgroupby"
		  "halton" "hash1" "hasmissing" "hessenbergd"
		  "hqrd" "hqrdmultq" "hqrdmultq1t" "hqrdp" "hqrdq" "hqrdq1" "hqrdr" "hqrdr1"
		  "inbase" "invHilbert" "invfft" "invorder" "invtokens" "invvech"
		  "iscomplex" "isdiagonal" "isfleeting" "ispointer" "isreal" 
		  "isrealvalues" "isstring" "issymmetric" "issymmetriconly" "isview" 
		  "jumble"
		 ) 'words))
	   "("
	   )
	 '(1 ado-mata-function-name-face t))
	(list
	 (concat
	  ado-start-cmd-null-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "lefteigensystem" "lefteigensystemselectf" "lefteigensystemselecti" "lefteigensystemselectr"
		   "leftgeigensystem" "leftgeigensystemelectf" "leftgeigensystemelecti" "leftgeigensystemelectr"
		   "length"
		  "liststruct" "lnnormal" "lnnormalden" "lowertriangle" "lud" "luinv" "lusolve"
		  "makesymmetric" "matexpsym" "matlogsym" "matpowersym" "maxindex"
		  "mean" "meanvariance" "minindex" "minmax" "missingof" "mkdir"
		  "moptimize" "moptimize_ado_cleanup" "moptimize_evaluate" "moptimize_init" "moptimize_query"
		  "more"
		  "mreldifre" "mreldifsym"
		  "mvnormal" "mvnormalcv" "mvnormalcvderiv" "mvnormalcvderivqp" "mvnormalcvqp" "mvnormalderiv" "mvnormalderivqp" "mvnormalqp"
		  "nameexternal" "nonmissing" "norm"
		  "optimize" "optimize_evaluate" "optimize_init" "optimize_query"
		  "order" "orgtype"
		  "panelsetup" "panelstats" "panelsubmatrix" "panelsubview"
		  "pathasciisuffix" "pathbasename" "pathisabs" "pathisurl" "pathjoin" "pathlist" 
		  "pathrmsuffix" "pathsearchlist" "pathsplit" "pathstatasuffix" "pathsubsysdir" "pathsuffix"
		  "pi" "pinv"
		  "polyadd" "polyderiv" "polydiv" "polyeval" "polyinteg" "polymult" "polyroots" "polysolve" "polytrim"
		  "printf" "pwd"
		  "range" "rangen" "rmdir" "rmexternal" "rngstate" "rowmax" "rowmissing" "rowscalefactors"
		  "qrd" "qrdp" "qrinv" "qrsolve" 
		  "quadcorrelation" "quadcross" "quadcrossdev" "quadrant" "quadcolsum" 
		  "quadmeanvariance" "quadrowsum" "quadrunningsum" "quadsum" "quadvariance" 
		  "querybreakintr"
		  "rank" "rdiscrete" "revorder" "rowmaxabs" "rowmin" "rowminmax" "rownonmissing" "rows" "rowshape" "rowsum" "rseed" "runningsum"
		  "schurd" "select" "selectindex"
		  "setbreakintr" "setmore" "setmoreonexit" "sinh" "sizeof" "smallestdouble"
		  "solve_tol" 
		  "solvelower" 
		  "solvenl_dump" "solvenl_init" "solvenl_solve"
		  "solveupper"
		  "sort" "spline3" "spline3eval" "sprintf"
		  "st_select" "stata" "statasetversion" "stataversion" 
		  "stritrim"
		  "strlower" "strltrim"
		  "strproper"
		  "strreverse" "strrpos" "strrtrim"
		  "strtoreal" "strtrim"
		  "structname" "strupper"
		  "sublowertriangle"
		  "svd" "svdsv" "svsolve" "swap"
		  "symeigensystem" "symeigensystemselecti" "symeigensystemselectr" "symeigenvalues"
		  "timer" 
		  "tokenallowhex" "tokenallownum" "tokenget" "tokengetall" "tokeninit" "tokeninitstata" "tokenoffset" 
		  "tokenpeek" "tokenrest" "tokens" "tokenset" "tokenpchars" "tokenqchars" "tokenwchars" "transposeonly"
		  "udsubstr" "uniqrows" "unitcircle" "unlink" "unorder" "uppertriangle"
		  "urldecode" "urlencode"
		  "valofexternal" "variance" "vec" "vech"
		 ) 'words))
	   "("
	   )
	 '(1 ado-mata-function-name-face t))


	  ;; the moptimize_init functions
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\<\\(moptimize_init_\\)"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			 "by" 
			 "cluster" "constraints" 
			 "conv_ignorenrtol" "conv_maxiter" "conv_nrtol" "conv_ptol" "conv_vtol" "conv_warning" 
			 "depvar" 
			 "eq_coefs" "eq_colnames" "eq_cons" "eq_exposure"
			 "eq_freeparm" "eq_indepvars" "eq_n" "eq_name" "eq_offset" 
			 "evaluations" "evaluator" "evaluatortype" 
			 "gnweightmatrix" 
			 "iterid" 
			 "ndepvars" "negH" "nmsimplexdeltas" "nuserinfo" 
			 "search" "search_bounds" "search_random" "search_repeat" "search_rescale" 
			 "singularHmethod" "svy" 
			 "technique" "touse" 
			 "trace_Hessian" "trace_ado" "trace_coefdiffs" "trace_coefs" "trace_dots" "trace_gradient" "trace_step" "trace_tol" "trace_value" 
			 "tracelevel" 
			 "userinfo" 
			 "valueid" "vcetype" "verbose"
			 "weight" "weighttype" "which" 			 
			) t ))
	   "("
	   )
	  '(1 ado-mata-function-name-face t) '(2 ado-mata-function-name-face t))

	  ;; the moptimize results prefix functions
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\<\\(moptimize_result_\\)"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"Hessian" 
			"V" "V_oim" "V_opg" "V_robust" "Vtype" 
			"coefs" "colstripe" "converged" 
			"display" 
			"eq_coefs" "errorcode" "errortext" "evaluations" 
			"gradient" 
			"iterations" "iterationlog" 
			"post" 
			"returncode" 
			"scores" 
			"value" "value0" 			 
			) t ))
	   "("
	   )
	  '(1 ado-mata-function-name-face t) '(2 ado-mata-function-name-face t))

	  ;; the moptimize_util prefix functions
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\<\\(moptimize_util_\\)"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"by" "depvar" "eq_indices" "matbysum" "matsum" "sum" "vecsum" "xb" 						) t ))
	   "("
	   )
	  '(1 ado-mata-function-name-face t) '(2 ado-mata-function-name-face t))
	  
	;; the solvenl_init functions
	(list
	 (concat
	  ado-start-cmd-null-regexp
	  "\\<\\(solvenl_init_\\)"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "argument"
		   "conv_iterchng" "conv_nearzero" "conv_maxiter"
		   "damping"
		   "evaluator"
		   "iter_dot" "iter_dot_indent" "iter_log"
		   "narguments" "numeq"
		   "startingvals"
		   "technique" "type"
		   ) t ))
	   "("
	   )
	  '(1 ado-mata-function-name-face t) '(2 ado-mata-function-name-face t))


	;; the solvenl_result functions
	(list
	 (concat
	  ado-start-cmd-null-regexp
	  "\\<\\(solvenl_result_\\)"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "Jacobian"
		   "converged" "conv_iter" "conv_iterchng" "conv_nearzero"
		   "error_code" "error_text"
		   "return_code"
		   "values"
		   ) t ))
	   "("
	   )
	  '(1 ado-mata-function-name-face t) '(2 ado-mata-function-name-face t))


	  ;; arrrgh the mata st_ functions
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\<\\(st_\\)"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "addobs" "addvar" 
		  "data" "dir" "dropobsif" "dropobsin" "dropvar"
		  "eclear"
		  "global" "global_hcat"
		  "isfmt" "islmname" "isname" "isnumfmt" "isnumvar" "isstrfmt" "isstrvar"
		  "keepobsif" "keepobsin" "keepvar"
		  "local"
		  "macroexpand" "matrix" "matrix_hcat" "matrixcolstripe" "matrixrowstripe"
		  "nobs" "numscalar" "numscalar_hcat" "nvar"
		  "rclear" "replacematrix"
		  "sclear" "sdata" "sstore" "store" "strscalar" "subview" "sview"
		  "tempfilename" "tempname" "tsrevar" 
		  "updata"
		  "varformat" "varindex" "varlabel" "varname" "varrename" "vartype" "varvaluelabel"
		  "view" "viewobs" "viewvars"
		  "vldrop" "vlexists" "vlload" "vlmap" "vlmodify" "vlsearch"
			) t ))
	   "("
	   )
	  '(1 ado-mata-function-name-face t) '(2 ado-mata-function-name-face t))

	  ;; mata optimize_init functions
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\<\\(optimize_init_\\)"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"argument"
			"cluster" "colstripe" "constraints" 
			"conv_maxiter" "conv_nrtol" "conv_ptol" "conv_vtol" "conv_warning"
			"evaluations" "evaluator" "evaluatortype"
			"ingnorenrtol" "iterid"
			"narguments" "negH" "nmsimplexdeltas"
			"params"
			"singularHmethod"
			"technique" 
			"trace_Hessian" "trace_dots" "trace_gradient" "trace_paramdiffs" "trace_params" "trace_step" "trace_tol" "trace_value"
			"tracelevel"
			"valueid" "verbose"
			"which"
			) t ))
	   "("
	   )
	  '(1 ado-mata-function-name-face t) '(2 ado-mata-function-name-face t))

	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\<\\(optimize_result_\\)"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "Hessian"
			  "V" "V_oim" "V_opg" "V_robust" "Vtype"
			  "converged"
			  "errorcode" "errortext" "evaluations"
			  "gradient"
			  "iterationlog" "iterations"
			  "params"
			  "returncode"
			  "scores"
			  "value" "value0"
			) t ))
	   "("
	   )
	  '(1 ado-mata-function-name-face t) '(2 ado-mata-function-name-face t))


	;; mata xl class functions and pdf class functions
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "\\([.]\\)"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"addColumn" "addImage" "addLineBreak" "addNewPage" "addParagraph"
			"addRow" "addString" "addTable" "addText"
			"add_sheet"
			"clearContent" "clear_book" "clear_sheet" "close" "close_book" "create_book"
			"delete_sheet" "delete_sheet_merge"
			"delColumn" "delRow"
			"fillData" "fillMataMatrix" "fillStataMatrix"
			"get_cell_type" "get_colnum" "get_last_error" "get_last_error_message" "get_number" "get_sheets" "get_string"
			"init"
			"load_book"
			"put_formula" "put_number" "put_picture" "put_string"
			"query"
			"save"
			"setBgColor" "setBorderColor" "setBorderWidth" "setBottomSpacing"
			"setCellBgColor" "setCellBorderColor" "setCellBorderWidth"
			"setCellColSpan" "setCellColor"
			"setCellContentImage"
			"setCellContentHAlignment"
			"setCellContentTable"
			"setCellContentVAlignment"
			"setCellFont" "setCellFontSize"
			"setCellMargin"
			"setCellRowSpan" "setCellSpan"
			"setContentHAlignment"
			"setContentImage"
			"setCellContentParagraph"
			"setCellContentString"
			"setColor"
			"setColumnWidths"
			"setContentTable" "setContentVAlignment"
			"setFirstIndent" "setFont" "setFontSize"
			"setHAlignment"
			"setIndentation"
			"setLeftIndent" "setLineSpace"
			"setMargins"
			"setPageSize"
			"setRightIndent" "setRowSplit"
			"setStrikethru" "setSubscript" "setSuperscript"
			"setTopSpacing" "setTotalWidth"
			"setUnderline" 
			"setVAlignment"
			"setWidthPercent"
			"set_border" "set_bottom_border"
			"set_column_width"
			"set_diagonal_border"
			"set_error_mode"
			"set_fill_pattern" "set_font" "set_font_bold" "set_font_italic" "set_font_script" "set_font_strikeout" "set_font_underline" "set_format_hidden" "set_format_lock"
			"set_keep_cell_format"
			"set_left_border"
			"set_horizontal_align"
			"set_missing" "set_mode"
			"set_number_format"
			"set_right_border" "set_row_height"
			"set_sheet" "set_sheet_gridlines" "set_sheet_merge" "set_shrink_to_fit"
			"set_text_indent" "set_text_rotate" "set_text_wrap"
			"set_top_border"
			"set_vertical_align"
			) t ))
	   "("
	   )
	  '(1 ado-constant-face t) '(2 ado-mata-function-name-face t))

	  ;; obsolete mata functions
	  ;; was listed as obsolete, for some reason; ok in Stata 13 "_conj"
	(list
	 (concat
	  ado-start-cmd-null-regexp
	  "\\<"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"_docx_query_document"
		 "acosr" "asinr" "atanr"
		 "ghkfastsetup"
		 "moptimize_init_view"
		 "optimize_init_type" "optimize_init_gnweightmatrix"
		 "uniformseed"
		 ) t ))
	   "("
	   )
	  '(1 ado-obsolete-face))
	  
	  ;; obsolete mata class functions
	(list
	 (concat
	  ado-start-cmd-null-regexp
	   "[.]"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"setCellBorderWidths" "setCellBottomBorderWidth" "setCellBottomMargin"
			"setCellLeftBorderWidth" "setCellLeftMargin"
			"setCellMargins" 
			"setCellRightBorderWidth" "setCellRightMargin" 
			"setCellTopBorderWidth" "setCellTopMargin"
		 ) t ))
	   "("
	   )
	  '(1 ado-obsolete-face t))

	;; all variable/macro stuff (put late so it will override)
	  ;; keep just before the obsolete commands!
	  ;; internal constants
	(list
	 (eval-when-compile
	   (regexp-opt 
		'(
		 "_N" "_merge" "_n" "_pi" "_rc"
		 ) 'words))
	  '(1 ado-constant-face t))

	  ;; some generated vars
	  ;; ... which are now out of date
	(list
	 "\\<\\(_result([1-9]+[0-9]*)\\)"
	 '(1 ado-obsolete-face t))

	;; global macros
	(list
	 (concat "\\([$]" ado-stata-name-regexp "\\)")
	 '(1 ado-variable-name-face t))

	;; local macros
	;;   highlights *before* the macro is ended, which 
	;;   could lead to typos, but gets rid of recursive
	;;   definitions.
	(list
	 (concat
	  "\\(`+\\)\\(?:\\+\\+\\|--\\)?"
	  ado-stata-local-name-bound-regexp
	  "\\('\\)"
	  ) 
	 '(1 ado-variable-name-face t) '(2 ado-variable-name-face t)
	 '(3 ado-variable-name-face t))
	
	(list
	  (concat
	   "`+"
	   "\\(\\(?:e\\|r\\|s\\)\\)"
	   "\\((\\)"
	   ado-stata-local-name-bound-regexp 
	   "\\()\\)"
	   "'+"
	   )
	  '(1 ado-function-name-face t) '(2 ado-constant-face t)
	  '(3 ado-variable-name-face t) '(4 ado-constant-face t))

	  ;; obsolete mfp arguments
	  ;;  none --- looks to have been a documentation error

	  ;; what few obsolete commands I've gathered
	  ;; lfit and score moved before the matrix command so that it won't affect
	  ;; the matrix score command  
	;; 
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile
		(regexp-opt 
		 '(
		   "_huber" "_qreg" 
		   "adjust"
		   "aorder"
		   "archlm"
		   "bgodfrey" "blogit" "bprobit" "bstrap"
		   "chdir"
		   "ch" "che" "chel" "chelp" 
		   "cnr" "cnre" "cnreg" 
		   "dprobit" "durbina" "dvech" "dwstat"
		   "ereg" "ereghet"
		   "fdades" "fdadesc" "fdadescr" "fdadescri" "fdadescrib" "fdadescribe" 
		   "fdasav" "fdasave" 
		   "fdause"
		   "findit" 
		   "fracgen" "fracpoly"
		   "gamma" "gammahet"
		   "glogit"
		   "gompertz" "gompertzhet"
		   "gprobit" "greigen"
		   "haver"
		   "hsearch"
		   "iis" "impute" "insheet" "ivreg"
		   "hettest" "hareg" "hereg" "hlogit" "hprobit" "hreg" "huber"
		   "imtest"
		   "llogist" "llogistic" "llogistichet" "lnormal" "lnormalhet"
		   "lo" "loo" "look" "looku" "lookup"
		   "mfx"
		   "mov" "move" 
		   "nlinit"
		   "ovtest"
		   "outs" "outsh" "outshe" "outshee" "outsheet"
		   "lstat"
		   "poisgof"
		   "sampsi" 
		   "shelltool"
		   "simul" "spikeplt" "stcurv" "stphtest" 
		   "svyintrg" "svylc" "svymlog" "svyolog" "svyoprob" "svypois" "svyprobt" "svyreg" "svytest" 
		   "szroeter"
		   "tis" "treatreg" 
		   "varfcast" "varirf" "vce" "vif"
		   "weibull" "weibullhet"
		   "xtclog" "xtcorr" "xthaus" "xtrchh" 
		   "xtmelogit" "xtmepoisson" "xtmixed"
		   "xtpois"
		   "ztnb" "ztb"
		  ) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-obsolete-face))

	  ;; the datasignature commands   
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "datasig" "datasign" "datasigna" "datasignat" "datasignatu" "datasignatur" "datasignature"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"clear"
			"set"
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmful-face t) '(2 ado-subcommand-face t))
			  
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "datasig" "datasign" "datasigna" "datasignat" "datasignatu" "datasignatur" "datasignature"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "conf" "confi" "confir" "confirm" 
			  "rep" "repo" "repor" "report" 
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t))

	;; the esize commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(esize\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "two" "twos" "twosa" "twosam" "twosamp" "twosampl" "twosample" 
		   "unp" "unpa" "unpai" "unpair" "unpaire" "unpaired" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t))
	 
	;; the estat commands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  "\\<\\(estat\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "abond" "acplot" "alt" "alte" "alter" "altern" "alterna" "alternat" "alternati" "alternativ" "alternative" "alternatives"
		   "anova" "anti" "archlm" "aroots" 
		   "bgo" "bgod" "bgodf" "bgodfr" "bgodfre" "bgodfrey"
		   "boot" "boots" "bootst" "bootstr" "bootstra" "bootstrap" 
		   "canontest" "clas" "class" "classfunctions" 
		   "classi" "classif" "classifi" "classific" 
		   "classifica" "classificat" "classificati" 
		   "classificatio" "classification" 
		   "classtable" 
		   "common" "compare"
		   "con" "conc" "conco" "concor" "concord" "concorda" "concordan" "concordanc" "concordance"
		   "config" "coordinates" 
		   "cor" "corr" "corre" "correl" "correla" "correlat" "correlati" "correlatio" "correlation"
		   "correlation" "correlations" 
		   "cov" "cova" "covar" "covari" "covaria" "covarian" "covarianc" "covariance" 
		   "cv"
		   "df" "distances" 
		   "dur" "duration" "durb" "durbi" "durbin" "durbina" "durbinal" "durbinalt" 
		   "dwa" "dwat" "dwats" "dwatso" "dwatson" 
		   "eff" "effe" "effec" "effect" "effects" 
		   "eform"
		   "endog" "endoge" "endogen" "endogeno" "endogenou" "endogenous" 
		   "eqg" "eqgo" "eqgof" 
		   "eqt" "eqte" "eqtes" "eqtest" 
		   "errorrate" "esize"
		   "factors"
		   "facw" "facwe" "facwei" "facweig" "facweigh" "facweight" "facweights" 
		   "first" "firsts" "firstst" "firststa" "firststag" "firststage"
		   "fra" "fram" "frame" "framew" "framewo" "framewor" "framework" 
		   "ggof" 
		   "gin" "ginv" "ginva" "ginvar" "ginvari" "ginvaria" "ginvarian" "ginvariant" 
		   "gof" "gofplot"
		   "gr" "grdistances" "gro" "grou" "group" "grmeans" "grsummarize"
		   "hett" "hette" "hettes" "hettest" 
		   "ic" "icc"
		   "impact"
		   "imt" "imte" "imtes" "imtest"
		   "inertia"
		   "kmo"
		   "lceff" "lceffe" "lceffec" "lceffect" "lceffects"
		   "lcgof" "lcmean" "lcprob"
		   "list" "loadings"
		   "manova" "mfx" 
		   "mi" "min" "mind" "mindi" "mindic" "mindice" "mindices" 
		   "moran" "mvreg"
		   "nproc"
		   "over" "overi" "overid" 
		   "ovt" "ovte" "ovtes" "ovtest" 
		   "pairwise" "period" "phtest" "predict" "profiles"
		   "quantiles"
		   "recov" "recova" "recovar" "recovari" "recovaria" "recovarian" "recovarianc" "recovariance"
		   "rep" "repo" "repor" "report" 
		   "res" "resi" "resid" "residu" "residua" "residual" "residuals" 
		   "rotate" "rotatecompare"
		   "sargan" "sbcusum" "sbknown" "sbsingle"
		   "score" "scoret" "scorete" "scoretes" "scoretest" "scoretests" 
		   "sd" "se" "single" "size" "smc" 
		   "sta" "stab" "stabl" "stable" 
		   "std" "stdi" "stdiz" "stdize" 
		   "strata" "stress" "structure"
		   "su" "subinertia" "sum" "summ" "summa" "summar" 
		   "summari" "summariz" "summarize" 
		   "svyset"
		   "szr" "szro" "szroe" "szroet" "szroete" "szroeter" 
		   "table"
		   "tef" "teff" "teffe" "teffec" "teffect" "teffects"
		   "transition"
		   "vce" "vif"
		   "wcor" "wcorr" "wcorre" "wcorrel" "wcorrela" "wcorrelat" "wcorrelati" "wcorrelatio" "wcorrelation" 
		   ) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; the estimates commands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   (eval-when-compile 
		 (regexp-opt 
       '(
			 "est" "esti" "estim" "estima" "estimat" "estimate" "estimates"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			 "clear"
			 "des" "desc" "descr" "descri" "describ" "describe"
			 "dir" 
			 "drop"
			 "esample"
			 "for"
			 "note" "notes" 
			 "q" "qu" "que" "quer" "query" 
			 "r" "re" 
			 "rep" "repl" "repla" "replay" 
			 "res" "rest" "resto" "restor" "restore" 
			 "save"
			 "stat" "stats" 
			 "sto" "stor" "store" 
			 "tab" "tabl" "table"
			 "title"
			 "use"
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

    ;; the extra estimates notes commands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   (eval-when-compile 
		 (regexp-opt 
       '(
			 "est" "esti" "estim" "estima" "estimat" "estimate" "estimates"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			 "note" "notes"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"drop"
			"l" "li" "lis" "list" 
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))

	;; forecast commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "fore" "forec" "foreca" "forecas" "forecast" 
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "ad" "adj" "adju" "adjus" "adjust"
		   "co" "coe" "coef" "coefv" "coefve" "coefvec" "coefvect" "coefvecto" "coefvector"
		   "d" "de" "des" "desc" "descr" "descri" "describ" "describe" 
		   "est" "esti" "estim" "estima" "estimat" "estimate" "estimates" 
		   "ex" "exo" "exog" "exoge" "exogen" "exogeno" "exogenou" "exogenous" 
		   "id" "ide" "iden" "ident" "identi" "identit" "identity" 
		   "l" "li" "lis" "list" 
		   "q" "qu" "que" "quer" "query" 
		   "s" "so" "sol" "solv" "solve" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	(list
	 (concat
	  ado-start-cmd-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "fore" "forec" "foreca" "forecas" "forecast" 
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "clear"
		   "cr" "cre" "crea" "creat" "create" 
		   "dr" "dro" "drop" 
		   "fore" "forec" "foreca" "forecas" "forecast" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	;; the fp plot
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(fp\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "plot"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t))

	;; the fp generate and predict commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(fp\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gen" "gene" "gener" "genera" "generat" "generate"
		   "predict"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face t) '(2 ado-subcommand-face t))


	;; the ivpoisson commands
	(list
	 (concat
	  ado-start-cmd-regexp
	  "\\<\\(ivpoisson\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "gmm"
		   "cfunc" "cfunct" "cfuncti" "cfunctio" "cfunction" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t))


	;; putexcel's subcommands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  "\\<\\(putexcel\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "clear"
		   "describe" 
		   "set"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))


	;; the ssd commands from SEM
	;; the estat commands
	;; am NOT including incomplete ssd set commands for now (because of optional number)
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  "\\<\\(ssd\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "addgr" "addgro" "addgrou" "addgroup" 
		   "build"
		   "d" "de" "des" "desc" "descr" "descri" "describ" "describe" 
		   "init"
		   "l" "li" "lis" "list" 
		   "repair"
		   "set"
		   "stat" "statu" "status" 
		   "unaddgr" "unaddgro" "unaddgrou" "unaddgroup" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; unicode commands, harmless
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "unicode" 
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "analyze"
		   "coll" "colla" "collat" "collato" "collator"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; unicode commands, harmful
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "unicode" 
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "conv" "conve" "conver" "convert" "convertf" "convertfi" "convertfil" "convertfile" 
		   "erasebackups"
		   "restore"
		   "retr" "retra" "retran" "retrans" "retransl" "retransla" "retranslat" "retranslate"
		   "tr" "tra" "tran" "trans" "transl" "transla" "translat" "translate" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	;; incomplete unicode commands
	;; unicode locale/uipackage commands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "unicode" 
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "en" "enc" "enco" "encod" "encodi" "encodin" "encoding" 
		   "loc" "loca" "local" "locale"
		   "ui" "uip" "uipa" "uipac" "uipack" "uipacka" "uipackag" "uipackage" 
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-needs-subcommand-face t) '(2 ado-needs-subcommand-face t))


	;; unicode encoding command
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "unicode" 
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "en" "enc" "enco" "encod" "encodi" "encodin" "encoding" 
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "alias"
		   "list"
		   "set"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t) '(3 ado-subcommand-face t))

	;; unicode locale/uipackage commands
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "unicode" 
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "loc" "loca" "local" "locale"
		   "ui" "uip" "uipa" "uipac" "uipack" "uipacka" "uipackag" "uipackage" 
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "list"
		   ) 'words))
	  ado-end-cmd-regexp )
	 '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t) '(3 ado-subcommand-face t))

	;; things which are partially obsolete

	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "jknife"
		  "parse"
		  "whelp"
		 ) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-obsolete-face))
    
	  ;; apparently obsolete window commands
	 (list
	  (concat
	   ado-start-cmd-no-prefix-regexp
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "win" "wind" "windo" "window"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "m" "me" "men" "menu"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "popout"
		  "set"
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-obsolete-face))

	 (list
	  (concat
	   ado-start-cmd-no-prefix-regexp
	   (eval-when-compile
		 (regexp-opt 
       '(
		 "win" "wind" "windo" "window"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "m" "me" "men" "menu"
			) 'words))
	   "[ \t]+"
	   "\\<\\(append\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "popout"
		  "string"
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-obsolete-face t) '(4 ado-obsolete-face t))

	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"ma" "mac" "macr" "macro"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"de" "def" "define" 
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmful-face) '(2 ado-obsolete-face t))

	  ;; multiword extended macro names using 'set', which are obsolete
	(list
	 (concat
	  ado-start-cmd-no-prefix-regexp
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		 ) 'words))
	   "[ \t]+"
	   ado-stata-local-name-bound-regexp 
        "[ \t]*:[ \t]*"
		"\\<\\(set\\)\\>"
		"[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "adosize" "graphics" "level" "linesize" "logtype" "matsize" "more" "pagesize"
		  "rmsg" "trace" "type" "virtual" 
			) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	  '(3 ado-obsolete-face t) '(4 ado-obsolete-face t))

	;; incomplete subcommands needing a single subcommand
	;;  anything with MULTIPLE subcommands is listed after its regular highlighting
	;; !! did not split by whether they should or should not have a prefix. ugh.
	;; exceptions, because they allow 0 args (partial list)
	;;;; estimates, fp
	(list
	 (concat
	  ado-start-cmd-regexp
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "#d" "#de" "#del" "#deli" "#delim" "#delimi" "#delimit" 
		 "_est" "_esti" "_estim" "_estima" "_estimat" "_estimate" "_estimates"
		 "_return"
		 "ado"
		 "bcal"
		 "bayesstats" "bayestest"
		 "call"
		 "char"
		 "churdle"
		 "ci" "cii" 
		 "classutil"
		 "cluster" "clustermat"
		 "cmdlog"
		 "conf" "confi" "confir" "confirm" 
		 "cons" "const" "constr" "constra" "constrai" "constrain" "constraint" 
		 "creturn"
		 "discrim"
		 "duplicates"
		 "eret" "eretu" "eretur" "ereturn" 
		 "esize" "estat"
		 "export"
		 "fcast" "file" 
		 "fore" "forec" "foreca" "forecas" "forecast" 
		 "foreach" 
		 "forv" "forva" "forval" "forvalu" "forvalue" "forvalues"
		 "fracreg"
		 "fvset" "fvunab"
		 "graph"
		 "icd9" "icdp"
		 "import"
		 "irf" "irt" "irtgraph"
		 "la" "lab" "labe" "label" 
		 "log"
		 "mat" "matname" "mat_put_rr" "matr" "matri" "matrix"
		 "mer" "merg" "merge"
		 "mgarch"
		 "mi"
		 "ml"
		 "misstable"
		 "mswitch"
		 "mvtest"
		 "npregress"
		 "net"
		 "note" "notes" 
		 "odbc"
		 "power"
		 "palette" "pause" "postutil"
		 "query"
		 "reshape" 
		 "ret" "retu" "retur" "return" 
		 "se" "set"
		 "set_defaults"
		 "snapshot"
		 "sret" "sretu" "sretur" "sreturn" 
		 "ssc"
		 "ssd"
		 "st_is"
		 "stopbox"
		 "stpow" "stpowe" "stpower"
		 "stteffects"
		 "tebalance" "teffects"
		 "timer"
		 "translator" "transmap"
		 "tsfilter" "tssmooth" "tsunab"
		 "twoway"
		 "unab" "unicode"
		 "vers" "versi" "versio" "version" 
		 "view"
		 "win" "wind" "windo" "window" 
		 "xtcointest" "xtunitroot"
		 ) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-needs-subcommand-face))

	;; those subcmds which must start a line
	
	(list
	  (concat
	   ado-start-cmd-no-prefix-regexp
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "se" "set"
		 ) 'words))
	   ado-end-cmd-regexp )
	  '(1 ado-needs-subcommand-face))

	;; simple *-style comments; w/o the [^/\n] term, old continuations fail
	(list "^[ \t]*\\([*]\\([^/\n].*\\|$\\)\\)" 
		  '(1 ado-comment-face t) '(2 ado-comment-face t))

	;; c++ comments at very end to overwrite all other syntaxes
	;; trying c++ comments here, instead of in syntax table
	(list "\\(^\\|[ \t]+\\)\\(//.*\\)$"
		  '(1 ado-comment-face t) '(2 ado-comment-face t))

    ;; uh oh... things with multiple subcommands

	;; set <foo> on/off commands (must start a line)


	)))
	
;;; here are all the added commands for highlighting user-written commands

(defun ado-add-plus ()
  "Adds/updates highlighting for all ado files in `ado-plus-dir'.
This includes the a, b, c, ... subdirectories. If ado-files have been
added or removed since the last update the highlighting list will be
updated appropriately. If `ado-plus-dir' is not set, it gets set using
the function `ado-reset-plus-dir'."
  (interactive)
  (unless ado-plus-dir
	(ado-reset-plus-dir))
  (ado-modify-font-lock-keywords 'plus (directory-file-name ado-plus-dir) ''ado-plus-harmless-face))

(defun ado-add-personal ()
  "Adds/updates highlighting for all ado files in `ado-personal-dir'.
This includes the a, b, c, ... subdirectories. If ado-files have been
added or removed since the last update the highlighting list will be
updated appropriately. If `ado-personal-dir' is not set, it gets set using
the function `ado-reset-personal-dir'."
  (interactive)
  (unless ado-personal-dir
	(ado-reset-personal-dir))
  (ado-modify-font-lock-keywords 'personal (directory-file-name ado-personal-dir) ''ado-personal-harmless-face))

(defun ado-add-oldplace ()
  "Adds/updates highlighting for all ado files in `ado-oldplace-dir'.
This includes the a, b, c, ... subdirectories. If ado-files have been
added or removed since the last update the highlighting list will be
updated appropriately. If `ado-oldplace-dir' is not set, it gets set using
the function `ado-reset-oldplace-dir'."
  (interactive)
  (unless ado-oldplace-dir
	(ado-reset-oldplace-dir))
  (ado-modify-font-lock-keywords 'oldplace (directory-file-name ado-oldplace-dir) ''ado-oldplace-harmless-face))

(defun ado-add-site ()
  "Adds/updates highlighting for all ado files in `ado-site-dir'.
This includes the a, b, c, ... subdirectories. If ado-files have been
added or removed since the last update the highlighting list will be
updated appropriately. If `ado-site-dir' is not set, it gets set using
the function `ado-reset-site-dir'."
  (interactive)
  (unless ado-site-dir
	(ado-reset-site-dir))
  (ado-modify-font-lock-keywords 'site (directory-file-name ado-site-dir) ''ado-site-harmless-face))

(defun ado-remove-personal ()
  "Removes highlighting for all ado files in `ado-personal-dir'.
This includes the a, b, c, ... subdirectories. If ado-files have been
added or removed since the last update the highlighting list will be
updated appropriately. If `ado-personal-dir' is not set, it gets set using
the function `ado-reset-personal-dir'."
  (interactive)
  (ado-modify-font-lock-keywords 'personal (directory-file-name ado-personal-dir) ''ado-personal-harmless-face t))

(defun ado-remove-plus ()
  "Removes highlighting for all ado files in `ado-plus-dir'.
This includes the a, b, c, ... subdirectories. If ado-files have been
added or removed since the last update the highlighting list will be
updated appropriately."
  (interactive)
  (ado-modify-font-lock-keywords 'plus (directory-file-name ado-plus-dir) ''ado-plus-harmless-face t))

(defun ado-remove-oldplace ()
  "Removes highlighting for all ado files in `ado-oldplace-dir'.
This includes the a, b, c, ... subdirectories. If ado-files have been
added or removed since the last update the highlighting list will be
updated appropriately."
  (interactive)
  (ado-modify-font-lock-keywords 'oldplace (directory-file-name ado-oldplace-dir) ''ado-oldplace-harmless-face t))

(defun ado-remove-site ()
  "Removes highlighting for all ado files in `ado-site-dir'.
This includes the a, b, c, ... subdirectories. If ado-files have been
added or removed since the last update the highlighting list will be
updated appropriately."
  (interactive)
  (ado-modify-font-lock-keywords 'site (directory-file-name ado-site-dir) ''ado-site-harmless-face t))

(defun ado-modify-font-lock-keywords (name dir face &optional remove subdir extension)
  "Base function used to add or remove keywords for font locking. 
Gets called by functions adding keywords for all commands in 
a directory, as well as the commands defined in the split 
directories (a, b, c etc.) that Stata loves so well. This 
function is called by wrappers to set up Stata's named 
directories, so unless you have some extra directories, you 
don't need this function.

The arguments are
  name:   the internal name ado-mode uses for tracking the changes
          this must be be a symbol
  dir:    the directory to look in
  face:   the face to use (must be double ''ed)
  remove: if nil, then add the keywords, otherwise remove them
  subdir: subdirectory behavior (defaults to -all-)
            self: just look in dir
            sub:  look in the subdirectories only
            all:  look in dir and subdirectories
  extension: defaults to 'ado' (there is no real reason for anything else yet)

Here is an example which adds and then removes the directory foo, 
which has an internal name of bar.

  ;; adding the ado-files
  (ado-modify-font-lock-keywords 'bar /Users/jsmith/foo 
     ''ado-mode-personal-harmless-face)
  ;; updating the highlighting uses the same command
  (ado-modify-font-lock-keywords 'bar /Users/jsmith/foo 
     ''ado-mode-personal-harmless-face)
  ;; removing the highlighting
  (ado-modify-font-lock-keywords 'bar /Users/jsmith/foo 
     ''ado-mode-personal-harmless-face t)"
  (unless extension
	(setq extension "ado"))
  (unless subdir
	(setq subdir "all"))
  ;; first check if dir is a directory
  (unless (file-directory-p dir)
	(error (concat "directory " dir " does not exist")))

  ;; now check to see if -name- exists
  (let (newList (oldList (assoc name ado-added-names)))
	(if oldList
		(progn 
		  (font-lock-remove-keywords 'ado-mode (cdr oldList))
		  (setq ado-added-names (assq-delete-all name ado-added-names))))
	(unless remove
	  (setq newList `((,(concat ado-start-cmd-regexp (regexp-opt 
				  (mapcar (function (lambda (name) (substring-no-properties name nil -4))) 
						  (apply 'append
								 (mapcar (function (lambda (dirname) (directory-files dirname nil ".*[.]ado$")))
										 (ado-find-ado-dirs dir subdir)
										 ))) 
				  'words)
								ado-end-cmd-regexp) 1 ,face)))
	  (font-lock-add-keywords 'ado-mode newList)
	  (setq ado-added-names (append ado-added-names `(,(cons name newList))))
	  )
	))


(provide 'ado-font-lock)
