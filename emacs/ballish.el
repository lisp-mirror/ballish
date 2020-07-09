;;; ballish.el --- An emacs package for ballish integration -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Florian Margaine <florian@margaine.com>.

;; Author: Florian Margaine <florian@margaine.com>
;; Version: 0.1
;; Keywords: ballish, indexing, search, grep
;; URL: https://gitlab.com/ralt/ballish

;;; Commentary:

;; This package provides integration with ballish itself, in order to
;; make the fast search available from Emacs itself.

;;; Code:

(defgroup ballish nil
  "Super-fast search at the tip of your fingers."
  :group 'tools)

(defcustom ballish-command "bl"
  "The path to the ballish client executable."
  :type 'string
  :group 'ballish)

;;;###autoload
(defun ballish-grep-in-repository (query)
  "Grep for a given query in the current repository."
  (interactive (list (read-from-minibuffer "Query: ")))
  (require 'grep)
  (compilation-start (concat ballish-command " --repository --grep --query " query) 'grep-mode))

;;;###autoload
(defun ballish-grep-everywhere (query)
  "Grep for a given query everywhere."
  (interactive (list (read-from-minibuffer "Query: ")))
  (require 'grep)
  (compilation-start (concat ballish-command " --grep --query " query) 'grep-mode))

;;;###autoload
(defun ballish-ivy-grep-in-repository ()
  "Grep for a given query in the current repository using ivy."
  (interactive)
  (require 'counsel)

  (ivy-read "query: " #'ballish--ivy-grep-in-repository-function
	    :initial-input ""
	    :dynamic-collection t
	    :keymap counsel-git-grep-map
	    :action #'counsel-git-grep-action
	    :history 'counsel-git-grep-history
	    :require-match t
	    :caller 'ballish-ivy-grep-in-repository))

(defun ballish--ivy-grep-in-repository-function (query)
  "Grep in the current Git repository for a given query."
  (or (ivy-more-chars)
      (progn
	(counsel--async-command
	 (concat ballish-command " --repository --grep --query " query))
	nil)))

;;;###autoload
(defun ballish-ivy-grep-everywhere ()
  "Grep for a given query everywhere using ivy."
  (interactive)
  (require 'counsel)

  (ivy-read "query: " #'ballish--ivy-grep-everywhere-function
	    :initial-input ""
	    :dynamic-collection t
	    :keymap counsel-git-grep-map
	    :action #'counsel-git-grep-action
	    :history 'counsel-git-grep-history
	    :require-match t
	    :caller 'ballish-ivy-grep-everywhere))

(defun ballish--ivy-grep-everywhere-function (query)
  "Grep everywhere for a given query."
  (or (ivy-more-chars)
      (progn
	(counsel--async-command
	 (concat ballish-command " --grep --query " query))
	nil)))

(provide 'ballish)

;;; ballish.el ends here
