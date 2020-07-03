(uiop:define-package :ballish/site/home
    (:use :cl :ballish/site/shared)
  (:import-from #:hunchentoot :define-easy-handler))

(in-package :ballish/site/home)

(define-easy-handler (home :uri "/") ()
  (with-page (:title "Home")
    (:article
     (:p "Once upon a time, you downloaded a copy of the Linux kernel source code.  A few moons later, you decided to look for some debugging facilities in the ext4 filesystem.  Because why not?")
     (:pre
      "[dev@tag ~]$ pwd
/home/dev
[dev@tag ~]$ time bl -q define+ext4_debug -g
/home/dev/kernel/fs/ext4/ext4.h:73:#define ext4_debug(f, a...)                                                \
/home/dev/kernel/fs/ext4/ext4.h:80:#define ext4_debug(fmt, ...)       no_printk(fmt, ##__VA_ARGS__)

real    0m0.083s
user    0m0.053s
sys     0m0.030s
")
     (:p "Interesting.  How often is that used?")
     (:pre "[dev@tag ~]$ time bl -q ext4_debug -g -c
11

real    0m0.056s
user    0m0.034s
sys     0m0.021s
")
     (:p "Not much.  Oh well.")
     (:p "But wait!  This was way too fast.  Feels a bit cheating.  How much data is this 'bl' actually looking into?")
     (:pre "[dev@tag ~]$ bl --status
server status: up
index size on disk: 2355.89M
in-flight files to index: 0
indexed folders:
  - /home/dev/
  - /usr/include/
")
     (:p "Oh, that's not bad.")
     (:p "But, you know, you're rather an Emacs lover.")
     (:img :src "/emacs-ballish.png" :alt "Ballish integration in Emacs")
     (:p "What about vim?")
     (:img :src "/vim-ballish.png" :alt "Ballish integration in Vim")
     (:hr)
     (:p "Those are the capabitilies that ballish gives you: a very fast search across all of the code on your machine.  All at the tips of your fingers.  Integrated in your favorite editors.")
     (:p "Ballish supports dozens of languages, even those that don't exist.  No size limits.  No new dependencies to add to make lsp work for your new language.  No fiddling with ctags options to make it \"mostly work\".  Instant search results.")
     (:p "Go and " (:a :href "https://gitlab.com/ralt/ballish/-/releases/" "get it for your favorite distribution!")))))
