(uiop:define-package :ballish/site/manual
    (:use :cl :ballish/site/shared)
  (:import-from #:hunchentoot :define-easy-handler))

(in-package :ballish/site/manual)

(define-easy-handler (manual :uri "/manual.html") ()
  (with-page (:title "Manual")
    (:p "Welcome to ballish documentation! You're going to learn how to search across all of the code living on your machine in a few milliseconds, with some minimal setup. This documentation starts with a mini-tutorial to explain how to use the basic features, then drills down to more serious use cases, and ends with explaining the internals.")

    (:h2 "Let's get started!")

    (:p "Assuming you got this to page from the " (:a :href "/download.html" "download page") ", you should have done the first step: installing the ballish daemon. It will do the magic for the search to work. Let's have a quick reminder in case you haven't done it:")
    (:pre "$ sudo systemctl enable --now ballish-daemon@\"$USER\"")
    (:p "That should be it. Let's verify it works:")
    (:pre "$ bl --status
server status: up
index size on disk: 0M
in-flight files to index: 0
indexed folders:
")
    (:p "Looks like so. \"indexed folders\" is empty though, let's make sure we start indexing something.")
    (:pre "$ bl --folder ~/dev")
    (:p "Or whatever your folder is where you store all your source code. If you're interested in the C things, you could also add this folder:")
    (:pre "$ bl -f /usr/include")
    (:p "Now let's see if the daemon is busy:")
    (:pre "$ bl -s
server status: up
index size on disk: 54.32M
in-flight files to index: more than 1000
indexed folders:
  - /home/user/dev/
  - /usr/include/")
    (:p "That sure looks better. And busy. Time to do our first search!")
    (:pre "$ bl --query glob64_t
/usr/include/glob.h")
    (:p "Nice. Let's see what's in there?")
    (:pre "$ bl -q glob64_t --grep # or -g
/usr/include/glob.h:134:  } glob64_t;
/usr/include/glob.h:164:		   glob64_t *__restrict __pglob) __THROW;
/usr/include/glob.h:166:extern void globfree64 (glob64_t *__pglob) __THROW;")
    (:blockquote :class "small" "Note: if you need to search with a whitespace, use \"+\" instead.")
    (:p "There we go! That sums it up for the minimal setup of searching with ballish.")

    (:h2 "Drilling down")

    (:ul
     (:li (:a :href "#tags" "Tags"))
     (:li (:a :href "#folder" "Folder search"))
     (:li (:a :href "#grep" "Grep"))
     (:li
      (:a :href "#editors" "Editors integrations")
      (:ul
       (:li (:a :href "#emacs" "Emacs"))
       (:li (:a :href "#vim" "Vim"))))
     (:li (:a :href "#internals" "Internals"))
     (:li (:a :href "#exit" "Exit codes")))

    (:h3 :id "tags" "Tags")

    (:p "Ballish will helpfully tag all the indexed files. Every file is tagged with the type of programming language it belongs to. For example, *.rs files get the \"rust\" tag, *.js files get the \"javascript\" tag, etc. The tags have only one purpose: helping you narrow down the results you're getting.")
    (:p "Here are some usage examples:")
    (:pre "$ bl --tags C --count # or bl -t c -c
79635")
    (:p "That's how many C files have been indexed. How many of those files have \"struct\" in them?")
    (:pre "$ bl -q struct -t c -c
60498")
    (:p "Well, that's less than I would've expected.")
    (:p "The full list of tags/file extension is defined " (:a :href "https://gitlab.com/ralt/ballish/-/blob/master/daemon/source-indexing-text-extensions.yaml" "here") ".")

    (:h3 :id "folder" "Folder search")

    (:p "Searching across everywhere is fun, but sometimes you want to search in just a single folder. Ballish offers several options:")

    (:ul
     (:li (:code "--location FOLDER, -l FOLDER") ": search in a specific folder")
     (:li (:code "--repository, -r") ": search in the current git repository"))

    (:p "Note that those options have no influence on the performance, they only help to narrow down the results you're getting. You can of course combine those with tags and count.")

    (:h3 :id "grep" "Grep")

    (:p "This was mentioned in the kickstart at the beginning, but you can grep for what you're querying, so that you can see the found values, rather than only the files found.")

    (:blockquote :class "small" "Note: if you get an error saying " (:code "fatal: too many results to grep") ", it's because the results were found in more than 100 files, which makes grep uncomfortable to use. If you decide to go above, you can set the limit in the " (:code "BL_MAX_GREP_RESULTS") " environment variable.")

    (:p "The primary goal of this option is to make ballish much more usable from within editors, such as Vim or Emacs. Which leads me to...")

    (:h3 :id "editors" "Editors integrations")

    (:p "Ballish comes installed with a couple of integrations for your favorite editors, namely: Vim and Emacs.")

    (:h4 :id "#emacs" "Emacs")

    (:p "After ballish is installed, you should be able to add this to your " (:code "init.el") " or equivalent:")
    (:pre "\(require 'ballish\)
\(global-set-key (kbd \"C-c j\"\) 'ballish-grep-in-repository\)
\(global-set-key (kbd \"C-c k\"\) 'ballish-grep-everywhere\)")
    (:p "And you should be able to use " (:code "C-c j") " to start searching in the current git repository you're in, or " (:code "C-c k") " to search everywhere.")
    (:blockquote :class "small" "Reminder: use \"+\" instead of spaces.")
    (:p "The Emacs package provided by ballish provides 4 functions at the moment:")
    (:ul
     (:li (:code "ballish-grep-in-repository") ": grep in the current repository, uses grep-mode")
     (:li (:code "ballish-ivy-grep-in-repository") ": same, with ivy integration. Can replace " (:code "counsel-git-grep") " with similar performance.")
     (:li (:code "ballish-grep-everywhere") ": grep everywhere, uses grep-mode")
     (:li (:code "ballish-ivy-grep-everywhere") ": same, with ivy integration."))

    (:h4 :id "#vim" "Vim")

    (:p "After ballish is installed, a Vim plugin is provided, and you can use these commands inside Vim:")
    (:pre ":BallishGrepInRepository <your search query>
:BallishGrepEverywhere <your search query>")
    (:blockquote :class "small" "Reminder: use \"+\" instead of spaces.")
    (:p "The Vim plugin opens the quickfix window by default, which you can disable by adding this to your " (:code ".vimrc") " or equivalent:")
    (:pre "let g:ballish_open_quickfix = 0")
    (:p "In the quickfix window, the lines are truncated to a given size to avoid polluting too much the window. The default value is 500 characters, but can be changed as such:")
    (:pre "let g:ballish_max_grep_line_length = 1000")

    (:h3 :id "internals" "Internals")

    (:p "This section explains a bit how ballish works, which should explain why some of the arguments exist.")
    (:p "Ballish essentially relies on 3 ideas:")

    (:ul
     (:li "It is watching for file changes on all the indexed folders to re-index whenever a change occurs.")
     (:li "It is using SQLite's " (:a :href "https://www.sqlite.org/fts5.html" "full-text search") " to index all the code.")
     (:li "The client is then searching directly in the SQLite database."))

    (:p "Given this, some limitations apply:")

    (:ul
     (:li "SQLite is trying its best to optimize the database file, but sometimes needs a nudge. The " (:code "--optimize") " argument is doing that. It can potentially make the queries faster. Maybe throw that in a daily cron.")
     (:li "It is technically complex to remove indexed files once you decide to stop indexing a folder. Which is why the " (:code "--delete FOLDER") " option only stops watching for changes in the files, and does not clear anything in the index. To clear the database, you need to use " (:code "--purge") ". This means that re-indexing has to happen after that, a necessary evil."))

    (:p "The SQLite database(s) live in the " (:code "$XDG_CACHE_HOME/ballish/") " folder, which is typically " (:code "~/.cache/ballish/") ".")

    (:h3 :id "exit" "Exit codes")

    (:p "The " (:code "bl") " client will exit with a set of known exit codes when a given set of operations occur.")

    (:ol
     :start "0"
     (:li :value "0" "The search returned results without any errors.")
     (:li :value "1" "An unqualified error occured. Unqualified means it was handled, but not important enough to have its own exit code. This is typically used for bad arguments or sloppy coding.")
     (:li :value "2" "The program was interrupted by the user.")
     (:li :value "3" "The ballish daemon is not started.")
     (:li :value "4" "The database is too busy, the user should try again later.")
     (:li :value "5" "An unhandled sqlite error occured.")
     (:li :value "6" "The --repository argument was provided outside of a Git repository.")
     (:li :value "7" "The --repository or --location arguments were provided without a query.")
     (:li :value "8" "The index was not found.")
     (:li :value "9" "Too many results were found to be able to use --grep.")
     (:li :value "255" "An unhandled error occured."))))
