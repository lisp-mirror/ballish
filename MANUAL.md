% BALLISH(1) Ballish User Manual
% Florian Margaine <florian@margaine.com>
% April 30, 2020

# NAME

ballish, bl - a pretty fast code search tool

# SYNOPSIS

bl [*OPTIONS*]...

# DESCRIPTION

Are you like this?

- Regularly working with codebases in various languages?
- Often wanting to navigate around those?
- Like having big codebases locally to load them in your editor?
- Tired of having to configure lsp/ctags/whatever have you for every
  language?
- Would love using grep but it's just too slow to search across
  thousands of files?

Welcome! ballish may fit in your workflow.

ballish can search across thousands and thousands of files in a few
milliseconds. How? It will index your source code, live, whenever it
changes. Then searching in the index is instant.

Indexing all standard libraries, complete virtual environments,
node_modules folders, and all of those is encouraged.

# OPTIONS

`--query` *QUERY*, `-q` *QUERY*
:   The query to search for.

`--tags` *TAGS*, `-t` *TAGS*
:   List of tags to search for, comma-separated.

`--folder` *FOLDER*, `-f` *FOLDER*
:   Add a folder to the index.

`--count`, `-c`
:   Print the number of results. Can accept `--query` and `--tags` as well.

`--grep`, `-g`
:   Run grep on every result. See GREP.

`--repository`, `-r`
:   Run the search in the current folder's repository.

`--localized` *FOLDER*, `-l` *FOLDER*
:   Run the search in the specified folder.

`--optimize`, `-o`
:   Optimize the index storage.

`--debug`, `-d`
:   Run in debug mode.

`--status`, `-s`
:   Print the indexing status.

`--version`, `-v`
:   Print the version.

Note: `--repository` and `--localized` do not make the search
faster. They only exist to help narrow down the list of results.

# GREP

By default, ballish results will be the files in which the results
have been found. You might prefer a better output with `<file>:<line
number>:<line>`, which is what the `--grep` option gives you.

Note that the option will error if more than 100 results are
found. The primary use case for this option is integration with
editors, where more than a handful of results leads to a bad UI. You
can override the limit by setting the `BL_MAX_GREP_RESULTS`
environment variable.

# EXAMPLES

Note: all those results return in less than 100ms.

Add a folder to the index:

```
$ bl -f /usr/include
```

Search for a struct in the index:

```
$ bl -q glob64_t
/usr/include/glob.h
$
```

Count the number of `struct` in the index:

```
$ bl -q struct -c
21584
$
```

Count the number of indexed python files:

```
$ bl -t python -c
20161
$
```

Grep for the results of `glob64_t`:

```
$ bl -q glob64_t -g
/usr/include/glob.h:134:  } glob64_t;
/usr/include/glob.h:164:                   glob64_t *__restrict __pglob) __THROW;
/usr/include/glob.h:166:extern void globfree64 (glob64_t *__pglob) __THROW;
$
```

# LICENSE

GPLv2

# SEE ALSO

The ballish source code and all documentation may be downloaded from
<https://gitlab.com/ralt/ballish>.
