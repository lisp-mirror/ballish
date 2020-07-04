# ballish

_A pretty fast code search tool_

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

## Installation

You can find the latest release on the [releases page][1]. Ballish
currently supports:

- Debian Stretch (10, currently oldstable)
- Debian Buster (11, currently stable)
- Debian Bullseye (12, currently testing/sid)
- Ubuntu 18.04 (LTS)
- Ubuntu 19.10 (non-LTS, currently before-latest)
- Ubuntu 20.04 (LTS, currently latest)
- Fedora 31 (currently before-latest, still supported)
- Fedora 32 (currently latest)
- Arch Linux

Once installed, you should start the systemd service:

```bash
sudo systemctl enable --now ballish-daemon@"$USER"
```

(Or replace `$USER` with your username.)

## Usage

See the [manual][0] for the generic instructions. Alternatively, use
`man ballish` once you have installed the distribution package.

### Emacs package

Ballish provides an Emacs package for a neat integration inside your
favorite OS.

Once the distribution package is installed, you can add this snippet
to your init.el or equivalent:

```elisp
(require 'ballish)
(global-set-key (kbd "...") 'ballish-grep-in-repository)
```

The emacs packages offers 2 functions:

- `ballish-grep-in-repository`: run ballish in your current repository
  and see the result in a grep-mode buffer.
- `ballish-ivy-grep-in-repository`: similar with ivy integration. Can
  replace `counsel-git-grep` with similar performance.

### Vim plugin

Ballish provides a vim plugin for a neat integration inside your
favorite editor.

Once the distribution package is installed, you can use this new
command:

```vim
:BallishGrepInRepository <your search query>
```

The vim plugin opens the quickfix window by default, which you can
prevent by adding this to your .vimrc or equivalent:

```vim
let g:ballish_open_quickfix = 0
```

## Hacking

If you want to build ballish yourself, you will need:

- [sbcl][2]
- [QuickLisp][3]
- [pandoc][4]

For sbcl:

- You need to compile sbcl manually with `sh make.sh --fancy
  --with-sb-linkable-runtime --with-sb-dynamic-core`.
- You need to skip version 2.0.4. Before and after are fine.

You also need to clone those in your `~/quicklisp/local-projects/`:

- [wild-package-inferred-system][5]
- [cffi][6] (unreleased fixes are needed)
- [cl-inotify][7] (fork to avoid a bad dependency)

At this point, you can run `make`, and the `ballish-daemon` and `bl`
binaries will appear in your folder.

You will need to run `ballish-daemon` as a daemon (likely as a systemd
service, see the sample [ballish-daemon.service][8] file) before
running any `bl` command.

## License

[GPLv2][9].

## Roadmap

- Potentially add search on Git commits.


  [0]: MANUAL.md
  [1]: https://gitlab.com/ralt/ballish/-/releases
  [2]: http://sbcl.org
  [3]: https://www.quicklisp.org/beta/
  [4]: https://pandoc.org/
  [5]: https://github.com/privet-kitty/wild-package-inferred-system
  [6]: https://github.com/cffi/cffi
  [7]: https://github.com/ralt/cl-inotify
  [8]: ballish-daemon.service
  [9]: LICENSE
