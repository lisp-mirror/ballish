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

## Usage

See the [manual][0].

## Installation

You can find the latest .rpm (Red Hat, CentOS, Fedora), .deb (Ubuntu,
Debian) or .pkg.tar.xz (Arch) on the [releases page][1].

If you want to build ballish yourself, you will need:

- [sbcl][2]
- [QuickLisp][3]
- [pandoc][4]

Clone [wild-package-inferred-system][5] in your
`~/quicklisp/local-projects/`, and then you can run `make`. The
`ballish-daemon` and `bl` binaries will appear in your folder.

You will need to run `ballish-daemon` as a daemon (likely as a systemd
service, see the sample [ballish-daemon.service][6] file) before
running any `bl` command.

## License

[GPLv2][7].

## Roadmap

- Add search on Git commits.
- Write an emacs package/vim plugin.


  [0]: MANUAL.md
  [1]: https://gitlab.com/ralt/ballish/-/releases
  [2]: http://sbcl.org
  [3]: https://www.quicklisp.org/beta/
  [4]: https://pandoc.org/
  [5]: https://github.com/privet-kitty/wild-package-inferred-system
  [6]: ballish-daemon.service
  [7]: LICENSE
