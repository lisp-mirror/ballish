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

N/A yet.

## Installation

N/A yet.

## License

N/A yet. Likely going to be GPLv3. I'd rather go for GPLv2 but I need
to look at the dependency graph.

## Roadmap

- Add search on Git commits.
- Offer some systemd service for the daemon + instructions on
  `inotify.max_user_files`.
- Write an emacs package/vim plugin.
- Provide .deb/.rpm/.pkgbuild packages.
- Use a logging library and log many things.
- Write a man page.
- Don't watch EPERM/read-only folders.
- Add :move-from/move-to.
- Complete list of ignored folders.
