Youbrick
========

![Screenshots of Youbrick](/screenshots/illustration.png)

An application with a convenient Terminal User Interface (TUI) to subscribe, get notified about, and watch new content from Youtube channels without a google account. This relies on the RSS feeds associated to each channel. The TUI is powered by the [Haskell Brick library](https://hackage.haskell.org/package/brick).

## Features

- No need for a Google Account
- Concise keyboard-controlled terminal user interface
- Quick glance at new content:
    - Overview of the list of subscribed channels
    - Latest content by channel
    - Watchlist of latest unseen content
- Open in video viewer (e.g mpv or vlc) or in web browser
- Manually toggle watch status
- Locally subscribe to a channel by coping any URL linking to a webpage of that channel (including any video on the channel)

## Motivations

This is an experimental personal Haskell project aimed at answering personal needs while having fun discovering and tinkering with:

* Terminal User Interfaces: here a TUI built using the popular [Haskell Brick library](https://hackage.haskell.org/package/brick) (see `Tui.hs`, and `Tui/*`)
* Network access, fetching and parsing RSS feeds and html webpages (see `Network.hs`)
* Relational data modelling using a variant of the ixset library ([ixset-typed](https://hackage.haskell.org/package/ixset-typed) with type guarantees at compile time using the haskell type system instead of runtime information), and familiarising with the language extensions it relies on. Ixset and ixset-typed are libraries for handling multi-indexed sets of data (see `Data.hs`)
* Basic lenses usage using the microlens library on which Brick (the TUI library) already depends (see TUI modules manipulating states defined in `Tui/State.hs`)
* Files (see `Main.hs`)
* Simple exception handling (see `Network.hs`, `Tui/Job.hs`)
* Concurrency (see `Tui/Job.hs`)
* Multiple other concepts along the way that did not make it to the current state of the program: monad transformers (traditional and mtl style), advanced ways to model and handle exceptions in effectful sections of the program, etc.

This project does not aim, at first, to be a ready for release and long-term maintained free software. Some classic features that would be expected in such an end-user ready program are missing: configuration file, prettier refined error reporting (currently displayed as raw error messages in popups), search, filters, sorting, "open with" dialog boxes, advanced configuration such as keybindings, eye-candy.

## Quick user manual

### Configuration

The video player and the web browser used to "watch" and "browse" channels and videos are specified in the `Config` module (by default "mpv" and "firefox"). They should be configured before building Youbrick.

### Building

Build with `stack`.

### Usage

No arguments are expected when launching Youbrick. Keybindings are displayed on screen. First add a few channels ("middle-click" paste any video or channel page URL in the add channel dialog box). The database (subscribed channels and which videos have been watched) is saved (only when exiting the program normally) in a persistent file whose location is also specified in the `Config` module.

Refresh to fetch latest content about subscribed channels online. Refreshing is not automatic. Dismiss error messages (e.g. invalid URL when adding a channel or connection issue) by pressing Enter.

## Contributing

Discussions, bug report, suggestions for improvement, expert advice, and pull requests are welcome.

## Documentation

Haddock documentation is provided for almost all modules, data structures, and functions unless their name or type are really self-explanatory. Although the overall structure is clear enough, basic knowledge of what a typical Brick (the TUI library) program looks like is expected to dive comfortably into technical specificities of the TUI.

## Screenshots

![Example channel list screenshot](/screenshots/channel-list.png)

![Example add channel dialog box screenshot](/screenshots/add-channel.png)

![Example channel content screenshot](/screenshots/channel-content.png)

![Example watchlist screenshot](/screenshots/watchlist.png)
