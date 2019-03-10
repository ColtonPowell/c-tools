# c-tools
A bunch of emacs functions that make your life a little easier.

Tools include:
- A window management mode (w/ optional integration with ace-window)
- Navigation tools to help you get around easier
- Quick-and-easy font modifications (They've def saved me on a presentation or 2)

## Installation
1. Clone the repo:
`git clone https://github.com/ColtonPowell/c-tools.git`

2. Add the following lines to your .emacs file (and change `/path/to/c-tools` accordingly):
```
(add-to-list 'load-path "/path/to/c-tools")
(require 'c-tools)
```

3. Install `ace-window` version >= 0.9.0 (https://github.com/abo-abo/ace-window).    
