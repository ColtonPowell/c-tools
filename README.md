# c-tools
c-tools is a collection of handy emacs things that may or may not make you a more productive programmer.

## Installation
1. Clone the repo:
`git clone https://github.com/ColtonPowell/c-tools.git`

2. Add the following lines to your .emacs file (and change /path/to/c-tools accordingly):
```
(add-to-list 'load-path "/path/to/c-tools")
(require 'c-tools)
```

Useful info:

#### nav-tools
A thing that helps you get around a little easier.

###### Info:
- `next-block-soe(&optional n)` and `previous-block-soe(&optional n)` move to the start or end of the next/previous block of text.

- (Also feel free to use `backward-delete-word` if you're tired of backward-kill-word cluttering your clipboard.)

#### window-manage-mode
A mode to encompass all of your emacs window management (moving, resizing, swapping, deleting, etc.) needs!

###### Info:
- To enable extra functionality, install `ace-window` (version >= 0.9.0) and set `wmm-require-ace-window` to `t`.

- Change `wmm-multiplier` to modify the power of `window-manage-mode`s resizing functions.

- `(describe-function 'window-manage-mode)` after installation and setup to see keybinds.

#### font-interface
A quicker way to test and change your font.

###### Info:
- `increment-font-size` and `decrement-font-size` inc/decrement the height of the font by `font-margin`, which is equal to 10 by default.

- You can change `font-margin` and the face you're editing (`face-to-edit`) by quickly using the `set-font-margin` and `set-face-to-edit` functions (respectively), or set them in your .emacs file

- Change the font family quickly by using `set-font-family`
    
    
