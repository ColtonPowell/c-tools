# c-tools
c-tools is a collection of handy emacs things that may or may not make you a more productive programmer.

### Installation
1. Clone the repo:
`git clone https://github.com/ColtonPowell/c-tools.git`

2. Add the following lines to your .emacs file:
```
(add-to-list 'load-path "/path/to/c-tools")
(require 'c-tools)
```

Useful info:

##### nav-tools
A thing that helps you get around a little easier.

- `next-block-soe(&optional n)` and `previous-block-soe(&optional n)` move to the start or end (soe; the first or last line) of the next/previous block of text.

- (Also feel free to use `backward-delete-word` if you're tired of backward-kill-word cluttering your clipboard.)

##### resize-window-mode
A minor mode for resizing the window.

  - Use `(rwm-toggle-default-keys)` ONCE to enable default keys (and again to disable them), shown below:
    - `C-p` and `<up>` (up arrow) to enlarge your current window vertically.
    - `C-n` and `<down>` to shrink your current window vertically.
    - `C-f` and `<right>` to enlarge your current window horizontally.
    - `C-b` and `<left>` to shrink your current window horizontally.
    - `C-g` and `<escape>` exit resize-window-mode

  - Use the `define-key` function to customize keybinds
  - Change `rwm-margin` to change the number of resizes per keystroke. Default value is 3, recommended is 1-5.

##### font-interface
A quicker way to test and change your font.

- `increment-font-size` and `decrement-font-size` inc/decrement the height of the font by `font-margin`, which is equal to 10 by default.

- You can change `font-margin` and the face you're editing (`face-to-edit`) by quickly using the `set-font-margin` and `set-face-to-edit` functions (respectively), or set them in your .emacs file 

- Change the font family quickly by using `set-font-family`
    
    