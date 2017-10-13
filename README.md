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

- The main functions are `next-section(&optional n)` and `previous-section(&optional n)`. They move to the next or previous section n times.
  - A section is the line at the beginning or end of a block of text:
  
  ```
  This is a section
  This is NOT a section
  This is NOT a section
  This is NOT a section
  This is a section

  This is a section

  This is a section
  This is a section		
  ```
  - This tool allows you to navigate more quickly by traversing entire
blocks of text with a single key.

##### resize-window-mode
A minor mode for resizing the window.

  - Default keys are:
    - `C-p` and `<up>` (up arrow) to enlarge your current window vertically.
    - `C-n` and `<down>` to shrink your current window vertically.
    - `C-b` and `<left>` to shrink your current window horizontally.
    - `C-f` and `<right>` to enlarge your current window horizontally.
    - `C-g` and `<escape>` exit resize-window-mode
    
  - To disable these keys, add the following to your .emacs file:
    `(setq rwm-enable-default-keys nil)`

  - To add your own custom keys, add each key **as a string** to their respective list in your .emacs file. There are 5:
    - `rwm-enlarge-window-vertically-keys`
    - `rwm-shrink-window-vertically-keys`
    - `rwm-shrink-window-horizontally-keys`
    - `rwm-enlarge-window-horizontally-keys`
    - `rwm-exit-keys`
    
  - Example: To have Alt-p enlarge your window vertically and have 5 shrink your window vertically, add these lines to your .emacs:
    - `(add-to-list 'rwm-enlarge-window-vertically-keys "M-p")`
    - `(add-to-list 'rwm-shrink-window-vertically-keys "5")`
    - etc.

  - To change the number of resize actions per keystroke, change `rwm-margin` accordingly. Default value is 3. Recommended values are 1-5.

  - Example: To change it to 4, add `(setq rwm-margin 4)` to your .emacs or eval the expression.

##### font-interface
A quicker way to test and change your font.

- `increment-font-size` and `decrement-font-size` inc/decrement the height of the font by `font-margin`, which is equal to 10 by default.

- You can change `font-margin` and the face you're editing (`face-to-edit`) by quickly using the `set-font-margin` and `set-face-to-edit` functions (respectively), or set them in your .emacs file 

- Change the font family quickly by using `set-font-family`
    
    