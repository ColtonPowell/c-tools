# c-tools
c-tools is a collection of handy emacs things that may or may not make you a more productive programmer.

### Installation
1. Clone the repo:
`git clone https://github.com/ColtonPowell/ctools.git`

2. Add the following lines to your .emacs file:
```
(add-to-list 'load-path "/path/to/c-tools")
(require 'c-tools)
```

3. (Optional) Setup:
##### resize-window-mode
  - Default keys are:
    - `C-p` and `<up>` (up arrow) to enlarge your current window vertically.
    - `C-n` and `<down>` to shrink your current window vertically.
    - `C-b` and `<left>` to shrink your current window horizontally.
    - `C-f` and `<right>` to enlarge your current window horizontally.
    - `C-g` and `<escape>` exit resize-window-mode
    
  - To disable these keys, add the following to your .emacs file:
    `(setq rw-enable-default-keys nil)`

  - To add your own custom keys, add each key **as a string** to their respective list in your .emacs file. There are 5:
    - `rw-enlarge-vertically-keys`
    - `rw-shrink-vertically-keys`
    - `rw-shrink-horizontally-keys`
    - `rw-enlarge-horizontally-keys`
    - `rw-exit-keys`
    
  - Example: To have Alt-p enlarge your window vertically and have 5 shrink your window vertically, add these lines to your .emacs:
    - `(add-to-list 'rw-enlarge-vertically-keys "M-p")`
    - `(add-to-list 'rw-shrink-vertically-keys "5")`
    - etc.

  - To change the number of resize actions per keystroke, change `rw-multiplier` accordingly. Default value is 3. Recommended values are 1-5.

  - Example: To change it to 4, add `(setq rw-multiplier 4)` to your .emacs
    
    