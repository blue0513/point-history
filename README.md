# Point History

Remember the points you visited before as history, and re-visit them quickly.

This package is greatly inspired from [point-undo.el](https://www.emacswiki.org/emacs/point-undo.el) and [jump-back!](https://qiita.com/zk_phi/items/c145b7bd8077b8a0f537).

## Whats' This

This package can remember the cursor-position as history, which you stayed before.  
When you want to back to the position, you can select the position from the history.

[![point-history](https://i.gyazo.com/68ef30d465487356e02b30790c49a8d4.gif)](https://gyazo.com/68ef30d465487356e02b30790c49a8d4)

## Features

+ Remember _point-info_ as history which you stayed over `point-history-save-timer`
+ _point-info_ contains buffer-name, cursor-position, line-content
+ Access the list of point-info's history and select them to visit the position again
+ Preview the buffer quickly in the other window

## Setup

### Requrements

This package use [`popwin-el`](https://github.com/m2ym/popwin-el).  
You need to install it beforehand.

### Settings

`git clone` and edit your init.el as below.

```elisp
(add-to-list 'load-path "YOUR PATH")
(require 'point-history)

;; enable minor mode
(point-history-mode t)

;; optional
(global-set-key (kbd "YOUR KEY") 'point-history-show)
```

## Usage

Use `M-x point-history-show` and you can open `point-history-show-buffer` with history.  
Then you can select the point in history and press `Enter` to jump to its position.

When you want to change the keymap in `point-history-show-buffer`, edit init.el as below.

```elisp
(define-key point-history-show-mode-map (kbd "n") 'point-history-next-line)
(define-key point-history-show-mode-map (kbd "p") 'point-history-prev-line)
```

## Customizable variables

|variable|usage|default value|
|:---:|:---:|:---:|
|point-history-max-item-num|Max number of points saved in history|100|
|point-history-show-buffer-height|Buffer height to show point-history|30|
|point-history-save-timer|Interval time to save point in history|1|
|point-history-should-preview|show the preview of buffers|t|

## Tips

If you edit `point-history-ignore-buffer` and `point-history-ignore-major-mode`, You can ignore points to save into history for specific buffers and major-modes.

```elisp
(setq point-history-ignore-buffer "^ \\*Minibuf\\|^ \\*point-history-show*")
(setq point-history-ignore-major-mode '(emacs-lisp-mode ruby-mode))
```

## Extensions

+ [ivy-point-history](https://github.com/SuzumiyaAoba/ivy-point-history): point-history with ivy interface 
