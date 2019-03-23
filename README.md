# Point History

Remember points you visited before as history, and re-visit them quickly.

## Whats' This

![gif]()

## Setup

`git clone` and edit your init.el as below.

```elisp
(add-to-list 'load-path "YOUR PATH")
(require 'point-history)

;; enable minor mode
(point-history-mode t)
```

## Usage

Use `M-x point-history-show` and you can open `point-history-show-buffer` with history.  
Then you can select the point in history and press `Enter` to jump to its position.

## Customizable variables

|variable|usage|default value|
|:---:|:---:|:---:|
|point-history-max-item-num|Max number of points saved in history|100|
|point-history-show-buffer-height|Buffer height to show point-history|30|
|point-history-save-timer|Interval time to save point in history|1|

## Tips

If you edit `point-history-ignore-buffer`, You can ignore points to save into history for specific buffers like minibuffer.

```elisp
(setq point-history-ignore-buffer "^ \\*Minibuf\\|^ \\*point-history-show*")
```
