# Digit Groups

The `digit-groups` Emacs package makes it easier to read large numbers by
highlighting digits at selected place-value positions (e.g., thousands place,
millions place, billions place, etc.).

For example, in the text `9876543210.123456789`, the default configuration
formats in bold the 3, 6, and 9 before the decimal (`.`) because they are in
the thousands, millions, and billions positions as well as the 3, 6, and 9
after the decimal (`.`) because they are in the thousandths, millionths, and
billionths positions.

To use this package, customize `digit-groups-mode-hooks` to be a list of mode
hooks for the modes in which you want highlighting and make sure
`font-lock-mode` is enabled for those modes.  For example, to enable
highlighting for all modes, either customize `digit-groups-mode-hooks` to be
`'(text-mode-hook prog-mode-hook special-mode-hook)` or add the following
clause to the `custom-set-variables` in your `.emacs`.

    (custom-set-variables
      ...
      '(digit-groups-mode-hooks
        (quote (text-mode-hook prog-mode-hook special-mode-hook)))
      ...)

If you want highlighting for just the current buffer, first, make sure
`font-lock-mode` is enabled for the current buffer, then call the
`digit-groups-enable` function.

The default configuration highlights digits by making them bold.  This can be
changed by customizing `digit-groups-default-face`, or you can highlight
different positions with different faces by customizing `digit-groups-groups`.

The default configuration highlights every third place-value position between
the novemdecillionths (10^-60) position and the novemdecillions (10^60)
position with the exception of the units (10^0) position.  That is to say, it
highlights the 10^i place-value position when i is a multiple of 3 between -60
and 60 (inclusive) but is not 0.  This highlights the thousands, millions,
billions, etc. positions as well as the thousandths, millionths, billionths,
etc. positions.  This can be changed by customizing `digit-groups-groups`.

Changes to the configuration take effect only when a mode hook in
`digit-groups-mode-hooks` is run.  Thus, you may need to reload any affected
buffers before you see the effect of any configuration changes.

## Source

The source code for this package is available at:
  <http://bitbucket.com/adamsmd/digit-groups>

## License

This package is licensed under the MIT license.  See the LICENSE file in the
source code.

## Installation

### Installation from MELPA

TODO: write once we can test installation from MELPA

### Manual Installation

- Copy `digit-groups.el` to your `load-path`.

- Load the package by either manually running `(require 'digit-groups)` or
  adding the following to your `.emacs`.  (If you don't use `after-init-hook`,
  you may get an error when `digit-groups` `require`s `dash`.)

        (add-hook 'after-init-hook (lambda () (require 'digit-groups)))

- Select the modes for which you want to highlight digit groups by customizing
  `digit-groups-mode-hooks`.  For example, to enable highlighting for all
  modes, either customize `digit-groups-mode-hooks` to be `'(text-mode-hook
  prog-mode-hook special-mode-hook)` or add the following clause to the
  `custom-set-variables` in your `.emacs`.

        (custom-set-variables
          ...
          '(digit-groups-mode-hooks
            (quote (text-mode-hook prog-mode-hook special-mode-hook)))
          ...)

- Make sure `font-lock-mode` is enabled for those modes.

## Limitations

- Due to how `font-lock` works, there is no good way to disable or remove the
  `font-lock` keywords added by this package.  Any changes or removals require
  reloading the affected buffers.

- Due to how `font-lock` works, this package can highlight only a fixed number
  of place-value positions.  Thus, the default configuration goes only between
  10^-60 and 10^60.

- Due to how regular expressions work on Emacs, this package doesn't support
  Unicode digits.  They are treated as non-digits.

- Due to how `font-lock` and regular expressions work in Emacs, this package
  may prevent highlighting that `font-lock` would normally do to the first
  non-digit character after a sequence of digits.

## Contributing

I am looking for someone to take over maintenance of this package.

I am not an expert on Emacs Lisp programming, so suggestions about better ways
to write or package the code are welcome.

I wrote this software to solve a particular problem that I had, and it now
solves that problem.  Unfortunately, I am fairly busy and don't have much time
for feature improvements or on-going maintenance.  This has a few
consequences.

- Suggestions, improvements, and fixes are welcome, but I may not get to them
  for quite some time.

- The less work I have to do to incorporate your improvement or fix, the more
  quickly and more likely I will include it.  The best case is a pull request
  that has a good comment explaining what it does and why it should be done
  and that has a simple enough diff that it is obviously correct.

- I would be happy to turn over maintenance of this package to someone
  qualified who is in a better position to do on-going maintenance than I.
