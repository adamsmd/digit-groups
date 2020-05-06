# Digit Groups

The `digit-groups` Emacs package adds the `digit-groups-mode`
minor mode, which makes it easier to read large numbers by
highlighting digits at selected place-value positions (e.g.,
thousands place, millions place, billions place, etc.).

The default configuration formats in bold every other group of
three digits.  So, for example, in `9876543210.123456789`, the
digits 3, 4, 5 and 9 are highlighted both before and after the
decimal (`.`).  This makes it easy to find the place-value
positions for thousands, millions, billions, and so forth.

To use this package, enable the `digit-groups-mode` minor mode in
the buffers in which you wish to use it, or to enable it for all
buffers, customize `digit-groups-global-mode` to `t`.

The default configuration highlights digits by making them bold.
This can be changed by customizing `digit-groups-default-face`,
or you can highlight different positions with different faces by
customizing `digit-groups-groups`.

The default configuration highlights every other group of three
digits between the novemdecillionths (10^-60) position and the
novemdecillions (10^60) position with the exception of the
units (10^0) position.  This can be changed by customizing
`digit-groups-groups`.

Changes to the configuration take effect only when the
`digit-groups-mode` minor mode is being turned on.  Thus, you may
need to toggle the mode off and on again in affected buffers
before you see the effect of any configuration changes.

## Source

The source code for this package is available at:
  <https://github.com/adamsmd/digit-groups/>

## License

This package is licensed under the MIT license.  See the LICENSE file in the
source code.

## Installation

- Install the package files

    - Option 1 (preferred): Install from MELPA

        - Enable installation of packages from MELPA
          (see <http://melpa.org/#/getting-started>).

        - Launch Emacs's package manager from the menu with `Options -> Manage
          Emacs Packages` or manually with `M-x package-list-packages`.

        - Select the `digit-groups` package for installation.

    - Option 2: Install manually

        - Download the source code with:
          `hg clone ssh://hg@bitbucket.org/adamsmd/digit-groups`

        - Copy `digit-groups.el` into a directory in your `load-path`.

        - Load the package by either running `(require 'digit-groups)`
          manually or adding the following to your `.emacs`.  (If you don't
          use `after-init-hook`, you may get an error when `digit-groups`
          `require`s `dash`.)

                (add-hook 'after-init-hook (lambda () (require 'digit-groups)))

- Enable the `digit-groups-mode` minor mode in the buffers in which you wish
  to use it, or to enable it for all buffers, customize
  `digit-groups-global-mode` to `t`.

## Limitations

- Since the regular expression for matching the selected place-value positions
  is computed only when turning on the minor mode, you need to toggle the mode
  off and on again in affected buffers before you see the effect of
  configuration changes to `digit-groups-groups`,
  `digit-groups-decimal-separator`, or `digit-groups-digits`.

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
