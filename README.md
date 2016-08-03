# Digit Groups

This Emacs package makes it easier to read large numbers by highlighting
digits at selected place-value positions (e.g., thousand, million, billion, etc.).

For example, in the text `9876543210.123456789`, the default
configuration would make bold the 3, 6, and 9 before the
decimal (`.`) because they are in the thousand's, million's, and
billion's positions as well as the 3, 6, and 9 after the
decimal (`.`) because they are in the thousandth's, millionth's,
and billionth's positions.

In order to work, requires that font-lock-mode be enabled.  To
enable `digit-groups`, customize `digit-groups-mode-hooks` to
include the mode hooks for the modes on which you want it
enabled.  To enable for all modes, customize
`digit-groups-mode-hooks` to `'(text-mode-hook prog-mode-hook
special-mode-hook).` To enable for just the current buffer call
`digit-groups-enable`.

The default configuration highlights every place-value position
for 10^i when i is a multiple of 3 between 3 and 60 or -3 and
-60 (inclusive).  This can be changed by customizing
`digit-groups-groups`.

Changes to the configuration may require you to reload any
affected buffers.

## License

This code is licensed under the MIT license.  See the LICENSE file.

## Limitations

 - Due to how `font-lock` works, there is no good way to disable or remove the
   `font-lock` keywords added by this package.

 - Due to how `font-lock` works, can only highlight a fixed number of
   place-value positions.

 - Due to how regular expressions work on Emacs, doesn't support Unicode digits.

## Contributing

I am looking for someone to take over maintenance of this package.

I wrote this software to solve a particular problem that I had, which it now
does.  Unfortunately, I am fairly busy and don't have much time for on-going
maintenance.  This has a few consequences.

 1. Suggestions and improvements are welcome, but I may not get to them for
    quite some time.

 2. The less work I have to do to include your improvement, the more quickly
    and more likely I will include it.  The best case is a pull request that
    has a good comment explaining what it does and why and has a simple enough
    diff that it is obviously correct.

 3. I would love to find someone who can take over maintenance of this package.
