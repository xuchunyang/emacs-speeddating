# `speeddating.el` [![MELPA badge][melpa-badge]][melpa-link] [![Travis CI Build Status][travis-badge]][travis-link]

  [melpa-link]: https://melpa.org/#/speeddating
  [melpa-badge]: https://melpa.org/packages/speeddating-badge.svg
  [travis-link]: https://travis-ci.org/xuchunyang/emacs-speeddating
  [travis-badge]: https://travis-ci.org/xuchunyang/emacs-speeddating.svg?branch=master

Increase date and time at point.

For example, saying point is on `31`, `M-x speeddating-increase` will change

    Fri, 31 Dec 1999 15:00:00 +0800

into

    Sat, 01 Jan 2000 15:00:00 +0800

## Requirement

Emacs 25.1 or newer

## Date and time formats

`speeddating.el` supports date and time formats in the user option
`speeddating-formats`, the formats use a subset syntax of
[`format-time-string`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html).
Currently, the following formats are supported out of box.

<!-- (dolist (s speeddating-formats) (insert (format "| `%s` | %s |\n" s (format-time-string s)))) -->
| Format                     | Example                         |
|----------------------------|---------------------------------|
| `%a, %d %b %Y %H:%M:%S %z` | Sun, 18 Mar 2018 20:37:23 +0800 |
| `%a %b %d %H:%M:%S %Y %z`  | Sun Mar 18 20:37:23 2018 +0800  |
| `%Y-%m-%dT%H:%M:%S%:z`     | 2018-03-18T20:37:23+08:00       |
| `%a %b %_d %H:%M:%S %Z %Y` | Sun Mar 18 20:37:23 CST 2018    |
| `%Y-%m-%d %H:%M:%S`        | 2018-03-18 20:37:23             |
| `%Y-%m-%d %H:%M`           | 2018-03-18 20:37                |
| `%A, %B %d, %Y`            | Sunday, March 18, 2018          |
| `%d %B %Y`                 | 18 March 2018                   |
| `%d %b %Y`                 | 18 Mar 2018                     |
| `%B %-d, %Y`               | March 18, 2018                  |
| `%Y-%m-%d`                 | 2018-03-18                      |
| `%Y/%m/%d`                 | 2018/03/18                      |
| `%H:%M:%S`                 | 20:37:23                        |
| `%A`                       | Sunday                          |
