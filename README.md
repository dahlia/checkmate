Checkmate
=========

[![Build Status][ci-badge]][ci]
[![Hackage][hackage-badge]][hackage]
[![GitHub release][release-badge]][latest release]

Checkmate is a small program to generate human-readable checklists from
a given patch (or pull request).  Leave `CHECK` comments (that are the same
fashion as `FIXME` or `TODO` comments); then Checkmate detects when a diff
touches some code scopes having any `CHECK` comments, and lists the checks.

It helps contributors and reviewers to remind matters that require attention
when a part of the code is changed.

[ci-badge]: https://travis-ci.org/spoqa/checkmate.svg?branch=master
[ci]: https://travis-ci.org/spoqa/checkmate
[hackage-badge]: https://img.shields.io/hackage/v/checkmate.svg
[hackage]: https://hackage.haskell.org/package/checkmate
[release-badge]: https://img.shields.io/github/release/spoqa/checkmate.svg?label=download&colorB=4c1


Situation
---------

Let's say there's a dictionary, and we should update the manual when a key
is added to or removed from it:

~~~~~~~~ python
TARGET_LANGUAGES = {
    # CHECK: Please update the manual on the project wiki when you add/remove
    # a language.

    'c': '*.c',
    'java': '*.java',
    'javascript': '*.js',
    'python': '*.py',
}
~~~~~~~~

The above example may be artificial, but suppose lines of the dictionary are
lengthy.  Such tasks should be done outside of the source code repository
so that they cannot be automated by simply eliminating code duplicates.
Contributors and reviewers altogether are easy to forget about such tasks.

To remind peers of such tasks, Checkmate detects `CHECK` comments like the above
example when a relevant *code block* is touched and show peers a checklist.


Listing relevant checks: overlapped blocks
------------------------------------------

How does Checkmate list only relevant checks to a diff?  It currently doesn't
have any language-specific algorithms, but only a language-agnostic heuristics
on indented blocks.

Suppose the following diff:

~~~~~~~~ diff
diff --git a/langs.py b/langs.py
--- a/langs.py
+++ b/langs.py
@@ -5,6 +5,7 @@ TARGET_LANGUAGES = {
     'c': '*.c',
     'java': '*.java',
     'javascript': '*.js',
+    'haskell': '*.hs',
     'python': '*.py',
 }
 
~~~~~~~~

Since it touched a code block with a `CHECK` comment, Checkmate generates
the following checklist:

> - Please update the manual on the project wiki when you add/remove a language.

Suppose a patch touches only code blocks without any `CHECK` comments too, e.g.:

~~~~~~~~ diff
diff --git a/langs.py b/langs.py
--- a/langs.py
+++ b/langs.py
@@ -8,6 +8,7 @@ TARGET_LANGUAGES = {
     'python': '*.py',
 }
 
 OTHER_DATA = {
     # This code block is not relevant to TARGET_LANGUAGES.
+    'haskell': '*.hs',
 }
~~~~~~~~

Since the touched block doesn't have any `CHECK` comments, Checkmate generates
an empty checklist.

Note that it doesn't parse code's semantics, but only scans blocks through
indentation.  Even if a block is wrapped in curly braces without indentation,
it isn't counted as a block.


Directory-level checklist
-------------------------

Some checks may need to be listed for a whole directory.  Checkmate recognizes
files named *.check* or *CHECK* in a directory and include checks in that to
the checklist if any file in the directory are changed.  Its syntax is basically
a simple bullet list and a bullet can be `*`/`-`/`+`/`CHECK` or digits followed
by `.`/`)`, e.g.:

    - Check 1
    - Check 2

    + A plus sign too can be a bullet.
    * An asterisk too.

    1. Numbered-bullets also can be used.
    2) A closing parenthesis as well can follow instead of a period.

    CHECK: For consistency `CHECK` keyword also can be a bullet as well.
    CHECK And a colon can be omitted.
    Lines without any bullet is continued from previous line(s).


Download
--------

We provide an official Linux x86_64 binary for [every release][].  See also
the [latest release][].  Note that official binaries are distributed as
statically-linked standalone executable, and they aren't gzipped.  Download and
give an `+x` permission; then it's ready.

On the other platforms you can download and install using Haskell [Cabal][]
or [Stack][] since source tarballs also are distributed on [Hackage][]:

~~~~~~~~ bash
stack install checkmate
~~~~~~~~

[every release]: https://github.com/spoqa/checkmate/releases
[latest release]: https://github.com/spoqa/checkmate/releases/latest
[Cabal]: https://www.haskell.org/cabal/
[Stack]: https://www.haskellstack.org/


Integration with CI
-------------------

Since Checkmate usually is executed as a part of CI build, we show examples
for widely-used CI services.

All examples assume the environment variables are defined:

 -  `GITHUB_TOKEN` contains the access token to leave comments on a
    corresponding GitHub repository.  See also GitHub's official article
    about [personal API tokens][].
 -  `CHECKMATE_DOWNLOAD_URL` contains the download link to the prebuilt binary
    of the latest release, i.e.:

    ~~~~~~~ bash
    CHECKMATE_DOWNLOAD_URL=https://github.com/spoqa/checkmate/releases/download/0.3.3/checkmate-linux-x86_64
    ~~~~~~~

[personal API tokens]: https://github.com/blog/1509-personal-api-tokens


### Travis CI

~~~~~~~~ yaml
install:
- curl -L -o ~/bin/checkmate "$CHECKMATE_DOWNLOAD_URL"
- chmod +x ~/bin/checkmate
script:
- ~/bin/checkmate github-travis --token "$GITHUB_TOKEN"
~~~~~~~~


### Circle CI

~~~~~~~~ yaml
dependencies:
  post:
  - curl -L -o ~/bin/checkmate "$CHECKMATE_DOWNLOAD_URL"
  - chmod +x ~/bin/checkmate
test:
  post:
  - ~/bin/checkmate github-circle --token "$GITHUB_TOKEN"
~~~~~~~~


### Other CI softwares/services

You can run `checkmate github` command with explicit arguments:

~~~~~~~~ bash
curl -L -o ~/bin/checkmate "$CHECKMATE_DOWNLOAD_URL"
chmod +x ~/bin/checkmate
# Suppose we're running a build of github.com/foo/bar/pull/123
~/bin/checkmate github \
    --token "$GITHUB_TOKEN" \
    --login foo \
    --repo bar \
    --pr 123
~~~~~~~~

If you're using GitHub Enterprise on premise use `--endpoint` option.
Further reading: `checkmate github --help`.
