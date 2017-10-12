Checkmate
=========

[![Build Status][ci-badge]][ci]
[![Hackage][hackage-badge]][hackage]

Checkmate is a small program to generate human-readable checklists from
a given patch (or pull request).  Leave `CHECK` comments (that are the same
fashion as `FIXME` or `TODO` comments); then Checkmate detects when a diff
touches some code scopes having any `CHECK` comments, and lists the checks.

It helps contributors and reviewers to remind matters that require attention
when a part of the code is changed.


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

[ci-badge]: https://travis-ci.org/spoqa/checkmate.svg?branch=master
[ci]: https://travis-ci.org/spoqa/checkmate
[hackage-badge]: https://img.shields.io/hackage/v/checkmate.svg
[hackage]: https://hackage.haskell.org/package/checkmate


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
    CHECKMATE_DOWNLOAD_URL=https://github.com/spoqa/checkmate/releases/download/0.2.1/checkmate-linux-x86_64
    ~~~~~~~

[personal API tokens]: https://github.com/blog/1509-personal-api-tokens


### Travis CI

~~~~~~~~ yaml
install:
- |
  if [[ "$TRAVIS_PULL_REQUEST" != "false" ]]
  then
    curl -L -o ~/bin/checkmate "$CHECKMATE_DOWNLOAD_URL"
    chmod +x ~/bin/checkmate
  fi
script:
- |
  git diff "$TRAVIS_COMMIT_RANGE" | \
    ~/bin/checkmate github-travis --token "$GITHUB_TOKEN"
~~~~~~~~


### Circle CI

~~~~~~~~ yaml
machine:
  environment:
    # As Circle CI doesn't provide any environment variable of PR's target
    # branch it needs to be hardcoded.
    # See also https://discuss.circleci.com/t/access-real-branch-name-for-pull-request-fork-build/907
    TARGET_BRANCH: master
dependencies:
  post:
  - |
    if [[ "$CI_PULL_REQUEST" ]]
    then
      curl -L -o ~/bin/checkmate "$CHECKMATE_DOWNLOAD_URL"
      chmod +x ~/bin/checkmate
    fi
test:
  post:
  - |
    if [[ "$CI_PULL_REQUEST" ]]
    then
      git diff "$TARGET_BRANCH..$CIRCLE_SHA1" | ~/bin/checkmate github \
        --token "$GITHUB_TOKEN" \
        --login "$CIRCLE_PROJECT_USERNAME" \
        --repo "$CIRCLE_PROJECT_REPONAME" \
        --pr "${CI_PULL_REQUEST##*/}"
    fi
~~~~~~~~
