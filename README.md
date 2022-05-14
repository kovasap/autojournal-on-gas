# ClojureScript and Clasp for easier Google Apps Script

Proof of concept (i.e., hello world) codebase that compiles ClojureScript to js
files which can be pushed to a Google Apps Suite project
with [clasp](https://github.com/google/clasp), Google's command line utility
for local Apps Script development.

See https://lambdaisland.com/blog/2016-10-01-clojurescript-and-google-apps-script.

## Usage

  1. Install `lein`, `clasp`, and `joker`
  1. Clone this repository and cd into the base directory
  1. Set up a `clasp` project as instructed
  1. Compile your ClojureScript and push it to your Apps Script project as
  shown below

```
./build.joke
```

## REPL

```
./start-repl.zsh
```

Should give a nREPL that can be used with vim-conjure. It would be cool if a
node repl could be started instead from cljsbuild (see
https://github.com/emezeske/lein-cljsbuild/issues/81) so that the
`(:require [autojournal.entrypoints]))` wouldn't cause a breakage when
developing.

## License

Copyright © 2019 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
