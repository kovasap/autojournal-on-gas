# ClojureScript and Clasp for easier Google Apps Script

Proof of concept (i.e., hello world) codebase that compiles ClojureScript to js
files which can be pushed to a Google Apps Suite project
with [clasp](https://github.com/google/clasp), Google's command line utility
for local Apps Script development.

See https://lambdaisland.com/blog/2016-10-01-clojurescript-and-google-apps-script.

## Setup

  1. Install `lein`, `clasp`, `npm i shadow-cljs`, and `joker`
  1. Clone this repository and cd into the base directory
  1. Set up a `clasp` project as instructed

## Deployment 

Compile your ClojureScript and push it to your Apps Script project with this
command:

```
./build.joke
```

## Development (Starting a REPL)

In one terminal, run:

```
npx shadow-cljs watch autojournal
```

In another, run:

```
node dev-Code.js
```

## TODOs

 - Send emails with insights (perhaps with biomarker correlator links)
