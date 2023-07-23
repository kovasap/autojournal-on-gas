# Autojournal Running on Google Apps Script

Proof of concept (i.e., hello world) codebase that compiles ClojureScript to js
files which can be pushed to a Google Apps Suite project
with [clasp](https://github.com/google/clasp), Google's command line utility
for local Apps Script development.

See https://lambdaisland.com/blog/2016-10-01-clojurescript-and-google-apps-script.

## Setup

  1. `sudo npm install -g @google/clasp`
  1. `npm i shadow-cljs`
  1. `sudo apt install joker`
  1. Clone this repository and cd into the base directory
  1. Set up a `clasp` project as instructed
  1. If a project already exists, just `clasp login` and `clasp clone`.

Go to project at https://script.google.com/home/my to run.

## Deployment 

Compile your ClojureScript and push it to your Apps Script project with this
command:

```
./build.joke
```

## Development (Starting a REPL)

In one terminal, run:

```
npx shadow-cljs watch autojournal-for-node
```

In another, run:

```
node dev-Code.js
```

### Viewing Generated Files

```
export FILENAME=vega.html; gdrive download query "name='$FILENAME' and trashed=false" --force; xdg-open $FILENAME
```

## TODOs

 - Send emails with insights (perhaps with biomarker correlator links)
 - Use notification emails from services (e.g. GitHub) as a way to get data into
 the system

## Food Tracking

Use https://github.com/jrmycanady/cronometer-export/pull/2/files to get food
data.

## ActivityWatch

### Windows

1. Install [activitywatch for windows](https://activitywatch.net/) and have it run continuously
2. Download sync-activitywatch.clj from this repo
3. Install babashka for windows: https://github.com/babashka/babashka/releases
4. Create a [scheduled task](https://stackoverflow.com/a/21502661) to run babashka with sync-activitywatch.clj as an argument on a schedule.
5. Sync the export file to google drive by adding the dir to the google drive desktop client's watched dirs.

![image](https://github.com/kovasap/autojournal-on-gas/assets/8763010/fc76b903-6095-425f-8f9d-a0b2a240b862)

