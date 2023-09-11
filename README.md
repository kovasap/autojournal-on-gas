# Autojournal Running on Google Apps Script (GAS)

## Vision

Autojournal is meant to be a personal information management tool that can aggregate data from many sources and then push it to visualization and analysis tools.
It tries to do this as automatically as possible.

This is useful to aid memory, and to help spot delayed consequences/effects of behavior and environment on mood and future behavior.

See also https://kovasap.github.io/docs/lifelogging/what-and-why/.

## Setup

  1. `sudo npm install -g @google/clasp`
  1. `npm i shadow-cljs`
  1. `sudo apt install joker`
  1. Clone this repository and cd into the base directory
  1. Set up a `clasp` project as instructed
  1. If a project already exists, just `clasp login` and `clasp clone`.

Go to project at https://script.google.com/home/my to run.

See https://lambdaisland.com/blog/2016-10-01-clojurescript-and-google-apps-script for background details.

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

## Manual Tracking

Use https://mementodatabase.com/ with a library like "Activity Journal.mlt2" in
this repo.

## Location Data

Download https://gpslogger.app/.
Make sure in the settings it is set to log to csv format only (creating a zip
file for each day), then set it up to write the files automatically to Google
Drive.

## ActivityWatch

### Android

Manually "export all buckets" in the android app, then use DriveSync to push the json file to Google Drive.

### Linux

To run every hour, run `crontab -e` and add this snippet (assuming you cloned
autojournal into your home directory ~/):

```
0 * * * * ~/autojournal-on-gas/sync-activitywatch.clj
```

You may need to specify the path to your `bb` like this:

```
0 * * * * /home/linuxbrew/.linuxbrew/bin/bb autojournal-on-gas/sync-activitywatch.clj &> awexport.log
```

### Windows

1. Install [activitywatch for windows](https://activitywatch.net/) and have it run continuously
2. Download sync-activitywatch.clj from this repo
3. Install babashka for windows: https://github.com/babashka/babashka/releases
4. Create a [scheduled task](https://stackoverflow.com/a/21502661) to run babashka with sync-activitywatch.clj as an argument on a schedule.
5. Sync the export file to google drive by adding the dir to the google drive desktop client's watched dirs.

![image](https://github.com/kovasap/autojournal-on-gas/assets/8763010/fc76b903-6095-425f-8f9d-a0b2a240b862)

