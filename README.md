# Autojournal Running on Google Apps Script (GAS)

The successor to https://github.com/kovasap/autojournal.

See https://github.com/kovasap/autojournal?tab=readme-ov-file#examples for some visual examples of what this program can do.

## Vision

Autojournal is meant to be a personal timeline information management tool that can aggregate data from many sources and then visualize/analyze it.
It tries to do this as automatically as possible.

This is useful to aid memory, and to help spot delayed consequences/effects of behavior and environment on mood and future behavior.

See also https://kovasap.github.io/docs/lifelogging/what-and-why/.

### "The Paths We Walk"

Visually, autojournal could be used to generate a widely zoomable life timeline.
This timeline could be zoomed out to view from birth to present day, or zoomed
in all the way to a daily resolution.

To facilitate this, events in the underlying data would need to be tagged with a
"significance" level to dictate what is filtered out when zooming out all the
way.
Additionally, recurring events should be grouped together; this would give a
view like "in 2021, I went climbing on average twice a week".

The fully zoomed out timeline (or the entire timeline) would be a target for
sharing, either on social media or in a database somewhere.
It would be interesting to view a sampling of how people live their lives this
way.

## Data Collection Protocol

Throughout: wear CGM, make sure ActivityWatch is running on all platforms, and
make sure GPS tracking is enabled.

Upon waking up, log perceived sleep quality via mementodb.

Whenever going to a bathroom throughout the day, log pee/poop and freeform
emotions text via mementodb.

When eating, take a picture of the food and log what it is via the custom food
tracker built into autojournal using mementodb (still experimental).

Every Saturday: Go through location data and create map from coordinates to
locations (that are new) + cooresponding activities.


## Technical Information

### Setup

  1. `sudo npm install -g @google/clasp`
  1. `npm i shadow-cljs`
  1. `sudo apt install joker`
  1. Clone this repository and cd into the base directory
  1. Set up a `clasp` project as instructed
  1. If a project already exists, just `clasp login` and `clasp clone`.

Go to project at https://script.google.com/home/my to run.

See https://lambdaisland.com/blog/2016-10-01-clojurescript-and-google-apps-script for background details.

### Deployment 

Compile your ClojureScript and push it to your Apps Script project with this
command:

```
./build.joke
```

### Development (Starting a REPL)

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

## Ingesting Data

### Food Tracking

Use https://github.com/jrmycanady/cronometer-export/pull/2/files to get food
data.

### Manual Tracking

Use https://mementodatabase.com/ with a library like "Activity Journal.mlt2" in
this repo.

### Location Data

Download https://gpslogger.app/.
Make sure in the settings it is set to log to csv format only (creating a zip
file for each day), then set it up to write the files automatically to Google
Drive.

### ActivityWatch

#### Android

Manually "export all buckets" in the android app, then use DriveSync to push the json file to Google Drive.

#### Linux

To run every hour, run `crontab -e` and add this snippet (assuming you cloned
autojournal into your home directory ~/):

```
0 * * * * ~/autojournal-on-gas/sync-activitywatch.clj
```

You may need to specify the path to your `bb` like this:

```
0 * * * * /home/linuxbrew/.linuxbrew/bin/bb autojournal-on-gas/sync-activitywatch.clj &> awexport.log
```

#### Windows

1. Install [activitywatch for windows](https://activitywatch.net/) and have it run continuously
2. Download sync-activitywatch.clj from this repo
3. Install babashka for windows: https://github.com/babashka/babashka/releases
4. Create a [scheduled task](https://stackoverflow.com/a/21502661) to run babashka with sync-activitywatch.clj as an argument on a schedule.
    - Have your program be `cmd.exe` and your arguments `/c C:\Users\kovas\babashka-1.3.182-windows-amd64\bb.exe C:\Users\kovas\sync-activitywatch.clj`
6. Sync the export file to google drive by adding the dir to the google drive desktop client's watched dirs.

![Screenshot 2024-01-18 183827](https://github.com/kovasap/autojournal-on-gas/assets/8763010/7096dda7-89ce-49e5-b7cf-883a4c7cbdaf)

## TODOs

 - Send emails with insights (perhaps with biomarker correlator links)
 - Use notification emails from services (e.g. GitHub) as a way to get data into
 the system
 - Try tailwind css for report styling.
