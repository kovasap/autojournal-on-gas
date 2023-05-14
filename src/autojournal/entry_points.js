function make_new_food_db_sheet() {
  autojournal.food.food_db_build.make_new_food_db_sheet();
}

function send_daily_report_email() {
  autojournal.core.send_report_email(1);
}

function send_weekly_report_email() {
  autojournal.core.send_report_email(7);
}

function update_lifelog() {
  autojournal.core.update_lifelog();
}

function write_report() {
  autojournal.core.write_report();
}
