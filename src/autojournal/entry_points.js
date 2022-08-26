function make_new_food_db_sheet() {
  autojournal.food_summary.make_new_food_db_sheet();
}

function summarize_food() {
  autojournal.core.summarize_food();
}

function add_today() {
  autojournal.core.update_lifelog();
}

function add_last_week() {
  autojournal.core.update_lifelog();
}

function regenerate_all() {
  autojournal.core.update_lifelog();
}
