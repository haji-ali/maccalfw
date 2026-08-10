let helpText = """
maccalq -- query and edit macOS Calendar/Reminders from the command line

USAGE
    maccalq COMMAND [--flag value ...]
    maccalq help

Each command prints its result to stdout in one of three formats,
chosen with the global --format flag (default: icalendar):
    --format icalendar     RFC5545 text (BEGIN:VEVENT ... END:VEVENT)
    --format json          [{"name":..., "params":..., "value":...}, ...]
    --format elisp         ((NAME PARAMS VALUE) ...) sexp triples

icalendar is output only -- update-event cannot read it back in yet,
so pass --format json or --format elisp when piping data to stdin.

Dates are ISO8601 (e.g. 2026-08-10T09:00:00Z). Flags that take no
value (--future, --include-completed) are boolean switches; passing
them at all turns them on.

COMMANDS

  calendars [--type event|reminder|all]
      List calendars. --type defaults to "event".

  events --start DATE --end DATE [--calendar ID ...]
      List events in [start, end). --calendar may repeat to restrict
      to specific calendar IDs; omit it to search all calendars.

  event --id ID [--start DATE]
      Fetch a single event by ID. --start disambiguates recurring
      instances that share an ID, by searching a window around it.

  update-event [--id ID] [--start DATE] [--future]
      Create or update an event. Reads the new property triples from
      stdin, encoded in --format. Omit --id to create a new event;
      with --id, --start disambiguates the recurring instance to
      update (see `event`), and --future applies the change to this
      and all following instances instead of just this one.

  remove-event --id ID [--start DATE] [--future]
      Delete an event. --start/--future as in update-event.

  reminders [--calendar ID ...] [--include-completed]
      List reminders. --include-completed also returns completed
      ones; by default only incomplete reminders are listed.

  timezones
      List known IANA timezone identifiers with display name,
      abbreviation and current UTC offset in seconds.

  refresh
      Force calendar sources to refresh before a subsequent `events`
      call, e.g. after an external change.

  help, --help, -h
      Print this message.
"""
