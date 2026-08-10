#!/bin/sh
# Stand-in for maccalq, used by test.el to exercise maccalfw.el's
# process-invocation/stdin/response-parsing logic without needing
# EventKit or a real Swift build. Canned response selected by
# $FAKE_MODE; prints elisp on stdout (matching --format elisp).

cmd="${1:-}"
[ "$#" -gt 0 ] && shift

case "$FAKE_MODE" in
  echo-args)
    printf '((ARGS nil "%s"))' "$cmd $*"
    exit 0
    ;;
  echo-stdin)
    stdin_content=$(cat)
    escaped=$(printf '%s' "$stdin_content" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g')
    printf '((STDIN nil "%s"))' "$escaped"
    exit 0
    ;;
  calendars)
    cat <<'EOF'
(((ID nil "cal-1") (TITLE nil "Home") (TYPE nil "event") (COLOR nil "#FF0000") (EDITABLE nil "yes") (DEFAULT nil "yes"))
 ((ID nil "cal-2") (TITLE nil "Work") (TYPE nil "event") (COLOR nil "#00FF00")))
EOF
    exit 0
    ;;
  timezones)
    cat <<'EOF'
(((ID nil "America/New_York") (NAME nil "Eastern Time") (ABBREV nil "EST") (OFFSET nil "-18000") (DEFAULT nil "yes"))
 ((ID nil "UTC") (NAME nil "UTC") (ABBREV nil "UTC") (OFFSET nil "0")))
EOF
    exit 0
    ;;
  events)
    cat <<'EOF'
(((UID nil "evt-1") (SUMMARY nil "Party") (DTSTART nil "20260101T090000Z") (DTEND nil "20260101T100000Z")))
EOF
    exit 0
    ;;
  remove-ok)
    printf '((REMOVED nil "yes"))'
    exit 0
    ;;
  not-authorized)
    echo "maccalq: nope" >&2
    printf '((ERROR nil "not-authorized") (MESSAGE nil "Calendar access was not granted"))'
    exit 2
    ;;
  general-error)
    echo "maccalq: nope" >&2
    printf '((ERROR nil "error") (MESSAGE nil "Something went wrong"))'
    exit 1
    ;;
  garbage-error)
    echo "maccalq: crashed" >&2
    printf 'not valid elisp {{{'
    exit 1
    ;;
  *)
    echo "unknown FAKE_MODE: $FAKE_MODE" >&2
    exit 99
    ;;
esac
