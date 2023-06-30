import Foundation
import CEmacsModule
import EventKit

let eventStore = EKEventStore()
var Qt : emacs_value?
var Qnil : emacs_value?

typealias EmacsDefunCallback  = (@convention(c)
                                 (UnsafeMutablePointer<emacs_env>?,
                                  Int,
                                  UnsafeMutablePointer<emacs_value?>?,
                                                       UnsafeMutableRawPointer?) -> emacs_value?)?

func AuthorizeCalendar(_ env: UnsafeMutablePointer<emacs_env>) throws {
    switch EKEventStore.authorizationStatus(for: .event) {
    case .authorized:
        return
    case .notDetermined:
        let semaphore = DispatchSemaphore(value: 0)
        eventStore.requestAccess(to: .event, completion: {
                                     (accessGranted: Bool, error: Error?) in
                                     semaphore.signal()})
        semaphore.wait()
        let status = EKEventStore.authorizationStatus(for: EKEntityType.event)
        if status == EKAuthorizationStatus.authorized {
            return
        }
        throw EmacsError.error("authorization-failed")
    case .restricted, .denied:
        fallthrough
    @unknown default:
        throw EmacsError.error("not-authorized")
    }
}

private func maccalfw_get_calendars(_ env: UnsafeMutablePointer<emacs_env>?,
                          _ nargs: Int,
                          _ args: UnsafeMutablePointer<emacs_value?>?,
                          _ data: UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env {
        do {
            try AuthorizeCalendar(env)

        let calendars = eventStore.calendars(for: .event)
        let Qid = env.pointee.intern(env, ":id")
        let Qtitle = env.pointee.intern(env, ":title")
        let Qcolor = env.pointee.intern(env, ":color")
        let Qeditable = env.pointee.intern(env, ":editable")
        let list : [EmacsCastable?] =
          Array(calendars.map
                {
                    let calendar_data : [emacs_value? : EmacsCastable?] =
                      [Qid : $0.calendarIdentifier,
                       Qtitle: $0.title,
                       Qcolor : $0.color.hexString,
                                           Qeditable :
                         ($0.allowsContentModifications ? Qt : nil)]
                    return calendar_data.toEmacsVal(env)
                })
        return list.toEmacsVal(env)
    }
        catch {
            emacs_process_error(env, error)
        }
    }
    return Qnil
}

private func maccalfw_fetch_events(
  _ env: UnsafeMutablePointer<emacs_env>?,
  _ nargs: Int,
  _ args: UnsafeMutablePointer<emacs_value?>?,
  _ data: UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env {
        do {
        if let args, let name = args[0],
               let calendar_id = try String.fromEmacsVal(env, name) {
                try AuthorizeCalendar(env)

            if let calendar = eventStore.calendar(withIdentifier: calendar_id) {
                    let start = try Date.fromEmacsVal(env, args[1]!)
                    let end = try Date.fromEmacsVal(env, args[2]!)

            let calendarEventsPredicate =
              eventStore.predicateForEvents(withStart: start!,
                                            end: end!,
                                            calendars: [calendar])

            let events = eventStore.events(matching: calendarEventsPredicate)

            let Qtitle = env.pointee.intern(env, ":title")
            let Qstart = env.pointee.intern(env, ":start")
            let Qend = env.pointee.intern(env, ":end")
            let Qlocation = env.pointee.intern(env, ":location")
            let Qnotes = env.pointee.intern(env, ":notes")

            let QisAllDay = env.pointee.intern(env, ":all-day-p")
            let QisDetached = env.pointee.intern(env, ":detached-p")
            let Qdate = env.pointee.intern(env, ":occurrence-date")
            let Qstatus = env.pointee.intern(env, ":status")
                let Qavailability = env.pointee.intern(env, ":availability")
            let Qorganizer = env.pointee.intern(env, ":organzier")
            let Qid = env.pointee.intern(env, ":id")
            let Qlast_modified = env.pointee.intern(env, ":last-modified")
            let Qcreated_date = env.pointee.intern(env, ":created-date")
            let Qurl = env.pointee.intern(env, ":url")
                let Qcalendar = env.pointee.intern(env, ":calendar-id")
                    let Qtimezone = env.pointee.intern(env, ":timezone")

                    let list : [EmacsCastable?] =
                      Array(events.map
                            {
                let event_data : [emacs_value? : EmacsCastable?] =
                                  [Qid : $0.eventIdentifier,
                       Qcalendar : calendar_id,
                                   Qtitle :$0.title,
                                   Qlocation : $0.location,
                                   Qnotes : $0.hasNotes ? $0.notes : nil,
                                   Qstart : $0.startDate,
                                   Qend : $0.endDate,
                                   Qdate : $0.occurrenceDate,
                                   QisDetached : $0.isDetached ? Qt : nil,
                                   QisAllDay : $0.isAllDay ? Qt : nil,
                                   Qcreated_date : $0.creationDate,
                                   Qlast_modified : $0.lastModifiedDate,
                                   Qtimezone : $0.timeZone?.identifier,
                                   Qstatus : $0.status.toEmacsVal(env),
                                   Qavailability : $0.availability.toEmacsVal(env),
                                   Qorganizer : $0.organizer?.name,
                                   Qurl : $0.url?.absoluteString]
                                return event_data.filter{$0.value != nil }.toEmacsVal(env)
                            })
            return list.toEmacsVal(env)
        }
            else {
                throw EmacsError.error("Cannot retrieve calendar.")
            }
        }
        else {
            throw EmacsError.wrong_type_argument("Wrong type for calendar id")
        }
        }
        catch {
            emacs_process_error(env, error)
        }
    }
    return Qnil
}

private func maccalfw_update_event(
  _ env: UnsafeMutablePointer<emacs_env>?,
  _ nargs: Int,
  _ args: UnsafeMutablePointer<emacs_value?>?,
  _ data: UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env {
        do {
            if let args, let arg0 = args[0] {
                try AuthorizeCalendar(env)

                let eventData = try emacs_parse_plist(env, arg0)
        let eventId =
                  try eventData["id"].map { try String.fromEmacsVal(env, $0) }
        var event : EKEvent

        if let eventId, let eventId {
            let old_event = eventStore.event(withIdentifier: eventId)
            if let old_event {
                event = old_event
            }
            else{
                        throw EmacsError.error("Cannot retrieve event")
            }
        }
        else{
            let calendar_id =
                      try eventData["calendar-id"].map { try String.fromEmacsVal(env, $0) }
            event = EKEvent(eventStore: eventStore)
                    if let calendar_id, let calendar_id,
                       let calendar = eventStore.calendar(withIdentifier: calendar_id) {
                event.calendar = calendar
            }
            else {
                        throw EmacsError.error("Cannot retrieve calendar")
            }
        }

        if let tmp = eventData["title"] {
                    event.title = try String.fromEmacsVal(env, tmp)
        }
        if let tmp = eventData["start"] {
                    event.startDate = try Date.fromEmacsVal(env, tmp)
        }
        if let tmp = eventData["end"] {
                    event.endDate = try Date.fromEmacsVal(env, tmp)
        }
        if let tmp = eventData["location"] {
                    event.location = try String.fromEmacsVal(env, tmp)
        }
        if let tmp = eventData["notes"] {
                    event.notes = try String.fromEmacsVal(env, tmp)
        }
        if let tmp = eventData["url"] {
                    event.url = URL(string: try String.fromEmacsVal(env, tmp) ?? "")
        }
        if let tmp = eventData["all-day-p"] {
            event.isAllDay = env.pointee.is_not_nil(env, tmp)
        }
        if let tmp = eventData["availability"] {
                    event.availability = try EKEventAvailability.parse(try emacs_symbol_to_string(env, tmp)!)
        }

        // Read-only: occurrenceDate, organizer, last_modified, created_date
        // detached-p, status

        do {
            try eventStore.save(event, span: .thisEvent)
            return event.eventIdentifier.toEmacsVal(env)
        } catch {
                    throw EmacsError.error("Failed to save event with error: \(error.localizedDescription)")
                }
            }
        }
        catch {
            emacs_process_error(env, error)
        }
    }
    return Qnil
}

private func maccalfw_remove_event(
  _ env: UnsafeMutablePointer<emacs_env>?,
  _ nargs: Int,
  _ args: UnsafeMutablePointer<emacs_value?>?,
  _ data: UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env, let args {
        do {
            try AuthorizeCalendar(env)
            if let arg0 = args[0], let eventId = try String.fromEmacsVal(env, arg0) {
                if let event = eventStore.event(withIdentifier: eventId) {
                    do {
                        try eventStore.remove(event, span: .thisEvent)
                        return Qt
                    } catch {
                        throw EmacsError.error("Failed to remove event with error: \(error.localizedDescription)")
                    }
                }
                else {
                    throw EmacsError.error("Cannot retrieve event")
                }
            }
        }
        catch {
            emacs_process_error(env, error)
        }
    }
    return Qnil
}


private func maccalfw_timezones(
  _ env: UnsafeMutablePointer<emacs_env>?,
  _ nargs: Int,
  _ args: UnsafeMutablePointer<emacs_value?>?,
  _ data: UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env {
        let Qid = env.pointee.intern(env, ":id")
        let Qname = env.pointee.intern(env, ":name")
        let Qabbrev = env.pointee.intern(env, ":abbrev")
        let Qoffset = env.pointee.intern(env, ":offset")
        return TimeZone.knownTimeZoneIdentifiers.map(
          {
              if let timezone = TimeZone(identifier: $0) {
                  let timezone_data : [emacs_value? : EmacsCastable?] =
                    [Qid : $0,
                     Qname : timezone.localizedName(for: .standard,
                                                    locale: Locale.current) ?? "",
                     Qabbrev : timezone.abbreviation() ?? "",
                     Qoffset : timezone.secondsFromGMT(for: Date())]
                  return timezone_data.toEmacsVal(env)
              }
              return nil
          }
        ).toEmacsVal(env)
    }
    return Qnil
}

private func maccalfw_test(
  _ env: UnsafeMutablePointer<emacs_env>?,
  _ nargs: Int,
  _ args: UnsafeMutablePointer<emacs_value?>?,
  _ data: UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env {
        do {
            if let args, let arg0 = args[0] {
                let eventData = try emacs_parse_plist(env, arg0)
        for (key, value) in eventData {
            print("\(key as Optional): \(value as Optional)")
        }

        if let tmp = eventData[":id"] {
                    let str = try String.fromEmacsVal(env, tmp)
            print("\n\nid: \(str as Optional)")
        }

        emacs_error(env, "error", "I hate you")
    }
        }
        catch {
            emacs_process_error(env, error)
        }
    }
    return Qnil
}

@_cdecl("emacs_module_init")
public func emacs_module_init(_ runtime: UnsafeMutablePointer<emacs_runtime>) -> Int32 {
    // Function implementation
    if MemoryLayout<emacs_runtime>.size > Int(runtime.pointee.size) {
        return 1
    }

    let env = runtime.pointee.get_environment(runtime) as UnsafeMutablePointer<emacs_env>?
    if let env {
        if MemoryLayout<emacs_env>.size > Int(env.pointee.size) {
            return 2
        }
        Qnil = env.pointee.make_global_ref(env, env.pointee.intern(env, "nil"))
        Qt = env.pointee.make_global_ref(env, env.pointee.intern(env, "t"))

        emacs_defun(env, "maccalfw-get-calendars", 0, 0, maccalfw_get_calendars,
                    """
Get a list of Mac calendars.
Each item in the list contains contains id, title, color and an editable predicated.
""")
        emacs_defun(env, "maccalfw-fetch-events", 3, 3, maccalfw_fetch_events,
                    """
Get a list of events in a calendar.
Takes as arguments the CALENDAR-ID, START-TIME and END-TIME.
The times are encoded times.
""")

        emacs_defun(env, "maccalfw-update-event", 1, 1, maccalfw_update_event,
"""
Update or create an event.
Takes as an argument a plist of the EVENT.
Only the key-value pairs in the plist are updated. If the plist contains a
non-nil `:id` then the corresponding event is updated. Otherwise, the plist
must contain `:calendar-id` entry and an event is created and its ID is
returned.

Note that if the event has a different `:calendar-id`, the event moved to
the new calendar.
""")

        emacs_defun(env, "maccalfw-remove-event", 1, 1, maccalfw_remove_event,
"""
Remove an event given its ID.
""")

        emacs_defun(env, "maccalfw-timezones", 0, 0, maccalfw_timezones,
"""
Returns a list of system timezones.
""")

        emacs_defun(env, "maccalfw--test", 1, 1, maccalfw_test,
                    "This is a simple test function")
    }
    return 0
}
