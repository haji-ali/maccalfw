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
        let Qdefault = env.pointee.intern(env, ":default")
        let defaultCal = eventStore.defaultCalendarForNewEvents?.calendarIdentifier
        let list : [EmacsCastable?] =
          Array(calendars.map
                {
                    var calendar_data : [emacs_value? : EmacsCastable?] =
                      [Qid : $0.calendarIdentifier,
                       Qtitle: $0.title,
                       Qcolor : $0.color.hexString,
                                           Qeditable :
                                             ($0.allowsContentModifications ? Qt : nil)]
                    if ($0.calendarIdentifier == defaultCal){
                        calendar_data[Qdefault] = Qt
                    }
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

private func maccalfw_event_to_plist(_ env: UnsafeMutablePointer<emacs_env>,
                                     _ event_data : EKEvent) -> emacs_value?
{
    let event_plist : [String : EmacsCastable?] =
      [":id" : event_data.eventIdentifier,
       ":calendar-id" : event_data.calendar.calendarIdentifier,
       ":title" :event_data.title,
       ":location" : event_data.location,
       ":notes" : event_data.hasNotes ? event_data.notes : nil,
       ":start" : event_data.startDate,
       ":end" : event_data.endDate,
       ":occurrence-date" : event_data.occurrenceDate,
       ":detached-p" : event_data.isDetached ? Qt : nil,
       ":all-day-p" : event_data.isAllDay ? Qt : nil,
       ":created-date" : event_data.creationDate,
       ":last-modified" : event_data.lastModifiedDate,
       ":timezone" : event_data.timeZone?.identifier,
       ":status" : event_data.status.toEmacsVal(env),
       ":availability" : event_data.availability.toEmacsVal(env),
       ":organizer" : event_data.organizer?.name,
       ":read-only" : ((event_data.organizer?.isCurrentUser ?? true) &&
                         event_data.calendar.allowsContentModifications) ? nil : Qt,
       ":url" : event_data.url?.absoluteString]

    let quoted_plist =
      Dictionary(uniqueKeysWithValues:
                   event_plist.map {
                       (env.pointee.intern(env, $0.key), $0.value) })

    return quoted_plist.filter{$0.value != nil }.toEmacsVal(env)
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
               let argStart = args[1],
               let argEnd = args[2] {
                let calendar_id = try String.fromEmacsVal(env, name)
                let start = try Date.fromEmacsVal(env, argStart)
                let end = try Date.fromEmacsVal(env, argEnd)

                if let start, let end {
                    try AuthorizeCalendar(env)

                    var calendarEventsPredicate : NSPredicate

                    if calendar_id == nil {
                        calendarEventsPredicate =
                          eventStore.predicateForEvents(withStart: start,
                                                        end: end,
                                                        calendars: nil)
                    }
                    else {
                        if let calendar = eventStore.calendar(withIdentifier:
                                                                calendar_id!) {
                            calendarEventsPredicate =
                              eventStore.predicateForEvents(withStart: start,
                                                            end: end,
                                                            calendars: [calendar])
                        }
                        else {
                            throw EmacsError.error("Cannot retrieve calendar.")
                        }
                    }

                    let events = eventStore.events(matching: calendarEventsPredicate)
                    let list : [EmacsCastable?] =
                      Array(events.map {return maccalfw_event_to_plist(env, $0)})
                    return list.toEmacsVal(env)
                }
                else {
                    throw EmacsError.wrong_type_argument("Wrong type for wrong arguments")
                }
            }
            else {
                throw EmacsError.wrong_type_argument("Wrong arguments")
            }
        }
        catch {
            emacs_process_error(env, error)
        }
    }
    return Qnil
}

private func maccalfw_get_event(
  _ env: UnsafeMutablePointer<emacs_env>?,
  _ nargs: Int,
  _ args: UnsafeMutablePointer<emacs_value?>?,
  _ data: UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env {
        do {
            if let args, let eventId = try String.fromEmacsVal(env, args[0]) {
                try AuthorizeCalendar(env)

                let event_data = eventStore.event(withIdentifier: eventId)
                if let event_data{
                    return maccalfw_event_to_plist(env, event_data)
                }
                else {
                    throw EmacsError.error("Failed to fetch event.")
                }
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
                    let str = try String.fromEmacsVal(env, tmp)
                    event.location = (str?.isEmpty ?? true) ? nil : str
        }
        if let tmp = eventData["notes"] {
            event.notes = try String.fromEmacsVal(env, tmp)
        }
        if let tmp = eventData["url"] {
                    let str = try String.fromEmacsVal(env, tmp)
                    event.url = (str?.isEmpty ?? true) ? nil : URL(string: str!)
        }
        if let tmp = eventData["timezone"] {
                    let str = try String.fromEmacsVal(env, tmp)
                    event.timeZone = (str?.isEmpty ?? true) ? nil : TimeZone(identifier: str!)
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
            return maccalfw_event_to_plist(env, event)
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
        let Qname = env.pointee.intern(env, ":name")
        let Qabbrev = env.pointee.intern(env, ":abbrev")
        let Qoffset = env.pointee.intern(env, ":offset")
        let Qdefault = env.pointee.intern(env, ":default")
        let defTimeZone = NSTimeZone.default
        return TimeZone.knownTimeZoneIdentifiers.map {
            let timezone = (defTimeZone.identifier == $0 ?
                              defTimeZone :
                              TimeZone(identifier: $0))
            if let timezone {
                var timezone_data : [emacs_value? : EmacsCastable?] =
                  [Qname : timezone.localizedName(for: .standard,
                                                  locale: Locale.current) ?? "",
                   Qabbrev : timezone.abbreviation() ?? "",
                   Qoffset : timezone.secondsFromGMT(for: Date())]
                if defTimeZone.identifier == $0 {
                    timezone_data[Qdefault] = Qt
                }
                return emacs_cons(env, $0.toEmacsVal(env), timezone_data.toEmacsVal(env))
            }
            return emacs_cons(env, $0.toEmacsVal(env), Qnil)
        }.toEmacsVal(env)
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
must contain `:calendar-id` entry and an event is created.

Note that if the event has a different `:calendar-id`, the event moved to
the new calendar.

Returns the data of the newly event.
""")

        emacs_defun(env, "maccalfw-get-event", 1, 1, maccalfw_remove_event,
                    "Return event details given its ID.")
        emacs_defun(env, "maccalfw-remove-event", 1, 1, maccalfw_remove_event,
                    "Remove an event given its ID.")
        emacs_defun(env, "maccalfw-timezones", 0, 0, maccalfw_timezones,
                    "Returns a list of system timezones.")
    }
    return 0
}
