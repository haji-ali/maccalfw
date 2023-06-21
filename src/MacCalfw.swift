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

func AuthorizeCalendar(_ env: UnsafeMutablePointer<emacs_env>) -> Bool {
    switch EKEventStore.authorizationStatus(for: .event) {
    case .authorized:
        return true
    case .notDetermined:
        let semaphore = DispatchSemaphore(value: 0)
        eventStore.requestAccess(to: .event, completion: {
                                     (accessGranted: Bool, error: Error?) in
                                     semaphore.signal()})
        semaphore.wait()
        let status = EKEventStore.authorizationStatus(for: EKEntityType.event)
        if status == EKAuthorizationStatus.authorized {
            return true
        }
        emacs_error(env, "error", "authorization-failed")
        return false
    case .restricted, .denied:
        fallthrough
    @unknown default:
        emacs_error(env, "error", "not-authorized")
        return false
    }
}

private func maccalfw_get_calendars(_ env: UnsafeMutablePointer<emacs_env>?,
                          _ nargs: Int,
                          _ args: UnsafeMutablePointer<emacs_value?>?,
                          _ data: UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env {
        if !AuthorizeCalendar(env){
            return Qnil
        }

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
    return Qnil
}

private func maccalfw_fetch_events(
  _ env: UnsafeMutablePointer<emacs_env>?,
  _ nargs: Int,
  _ args: UnsafeMutablePointer<emacs_value?>?,
  _ data: UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env {
        if let args, let name = args[0],
           let calendar_id = String.fromEmacsVal(env, name) {
        if !AuthorizeCalendar(env){
            return Qnil
        }

            if let calendar = eventStore.calendar(withIdentifier: calendar_id) {
            let start = Date.fromEmacsVal(env, args[1]!)
            let end = Date.fromEmacsVal(env, args[2]!)

            var list = Qnil;

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

            for event in events {
                let event_data : [emacs_value? : EmacsCastable?] =
                  [Qid : event.eventIdentifier,
                       Qcalendar : calendar_id,
                   Qtitle :event.title,
                   Qlocation : event.location,
                   Qnotes : event.hasNotes ? event.notes : nil,
                   Qstart : event.startDate,
                   Qend : event.endDate,
                   Qdate : event.occurrenceDate,
                   QisDetached : event.isDetached ? Qt : nil,
                   QisAllDay : event.isAllDay ? Qt : nil,
                   Qcreated_date : event.creationDate,
                   Qlast_modified : event.lastModifiedDate,
                   Qstatus : event.status.toEmacsVal(env),
                   Qavailability : event.availability.toEmacsVal(env),
                   Qorganizer : event.organizer?.name,
                   Qurl : event.url?.absoluteString]

                list = emacs_cons(env,
                                              event_data.filter{
                                      $0.value != nil }.toEmacsVal(env),
                                  list)
            }
            return list
        }
            else {
                emacs_error(env, "error", "Cannot retrieve calendar.")
            }
        }
        else {
            emacs_error(env, "wrong-type-argument")
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
    if let env, let args, let arg0 = args[0] {
        if !AuthorizeCalendar(env){
            return Qnil
        }

        let eventData = emacs_parse_plist(env, arg0)
        let eventId =
          eventData["id"].map { String.fromEmacsVal(env, $0) }

        var event : EKEvent

        if let eventId {
            let old_event = eventStore.event(withIdentifier: eventId!)
            if let old_event {
                event = old_event
            }
            else{
                emacs_error(env, "error", "Cannot retrieve event")
                return Qnil
            }
        }
        else{
            let calendar_id =
              eventData["calendar-id"].map { String.fromEmacsVal(env, $0) }
            event = EKEvent(eventStore: eventStore)
            if let calendar_id, let calendar = eventStore.calendar(withIdentifier: calendar_id!) {
                event.calendar = calendar
            }
            else {
                emacs_error(env, "error", "Cannot retrieve calendar")
                return Qnil
            }
        }

        if let tmp = eventData["title"] {
            event.title = String.fromEmacsVal(env, tmp)
        }
        if let tmp = eventData["start"] {
            event.startDate = Date.fromEmacsVal(env, tmp)
        }
        if let tmp = eventData["end"] {
            event.endDate = Date.fromEmacsVal(env, tmp)
        }
        if let tmp = eventData["location"] {
            event.location = String.fromEmacsVal(env, tmp)
        }
        if let tmp = eventData["notes"] {
            event.notes = String.fromEmacsVal(env, tmp)
        }
        if let tmp = eventData["url"] {
            event.url = URL(string: String.fromEmacsVal(env, tmp) ?? "")
        }
        if let tmp = eventData["all-day-p"] {
            event.isAllDay = env.pointee.is_not_nil(env, tmp)
        }
        if let tmp = eventData["availability"] {
            event.availability = EKEventAvailability.parse(emacs_symbol_to_string(env, tmp)!)
        }

        // Read-only: occurrenceDate, organizer, last_modified, created_date
        // detached-p, status

        do {
            try eventStore.save(event, span: .thisEvent)
            return event.eventIdentifier.toEmacsVal(env)
        } catch {
            emacs_error(env, "error", "Failed to save event with error: \(error.localizedDescription)")
        }
    }
    return Qnil
}

private func maccalfw_test(
  _ env: UnsafeMutablePointer<emacs_env>?,
  _ nargs: Int,
  _ args: UnsafeMutablePointer<emacs_value?>?,
  _ data: UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env, let args, let arg0 = args[0] {
        let eventData = emacs_parse_plist(env, arg0)
        for (key, value) in eventData {
            print("\(key as Optional): \(value as Optional)")
        }

        if let tmp = eventData[":id"] {
            let str = String.fromEmacsVal(env, tmp)
            print("\n\nid: \(str as Optional)")
        }

        emacs_error(env, "error", "I hate you")
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
Get a plist of Mac calendars.
Each item in the list contains contains id, title, color and an editable predicated.
""")
        emacs_defun(env, "maccalfw-fetch-events", 3, 3, maccalfw_fetch_events,
                    """
Get a plist event in a calendar.
Takes as arguments the CALENDAR-ID, START-TIME and END-TIME.
The times are encoded times.
""")

        emacs_defun(env, "maccalfw-update-event", 1, 1, maccalfw_update_event,
"""
Update or create an event.
Takes as an argument a plist of the event.
Only the keys in the plist are updated. If the plist contains an `:id`
then the corresponding event is updated. Otherwise, the plist must containg
`:calendar-id` entry and an event is created and its ID is returned.

Note that if the event has a different `:calendar-id` than is provided, the
event moved to the new calendar.
""")

        emacs_defun(env, "maccalfw--test", 1, 1, maccalfw_test,
                    "This is a simple test function")
    }
    return 0
}
