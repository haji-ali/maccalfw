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
        env.pointee.non_local_exit_signal(
          env, env.pointee.intern(env, "authorization-failed"), Qnil);
        fallthrough
    case .restricted, .denied:
        fallthrough
    @unknown default:
        env.pointee.non_local_exit_signal(
          env, env.pointee.intern(env, "not-authorized"), Qnil);
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
        var list = Qnil;


        let Qid = env.pointee.intern(env, ":id")
        let Qtitle = env.pointee.intern(env, ":title")
        let Qcolor = env.pointee.intern(env, ":color")
        let Qeditable = env.pointee.intern(env, ":editable")


        for calendar in calendars {
            list = emacs_cons(env,
                              emacs_plist(env,
                                          [Qid : calendar.calendarIdentifier,
                                           Qtitle: calendar.title,
                                           Qcolor : calendar.color.hexString,
                                           Qeditable :
                                             (calendar.allowsContentModifications
                                                ? Qt : nil)]),
                              list);
        }
        return list
    }
    return Qnil
}

private func maccalfw_fetch_events(
  _ env: UnsafeMutablePointer<emacs_env>?,
  _ nargs: Int,
  _ args: UnsafeMutablePointer<emacs_value?>?,
  _ data: UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env, let args, let name = args[0],
       let calendar_name = String.fromEmacsVal(env, name) {
        if !AuthorizeCalendar(env){
            return Qnil
        }

        if let calendar = eventStore.calendar(withIdentifier: calendar_name) {
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
            let Qorganizer = env.pointee.intern(env, ":organzier")
            let Qid = env.pointee.intern(env, ":id")
            let Qlast_modified = env.pointee.intern(env, ":last-modified")
            let Qcreated_date = env.pointee.intern(env, ":created-date")
            let Qurl = env.pointee.intern(env, ":url")

            for event in events {
                let event_data : [emacs_value? : EmacsCastable?] =
                  [Qid : event.eventIdentifier,
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
                   Qorganizer : event.organizer?.name,
                   Qurl : event.url?.absoluteString]

                list = emacs_cons(env,
                                  emacs_plist(env,
                                              event_data.filter{
                                                  $0.value != nil }),
                                  list)
            }
            return list
        }
    }
    return Qnil
}


@_cdecl("emacs_module_init")
func emacs_module_init(_ runtime: UnsafeMutablePointer<emacs_runtime>) -> Int32 {
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

        emacs_defun(env, "maccalfw-get-calendars", 0, 0, maccalfw_get_calendars)
        emacs_defun(env, "maccalfw-fetch-events", 3, 3, maccalfw_fetch_events)
    }
    return 0
}
