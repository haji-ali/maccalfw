import Foundation
import CEmacsModule
import EventKit

import Cocoa

extension NSColor {
    var hexString: String? {
        guard let color = self.usingColorSpace(.sRGB) else { return nil }

        let red = Int(color.redComponent * 255)
        let green = Int(color.greenComponent * 255)
        let blue = Int(color.blueComponent * 255)

        return String(format: "#%02X%02X%02X", red, green, blue)
    }
}


private func emacs_defun(_ env: UnsafeMutablePointer<emacs_env>,
                 _ name: String,
                 _ min: Int,
                 _ max: Int,
                 _ fun: EmacsDefunCallback) {
    let internSymbol = env.pointee.intern(env, "defalias")
    let functionName = env.pointee.intern(env, name)
    let function = env.pointee.make_function(env, min, max, fun,
                                             nil, nil)

    withExtendedLifetime(internSymbol) {
        withExtendedLifetime(functionName) {
            withExtendedLifetime(function) {
                var arguments: [emacs_value?] = [functionName, function]
                _ = arguments.withUnsafeMutableBufferPointer{
                    bufferPointer in
                    env.pointee.funcall(env, internSymbol, 2,
                                        bufferPointer.baseAddress!)
                }
            }
        }
    }
}


private func emacs_cast(_ env: UnsafeMutablePointer<emacs_env>, _ str: String?) -> emacs_value? {
    if let str {
        return env.pointee.make_string(env, str, str.count)
    }
    else { return Qnil}
}

private func emacs_cons(_ env: UnsafeMutablePointer<emacs_env>,
                        _ a: emacs_value?, _ b: emacs_value?) -> emacs_value? {
    var arguments: [emacs_value?] = [a, b]
    return arguments.withUnsafeMutableBufferPointer{
        bufferPointer in
        env.pointee.funcall(env, env.pointee.intern(env, "cons"), 2,
                            bufferPointer.baseAddress!)
    }
}

private func emacs_list(_ env: UnsafeMutablePointer<emacs_env>,
                        _ args: [emacs_value?]) -> emacs_value? {
    var arguments = Array(args)
    let count = arguments.count
    return arguments.withUnsafeMutableBufferPointer{
        bufferPointer in
        env.pointee.funcall(env, env.pointee.intern(env, "list"),
                            count,
                            bufferPointer.baseAddress!)
    }
}


private func emacs_plist(_ env: UnsafeMutablePointer<emacs_env>,
                         _ args: [emacs_value?]) -> emacs_value? {
    // let arguments = args.enumerated().compactMap { index, element in
    //     if element == nil || index > 0 && args[index - 1] == nil {
    //         return nil
    //     }
    //     return element!
    // }
    return emacs_list(env, args)
}

private func emacs_str(_ env: UnsafeMutablePointer<emacs_env>,
                       _ val: emacs_value) -> String? {
    var size: Int = 0
    if !env.pointee.copy_string_contents(env, val, nil, &size) {
        return nil
    }

    let buffer = UnsafeMutablePointer<CChar>.allocate(capacity: Int(size))
    defer {
        buffer.deallocate()
    }

    if !env.pointee.copy_string_contents(env, val, buffer, &size) {
        return nil
    }
    return String(cString: buffer)
}


private func emacs_date(_ env: UnsafeMutablePointer<emacs_env>,
                       _ val: emacs_value) -> Date {
    // val should be a list of 3 elements having day, month and year.
    let time = env.pointee.extract_time(env, val)
    let timeInterval = TimeInterval(time.tv_sec) + TimeInterval(time.tv_nsec) / 1_000_000_000
    return Date(timeIntervalSince1970: timeInterval)
}


private func emacs_cast(_ env: UnsafeMutablePointer<emacs_env>,
                        _ val: Date?) -> emacs_value? {
    // val should be a list of 3 elements having day, month and year.
    if let val{
        let timeInterval = val.timeIntervalSince1970
        let seconds = Int(timeInterval)
        let nanoseconds = Int((timeInterval - Double(seconds)) * 1_000_000_000)

        let timespecObj = timespec(tv_sec: seconds, tv_nsec: nanoseconds)
        return env.pointee.make_time(env, timespecObj)
    }
    else {return Qnil}
}

let eventStore = EKEventStore()
var Qt : emacs_value?
var Qnil : emacs_value?

typealias EmacsDefunCallback  = (@convention(c)
                                 (UnsafeMutablePointer<emacs_env>?,
                                  Int,
                                  UnsafeMutablePointer<emacs_value?>?,
                                                       UnsafeMutableRawPointer?) -> emacs_value?)?

func AuthorizeCalendar(_ env: UnsafeMutablePointer<emacs_env>) -> Bool {
    var status = EKEventStore.authorizationStatus(for: .event)


switch status {
case .notDetermined:
    print("Authorization Status: Not Determined")
case .restricted:
    print("Authorization Status: Restricted")
case .denied:
    print("Authorization Status: Denied")
case .authorized:
    print("Authorization Status: Authorized")
@unknown default:
    print("Authorization Status: Unknown")
}

    if status != EKAuthorizationStatus.authorized {
        let semaphore = DispatchSemaphore(value: 0)
        eventStore.requestAccess(to: .event, completion: {
                                     (accessGranted: Bool, error: Error?) in
                                     semaphore.signal()
                                     // if accessGranted == true {
                                     //     semaphore.signal()
                                     // }
                                     //print("Returned: \(accessGranted) and \(error)")
                                 })
        semaphore.wait()
        status = EKEventStore.authorizationStatus(for: EKEntityType.event)
        if status != EKAuthorizationStatus.authorized {
            // Raise error
            env.pointee.non_local_exit_signal(
              env, env.pointee.intern(env, "not-authorized"), Qnil);
            return false
        }
    }
    return true

    // switch (status) {
    // case EKAuthorizationStatus.notDetermined:
    //     print("notDetermined")

    //     eventStore.requestAccess(to: EKEntityType.event, completion: {
    //                                  (accessGranted: Bool, error: Error?) in

    //                                  if accessGranted == true {
    //                                      print("Granted")
    //                                  } else {
    //                                      print("Denied")
    //                                  }
    //                              })


    // case EKAuthorizationStatus.authorized:
    //     let calendars = eventStore.calendars(for: .event)
    //     for calendar in calendars {
    //         print(calendar.title)
    //     }

    //     case EKAuthorizationStatus.restricted, EKAuthorizationStatus.denied:
    //         print("denied")

    //         default:
    //             print("Others!")
    // }
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


        for calendar in calendars {
            list = emacs_cons(env,
                              emacs_plist(env,
                                          [Qid,
                                           emacs_cast(env, calendar.calendarIdentifier),
                                           Qtitle,
                                           emacs_cast(env, calendar.title),
                                           Qcolor,
                                           emacs_cast(env, calendar.color.hexString!)]),
                              list);
        }
        return list
    }
    return Qnil
}

private func maccalfw_fetch_events(_ env: UnsafeMutablePointer<emacs_env>?,
                          _ nargs: Int,
                          _ args: UnsafeMutablePointer<emacs_value?>?,
                          _ data: UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env, let args, let name = args[0],
       let calendar_name = emacs_str(env, name) {
        if !AuthorizeCalendar(env){
            return Qnil
        }

        if let calendar = eventStore.calendar(withIdentifier: calendar_name) {
            let start = emacs_date(env, args[1]!)
            let end = emacs_date(env, args[2]!)

            var list = Qnil;

            let calendarEventsPredicate =
              eventStore.predicateForEvents(withStart: start,
                                            end: end,
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

                let event_data = [Qid,
                                  emacs_cast(env, event.eventIdentifier),
                                  Qtitle,
                                  emacs_cast(env, event.title),
                                  Qlocation,
                                  emacs_cast(env, event.location),
                                  Qnotes,
                                  event.hasNotes
                                    ? emacs_cast(env, event.notes) : Qnil,
                                  Qstart,
                                  emacs_cast(env, event.startDate),
                                  Qend,
                                  emacs_cast(env, event.endDate),
                                  Qdate,
                                  emacs_cast(env, event.occurrenceDate),
                                  QisDetached,
                                  event.isDetached ? Qt : Qnil,
                                  QisAllDay,
                                  event.isAllDay ? Qt : Qnil,
                                  Qcreated_date,
                                  emacs_cast(env, event.creationDate),
                                  Qlast_modified,
                                  emacs_cast(env, event.lastModifiedDate),
                                  Qstatus,
                                  env.pointee.intern(env,
                                                     String(describing:
                                                              event.status)),
                                  Qorganizer,
                                  event.organizer != nil
                                    ? emacs_cast(env, event.organizer!.name)
                                    : Qnil,
                                  Qurl,
                                  event.url != nil
                                    ? emacs_cast(env,
                                                event.url!.absoluteString)
                                    : Qnil]

                list = emacs_cons(env, emacs_plist(env, event_data), list)
            }
            return list
        }
    }

    return Qnil
}


// TODO: Is it really?
@_cdecl("plugin_is_GPL_compatible")
func plugin_is_GPL_compatible() {}

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
