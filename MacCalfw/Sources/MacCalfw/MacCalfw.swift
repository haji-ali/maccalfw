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


private func emacs_str(_ env: UnsafeMutablePointer<emacs_env>, _ str: String) -> emacs_value? {
    return env.pointee.make_string(env, str, str.count)
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

private func emacs_cstr(_ env: UnsafeMutablePointer<emacs_env>,
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

let eventStore = EKEventStore()
var Qt : emacs_value?
var Qnil : emacs_value?

typealias EmacsDefunCallback  = (@convention(c)
                                 (UnsafeMutablePointer<emacs_env>?,
                                  Int,
                                  UnsafeMutablePointer<emacs_value?>?,
                                                       UnsafeMutableRawPointer?) -> emacs_value?)?

func AuthorizeCalendar() {
    let status = EKEventStore.authorizationStatus(for: EKEntityType.event)
    if status != EKAuthorizationStatus.authorized {
        eventStore.requestAccess(to: EKEntityType.event, completion: {
                                     (accessGranted: Bool, error: Error?) in
                                     if accessGranted == true {
                                         print("Granted!")
                                     }
                                 })
    }
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
    AuthorizeCalendar()

    if let env {
        let calendars = eventStore.calendars(for: .event)
        var list = Qnil;

        for calendar in calendars {
            list = emacs_cons(env,
                              emacs_cons(env,
                                         emacs_str(env, calendar.title),
                                         emacs_str(env, calendar.color.hexString!)),
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
    AuthorizeCalendar()

    if let env, let args, let name = args[0] {
        let calendar = emacs_cstr(env, name)
        let start = env.pointee.extract_integer(env, args[1]!)
        let end = env.pointee.extract_integer(env, args[2]!)

        var list = Qnil;

        let calendarEventsPredicate =
          eventStore.predicateForEvents(withStart: Date().addingTimeInterval(start*60*60*24*365),
                                        end: Date().addingTimeInterval(60*60*24*365),
                                        calendars: [calendar])

        let events = eventStore.events(matching: calendarEventsPredicate)

        for event in events {
            let eventTitle = event.title
            let eventStartDate = event.startDate ?? Date()
            let eventEndDate = event.endDate ?? Date()
            let eventSummary = event.summary
            let eventColor = event.color.hexString
            let eventLocation = event.location
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
