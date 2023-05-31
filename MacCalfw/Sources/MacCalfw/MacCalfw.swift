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

func emacs_defun(_ env: UnsafeMutablePointer<emacs_env>,
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


@_cdecl("maccalfw_get_calendars")
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
            list = emacs_cons(env, emacs_str(env, calendar.title), list);
        }
        return Qt
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

        emacs_defun(env, "maccalfw_get_calendars", 2, 2, maccalfw_get_calendars)
    }
    return 0
}
