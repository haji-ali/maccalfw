import Cocoa
import Foundation
import CEmacsModule
import EventKit


@_cdecl("plugin_is_GPL_compatible")
public func plugin_is_GPL_compatible() {}

extension NSColor {
    var hexString: String? {
        guard let color = self.usingColorSpace(.sRGB) else { return nil }

        let red = Int(color.redComponent * 255)
        let green = Int(color.greenComponent * 255)
        let blue = Int(color.blueComponent * 255)

        return String(format: "#%02X%02X%02X", red, green, blue)
    }
}

protocol EmacsCastable {
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value?
}


extension emacs_value : EmacsCastable {
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        return self
    }
}

extension String : EmacsCastable {
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        let cString = self.utf8CString
        return cString.withUnsafeBufferPointer { bufferPointer in
            // Note: We pass cString.count-1 as the size to exclude the
            // terminating NULL byte
            env.pointee.make_string(env, bufferPointer.baseAddress,
                                    cString.count-1)
        }
    }

    static func fromEmacsVal(_ env: UnsafeMutablePointer<emacs_env>,
                             _ val : emacs_value?) -> String? {
        if let val, env.pointee.is_not_nil(env, val) {
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
        else {
            return nil
        }
    }
}

extension Date : EmacsCastable {
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        // val should be a list of 3 elements having day, month and year.
        let timeInterval = self.timeIntervalSince1970
        let seconds = Int(timeInterval)
        let nanoseconds = Int((timeInterval - Double(seconds)) * 1_000_000_000)

        let timespecObj = timespec(tv_sec: seconds, tv_nsec: nanoseconds)
        return env.pointee.make_time(env, timespecObj)
    }

    static func fromEmacsVal(_ env: UnsafeMutablePointer<emacs_env>,
                             _ val : emacs_value?) -> Date? {
        // val should be a list of 3 elements having day, month and year.
        if let val, env.pointee.is_not_nil(env, val) {
            let time = env.pointee.extract_time(env, val)
            let timeInterval = TimeInterval(time.tv_sec) + TimeInterval(time.tv_nsec) / 1_000_000_000
            return Date(timeIntervalSince1970: timeInterval)
        }
        else {
            return nil
        }
    }
}

func emacs_funcall(_ env: UnsafeMutablePointer<emacs_env>,
                   _ func_name : emacs_value?,
                   _ args : [emacs_value?]) -> emacs_value? {
    var arguments = args
    let count = arguments.count
    return arguments.withUnsafeMutableBufferPointer{
        bufferPointer in
        env.pointee.funcall(env, func_name,
                            count,
                            bufferPointer.baseAddress!)
    }
}

extension Array : EmacsCastable where Element == EmacsCastable?  {
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        var arguments = self.map{emacs_cast(env, $0)}
        let count = arguments.count
        return arguments.withUnsafeMutableBufferPointer{
            bufferPointer in
            env.pointee.funcall(env, env.pointee.intern(env, "list"),
                                count,
                                bufferPointer.baseAddress!)
        }
    }
}

extension Dictionary : EmacsCastable where Key == emacs_value?, Value == EmacsCastable?  {
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        let flatArgs : [EmacsCastable?] =
          Array(self.flatMap
                { key, value in return
                                  [key as EmacsCastable?,
                                   value as EmacsCastable?]})
        return flatArgs.toEmacsVal(env)
    }
}

extension EKEventAvailability : EmacsCastable  {
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        switch self {
        case .tentative:
            return env.pointee.intern(env, "tentative")
        case .free:
            return env.pointee.intern(env, "free")
        case .busy:
            return env.pointee.intern(env, "busy")
        case .unavailable:
            return env.pointee.intern(env, "unavailable")
        case .notSupported:
            fallthrough
        @unknown default:
            return nil
        }
    }
    static func parse(_ val: String ) -> EKEventAvailability {
        switch val {
        case "notSupported":
            return .notSupported
        case "busy":
            return .busy
        case "free":
            return .free
        case "tentative":
            return .tentative
        case "unavailable":
            return .unavailable
        default:
            return .free // set to default availability
        }
    }
}

extension EKEventStatus : EmacsCastable  {
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        switch self {
        case .confirmed:
            return env.pointee.intern(env, "confirmed")
        case .tentative:
            return env.pointee.intern(env, "tentative")
        case .canceled:
            return env.pointee.intern(env, "cancelled")
        case .none:
            fallthrough
        @unknown default:
            return nil
        }
    }
}

func emacs_cast(_ env: UnsafeMutablePointer<emacs_env>,
                        _ val: EmacsCastable?) -> emacs_value? {
    return val?.toEmacsVal(env) ?? Qnil
}

func emacs_defun(_ env: UnsafeMutablePointer<emacs_env>,
                         _ name: String,
                         _ min: Int,
                         _ max: Int,
                 _ fun: EmacsDefunCallback,
                 _ doc : String?) {
    // TODO Should we be careful with passing strings here?
    let internSymbol = env.pointee.intern(env, "defalias")
    let functionName = env.pointee.intern(env, name)
    let function = env.pointee.make_function(env, min, max, fun,
                                             doc, nil)

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

func emacs_cons(_ env: UnsafeMutablePointer<emacs_env>,
                        _ a: emacs_value?, _ b: emacs_value?) -> emacs_value? {
    var arguments: [emacs_value?] = [a, b]
    return arguments.withUnsafeMutableBufferPointer{
        bufferPointer in
        env.pointee.funcall(env, env.pointee.intern(env, "cons"), 2,
                            bufferPointer.baseAddress!)
    }
}

func emacs_parse_list(_ env: UnsafeMutablePointer<emacs_env>,
                      _ val: emacs_value?) -> [emacs_value?] {
    var ret: [emacs_value?] = []
    var cdr: emacs_value? = val
    let Qcar = env.pointee.intern(env, "car")
    let Qcdr = env.pointee.intern(env, "cdr")
    while (env.pointee.is_not_nil(env, cdr)) {
        ret.append(emacs_funcall(env, Qcar, [cdr]))
        cdr = emacs_funcall(env, Qcdr, [cdr])
    }
    return ret
}

func emacs_symbol_to_string(_ env: UnsafeMutablePointer<emacs_env>,
                            _ val: emacs_value?) -> String? {
    let Qsymbol_name = env.pointee.intern(env, "symbol-name")
    return String.fromEmacsVal(env,emacs_funcall(env, Qsymbol_name, [val]))
}

func emacs_parse_plist(_ env: UnsafeMutablePointer<emacs_env>,
                       _ val: emacs_value?) -> [String?: emacs_value?] {
    let list_data = emacs_parse_list(env, val)
    let keys = stride(from: 0, to: list_data.count, by: 2)
    let values = stride(from: 1, to: list_data.count, by: 2)
    let zipped = zip(keys, values)
    let plist_data =
      Dictionary<String?, emacs_value?> (uniqueKeysWithValues:
       zipped.map { (i, j) in
           (// TODO: Remove colon
             emacs_symbol_to_string(env, list_data[i]), list_data[j])})
    return plist_data
}


func emacs_error(_ env: UnsafeMutablePointer<emacs_env>,
                 _ symbol: String,
                 _ msg : String? = nil) {
    env.pointee.non_local_exit_signal(
      env, env.pointee.intern(env, symbol),
      ([msg] as [EmacsCastable?]).toEmacsVal(env) ?? nil);
}
