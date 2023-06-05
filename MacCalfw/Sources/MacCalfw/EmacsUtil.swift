import Cocoa
import Foundation
import CEmacsModule
import EventKit


// TODO: Is it really?
@_cdecl("plugin_is_GPL_compatible")
func plugin_is_GPL_compatible() {}

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


extension emacs_value : EmacsCastable{
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        return self
    }
}

extension String : EmacsCastable {
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        return env.pointee.make_string(env, self, self.count)
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

func emacs_cons(_ env: UnsafeMutablePointer<emacs_env>,
                        _ a: emacs_value?, _ b: emacs_value?) -> emacs_value? {
    var arguments: [emacs_value?] = [a, b]
    return arguments.withUnsafeMutableBufferPointer{
        bufferPointer in
        env.pointee.funcall(env, env.pointee.intern(env, "cons"), 2,
                            bufferPointer.baseAddress!)
    }
}

func emacs_list(_ env: UnsafeMutablePointer<emacs_env>,
                        _ args: [EmacsCastable?]) -> emacs_value? {
    var arguments = args.map{emacs_cast(env, $0)}
    let count = arguments.count
    return arguments.withUnsafeMutableBufferPointer{
        bufferPointer in
        env.pointee.funcall(env, env.pointee.intern(env, "list"),
                            count,
                            bufferPointer.baseAddress!)
    }
}


func emacs_plist(_ env: UnsafeMutablePointer<emacs_env>,
                         _ args: [emacs_value?: EmacsCastable?]) -> emacs_value? {
    // let arguments = args.enumerated().compactMap { index, element in
    //     if element == nil || index > 0 && args[index - 1] == nil {
    //         return nil
    //     }
    //     return element!
    // }

    let flatArgs : [EmacsCastable?] =
      Array(args.flatMap
            { key, value in return
                              [key as EmacsCastable?,
                               value as EmacsCastable?]})
    return emacs_list(env, flatArgs)
}
