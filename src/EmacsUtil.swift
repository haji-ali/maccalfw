import Cocoa
import Foundation
import CEmacsModule
import EventKit

@_cdecl("plugin_is_GPL_compatible")
public func plugin_is_GPL_compatible() {}

typealias EmacsDefunCallback  = (@convention(c)
                                 (UnsafeMutablePointer<emacs_env>?,
                                  Int,
                                  UnsafeMutablePointer<emacs_value?>?,
                                  UnsafeMutableRawPointer?) -> emacs_value?)?

enum EmacsError : Error {
    case error(String)
    case wrong_type_argument(String)
}

extension NSColor {
    var hexString: String? {
        guard let color = self.usingColorSpace(.sRGB) else { return nil }

        let red = Int(color.redComponent * 255)
        let green = Int(color.greenComponent * 255)
        let blue = Int(color.blueComponent * 255)

        return String(format: "#%02X%02X%02X", red, green, blue)
    }
}

func toEmacsVal_Enum<T : Hashable>
  (_ env: UnsafeMutablePointer<emacs_env>,
   _ map: [T: String],
   _ t : T) -> emacs_value? {
    if let emacsString = map[t] {
        return emacs_intern(env, emacsString)
    }
    return nil
}

func fromEmacsVal_Enum<T : Hashable>
  (_ env: UnsafeMutablePointer<emacs_env>,
   _ map: [T: String],
   _ v: emacs_value?) throws -> T {
    guard let key = try emacs_symbol_to_string(env, v) else {
        throw EmacsError.wrong_type_argument("Invalid key")
    }

    if let enumvalue = map.first(where: { $0.value == key })?.key {
        return enumvalue
    }
    throw EmacsError.wrong_type_argument("Unrecognized value: \(key)")
}

protocol EmacsCastable {
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value?
}

extension emacs_value : EmacsCastable {
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        return self
    }
}

extension Int  : EmacsCastable {
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        return env.pointee.make_integer(env, self)
    }

    static func fromEmacsVal(_ env: UnsafeMutablePointer<emacs_env>,
                             _ val : emacs_value?) -> Self? {
        if let val, is_not_nil(env, val) {
            return env.pointee.extract_integer(env, val);
        }
        else {
            return nil
        }
    }
}


extension Bool : EmacsCastable {
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        return self ? Qt : Qnil;
    }

    static func fromEmacsVal(_ env: UnsafeMutablePointer<emacs_env>,
                             _ val : emacs_value?) throws -> Self {
        return is_not_nil(env, val)
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
                             _ val : emacs_value?) throws -> Self? {
        if let val, is_not_nil(env, val) {
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
        let timeInterval = self.timeIntervalSince1970
        let seconds = Int(timeInterval)
        let nanoseconds = Int((timeInterval - Double(seconds)) * 1_000_000_000)

        let timespecObj = timespec(tv_sec: seconds, tv_nsec: nanoseconds)
        return env.pointee.make_time(env, timespecObj)
    }

    static func fromEmacsVal(_ env: UnsafeMutablePointer<emacs_env>,
                             _ val : emacs_value?) throws -> Date? {
        // val should be a list of 3 elements having day, month and year.
        if let val, is_not_nil(env, val) {
            let time = env.pointee.extract_time(env, val)
            let timeInterval = TimeInterval(time.tv_sec) + TimeInterval(time.tv_nsec) / 1_000_000_000
            return Date(timeIntervalSince1970: timeInterval)
        }
        else {
            return nil
        }
    }
}

extension Array : EmacsCastable where Element == EmacsCastable?  {
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        var arguments = self.map{$0?.toEmacsVal(env) ?? Qnil}
        let count = arguments.count
        return arguments.withUnsafeMutableBufferPointer{
            bufferPointer in
            env.pointee.funcall(env, emacs_intern(env, "list"),
                                count,
                                bufferPointer.baseAddress!)
        }
    }
}

// extension Tuple  : EmacsCastable where
//   T1 == EmacsCastable? and T2 == EmacsCastable? {
//     func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
//         return emacs_cons()
//     }
// }

extension Dictionary : EmacsCastable where Key == emacs_value?, Value == EmacsCastable?  {
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        let flatArgs : [EmacsCastable?] =
          Array(self.flatMap
                { key, value in return
                                  [key as EmacsCastable?,
                                   value]})
        return flatArgs.toEmacsVal(env)
    }
}

func emacs_funcall(_ env: UnsafeMutablePointer<emacs_env>,
                   _ func_name : emacs_value?,
                   _ args : [emacs_value?]) -> emacs_value? {
    var arguments = args.map{$0 ?? Qnil} // Change nil to Qnil
    let count = arguments.count
    return arguments.withUnsafeMutableBufferPointer{
        bufferPointer in
        env.pointee.funcall(env, func_name,
                            count,
                            bufferPointer.baseAddress!)
    }
}

func emacs_defun(_ env: UnsafeMutablePointer<emacs_env>,
                         _ name: String,
                         _ min: Int,
                         _ max: Int,
                 _ fun: EmacsDefunCallback,
                 _ doc : String?) {
    // TODO Should we be careful with passing strings here?
    let internSymbol = emacs_intern(env, "defalias")
    let functionName = emacs_intern(env, name)
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
        env.pointee.funcall(env, emacs_intern(env, "cons"), 2,
                            bufferPointer.baseAddress!)
    }
}

func fromEmacsVal_list(_ env: UnsafeMutablePointer<emacs_env>,
                      _ val: emacs_value?) -> [emacs_value?] {
    var ret: [emacs_value?] = []
    var cdr: emacs_value? = val
    let Qcar = emacs_intern(env, "car")
    let Qcdr = emacs_intern(env, "cdr")
    while (is_not_nil(env, cdr)) {
        ret.append(emacs_funcall(env, Qcar, [cdr]))
        cdr = emacs_funcall(env, Qcdr, [cdr])
    }
    return ret
}

func emacs_symbol_to_string(_ env: UnsafeMutablePointer<emacs_env>,
                            _ val: emacs_value?) throws -> String? {
    if let val {
        let Qsymbol_name = emacs_intern(env, "symbol-name")
        return try String.fromEmacsVal(env,emacs_funcall(env, Qsymbol_name, [val]))
    }
    return nil
}

func toEmacsVal_plist(_ env: UnsafeMutablePointer<emacs_env>,
                      _ dict : [String: EmacsCastable?]) -> emacs_value?
{
    let quoted_plist =
      Dictionary(uniqueKeysWithValues:
                   dict.map {
                       (emacs_intern(env, $0.key), $0.value) })

    return quoted_plist.filter{$0.value != nil }.toEmacsVal(env)
}

func fromEmacsVal_plist(_ env: UnsafeMutablePointer<emacs_env>,
                        _ val: emacs_value?) throws -> [String : emacs_value] {
    let list_data = fromEmacsVal_list(env, val)
    var result: [String: emacs_value] = [:]

    for index in stride(from: 0, to: list_data.count, by: 2) {
        if index+1 < list_data.count {
            if let key = try emacs_symbol_to_string(env, list_data[index]),
               let value = list_data[index + 1] {
                result[key.hasPrefix(":") ? String(key.dropFirst()) : key] = value
                }
            }
        else{
            throw EmacsError.wrong_type_argument("plist should have an even number of elements")
        }
        }
    return result
}

func emacs_error(_ env: UnsafeMutablePointer<emacs_env>,
                 _ symbol: String,
                 _ msg : String? = nil) {
    env.pointee.non_local_exit_signal(
      env, emacs_intern(env, symbol),
      ([msg]).toEmacsVal(env));
}

func emacs_process_error(_ env: UnsafeMutablePointer<emacs_env>,
                         _ error : Error) {
    if let e_error = error as? EmacsError {
        switch e_error {
        case .wrong_type_argument(let message):
            emacs_error(env, "wrong-type-argument", message)
        case .error(let message):
            emacs_error(env, "error", message)
        }
    }
    else {
        emacs_error(env, "error", "Unexpected error: \(error.localizedDescription)")
    }
}


extension NSNumber  : EmacsCastable {
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        return env.pointee.make_integer(env, self.intValue)
    }
}

func emacs_intern(_ env: UnsafeMutablePointer<emacs_env>,
                  _ symbol: String) -> emacs_value? {
    return env.pointee.intern(env, symbol)
}

func is_not_nil(_ env: UnsafeMutablePointer<emacs_env>,
                _ val: emacs_value?) -> Bool {
    if let val{
        return env.pointee.is_not_nil(env, val)
    }
    return true
}
