public enum ICalElispDecodeError: Error, CustomStringConvertible {
    case unexpectedEnd
    case unexpectedCharacter(Character, expected: String)

    public var description: String {
        switch self {
        case .unexpectedEnd:
            return "Unexpected end of input while parsing elisp sexp"
        case .unexpectedCharacter(let ch, let expected):
            return "Unexpected character '\(ch)', expected \(expected)"
        }
    }
}

/// Parses the elisp sexp shape produced by `ICalElispEncoder` -- a list of
/// `(NAME PARAMS VALUE)` triples, where PARAMS is `nil` or a flat list
/// alternating param-name symbols and param-value strings -- back into
/// `[ICalProperty]`. This is a small hand-written recursive-descent
/// parser for exactly this restricted grammar, not a general elisp
/// reader.
public enum ICalElispDecoder {
    public static func decode(_ text: String) throws -> [ICalProperty] {
        var scanner = Scanner(text)
        scanner.skipWhitespace()
        try scanner.expect("(")
        var properties: [ICalProperty] = []
        scanner.skipWhitespace()
        while !scanner.peek(")") {
            properties.append(try parseProperty(&scanner))
            scanner.skipWhitespace()
        }
        try scanner.expect(")")
        return properties
    }

    private static func parseProperty(_ scanner: inout Scanner) throws -> ICalProperty {
        try scanner.expect("(")
        scanner.skipWhitespace()
        let name = try scanner.readSymbol()
        scanner.skipWhitespace()
        let params = try parseParams(&scanner)
        scanner.skipWhitespace()
        let value = try scanner.readStringLiteral()
        scanner.skipWhitespace()
        try scanner.expect(")")
        return ICalProperty(name, params: params, value: value)
    }

    private static func parseParams(_ scanner: inout Scanner) throws -> [ICalParam] {
        if scanner.tryConsumeSymbol("nil") {
            return []
        }
        try scanner.expect("(")
        var params: [ICalParam] = []
        scanner.skipWhitespace()
        while !scanner.peek(")") {
            let pname = try scanner.readSymbol()
            scanner.skipWhitespace()
            let pvalue = try scanner.readStringLiteral()
            params.append(ICalParam(pname, pvalue))
            scanner.skipWhitespace()
        }
        try scanner.expect(")")
        return params
    }

    private struct Scanner {
        let chars: [Character]
        var pos: Int = 0

        init(_ s: String) { chars = Array(s) }

        func peekChar() -> Character? { pos < chars.count ? chars[pos] : nil }

        func peek(_ ch: Character) -> Bool { peekChar() == ch }

        mutating func skipWhitespace() {
            while let ch = peekChar(), ch.isWhitespace { pos += 1 }
        }

        mutating func expect(_ ch: Character) throws {
            guard let c = peekChar() else { throw ICalElispDecodeError.unexpectedEnd }
            guard c == ch else {
                throw ICalElispDecodeError.unexpectedCharacter(c, expected: "'\(ch)'")
            }
            pos += 1
        }

        /// Reads a bare symbol (stops at whitespace or a parenthesis).
        mutating func readSymbol() throws -> String {
            let start = pos
            while let ch = peekChar(), !ch.isWhitespace, ch != "(", ch != ")" {
                pos += 1
            }
            guard pos > start else { throw ICalElispDecodeError.unexpectedEnd }
            return String(chars[start..<pos])
        }

        /// Tries to consume SYMBOL as a whole token (not a prefix of a
        /// longer symbol); backtracks and returns false if it doesn't
        /// match.
        mutating func tryConsumeSymbol(_ symbol: String) -> Bool {
            let save = pos
            guard let sym = try? readSymbol(), sym == symbol else {
                pos = save
                return false
            }
            return true
        }

        /// Reads a double-quoted elisp string literal, unescaping `\\`
        /// and `\"` (the only two escapes `ICalElispEncoder` emits).
        mutating func readStringLiteral() throws -> String {
            try expect("\"")
            var out = ""
            while true {
                guard let ch = peekChar() else { throw ICalElispDecodeError.unexpectedEnd }
                pos += 1
                if ch == "\"" {
                    return out
                } else if ch == "\\" {
                    guard let escaped = peekChar() else {
                        throw ICalElispDecodeError.unexpectedEnd
                    }
                    pos += 1
                    out.append(escaped)
                } else {
                    out.append(ch)
                }
            }
        }
    }
}
