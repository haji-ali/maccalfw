import Foundation
import CEmacsModule
import EventKit

////////////////////////////////////////////////////

// TODO: Avoid having global variables? Maybe we can use function data or
// `make_user_ptr`instead
let eventStore = EKEventStore()
var Qt : emacs_value?
var Qnil : emacs_value?

protocol iCalCastable  {
    func toiCal() -> String
    static func fromiCal(_ v : String ) throws -> Self
}

extension String : iCalCastable {
    func toiCal() -> String {
        return self
    }
    static func fromiCal(_ v : String ) throws -> Self
    {
        return Self(v)
    }
}

protocol iCalEnumCastable: iCalCastable, Hashable {
    static var enumMap: [Self: String] { get }
}

extension Int : iCalCastable {
    func toiCal() -> String {
        return String(self)
    }

    static func fromiCal(_ val : String) throws -> Self {
        guard let ret = Self(val) else {
            throw EmacsError.error("Unable to parse integer") // TODO: a better class?
        }
        return ret
    }
}

extension Array : iCalCastable where Element == iCalCastable?  {
    func toiCal() -> String {
        let elem = self.compactMap{$0?.toiCal() ?? nil}
        return elem.joined(separator: ",")
    }
    static func fromiCal(_ v : String ) throws -> Self
    {
        // TODO
        return Self()
    }
}

////////////////////////////////////////////////////

extension iCalEnumCastable {
    func toiCal() -> String {
        return Self.enumMap[self]!.uppercased()
    }

    static func fromiCal(_ v: String) throws -> Self {
        let lv = v.lowercased()
        if let tmp = Self.enumMap.first(where: { $0.value == lv }) {
            return tmp.key
        }
        throw EmacsError.error("Invalid value when parsing \(String(describing: Self.self))")
    }
}

extension EKEventAvailability : EmacsEnumCastable, iCalEnumCastable  {
    static let enumMap: [Self : String] = [
      .tentative: "tentative",
      .free: "free",
      .busy: "busy",
      .unavailable: "unavailable",
      .notSupported: "notSupported"]
}

extension EKRecurrenceFrequency : EmacsEnumCastable, iCalEnumCastable  {
    static let enumMap: [Self: String] = [
      .daily: "daily",
      .weekly: "weekly",
      .monthly: "monthly",
      .yearly: "yearly"]

}

extension EKWeekday : EmacsEnumCastable, iCalEnumCastable {
    static let enumMap: [Self: String] = [
        .sunday: "su",
        .monday: "mo",
        .tuesday: "tu",
        .wednesday: "we",
        .thursday: "th",
        .friday: "fr",
        .saturday: "sa"
    ]
}

extension EKEventStatus : EmacsEnumCastable, iCalEnumCastable  {
    static let enumMap: [Self: String] = [
      .none: "none",
      .confirmed: "confirmed",
      .tentative: "tentative",
      .canceled: "cancelled"]
}

extension Date : iCalCastable {
    func toiCal() -> String {
        // TODO See https://developer.apple.com/documentation/foundation/iso8601dateformatter
        let dateFormatter = DateFormatter()
        dateFormatter.locale = Locale(identifier: "en_US_POSIX")
        dateFormatter.dateFormat = "yyyyMMdd'T'HHmmss'Z'"
        return dateFormatter.string(from: self)
    }

    static func fromiCal(_ dateString: String) throws -> Self {
        // TODO See https://developer.apple.com/documentation/foundation/iso8601dateformatter
        let dateFormatter = DateFormatter()
        dateFormatter.locale = Locale(identifier: "en_US_POSIX")
        dateFormatter.dateFormat = "yyyyMMdd'T'HHmmss'Z'"
        dateFormatter.timeZone = TimeZone(secondsFromGMT: 0)
        if let date = dateFormatter.date(from: dateString) {
            return date
        }
        throw EmacsError.error("Failed to parse iCal date: \(dateString)")
    }

    // More custom implementation for Emacs casting
    func toiCal_list(_ all_day : Bool = false,
                _ timeZoneId : String? = nil) -> [EmacsCastable?] {
        var params : [EmacsCastable?]? = nil
        // TODO See https://developer.apple.com/documentation/foundation/iso8601dateformatter
        let dateFormatter = DateFormatter()
        dateFormatter.locale = Locale(identifier: "en_US_POSIX")
        if all_day {
            params = [EmacsQuote("VALUE"), "DATE"]
            dateFormatter.dateFormat = "yyyyMMdd"
        }
        else {
            if let timeZoneId {
                params = [EmacsQuote("TZID"), timeZoneId]
            }
            dateFormatter.dateFormat = "yyyyMMdd'T'HHmmss"
            if timeZoneId == nil{
                dateFormatter.dateFormat += "'Z'"
            }
        }
        return [params, dateFormatter.string(from: self)]
    }

    static func fromiCal_list(_ env: PEmacsEnv, _ v: [emacs_value?]) throws ->
      (date: Self, isAllDay: Bool, timeZone: String?) {
        guard v.count == 2, let dateString = String.fromEmacsVal(env, v[1]) else {
            throw EmacsError.error("Invalid iCal input format: \(v.count)")
        }

        // TODO See https://developer.apple.com/documentation/foundation/iso8601dateformatter
        let dateFormatter = DateFormatter()
        dateFormatter.locale = Locale(identifier: "en_US_POSIX")
        let params = fromEmacsVal_list(env, v[0])
        if params.count >= 2 {
            // Check for "VALUE=DATE" (all-day event)
            let qSymbolName = emacs_intern(env, "symbol-name")
            if let skey = emacs_funcall(env, qSymbolName, [params[0]]),
               let key = String.fromEmacsVal(env, skey),
               let value = String.fromEmacsVal(env, params[1]) {
                if key == "VALUE", value == "DATE" {
                    dateFormatter.dateFormat = "yyyyMMdd"
                    if let date = dateFormatter.date(from: dateString) {
                        return (date, true, nil)
                    }
                }
                // Check for "TZID=<timeZoneId>" (specific time zone)
                if key == "TZID" {
                    let timeZoneId = value
                    dateFormatter.dateFormat = "yyyyMMdd'T'HHmmss"
                    dateFormatter.timeZone = TimeZone(identifier: timeZoneId)
                    if let date = dateFormatter.date(from: dateString) {
                        return (date, false, timeZoneId)
                    }
                }
            }
        } else {
            // Default case: assume UTC, not all day event
            dateFormatter.dateFormat = "yyyyMMdd'T'HHmmss'Z'"
            dateFormatter.timeZone = TimeZone(secondsFromGMT: 0)
            if let date = dateFormatter.date(from: dateString) {
                return (date, false, nil)
            }
        }
        throw EmacsError.error("Failed to parse iCal date")
    }
}

extension EKEvent : EmacsCastable {
    func toEmacsVal(_ env: PEmacsEnv) -> emacs_value? {
        let event_plist : [String : EmacsCastable?] =
          [":id" : self.eventIdentifier,
           ":calendar-id" : self.calendar.calendarIdentifier,
           ":title" :self.title,
           ":location" : self.location,
           ":notes" : self.hasNotes ? self.notes : nil,
           ":start" : self.startDate,
           ":end" : self.endDate,
           ":occurrence-date" : self.occurrenceDate,
           ":detached-p" : self.isDetached ? Qt : nil,
           ":all-day-p" : self.isAllDay ? Qt : nil,
           ":created-date" : self.creationDate,
           ":last-modified" : self.lastModifiedDate,
           ":timezone" : self.timeZone?.identifier,
           ":status" : self.status,
           ":availability" : self.availability,
           ":organizer" : self.organizer?.name,
           ":recurrence" : self.hasRecurrenceRules ?
             (self.recurrenceRules as [EmacsCastable?]?) : nil,
           ":organizer-current-user" : (self.organizer?.isCurrentUser ?? true) ? nil : Qt,
           ":read-only" : self.calendar.allowsContentModifications ? nil : Qt,
           ":url" : self.url?.absoluteString]

        return toEmacsVal_plist(env, event_plist)
    }

    func updateFromPList(_ env: PEmacsEnv,
                         _ eventData: [String : emacs_value]) throws {
        if let tmp = eventData["calendar-id"] {
            let calendar_id = String.fromEmacsVal(env, tmp)
            if let calendar_id,
               let calendar = eventStore.calendar(withIdentifier: calendar_id) {
                self.calendar = calendar
            }
            else {
                throw EmacsError.error("Cannot retrieve calendar")
            }
        }

        if let tmp = eventData["title"] {
            self.title = String.fromEmacsVal(env, tmp)
        }
        if let tmp = eventData["recurrence"] {
            self.recurrenceRules = try fromEmacsVal_list(env, tmp).map{
                try EKRecurrenceRule.fromEmacsVal(env, $0)}
        }
        if let tmp = eventData["start"] {
            self.startDate = Date.fromEmacsVal(env, tmp)
        }
        if let tmp = eventData["end"] {
            self.endDate = Date.fromEmacsVal(env, tmp)
        }
        if let tmp = eventData["location"] {
            let str = String.fromEmacsVal(env, tmp)
            self.location = (str?.isEmpty ?? true) ? nil : str
        }
        if let tmp = eventData["notes"] {
            self.notes = String.fromEmacsVal(env, tmp)
        }
        if let tmp = eventData["url"] {
            let str = String.fromEmacsVal(env, tmp)
            self.url = ((str?.isEmpty ?? true) ? nil
                          : URL(string: str!))
        }
        if let tmp = eventData["timezone"] {
            let str = String.fromEmacsVal(env, tmp)
            self.timeZone = ((str?.isEmpty ?? true) ? nil
                               : TimeZone(identifier: str!))
        }

        if let tmp = eventData["all-day-p"] {
            self.isAllDay = is_not_nil(env, tmp)
        }
        if let tmp = eventData["availability"] {
            self.availability = try EKEventAvailability.fromEmacsVal(env, tmp)
        }
        // Read-only: occurrenceDate, organizer, last_modified, created_date
        // detached-p, status
    }

    func toiCal() -> EmacsCastable {
        let event_plist : [String : EmacsCastable?] =
          ["UID" : self.eventIdentifier,
           "SUMMARY" : self.title,
           "LOCATION" : self.location,
           "DESCRIPTION" : self.hasNotes ? self.notes : nil,
           "DTSTART" : self.startDate?.toiCal_list(self.isAllDay,
                                                   self.timeZone?.identifier),
           // NOTE: iCal and apple calendar support different timezones, but
           // not EKEvent, it seems
           "DTEND" : self.endDate?.toiCal_list(self.isAllDay,
                                               self.timeZone?.identifier),
           "DTSTAMP" : self.creationDate?.toiCal(),
           "LAST-MODIFIED" : self.lastModifiedDate?.toiCal(),
           "STATUS" : self.status.toiCal(),
           "ORGANIZER" : self.organizer?.name,
           "URL" : self.url?.absoluteString,
           // TRANSP Doesn't support tentative nor notsupported.
           "TRANSP" : (self.availability == EKEventAvailability.free) ?
             "TRANSPARENT" : "OPAQUE",
           "X-EMACS-AVAILABILITY" : self.availability.toiCal(),
           "X-EMACS-CALID" : self.calendar.calendarIdentifier,
           "X-EMACS-READ-ONLY" : self.calendar.allowsContentModifications ? nil : "yes",
           // These are currently unused in maccalfw.el
           "X-EMACS-OCCURENCE-DATE" : self.occurrenceDate.toiCal(),
           "X-EMACS-DETACHED-P" : self.isDetached ? "yes" : nil,
           "X-EMACS-ORGANIZER-CURRENT-USER" : (self.organizer?.isCurrentUser ?? true) ? nil : "yes"]

        var flatArgs : [EmacsCastable?] =
          event_plist.compactMap { (key, value) in
              guard let unwrappedValue = value else { return nil }
              return [EmacsQuote(key)] +
                ((unwrappedValue as? [EmacsCastable?]) ?? [nil, value])
          }


        if self.hasRecurrenceRules, let rules = self.recurrenceRules {
            for rule in rules {
                flatArgs.append([EmacsQuote("RRULE"), nil, rule.toiCal()])
            }
        }
        return flatArgs
    }

    func updateFromiCalAList(_ env: PEmacsEnv,
                             _ eventData: [(key: String,
                                            value: [emacs_value?])]) throws {
        for item in eventData {
            let element_raw = item.value.count > 1 ? item.value[1] : nil
            switch item.key {
            case "SUMMARY":
                self.title = String.fromEmacsVal(env, element_raw)

            case "LOCATION":
                let str = String.fromEmacsVal(env, element_raw)
                self.location = (str?.isEmpty ?? true) ? nil : str

            case "DESCRIPTION":
                self.notes = String.fromEmacsVal(env, element_raw)

            case "DTSTART":
                let date = try Date.fromiCal_list(env, item.value)
                self.startDate = date.date
                self.isAllDay = date.isAllDay
                self.timeZone = ((date.timeZone?.isEmpty ?? true) ? nil
                                   : TimeZone(identifier: date.timeZone!))

            case "DTEND":
                let date = try Date.fromiCal_list(env, item.value)
                self.endDate = date.date
                // NOTE: We'll ignore timeZone from endDate for now
                // while we figure out a solution for EKEvent
                // self.isAllDay = date.isAllDay
                // self.timeZone = ((date.timeZone?.isEmpty ?? true) ? nil
                //                    : TimeZone(identifier: date.timeZone!))

            case "X-EMACS-AVAILABILITY":
                self.availability = try EKEventAvailability.fromiCal(
                  String.fromEmacsVal(env, element_raw) ?? "")

            case "URL":
                let str = String.fromEmacsVal(env, element_raw)
                self.url = ((str?.isEmpty ?? true) ? nil : URL(string: str!))

            case "X-EMACS-CALID":
                let calendarId = String.fromEmacsVal(env, element_raw)
                if let calendarId,
                   let calendar = eventStore.calendar(withIdentifier: calendarId) {
                    self.calendar = calendar
                } else {
                    throw EmacsError.error("Cannot retrieve calendar")
                }

            case "RRULE":
                guard let ruleString = String.fromEmacsVal(env, element_raw)
                else {
                    throw EmacsError.error("RRULE should have a string element.")
                }
                self.recurrenceRules = nil
                self.addRecurrenceRule(try EKRecurrenceRule.fromiCal(ruleString))

            default:
                // Ignore unhandled keys
                break
            }
        }
    }
}

extension EKRecurrenceDayOfWeek : EmacsCastable {
    func toEmacsVal(_ env: PEmacsEnv) -> emacs_value? {
        let plist : [String : EmacsCastable?] =
          [":week-day" : self.dayOfTheWeek,
           ":week-number" : self.weekNumber]

        return toEmacsVal_plist(env, plist)
    }

    static func fromEmacsVal(_ env: PEmacsEnv,
                             _ val: emacs_value?) throws -> Self {
      let data = try fromEmacsVal_plist(env, val)
      let dayOfTheWeek  = try EKWeekday.fromEmacsVal(env, data["week-day"])
      if let tmp = data["week-number"] {
          let weekNumber : Int = Int.fromEmacsVal(env, tmp) ?? 0
          return Self(dayOfTheWeek: dayOfTheWeek, weekNumber: weekNumber)
      }
      return Self.init(dayOfTheWeek)
    }
}

extension EKRecurrenceDayOfWeek : iCalCastable {
    func toiCal() -> String {
        let str = self.dayOfTheWeek.toiCal()
        if self.weekNumber > 0 {
            return String(self.weekNumber) + str
        }
        return str
    }

    static func fromiCal(_ val : String) throws -> Self {
        // Define the Swift Regex pattern with groups
        let regex = #/^(?:([-+]?\d+))?([A-Z]+)$/#

        // Match the input string
        guard let match = try? regex.wholeMatch(in: val) else {
            throw EmacsError.error("Invalid iCal string format: \(val)")
        }
        // Extract dayOfWeek and optional weekNumber
        let dayOfWeek = try EKWeekday.fromiCal(String(match.output.2))
        let weekNumber = match.output.1.flatMap { Int($0) } ?? 0
        // Return the constructed EKRecurrenceDayOfWeek
        return Self(dayOfWeek, weekNumber: weekNumber)
    }
}

extension EKRecurrenceRule : EmacsCastable {
    func toEmacsVal(_ env: PEmacsEnv) -> emacs_value? {
        let plist : [String : EmacsCastable?] =
          [":end-date" : self.recurrenceEnd?.endDate,
           ":occurrence-count" : self.recurrenceEnd?.occurrenceCount,
           // Seems to just be "gregorian!"
           // ":calendar-id": self.calendarIdentifier,
           ":interval": self.interval,
           ":frequency": self.frequency,
           ":week-first-day": self.firstDayOfTheWeek,
           ":week-days": self.daysOfTheWeek as [EmacsCastable?]?,
           ":month-days": self.daysOfTheMonth?.compactMap{$0.intValue},
           ":year-days": self.daysOfTheYear?.compactMap{$0.intValue},
           ":year-weeks": self.weeksOfTheYear?.compactMap{$0.intValue},
           ":year-months": self.monthsOfTheYear?.compactMap{$0.intValue},
           ":set-positions": self.setPositions?.compactMap{$0.intValue}]

        return toEmacsVal_plist(env, plist)
    }

    static func fromEmacsVal(_ env: PEmacsEnv,
                             _ val: emacs_value?) throws -> Self {
        let data = try fromEmacsVal_plist(env, val)
        let freq = try EKRecurrenceFrequency.fromEmacsVal(env, data["frequency"])
        // Default to an interval of 1
        let interval = Int.fromEmacsVal(env, data["interval"]) ?? 1
        var daysOfTheWeek: [EKRecurrenceDayOfWeek]? = nil
        var end: EKRecurrenceEnd? = nil
        var daysOfTheMonth, daysOfTheYear,
              weeksOfTheYear, monthsOfTheYear, setPositions : [NSNumber]?

        // Simple rule
        if let tmp = data["end-date"],
           let val = Date.fromEmacsVal(env, tmp)  {
            end = EKRecurrenceEnd(end: val)
        }
        if let tmp = data["occurrence-count"],
           let val = Int.fromEmacsVal(env, tmp) {
            end = EKRecurrenceEnd(occurrenceCount: val)
        }
        // Complex rule
        if let tmp = data["week-days"] {
            daysOfTheWeek = try fromEmacsVal_list(env, tmp).map{
                try EKRecurrenceDayOfWeek.fromEmacsVal(env, $0)}
        }
        if let tmp = data["month-days"] {
            daysOfTheMonth = fromEmacsVal_list(env, tmp).compactMap{
                Int.fromEmacsVal(env, $0).map(NSNumber.init)}
        }
        if let tmp = data["year-days"] {
            daysOfTheYear = fromEmacsVal_list(env, tmp).compactMap{
                Int.fromEmacsVal(env, $0).map(NSNumber.init)}
        }
        if let tmp = data["year-weeks"] {
            weeksOfTheYear = fromEmacsVal_list(env, tmp).compactMap{
                Int.fromEmacsVal(env, $0).map(NSNumber.init)}
        }
        if let tmp = data["year-months"] {
            monthsOfTheYear = fromEmacsVal_list(env, tmp).compactMap{
                Int.fromEmacsVal(env, $0).map(NSNumber.init)}
        }
        if let tmp = data["set-positions"] {
            setPositions =  fromEmacsVal_list(env, tmp).compactMap{
                Int.fromEmacsVal(env, $0).map(NSNumber.init)}
        }
        return Self(recurrenceWith: freq,
                    interval: interval,
                    daysOfTheWeek: daysOfTheWeek,
                    daysOfTheMonth: daysOfTheMonth,
                    monthsOfTheYear: monthsOfTheYear,
                    weeksOfTheYear: weeksOfTheYear,
                    daysOfTheYear: daysOfTheYear,
                    setPositions: setPositions,
                    end: end)
    }
}

extension EKRecurrenceRule : iCalCastable {
    func toiCal() -> String {
        let rrule : [String : iCalCastable?] =
          ["UNTIL" : self.recurrenceEnd?.endDate,
           "COUNT" :
             (self.recurrenceEnd?.occurrenceCount).flatMap { $0 > 0 ? $0 : nil },
           "INTERVAL": self.interval,
           "FREQ": self.frequency,
           "WKST": self.firstDayOfTheWeek,
           "BYDAY": self.daysOfTheWeek as [iCalCastable?]?,
           "BYMONTHDAY": self.daysOfTheMonth?.compactMap{$0.intValue},
           "BYYEARDAY": self.daysOfTheYear?.compactMap{$0.intValue},
           "BYWEEKNO": self.weeksOfTheYear?.compactMap{$0.intValue},
           "BYMONTH": self.monthsOfTheYear?.compactMap{$0.intValue},
           "BYSETPOS": self.setPositions?.compactMap{$0.intValue}]

        // For now, just join then in KEY=VAL;
        return rrule.compactMap{ (key, value) -> String? in
            guard let value else {
                return nil
            }
            return "\(key)=\(value.toiCal())"
        }.joined(separator: ";")
    }

    static func fromiCal(_ val: String) throws -> Self {
        let components = val.components(separatedBy: ";")
        let result = components.map { component in
            let keyValue = component.split(separator: "=",
                                           maxSplits: 1).map(String.init)
            if keyValue.count == 2 {
                // Split the value by "," for multi-value keys
                return [keyValue[0]] + keyValue[1].split(
                  separator: ",").map(String.init)
            }
            return [keyValue[0]]
        }

        var freq = EKRecurrenceFrequency.daily
        // Default to an interval of 1
        var interval : Int = 1
        var daysOfTheWeek: [EKRecurrenceDayOfWeek]? = nil
        var end: EKRecurrenceEnd? = nil
        var daysOfTheMonth, daysOfTheYear,
            weeksOfTheYear, monthsOfTheYear, setPositions : [NSNumber]?

        for field in result {
            switch field[0] {
            case "FREQ":
                freq = try EKRecurrenceFrequency.fromiCal(field[1])
            case "UNTIL":
                end = EKRecurrenceEnd(end: try Date.fromiCal(field[1]))
            case "BYDAY":
                daysOfTheWeek = try field.dropFirst().compactMap {
                    try EKRecurrenceDayOfWeek.fromiCal($0) }
            default:
                let parsed = field.dropFirst().compactMap{
                    Int($0).map(NSNumber.init)}
                switch field[0] {
                case "WKST":
                    if parsed[0] != 2 {
                        throw EmacsError.error("Cannot handle non-standard week start.")
                    }
                case "COUNT":
                    end = EKRecurrenceEnd(occurrenceCount: parsed[0].intValue)
                case "INTERVAL":
                    interval = parsed[0].intValue
                case "BYMONTHDAY":
                    daysOfTheMonth = parsed
                case "BYMONTH":
                    monthsOfTheYear = parsed
                case "BYWEEKNO":
                    weeksOfTheYear = parsed
                case "BYYEARDAY":
                    daysOfTheYear = parsed
                case "BYSETPOS":
                    setPositions = parsed
                default:
                    throw EmacsError.error("Invalid format for recurrence rule")
                }
            }
        }
        return Self(recurrenceWith: freq,
                    interval: interval,
                    daysOfTheWeek: daysOfTheWeek,
                    daysOfTheMonth: daysOfTheMonth,
                    monthsOfTheYear: monthsOfTheYear,
                    weeksOfTheYear: weeksOfTheYear,
                    daysOfTheYear: daysOfTheYear,
                    setPositions: setPositions,
                    end: end)
    }
}


private func AuthorizeCalendar(_ env: PEmacsEnv) throws {
    switch EKEventStore.authorizationStatus(for: .event) {
    case .authorized: fallthrough
    case .fullAccess:
        return
    case .notDetermined:
        let semaphore = DispatchSemaphore(value: 0)
        eventStore.requestFullAccessToEvents(completion: {
                                     (accessGranted: Bool, error: Error?) in
                                     semaphore.signal()})
        semaphore.wait()
        let status = EKEventStore.authorizationStatus(for: EKEntityType.event)
        if status == EKAuthorizationStatus.fullAccess {
            return
        }
        throw EmacsError.error("authorization-failed")
    case .restricted, .denied, .writeOnly:
        fallthrough
    @unknown default:
        throw EmacsError.error("not-authorized")
    }
}

private func getEKEvent(_ event_id : String,
                        _ start: Date?) throws -> EKEvent {
    let event_data = eventStore.event(withIdentifier: event_id)
    if let event_data {
        if let start, event_data.startDate != start {
            let delta : TimeInterval = 60*60
            let calendarEventsPredicate =
              eventStore.predicateForEvents(withStart: start-delta/2,
                                            end: start+delta/2,
                                            calendars: nil)
            let events = eventStore.events(matching: calendarEventsPredicate)
            // Find event with the same id and start date
            if let event = (events.first(
                              where: { $0.eventIdentifier == event_id &&
                                         $0.startDate == start})){
                return event
            }
        }
        return event_data
    }
    throw EmacsError.error("Failed to fetch event.")
}

private func maccalfw_get_calendars(_ env: PEmacsEnv?,
                          _ nargs: Int,
                          _ args: POptEmacsValue?,
                          _ : UnsafeMutableRawPointer?) -> emacs_value? {
    if let env {
        do {
            try AuthorizeCalendar(env)

            let calendars = eventStore.calendars(for: .event)
            let Qid = emacs_intern(env, ":id")
            let Qtitle = emacs_intern(env, ":title")
            let Qcolor = emacs_intern(env, ":color")
            let Qeditable = emacs_intern(env, ":editable")
            let Qdefault = emacs_intern(env, ":default")
            let defaultCal = eventStore.defaultCalendarForNewEvents?.calendarIdentifier
            let list : [EmacsCastable?] =
              calendars.map {
                  var calendar_data : [emacs_value? : EmacsCastable?] =
                    [Qid : $0.calendarIdentifier,
                     Qtitle: $0.title,
                     Qcolor : $0.color.hexString,
                     Qeditable :
                       ($0.allowsContentModifications ? Qt : nil)]
                  if ($0.calendarIdentifier == defaultCal){
                      calendar_data[Qdefault] = Qt
                  }
                  return calendar_data
              }
            return list.toEmacsVal(env)
        }
        catch {
            emacs_process_error(env, error)
        }
    }
    return Qnil
}

private func maccalfw_fetch_events(
  _ env: PEmacsEnv?,
  _ nargs: Int,
  _ args: POptEmacsValue?,
  _ : UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env {
        do {
            if let args, let arg0 = args[0],
               let argStart = args[1],
               let argEnd = args[2] {
                var calendar_ids : [String?]? = nil
                if is_not_nil(env, arg0){
                    if is_not_nil(
                         env,
                         emacs_funcall(env,emacs_intern(env, "stringp"),
                                       [arg0])){
                        calendar_ids = [String.fromEmacsVal(env, arg0)]
                    }
                    else if is_not_nil(
                              env,
                              emacs_funcall(env,emacs_intern(env, "listp"),
                                            [arg0])){
                        calendar_ids = fromEmacsVal_list(env, arg0).map{
                            String.fromEmacsVal(env, $0)}
                    }
                    else {
                        throw EmacsError.wrong_type_argument(
                          "First argument can be nil, a list or a string")
                    }
                }

                try AuthorizeCalendar(env)
                let calendars = try calendar_ids?.map{
                    if let id=$0,
                       let cal = eventStore.calendar(withIdentifier: id) {
                        return cal
                    }
                    else {
                        throw EmacsError.error("Unable to fetch one of the calendars.")
                    }}

                let start = Date.fromEmacsVal(env, argStart)
                let end = Date.fromEmacsVal(env, argEnd)

                if let start, let end {
                    let calendarEventsPredicate =
                      eventStore.predicateForEvents(withStart: start,
                                                    end: end,
                                                    calendars: calendars)
                    let events = eventStore.events(matching:
                                                     calendarEventsPredicate)
                    return (events.map{$0.toiCal()}).toEmacsVal(env)
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
  _ env: PEmacsEnv?,
  _ nargs: Int,
  _ args: POptEmacsValue?,
  _ : UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env {
        do {
            try AuthorizeCalendar(env)
            if let args, let eventId = String.fromEmacsVal(env, args[0]) {
                let start = nargs > 1 ? Date.fromEmacsVal(env, args[1]) : nil
                let event_data = try getEKEvent(eventId, start)
                return event_data.toiCal().toEmacsVal(env)
            }
        }
        catch {
            emacs_process_error(env, error)
        }
    }
    return Qnil
}

private func maccalfw_update_event(
  _ env: PEmacsEnv?,
  _ nargs: Int,
  _ args: POptEmacsValue?,
  _ : UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env {
        do {
            if let args, let arg0 = args[0], let arg1 = args[1] {
                try AuthorizeCalendar(env)
                let eventId = String.fromEmacsVal(env, arg0)

                let start = nargs > 1 ? Date.fromEmacsVal(env, args[2]) : nil
                let future = nargs > 2 ? Bool.fromEmacsVal(env, args[3]) : false
                let event : EKEvent

                // Transform the data
                let qSymbolName = emacs_intern(env, "symbol-name")
                let eventData = fromEmacsVal_list(env, arg1).map {
                    let lst = fromEmacsVal_list(env, $0);
                    guard let first = lst.first,
                          let sName = emacs_funcall(env, qSymbolName, [first]),
                          let sfirst = String.fromEmacsVal(env, sName)
                    else {return (key: "", value: lst) }
                    return (key : sfirst,
                            value: Array(lst.dropFirst()))}

                if let eventId  {
                    event = try getEKEvent(eventId, start)
                }
                else {
                    event = EKEvent(eventStore: eventStore)
                }

                try event.updateFromiCalAList(env, eventData)


                do {
                    try eventStore.save(event,
                                        span: future ? .futureEvents : .thisEvent,
                                        commit: true)
                    return event.toEmacsVal(env)
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
  _ env: PEmacsEnv?,
  _ nargs: Int,
  _ args: POptEmacsValue?,
  _ : UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env, let args {
        do {
            try AuthorizeCalendar(env)
            if let arg0 = args[0], let eventId = String.fromEmacsVal(env, arg0) {
                let start = nargs > 1 ? Date.fromEmacsVal(env, args[1]) : nil
                let future = nargs > 2 ? Bool.fromEmacsVal(env, args[2]) : false
                let event = try getEKEvent(eventId, start)

                do {
                    try eventStore.remove(event,
                                          span: future ? .futureEvents : .thisEvent,
                                          commit: true)
                }
                catch {
                    throw EmacsError.error("Failed to remove event with error: \(error.localizedDescription)")
                }
                return Qt
            }
        }
        catch {
            emacs_process_error(env, error)
        }
    }
    return Qnil
}


private func maccalfw_timezones(
  _ env: PEmacsEnv?,
  _ nargs: Int,
  _ args: POptEmacsValue?,
  _ : UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env {
        do {
            try AuthorizeCalendar(env)
        let Qname = emacs_intern(env, ":name")
        let Qabbrev = emacs_intern(env, ":abbrev")
        let Qoffset = emacs_intern(env, ":offset")
        let Qdefault = emacs_intern(env, ":default")
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
                return emacs_cons(env, $0.toEmacsVal(env),
                                  timezone_data.toEmacsVal(env))
            }
            return emacs_cons(env, $0.toEmacsVal(env), Qnil)
        }.toEmacsVal(env)
    }
        catch {
            emacs_process_error(env, error)
        }
    }
    return Qnil
}


private func maccalfw_refresh(
  _ env: PEmacsEnv?,
  _ nargs: Int,
  _ args: POptEmacsValue?,
  _ : UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env {
        do {
            try AuthorizeCalendar(env)
            eventStore.refreshSourcesIfNecessary()
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

    let env = runtime.pointee.get_environment(runtime) as PEmacsEnv?
    if let env {
        if MemoryLayout<emacs_env>.size > Int(env.pointee.size) {
            return 2
        }
        Qnil = env.pointee.make_global_ref(env, emacs_intern(env, "nil"))
        Qt = env.pointee.make_global_ref(env, emacs_intern(env, "t"))

        // TODO: When should I call free_global_ref?

        emacs_defun(env, "maccalfw-get-calendars", 0, 0, maccalfw_get_calendars,
"""
Get a list of Mac calendars.
Each item in the list contains contains id, title, color and an editable predicated.
""")
        emacs_defun(env, "maccalfw-fetch-events", 3, 3, maccalfw_fetch_events,
"""
Get a list of events in a calendar between START-TIME and END-TIME.
The times are encoded times.
If CALENDAR-ID is nil, return all events. CALENDAR-ID can also be a list of
calendar IDs.

(fn CALENDAR-ID START-TIME END-TIME)
""")

        emacs_defun(env, "maccalfw-update-event", 1, 4, maccalfw_update_event,
"""
Update or create an event.

ID is the event identifier if modifying an event, or nil when creating a new
one. START specifies the old event's start date, which is used to distinguish
events with the same ID (typically recurring events). Note that this should be
the old start date of the event if you are updating it.

CHANGED-DATA is an alist specifying the event data in ical format. When
updating an old event only the fields appearing in the alist are updated,
leaving others unchanged. In particular, if ID is nil, the alist must contain
a `X-EMACS-CALID` entry specifying the calendar identifier, so that the new
event is created in that calendar. When updating an old event, if the alist
contains an `X-EMACS-CALID`, the event is moved to the new calendar.

If FUTURE is non-nil, all future occurrences in a recurring series are updated;
otherwise, only the specific event matching the `START` date is updated.

Returns the data of the newly created or updated event.

(fn ID CHANGED-DATA &optional START FUTURE)
""")

        emacs_defun(env, "maccalfw-get-event", 1, 2, maccalfw_get_event,
"""
Return event details given its ID.

START specifies the start date of the event, which is used to
distinguish between events with the same ID (typically recurring events).

(fn ID &optional START)
""")
        emacs_defun(env, "maccalfw-remove-event", 1, 3, maccalfw_remove_event,
"""
Remove an event given its ID.

START specifies the start date of the event, which is used to
distinguish between events with the same ID (typically recurring events).

If FUTURE is non-nil, all future occurrences in a recurring series are
removed; otherwise, only the specific event matching the START date is
removed.

(fn ID &optional START FUTURE)
""")
        emacs_defun(env, "maccalfw-timezones", 0, 0, maccalfw_timezones,
                    "Returns a list of system timezones.")
        emacs_defun(env, "maccalfw-refresh", 0, 0, maccalfw_refresh,
                    "Refreshes local data from remote sources.")

    }
    return 0
}
