import Foundation
import CEmacsModule
import EventKit

let eventStore = EKEventStore()
var Qt : emacs_value?
var Qnil : emacs_value?

extension NSNumber  : EmacsCastable {
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        return env.pointee.make_integer(env, self.intValue)
    }
}

extension EKEventAvailability : EmacsCastable  {
    private static let enumMap: [Self : String] = [
      .tentative: "tentative",
      .free: "free",
      .busy: "busy",
      .unavailable: "unavailable"]

    // TODO: I don't know if I can get away without repeating these definitions
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        return toEmacsVal_Enum(env, Self.enumMap, self)
        }

    static func parse(_ env: UnsafeMutablePointer<emacs_env>,
                      _ val: emacs_value?) throws -> Self {
        return try parseEmacsVal_Enum(env, Self.enumMap, val)
    }
}

extension EKRecurrenceFrequency : EmacsCastable  {
    private static let enumMap: [Self: String] = [
      .daily: "daily",
      .weekly: "weekly",
      .monthly: "monthly",
      .yearly: "yearly"]


    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        return toEmacsVal_Enum(env, Self.enumMap, self)
        }

    static func parse(_ env: UnsafeMutablePointer<emacs_env>,
                      _ val: emacs_value?) throws -> Self {
        return try parseEmacsVal_Enum(env, Self.enumMap, val)
    }
}

extension EKEventStatus : EmacsCastable  {
    private static let enumMap: [Self: String] = [
      .confirmed: "confirmed",
      .tentative: "tentative",
      .canceled: "cancelled"]

    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        return toEmacsVal_Enum(env, Self.enumMap, self)
    }

    static func parse(_ env: UnsafeMutablePointer<emacs_env>,
                      _ val: emacs_value?) throws -> Self {
        return try parseEmacsVal_Enum(env, Self.enumMap, val)
    }
}

extension EKEvent : EmacsCastable {
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
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

        let quoted_plist =
          Dictionary(uniqueKeysWithValues:
                       event_plist.map {
                           (env.pointee.intern(env, $0.key), $0.value) })

        return quoted_plist.filter{$0.value != nil }.toEmacsVal(env)
    }
}

extension EKRecurrenceDayOfWeek : EmacsCastable {
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        let plist : [String : EmacsCastable?] =
          [":week-day" : self.dayOfTheWeek.rawValue,
           ":week-number" : self.weekNumber]

        let quoted_plist =
          Dictionary(uniqueKeysWithValues:
                       plist.map {
                           (env.pointee.intern(env, $0.key), $0.value) })

        return quoted_plist.filter{$0.value != nil }.toEmacsVal(env)
    }
}

extension EKRecurrenceRule : EmacsCastable {
    func toEmacsVal(_ env: UnsafeMutablePointer<emacs_env>) -> emacs_value? {
        let plist : [String : EmacsCastable?] =
          [":end-date" : self.recurrenceEnd?.endDate as EmacsCastable?,
           ":occurrence-count" : self.recurrenceEnd?.occurrenceCount,
           ":calendar-id": self.calendarIdentifier,
           ":interval": self.interval,
           ":frequency": self.frequency,
           ":week-first-day": self.firstDayOfTheWeek,
           ":week-days": self.daysOfTheWeek as [EmacsCastable?]?,
           ":month-days": self.daysOfTheMonth as [EmacsCastable?]?,
           ":year-days": self.daysOfTheYear as [EmacsCastable?]?,
           ":year-weeks": self.weeksOfTheYear as [EmacsCastable?]?,
           ":year-months": self.monthsOfTheYear as [EmacsCastable?]?,
           ":set-positions": self.setPositions as [EmacsCastable?]?
          ]

        let quoted_plist =
          Dictionary(uniqueKeysWithValues:
                       plist.map {
                           (env.pointee.intern(env, $0.key), $0.value) })

        return quoted_plist.filter{$0.value != nil }.toEmacsVal(env)
    }
}


private func AuthorizeCalendar(_ env: UnsafeMutablePointer<emacs_env>) throws {
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
                        _ start: Date?) -> EKEvent? {
    let event_data = eventStore.event(withIdentifier: event_id)
    if let event_data, let start {
        if (event_data.startDate != start) {
            let delta : TimeInterval = 60*60
            let calendarEventsPredicate =
              eventStore.predicateForEvents(withStart: start-delta/2,
                                            end: start+delta/2,
                                            calendars: nil)
            let events = eventStore.events(matching: calendarEventsPredicate)
            // Find event with the same id and start date
            return events.first(
              where: { $0.eventIdentifier == event_id && $0.startDate == start})
        }
    }
    return event_data
}

private func maccalfw_get_calendars(_ env: UnsafeMutablePointer<emacs_env>?,
                          _ nargs: Int,
                          _ args: UnsafeMutablePointer<emacs_value?>?,
                          _ data: UnsafeMutableRawPointer?) -> emacs_value? {
    if let env {
        do {
            try AuthorizeCalendar(env)

            let calendars = eventStore.calendars(for: .event)
            let Qid = env.pointee.intern(env, ":id")
            let Qtitle = env.pointee.intern(env, ":title")
            let Qcolor = env.pointee.intern(env, ":color")
            let Qeditable = env.pointee.intern(env, ":editable")
            let Qdefault = env.pointee.intern(env, ":default")
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
  _ env: UnsafeMutablePointer<emacs_env>?,
  _ nargs: Int,
  _ args: UnsafeMutablePointer<emacs_value?>?,
  _ data: UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env {
        do {
            if let args, let arg0 = args[0],
               let argStart = args[1],
               let argEnd = args[2] {
                var calendar_ids : [String?]? = nil
                if env.pointee.is_not_nil(env, arg0){
                    if env.pointee.is_not_nil(
                         env,
                         emacs_funcall(env,env.pointee.intern(env, "stringp"),
                                       [arg0])){
                        calendar_ids = [try String.fromEmacsVal(env, arg0)]
                    }
                    else if env.pointee.is_not_nil(
                              env,
                              emacs_funcall(env,env.pointee.intern(env, "listp"),
                                            [arg0])){
                        calendar_ids = try emacs_parse_list(env, arg0).map{
                            try String.fromEmacsVal(env, $0)}
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

                let start = try Date.fromEmacsVal(env, argStart)
                let end = try Date.fromEmacsVal(env, argEnd)

                if let start, let end {
                    let calendarEventsPredicate =
                      eventStore.predicateForEvents(withStart: start,
                                                    end: end,
                                                    calendars: calendars)
                    let events = eventStore.events(matching:
                                                     calendarEventsPredicate)
                    return (events as [EmacsCastable?]).toEmacsVal(env)
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
  _ env: UnsafeMutablePointer<emacs_env>?,
  _ nargs: Int,
  _ args: UnsafeMutablePointer<emacs_value?>?,
  _ data: UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env {
        do {
            try AuthorizeCalendar(env)
            if let args, let eventId = try String.fromEmacsVal(env, args[0]) {
                let start = nargs > 1 ? try Date.fromEmacsVal(env, args[1]) : nil
                if let event_data = getEKEvent(eventId, start){
                    return event_data.toEmacsVal(env)
                }
                else {
                    throw EmacsError.error("Failed to fetch event.")
                }
            }
        }
        catch {
            emacs_process_error(env, error)
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
    if let env {
        do {
            if let args, let arg0 = args[0] {
                try AuthorizeCalendar(env)

                let eventData = try emacs_parse_plist(env, arg0)
                let eventId =
                  try eventData["id"].map { try String.fromEmacsVal(env, $0) }
                var event : EKEvent

                if let eventId, let eventId {
                    let old_event = eventStore.event(withIdentifier: eventId)
                    if let old_event {
                        event = old_event
                    }
                    else{
                        throw EmacsError.error("Cannot retrieve event")
                    }
                }
                else {
                    event = EKEvent(eventStore: eventStore)
                }

                if let tmp = eventData["calendar-id"] {
                    let calendar_id = try String.fromEmacsVal(env, tmp)
                    if let calendar_id,
                       let calendar = eventStore.calendar(withIdentifier: calendar_id) {
                        event.calendar = calendar
                    }
                    else {
                        throw EmacsError.error("Cannot retrieve calendar")
                    }
                }

                if let tmp = eventData["title"] {
                    event.title = try String.fromEmacsVal(env, tmp)
                }
                if let tmp = eventData["start"] {
                    event.startDate = try Date.fromEmacsVal(env, tmp)
                }
                if let tmp = eventData["end"] {
                    event.endDate = try Date.fromEmacsVal(env, tmp)
                }
                if let tmp = eventData["location"] {
                    let str = try String.fromEmacsVal(env, tmp)
                    event.location = (str?.isEmpty ?? true) ? nil : str
                }
                if let tmp = eventData["notes"] {
                    event.notes = try String.fromEmacsVal(env, tmp)
                }
                if let tmp = eventData["url"] {
                    let str = try String.fromEmacsVal(env, tmp)
                    event.url = ((str?.isEmpty ?? true) ? nil
                                   : URL(string: str!))
                }
                if let tmp = eventData["timezone"] {
                    let str = try String.fromEmacsVal(env, tmp)
                    event.timeZone = ((str?.isEmpty ?? true) ? nil
                                        : TimeZone(identifier: str!))
                }

                if let tmp = eventData["all-day-p"] {
                    event.isAllDay = env.pointee.is_not_nil(env, tmp)
                }
                if let tmp = eventData["availability"] {
                    event.availability = try EKEventAvailability.parse(env, tmp)
                }


                // Read-only: occurrenceDate, organizer, last_modified, created_date
                // detached-p, status

                do {
                    try eventStore.save(event, span: .thisEvent)
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
  _ env: UnsafeMutablePointer<emacs_env>?,
  _ nargs: Int,
  _ args: UnsafeMutablePointer<emacs_value?>?,
  _ data: UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env, let args {
        do {
            try AuthorizeCalendar(env)
            if let arg0 = args[0], let eventId = try String.fromEmacsVal(env, arg0) {
                let start = nargs > 1 ? try Date.fromEmacsVal(env, args[1]) : nil

                if let event = getEKEvent(eventId, start) {
                    do {
                        try eventStore.remove(event, span: .thisEvent)
                        return Qt
                    } catch {
                        throw EmacsError.error("Failed to remove event with error: \(error.localizedDescription)")
                    }
                }
                else {
                    throw EmacsError.error("Cannot retrieve event")
                }
            }
        }
        catch {
            emacs_process_error(env, error)
        }
    }
    return Qnil
}


private func maccalfw_timezones(
  _ env: UnsafeMutablePointer<emacs_env>?,
  _ nargs: Int,
  _ args: UnsafeMutablePointer<emacs_value?>?,
  _ data: UnsafeMutableRawPointer?) -> emacs_value?
{
    if let env {
        do {
            try AuthorizeCalendar(env)
        let Qname = env.pointee.intern(env, ":name")
        let Qabbrev = env.pointee.intern(env, ":abbrev")
        let Qoffset = env.pointee.intern(env, ":offset")
        let Qdefault = env.pointee.intern(env, ":default")
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
  _ env: UnsafeMutablePointer<emacs_env>?,
  _ nargs: Int,
  _ args: UnsafeMutablePointer<emacs_value?>?,
  _ data: UnsafeMutableRawPointer?) -> emacs_value?
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

    let env = runtime.pointee.get_environment(runtime) as UnsafeMutablePointer<emacs_env>?
    if let env {
        if MemoryLayout<emacs_env>.size > Int(env.pointee.size) {
            return 2
        }
        Qnil = env.pointee.make_global_ref(env, env.pointee.intern(env, "nil"))
        Qt = env.pointee.make_global_ref(env, env.pointee.intern(env, "t"))

        emacs_defun(env, "maccalfw-get-calendars", 0, 0, maccalfw_get_calendars,
"""
Get a list of Mac calendars.
Each item in the list contains contains id, title, color and an editable predicated.
""")
        emacs_defun(env, "maccalfw-fetch-events", 3, 3, maccalfw_fetch_events,
"""
Get a list of events in a calendar.
Takes as arguments the CALENDAR-ID, START-TIME and END-TIME.
The times are encoded times.
If CALENDAR-ID is nil, return all events. CALENDAR-ID can also be a list of
calendar IDs.
""")

        emacs_defun(env, "maccalfw-update-event", 1, 1, maccalfw_update_event,
"""
Update or create an event.
Takes as an argument a plist of the EVENT.
Only the key-value pairs in the plist are updated. If the plist contains a
non-nil `:id` then the corresponding event is updated. Otherwise, the plist
must contain `:calendar-id` entry and an event is created.

Note that if the event has a different `:calendar-id`, the event moved to
the new calendar.

Returns the data of the newly event.
""")

        emacs_defun(env, "maccalfw-get-event", 1, 2, maccalfw_get_event,
                    "Return event details given its ID.")
        emacs_defun(env, "maccalfw-remove-event", 1, 2, maccalfw_remove_event,
                    "Remove an event given its ID.")
        emacs_defun(env, "maccalfw-timezones", 0, 0, maccalfw_timezones,
                    "Returns a list of system timezones.")
        emacs_defun(env, "maccalfw-refresh", 0, 0, maccalfw_refresh,
                    "Refreshes local data from remote sources.")

    }
    return 0
}
