#if canImport(EventKit)
    import EventKit
    import Foundation
    import MacCalfwCore

    func authorizeCalendar(_ eventStore: EKEventStore) throws {
        switch EKEventStore.authorizationStatus(for: .event) {
        case .fullAccess, .authorized:
            return
        case .notDetermined:
            let semaphore = DispatchSemaphore(value: 0)
            var granted = false
            eventStore.requestFullAccessToEvents { ok, _ in
                granted = ok
                semaphore.signal()
            }
            semaphore.wait()
            guard granted else {
                throw CLIError.notAuthorized("Calendar access was not granted")
            }
        case .restricted, .denied, .writeOnly:
            throw CLIError.notAuthorized("Calendar access is not authorized")
        @unknown default:
            throw CLIError.notAuthorized("Calendar access is not authorized")
        }
    }

    func authorizeReminders(_ eventStore: EKEventStore) throws {
        switch EKEventStore.authorizationStatus(for: .reminder) {
        case .fullAccess, .authorized:
            return
        case .notDetermined:
            let semaphore = DispatchSemaphore(value: 0)
            var granted = false
            eventStore.requestFullAccessToReminders { ok, _ in
                granted = ok
                semaphore.signal()
            }
            semaphore.wait()
            guard granted else {
                throw CLIError.notAuthorized("Reminders access was not granted")
            }
        case .restricted, .denied, .writeOnly:
            throw CLIError.notAuthorized("Reminders access is not authorized")
        @unknown default:
            throw CLIError.notAuthorized("Reminders access is not authorized")
        }
    }
#endif
