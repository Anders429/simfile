//! Aliases for the `log` crate's logging macros.
//!
//! Since logging is optional, these aliases provide a simplified callsite for cases where logging
//! is either enabled or disabled. The caller can simply call the macros here, and they will log
//! only if logging is enabled, being a no-op otherwise.

/// Logs a message at the trace level.
#[allow(unused)]
macro_rules! _trace {
    ($($x:tt)*) => {
        #[cfg(feature = "log")]
        {
            log::trace!($($x)*)
        }
    }
}
#[allow(unused)]
pub(crate) use _trace as trace;

/// Logs a message at the debug level.
#[allow(unused)]
macro_rules! _debug {
    ($($x:tt)*) => {
        #[cfg(feature = "log")]
        {
            log::debug!($($x)*)
        }
    }
}
#[allow(unused)]
pub(crate) use _debug as debug;

/// Logs a message at the info level.
#[allow(unused)]
macro_rules! _info {
    ($($x:tt)*) => {
        #[cfg(feature = "log")]
        {
            log::info!($($x)*)
        }
    }
}
#[allow(unused)]
pub(crate) use _info as info;

/// Logs a message at the warn level.
#[allow(unused)]
macro_rules! _warn {
    ($($x:tt)*) => {
        #[cfg(feature = "log")]
        {
            log::warn!($($x)*)
        }
    }
}
#[allow(unused)]
pub(crate) use _warn as warn;

/// Logs a message at the error level.
#[allow(unused)]
macro_rules! _error {
    ($($x:tt)*) => {
        #[cfg(feature = "log")]
        {
            log::error!($($x)*)
        }
    }
}
#[allow(unused)]
pub(crate) use _error as error;

#[cfg(test)]
mod tests {
    use crate::internal_log;
    use log::Level;

    #[test]
    #[cfg_attr(not(feature = "log"), ignore)]
    fn trace_log_on() {
        testing_logger::setup();

        internal_log::trace!("foo");

        testing_logger::validate(|captured_logs| {
            assert_eq!(captured_logs.len(), 1);
            assert_eq!(captured_logs[0].body, "foo");
            assert_eq!(captured_logs[0].level, Level::Trace);
        });
    }

    #[test]
    #[cfg_attr(feature = "log", ignore)]
    fn trace_log_off() {
        testing_logger::setup();

        internal_log::trace!("foo");

        testing_logger::validate(|captured_logs| {
            assert!(captured_logs.is_empty());
        })
    }

    #[test]
    #[cfg_attr(not(feature = "log"), ignore)]
    fn debug_log_on() {
        testing_logger::setup();

        internal_log::debug!("foo");

        testing_logger::validate(|captured_logs| {
            assert_eq!(captured_logs.len(), 1);
            assert_eq!(captured_logs[0].body, "foo");
            assert_eq!(captured_logs[0].level, Level::Debug);
        });
    }

    #[test]
    #[cfg_attr(feature = "log", ignore)]
    fn debug_log_off() {
        testing_logger::setup();

        internal_log::debug!("foo");

        testing_logger::validate(|captured_logs| {
            assert!(captured_logs.is_empty());
        })
    }

    #[test]
    #[cfg_attr(not(feature = "log"), ignore)]
    fn info_log_on() {
        testing_logger::setup();

        internal_log::info!("foo");

        testing_logger::validate(|captured_logs| {
            assert_eq!(captured_logs.len(), 1);
            assert_eq!(captured_logs[0].body, "foo");
            assert_eq!(captured_logs[0].level, Level::Info);
        });
    }

    #[test]
    #[cfg_attr(feature = "log", ignore)]
    fn info_log_off() {
        testing_logger::setup();

        internal_log::info!("foo");

        testing_logger::validate(|captured_logs| {
            assert!(captured_logs.is_empty());
        })
    }

    #[test]
    #[cfg_attr(not(feature = "log"), ignore)]
    fn warn_log_on() {
        testing_logger::setup();

        internal_log::warn!("foo");

        testing_logger::validate(|captured_logs| {
            assert_eq!(captured_logs.len(), 1);
            assert_eq!(captured_logs[0].body, "foo");
            assert_eq!(captured_logs[0].level, Level::Warn);
        });
    }

    #[test]
    #[cfg_attr(feature = "log", ignore)]
    fn warn_log_off() {
        testing_logger::setup();

        internal_log::warn!("foo");

        testing_logger::validate(|captured_logs| {
            assert!(captured_logs.is_empty());
        })
    }

    #[test]
    #[cfg_attr(not(feature = "log"), ignore)]
    fn error_log_on() {
        testing_logger::setup();

        internal_log::error!("foo");

        testing_logger::validate(|captured_logs| {
            assert_eq!(captured_logs.len(), 1);
            assert_eq!(captured_logs[0].body, "foo");
            assert_eq!(captured_logs[0].level, Level::Error);
        });
    }

    #[test]
    #[cfg_attr(feature = "log", ignore)]
    fn error_log_off() {
        testing_logger::setup();

        internal_log::error!("foo");

        testing_logger::validate(|captured_logs| {
            assert!(captured_logs.is_empty());
        })
    }
}
