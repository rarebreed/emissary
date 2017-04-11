# What is emissary?

Emissary is a toolkit for microservices written in purescript with the overall theme of running and monitoring tests.
The first microservices that will be worked on include:

- Log monitoring
- File system monitoring
- Testsuite Runner
- Command service
- Provisioner

# Rationale

The idea is to have a loose set of services that can be changed or swapped out at will.  The services all interact based
on the reception of messages.  One of the goals is to eliminate the need for jenkins totally.  Other services are just 
useful to testing in general.

# Log Monitoring

Tests often need the ability to check if some wording or phrase exists (or not) in a log file.  A more complex use of
this service is analyzing log files.  This analysis could be in the form of linking error messages to code.  It could
even be used to predict problems.

This will be challenging due to parsing out of the log files.  Every log file will be a little different, and you have
to extract the message from metadata in the log output (for example, timestamp, log level, method, package or thread
name).  Once the actual message body can be extracted, this would need to be examined.

# File System Monitoring

Tests often need to check if a file exists (or not) after some action, or they might need to detect if a file was
modified by an operation during a test.

This service could also be used to "rollback" file system changes (ie, make copies before a test starts, and then
replace any edited files with their original)

# Command Service

Instead of using SSH, a service to execute commands could be implemented.

# Test suite Runner

This would basically be an
