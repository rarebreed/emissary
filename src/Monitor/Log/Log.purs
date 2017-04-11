{-- This module runs as a background service, examining one or more log files.  More than a simple tail, it will do
    parsing and analysis of the file to try to classify and predict behavior.  This is a combination of supervised and
    unsupervised learning, and as such will require some input by human for tweaking.

    The trick with this is that log outputs are only semi-structured.  Although a line is timestamped (usually), and
    most logs have some kind of log level, everything else is up in the air.  This makes even just parsing a log very
    difficult.  So how will we do this?

    We will have a grammar file for basic parsing.  This is to separate the message content from the time stamp, log
    level or any other metadata (for example, the method name, package, or thread name).  The message body is primarily
    what we are interested, but the metadata is also needed and useful.  Natural language processing is of course an
    extremely hard problem.  However, we are not trying to "understand" the message, what we are looking for is patterns
    or "shapes" so that we can classify or predict
--}

module Log where 
