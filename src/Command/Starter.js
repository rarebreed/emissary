/* global exports */
"use strict";

// module Command.Starter

//var subproc = require('child_process')

exports.launch = (cmd) => {
    return (args) => {
    	return (opts) => {
    	    let proc = subproc.spawn(cmd, args);
    	    proc.stdout.pipe(process.stdout);

    	    proc.on('close', (code, signal) => {
    		      process.stdout.write(`child process terminated due to receipt of signal ${signal}`);
    	    });

    	    proc.on('error', (err) => {
    		      process.stdout.write(err);
    	    });
          return proc;
    	}
    }
}
