"use strict";
/**
 * Copyright (c) 2017, Daniel Imms (MIT License).
 * Copyright (c) 2018, Microsoft Corporation (MIT License).
 */
Object.defineProperty(exports, "__esModule", { value: true });
var assert = require("assert");
var windowsTerminal_1 = require("./windowsTerminal");
var unixTerminal_1 = require("./unixTerminal");
var terminalConstructor = (process.platform === 'win32') ? windowsTerminal_1.WindowsTerminal : unixTerminal_1.UnixTerminal;
var SHELL = (process.platform === 'win32') ? 'cmd.exe' : '/bin/bash';
var terminalCtor;
if (process.platform === 'win32') {
    terminalCtor = require('./windowsTerminal');
}
else {
    terminalCtor = require('./unixTerminal');
}
describe('Terminal', function () {
    describe('constructor', function () {
        it('should do basic type checks', function () {
            assert.throws(function () { return new terminalCtor('a', 'b', { 'name': {} }); }, 'name must be a string (not a object)');
        });
    });
    describe('automatic flow control', function () {
        it('should respect ctor flow control options', function () {
            var pty = new terminalConstructor(SHELL, [], { handleFlowControl: true, flowControlPause: 'abc', flowControlResume: '123' });
            assert.equal(pty.handleFlowControl, true);
            assert.equal(pty._flowControlPause, 'abc');
            assert.equal(pty._flowControlResume, '123');
        });
        // TODO: I don't think this test ever worked due to pollUntil being used incorrectly
        // it('should do flow control automatically', async function(): Promise<void> {
        //   // Flow control doesn't work on Windows
        //   if (process.platform === 'win32') {
        //     return;
        //   }
        //   this.timeout(10000);
        //   const pty = new terminalConstructor(SHELL, [], {handleFlowControl: true, flowControlPause: 'PAUSE', flowControlResume: 'RESUME'});
        //   let read: string = '';
        //   pty.on('data', data => read += data);
        //   pty.on('pause', () => read += 'paused');
        //   pty.on('resume', () => read += 'resumed');
        //   pty.write('1');
        //   pty.write('PAUSE');
        //   pty.write('2');
        //   pty.write('RESUME');
        //   pty.write('3');
        //   await pollUntil(() => {
        //     return stripEscapeSequences(read).endsWith('1pausedresumed23');
        //   }, 100, 10);
        // });
    });
});
function stripEscapeSequences(data) {
    return data.replace(/\u001b\[0K/, '');
}
//# sourceMappingURL=terminal.test.js.map