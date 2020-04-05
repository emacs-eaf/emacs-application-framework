/**
 * Copyright (c) 2017, Daniel Imms (MIT License).
 * Copyright (c) 2018, Microsoft Corporation (MIT License).
 */

import * as assert from 'assert';
import { WindowsTerminal } from './windowsTerminal';
import { UnixTerminal } from './unixTerminal';
import { pollUntil } from './testUtils.test';

const terminalConstructor = (process.platform === 'win32') ? WindowsTerminal : UnixTerminal;
const SHELL = (process.platform === 'win32') ? 'cmd.exe' : '/bin/bash';

let terminalCtor: WindowsTerminal | UnixTerminal;
if (process.platform === 'win32') {
  terminalCtor = require('./windowsTerminal');
} else {
  terminalCtor = require('./unixTerminal');
}


describe('Terminal', () => {
  describe('constructor', () => {
    it('should do basic type checks', () => {
      assert.throws(
        () => new (<any>terminalCtor)('a', 'b', { 'name': {} }),
        'name must be a string (not a object)'
      );
    });
  });

  describe('automatic flow control', () => {
    it('should respect ctor flow control options', () => {
      const pty = new terminalConstructor(SHELL, [], {handleFlowControl: true, flowControlPause: 'abc', flowControlResume: '123'});
      assert.equal(pty.handleFlowControl, true);
      assert.equal((pty as any)._flowControlPause, 'abc');
      assert.equal((pty as any)._flowControlResume, '123');
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

function stripEscapeSequences(data: string): string {
  return data.replace(/\u001b\[0K/, '');
}
