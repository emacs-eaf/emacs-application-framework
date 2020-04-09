/**
 * Copyright (c) 2017, Daniel Imms (MIT License).
 * Copyright (c) 2018, Microsoft Corporation (MIT License).
 */

import { UnixTerminal } from './unixTerminal';
import * as assert from 'assert';
import * as path from 'path';
import { pollUntil } from './testUtils.test';

const FIXTURES_PATH = path.normalize(path.join(__dirname, '..', 'fixtures', 'utf8-character.txt'));

if (process.platform !== 'win32') {
  describe('UnixTerminal', () => {
    describe('Constructor', () => {
      it('should set a valid pts name', () => {
        const term = new UnixTerminal('/bin/bash', [], {});
        let regExp;
        if (process.platform === 'linux') {
          // https://linux.die.net/man/4/pts
          regExp = /^\/dev\/pts\/\d+$/;
        }
        if (process.platform === 'darwin') {
          // https://developer.apple.com/legacy/library/documentation/Darwin/Reference/ManPages/man4/pty.4.html
          regExp = /^\/dev\/tty[p-sP-S][a-z0-9]+$/;
        }
        if (regExp) {
          assert.ok(regExp.test((<any>term)._pty), '"' + (<any>term)._pty + '" should match ' + regExp.toString());
        }
      });
    });

    describe('PtyForkEncodingOption', () => {
      it('should default to utf8', (done) => {
        const term = new UnixTerminal('/bin/bash', [ '-c', `cat "${FIXTURES_PATH}"` ]);
        term.on('data', (data) => {
          assert.equal(typeof data, 'string');
          assert.equal(data, '\u00E6');
          done();
        });
      });
      it('should return a Buffer when encoding is null', (done) => {
        const term = new UnixTerminal('/bin/bash', [ '-c', `cat "${FIXTURES_PATH}"` ], {
          encoding: null
        });
        term.on('data', (data) => {
          assert.equal(typeof data, 'object');
          assert.ok(data instanceof Buffer);
          assert.equal(0xC3, data[0]);
          assert.equal(0xA6, data[1]);
          done();
        });
      });
      it('should support other encodings', (done) => {
        const text = 'test æ!';
        const term = new UnixTerminal(null, ['-c', 'echo "' + text + '"'], {
          encoding: 'base64'
        });
        let buffer = '';
        term.on('data', (data) => {
          assert.equal(typeof data, 'string');
          buffer += data;
        });
        term.on('exit', () => {
          assert.equal(Buffer.alloc(8, buffer, 'base64').toString().replace('\r', '').replace('\n', ''), text);
          done();
        });
      });
    });

    describe('open', () => {
      let term: UnixTerminal;

      afterEach(() => {
        if (term) {
          term.slave.destroy();
          term.master.destroy();
        }
      });

      it('should open a pty with access to a master and slave socket', (done) => {
        let doneCalled = false;
        term = UnixTerminal.open({});

        let slavebuf = '';
        term.slave.on('data', (data) => {
          slavebuf += data;
        });

        let masterbuf = '';
        term.master.on('data', (data) => {
          masterbuf += data;
        });

        pollUntil(() => {
          if (masterbuf === 'slave\r\nmaster\r\n' && slavebuf === 'master\n') {
            done();
            return true;
          }
          return false;
        }, 200, 10);

        term.slave.write('slave\n');
        term.master.write('master\n');
      });
    });
  });
}
