"use strict";
/**
 * Copyright (c) 2017, Daniel Imms (MIT License).
 * Copyright (c) 2018, Microsoft Corporation (MIT License).
 */
Object.defineProperty(exports, "__esModule", { value: true });
var unixTerminal_1 = require("./unixTerminal");
var assert = require("assert");
var path = require("path");
var testUtils_test_1 = require("./testUtils.test");
var FIXTURES_PATH = path.normalize(path.join(__dirname, '..', 'fixtures', 'utf8-character.txt'));
if (process.platform !== 'win32') {
    describe('UnixTerminal', function () {
        describe('Constructor', function () {
            it('should set a valid pts name', function () {
                var term = new unixTerminal_1.UnixTerminal('/bin/bash', [], {});
                var regExp;
                if (process.platform === 'linux') {
                    // https://linux.die.net/man/4/pts
                    regExp = /^\/dev\/pts\/\d+$/;
                }
                if (process.platform === 'darwin') {
                    // https://developer.apple.com/legacy/library/documentation/Darwin/Reference/ManPages/man4/pty.4.html
                    regExp = /^\/dev\/tty[p-sP-S][a-z0-9]+$/;
                }
                if (regExp) {
                    assert.ok(regExp.test(term._pty), '"' + term._pty + '" should match ' + regExp.toString());
                }
            });
        });
        describe('PtyForkEncodingOption', function () {
            it('should default to utf8', function (done) {
                var term = new unixTerminal_1.UnixTerminal('/bin/bash', ['-c', "cat \"" + FIXTURES_PATH + "\""]);
                term.on('data', function (data) {
                    assert.equal(typeof data, 'string');
                    assert.equal(data, '\u00E6');
                    done();
                });
            });
            it('should return a Buffer when encoding is null', function (done) {
                var term = new unixTerminal_1.UnixTerminal('/bin/bash', ['-c', "cat \"" + FIXTURES_PATH + "\""], {
                    encoding: null
                });
                term.on('data', function (data) {
                    assert.equal(typeof data, 'object');
                    assert.ok(data instanceof Buffer);
                    assert.equal(0xC3, data[0]);
                    assert.equal(0xA6, data[1]);
                    done();
                });
            });
            it('should support other encodings', function (done) {
                var text = 'test æ!';
                var term = new unixTerminal_1.UnixTerminal(null, ['-c', 'echo "' + text + '"'], {
                    encoding: 'base64'
                });
                var buffer = '';
                term.on('data', function (data) {
                    assert.equal(typeof data, 'string');
                    buffer += data;
                });
                term.on('exit', function () {
                    assert.equal(Buffer.alloc(8, buffer, 'base64').toString().replace('\r', '').replace('\n', ''), text);
                    done();
                });
            });
        });
        describe('open', function () {
            var term;
            afterEach(function () {
                if (term) {
                    term.slave.destroy();
                    term.master.destroy();
                }
            });
            it('should open a pty with access to a master and slave socket', function (done) {
                var doneCalled = false;
                term = unixTerminal_1.UnixTerminal.open({});
                var slavebuf = '';
                term.slave.on('data', function (data) {
                    slavebuf += data;
                });
                var masterbuf = '';
                term.master.on('data', function (data) {
                    masterbuf += data;
                });
                testUtils_test_1.pollUntil(function () {
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
//# sourceMappingURL=unixTerminal.test.js.map