(function (factory) {
    if (typeof module === "object" && typeof module.exports === "object") {
        var v = factory(require, exports);
        if (v !== undefined) module.exports = v;
    }
    else if (typeof define === "function" && define.amd) {
        define(["require", "exports", "./Driver"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    const Driver_1 = require("./Driver");
    class Tester {
        constructor(remoteUrl) {
            this.remoteUrl = remoteUrl;
        }
        runForMany(browsers, callback) {
            return Promise.all(browsers.map((browser) => this.runFor(browser, callback))).then(() => {
            });
        }
        runFor(browser, callback) {
            const driver = Driver_1.Driver.create(browser, this.remoteUrl);
            return new Promise((resolve) => {
                resolve(callback(driver));
            })
                .then(() => {
                return driver.quit();
            }, (error) => {
                return Promise.race([
                    driver.quit(),
                    new Promise((resolve) => setTimeout(resolve, 2000))
                ]).then(() => Promise.reject(error));
            });
        }
        test(testName, callback) {
            return new Promise((resolve, reject) => {
                this.log(`Starting test '${testName}'`, 33);
                resolve(callback());
            })
                .then(() => {
                this.log(`test '${testName}' succeed`, 32);
            }, (error) => {
                this.log(`test '${testName}' failed`, 31);
                console.log(error);
                throw error;
            });
        }
        log(content, color) {
            console.log(this.colorString(content, color));
        }
        colorString(content, color = 0) {
            return `\x1b[${color}m${content}\x1b[0m`;
        }
    }
    exports.Tester = Tester;
});
