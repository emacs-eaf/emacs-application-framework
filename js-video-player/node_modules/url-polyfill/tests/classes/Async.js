(function (factory) {
    if (typeof module === "object" && typeof module.exports === "object") {
        var v = factory(require, exports);
        if (v !== undefined) module.exports = v;
    }
    else if (typeof define === "function" && define.amd) {
        define(["require", "exports"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class Async {
        static $delay(timeout) {
            return new Promise((resolve) => {
                setTimeout(resolve, timeout);
            });
        }
        static $yield() {
            return new Promise((resolve) => {
                // process.nextTick(resolve);
                // setTimeout(resolve, 0);
                setImmediate(resolve);
            });
        }
        static async $await(callback, timeout = 0) {
            const startTime = Date.now();
            while (!callback()) {
                await this.$yield();
                if ((timeout > 0) && (Date.now() - startTime > timeout))
                    throw new Error('Timeout reached');
            }
        }
    }
    exports.Async = Async;
});
