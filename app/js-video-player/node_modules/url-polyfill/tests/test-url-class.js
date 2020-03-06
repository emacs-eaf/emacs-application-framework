(function (factory) {
    if (typeof module === "object" && typeof module.exports === "object") {
        var v = factory(require, exports);
        if (v !== undefined) module.exports = v;
    }
    else if (typeof define === "function" && define.amd) {
        define(["require", "exports", "../do-not-use/url"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    const originalURL = window.URL;
    const url_1 = require("../do-not-use/url");
    const test = () => {
        let a = new originalURL('?fr=yset_ie_syc_oracle&type=orcl_hpset#page0', 'https://username:password@www.yahoo.com:80/path');
        let b = new url_1.URL('?fr=yset_ie_syc_oracle&type=orcl_hpset#page0', 'https://username:password@www.yahoo.com:80/path');
        // let b = new URL('https://username:password@www.yahoo.com:80/path?fr=yset_ie_syc_oracle&type=orcl_hpset#page0', '');
        window.a = a;
        window.b = b;
        // 'a+"*ç%&/()=?±“#Ç[]|{}≠b'
        for (let key in a) {
            let value = a[key];
            switch (typeof value) {
                case 'boolean':
                case 'number':
                case 'string':
                    if (value !== b[key]) {
                        throw new Error('Values mismatch for key "' + key + '" : \n' + value + '\n' + b[key]);
                    }
                    break;
            }
        }
    };
    test();
});
