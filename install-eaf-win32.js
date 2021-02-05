const { execSync } = require("child_process");
const path = require("path");
const http = require("http");
const https = require("https");
const fs = require("fs");
const crypto = require("crypto");
const os = require("os");
const url = require("url");

const nodeApps = ["js-video-player", "markdown-previewer", "terminal", "image-viewer"];

function getAppPath(name) {
    return path.join("app", name);
}

function installNodeDep() {
    for (let i = 0; i < nodeApps.length; i++) {
        execSync("npm install", {
            cwd: getAppPath(nodeApps[i]),
            stdio: [0, 1, 2],
        });
    }
}

const pythonDeps = [
    "pyqt5",
    "pyqt5-sip",
    "pyqtwebengine",
    "pymupdf",
    "qrcode",
    "markdown",
    "epc",
    "retrying",
    "pygetwindow"
];

function installPythonDep() {
    const installCmd = "pip install " + pythonDeps.join(" ");
    console.log(installCmd);
    execSync(installCmd, {
        stdio: [0, 1, 2],
    });
}

function downloadFile(urlStr, sha256hash) {
    const u = url.parse(urlStr);
    let handler = http;
    if (u.protocol === "https:") {
        handler = https;
    }
    const hasher = crypto.createHash("sha256");
    const name = path.posix.basename(u.pathname);
    const file = path.join(os.tmpdir(), name);
    return new Promise((resolve, reject) => {
        fs.open(file, "w", (err, fd) => {
            if (err) {
                reject(err);
                return;
            }
            handler.get(u, (res) => {
                res.on("data", (chunk) => {
                    fs.write(fd, chunk, (err) => {
                        if (err) {
                            reject(err);
                            return;
                        }
                    });
                    hasher.update(chunk);
                });
                res.on("end", () => {
                    const hash = hasher.digest("hex");
                    if (hash !== sha256hash) {
                        reject(new Error("download failed: hash mismatch"));
                        return
                    }
                    resolve(file);
                });
            }).on("error", (err) => {
                reject(err)
            });
        });
    });
}

const kliteCodecPackURL = "https://files3.codecguide.com/K-Lite_Codec_Pack_1595_Basic.exe";
const kliteCodecPackHash = "73772a2a0e8f9bc4ec4ca35578608cda76847e563613d1600bd5bf659a908559";

async function installCodecPack() {
    const installer = await downloadFile(kliteCodecPackURL, kliteCodecPackHash);
    const installCmd = `${installer} /verysilent /nocancel /norestart`;
    console.log("installing k-lite-codec-pack basic ...");
    execSync(installCmd);
}

try {
    installPythonDep();
} catch (err) {
    console.error(err);
}

try {
    installNodeDep();
} catch (err) {
    console.error(err);
}

(async function() {
    try {
        await installCodecPack();
    } catch (err) {
        console.error(err);
    }
})();
