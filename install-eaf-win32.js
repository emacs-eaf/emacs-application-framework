const { execSync } = require("child_process");
const path = require("path");
const http = require("http");
const https = require("https");
const fs = require("fs");
const crypto = require("crypto");
const os = require("os");

const nodeApps = ["js-video-player", "mermaid", "terminal"];

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
    "grip",
    "qrcode",
    "markdown",
    "epc"
];

function installPythonDep() {
    const installCmd = "pip install " + pythonDeps.join(" ");
    console.log(installCmd);
    execSync(installCmd, {
        stdio: [0, 1, 2],
    });
}

const kliteCodecPackURL = "https://files3.codecguide.com/K-Lite_Codec_Pack_1595_Basic.exe";
const kliteCodecPackHash = "73772a2a0e8f9bc4ec4ca35578608cda76847e563613d1600bd5bf659a908559";

function downloadFile(url) {
    let handler = http;
    if (url.startsWith("https://")) {
        handler = https;
    }
    const name = crypto.randomBytes(4).readUInt32LE(0) + ".exe";
    const file = path.join(os.tmpdir(), name);
    return new Promise((resolve, reject) => {
        handler.get(url, (res) => {
            let data = [];
            res.on("data", (chunk) => {
                data.push(chunk);
            });
            res.on("end", () => {
                fs.writeFileSync(file, Buffer.concat(data));
                resolve(file);
            });
        }).on("error", (err) => {
            reject(err)
        });
    });
}

async function installCodecPack() {
    const installer = await downloadFile(kliteCodecPackURL);
    const installCmd = `${installer} /verysilent /nocancel /norestart`;
    console.log("installing k-lite-codec-pack basic ...");
    execSync(installCmd);
}

installNodeDep();

installPythonDep();

(async function() {
    try {
    await installCodecPack();
    } catch (err) {
        console.error(err);
    }
})();
