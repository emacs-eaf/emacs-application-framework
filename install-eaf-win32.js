const { spawn, execSync } = require("child_process");
const path = require("path");
const http = require("http");
const https = require("https");
const fs = require("fs");
const crypto = require("crypto");
const os = require("os");
const url = require("url");
const process = require("process");

async function execCmd(cmd, args) {
    return new Promise((resolve, reject) => {
        const res = spawn(cmd, args);
        let output = "";
        let err = "";

        res.stdout.on("data", function(data) {
            process.stdout.write(data);
            const dataStr = data.toString();
            output += dataStr;
        });

        res.stderr.on("data", function(data) {
            process.stderr.write(data);
            const errStr = data.toString();
            err += errStr;
        });

        res.on("close", function(code) {
            if (code === 0) {
                resolve(output);
            } else {
                reject(err);
            }
        });
        res.on("error", (err) => {
            reject(err);
        });
    });
}

function printProgress(progress) {
    process.stdout.clearLine();
    process.stdout.cursorTo(0);
    process.stdout.write(progress);
}

async function downloadFile(downloadUrlStr, sha256hash) {
    const downloadUrl = url.parse(downloadUrlStr);

    let downloader = http;
    if (downloadUrl.protocol === "https:") {
        downloader = https;
    }
    const hasher = crypto.createHash("sha256");

    const filename = path.posix.basename(downloadUrl.pathname);
    const filepath = path.join(os.tmpdir(), filename);

    return new Promise((resolve, reject) => {
        fs.open(filepath, "w", (err, fd) => {
            if (err) {
                reject(err);
                return;
            }
            downloader
                .get(downloadUrl, (res) => {
                    const totalSize = res.headers["content-length"];
                    let downloadedSize = 0;
                    let progress = 0;

                    res.on("data", (chunk) => {
                        fs.write(fd, chunk, (err) => {
                            if (err) {
                                reject(err);
                                return;
                            }
                        });

                        hasher.update(chunk);

                        downloadedSize += chunk.length;
                        progress = ((downloadedSize / totalSize) * 100).toFixed(2);
                        printProgress(
                            `Downloading ${filename}, Downloaded: ${progress}%  `
                        );
                    });

                    res.on("end", () => {
                        const hash = hasher.digest("hex");
                        if (hash !== sha256hash) {
                            reject(new Error("download failed: hash mismatch"));
                            return;
                        }

                        fs.close(fd, (err) => {
                            if (err) {
                                reject(err);
                                return;
                            }
                            console.info(`\nDownload ${filename} successed!`);
                            resolve(filepath);
                        });
                    });
                })
                .on("error", (err) => {
                    reject(err);
                });
        });
    });
}

async function installCoreDeps() {
    await execCmd("npm.cmd", ["install"]);
    await execCmd("pip3", ["install", "-r", "requirements.txt"]);
}

async function checkQtWebEngine() {
    const pythonScript =
        '\
import sys\n\
from PyQt5.QtCore import QUrl\n\
from PyQt5.QtWebEngineWidgets import QWebEngineView\n\
from PyQt5.QtWidgets import QApplication\n\
app = QApplication(sys.argv)\n\
view = QWebEngineView()\n\
view.setUrl(QUrl("http://127.0.0.1:12345"))\n\
view.resize(1024, 750)\n\
view.show()\n\
app.exec()\n\
';

    const tempPythonFilePath = path.join(
        os.tmpdir(),
        "install_eaf_win32_test.py"
    );
    const fd = fs.openSync(tempPythonFilePath, "w");
    fs.writeSync(fd, pythonScript);
    fs.closeSync(fd);

    const requestListener = function(_req, res) {
        res.writeHead(200);
        res.end(
            "<h1 style='color: green'>QtWebEngine is work! Close the window to continue the installation process</h1>"
        );
    };
    const server = http.createServer(requestListener);
    server.listen(12345);
    try {
        await execCmd("python", [tempPythonFilePath]);
        server.close();
    } catch (err) {
        server.close();
        throw err;
    }
}

function checkCodecPack() {
    try {
        execSync(
            "REG QUERY HKEY_LOCAL_MACHINE\\SOFTWARE\\WOW6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\KLiteCodecPack_is1",
            { stdio: "ignore" }
        );
        return true;
    } catch (e) {
        return false;
    }
}

async function installCodecPack() {
    const kliteCodecPackURL =
        "https://files3.codecguide.com/K-Lite_Codec_Pack_1605_Basic.exe";
    const kliteCodecPackHash =
        "1f2b2593c6de1f4f3724df673c2cf1d31c4a416355b4a64f532a52a825e4694a";

    if (checkCodecPack()) {
        console.info("k-lite-codec-pack is installed!");
        return;
    }

    const installer = await downloadFile(kliteCodecPackURL, kliteCodecPackHash);
    console.info("installing k-lite-codec-pack basic ...");
    execSync(installer, { stdio: "inherit" });
    console.info("install k-lite-codec-pack basic finished");
}

(async function() {
    try {
        await installCoreDeps();

        await checkQtWebEngine();

        console.info('\x1b[32m%s\x1b[0m', "\n\nCore dependency install finished! You can use CTRL+C to terminal the following steps and enjoy EAF!\n\n");

        await installCodecPack();

    } catch (err) {
        console.error(err);
    }
})();
