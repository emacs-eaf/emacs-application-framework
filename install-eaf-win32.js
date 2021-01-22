const { execSync } = require("child_process");
const path = require("path");

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

installNodeDep();

installPythonDep();
