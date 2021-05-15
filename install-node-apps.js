const { execSync } = require("child_process");
const path = require("path");
const pkg = require("./package.json");

const nodeApps = pkg.apps || [];
const rebuildApps = pkg.rebuildApps || [];

function installNodeApps() {
    for (let i = 0; i < nodeApps.length; i++) {
        execSync("npm install", {
            cwd: path.join(__dirname, nodeApps[i]),
            stdio: "inherit",
        });
    }
}

const vueApps = pkg.vueApps || [];

function installVueApps() {
    for (let i = 0; i < vueApps.length; i++) {
        execSync("npm install", {
            cwd: path.join(__dirname, vueApps[i]),
            stdio: "inherit",
        });
    }

    for (let i = 0; i < vueApps.length; i++) {
        execSync("npm run build", {
            cwd: path.join(__dirname, vueApps[i]),
            stdio: "inherit",
        });
    }
}

function rebuildNodeApps() {
    for (let i = 0; i < rebuildApps.length; i++) {
        execSync("npm rebuild", {
            cwd: path.join(__dirname, rebuildApps[i]),
            stdio: "inherit",
        });
    }
}

installNodeApps();
installVueApps();
rebuildNodeApps();
