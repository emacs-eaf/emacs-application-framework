const { execSync } = require("child_process");
const pkg = require("./package.json");

const nodeApps = pkg.apps || [];

function installNodeApps() {
    for (let i = 0; i < nodeApps.length; i++) {
        execSync("npm install", {
            cwd: nodeApps[i],
            stdio: "inherit",
        });
    }
}

installNodeApps();

