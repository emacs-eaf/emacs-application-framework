const { execSync } = require("child_process");
const path = require("path");
const pkg = require("./package.json");

const nodeApps = pkg.apps || [];
const rebuildApps = pkg.rebuildApps || [];

var failedApps = [];

function installExecSync(command, app_dir) {
    try {
        console.info("[EAF] Preparing to [" + command + "] on [" + app_dir + "]");
        execSync(command, {
            cwd: path.join(__dirname, app_dir),
            stdio: "inherit",
        });
        console.info("[EAF] [" + command + "] on [" + app_dir + "] has succeeded! :)");
    } catch (err) {
        console.info("[EAF] [" + command + "] on [" + app_dir + "] has failed! :(");
        failedApps.push(app_dir);
    }
}

function installNodeApps() {
    for (let i = 0; i < nodeApps.length; i++) {
        installExecSync("npm install", nodeApps[i]);
    }
}

const vueApps = pkg.vueApps || [];

function installVueApps() {
    for (let i = 0; i < vueApps.length; i++) {
        installExecSync("npm install", vueApps[i]);
        installExecSync("npm run build", vueApps[i]);
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
if(failedApps.length > 0) {
    console.info("[EAF] Failed to install dependencies for the following EAF applications, they may not work correctly!");
    for(let i = 0; i < failedApps.length; i++) {
	    console.info("> " + failedApps[i]);
    }
    console.info("------");
    console.info("The usage of other EAF applications not listed here may not be affected.");
    console.info("Nonetheless please report the error log to EAF maintainers:");
    console.info("https://github.com/manateelazycat/emacs-application-framework/issues");
}
