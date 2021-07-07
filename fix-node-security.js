const { execSync } = require("child_process");
const path = require("path");
const pkg = require("./package.json");

const nodeApps = pkg.apps || [];

function fixNodeSecurity() {
  for (let i = 0; i < nodeApps.length; i++) {
    console.log("Fix node security: " + path.join(__dirname, nodeApps[i]));

    execSync("npm audit fix --force", {
      cwd: path.join(__dirname, nodeApps[i]),
      stdio: "inherit",
    });
  }
}

fixNodeSecurity();
