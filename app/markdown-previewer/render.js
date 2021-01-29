const path = require("path");
const fs = require("fs");
const os = require("os");
const http = require("http");
const url = require("url");

const mume = require("@shd101wyy/mume");

function readFile(file, options) {
    return new Promise((resolve, reject) => {
        fs.readFile(file, options, (error, text) => {
            if (error) {
                return reject(error.toString());
            }
            else {
                return resolve(text.toString());
            }
        });
    });
}

function writeFile(file, text, options) {
    return new Promise((resolve, reject) => {
        fs.writeFile(file, text, options, (error) => {
            if (error) {
                return reject(error.toString());
            }
            else {
                return resolve();
            }
        });
    });
}

async function render(inputFile, outputFile, darkMode) {
    const configPath = path.resolve(os.homedir(), ".mume");
    await mume.init(configPath);

    const engine = new mume.MarkdownEngine({
        filePath: inputFile,
        config: {
            configPath: configPath,
            previewTheme: darkMode ? "atom-dark.css" : "atom-light.css",
            mermaidTheme: darkMode ? "mermaid.dark.css" : "mermaid.css",
            codeBlockTheme: darkMode ? "atom-dark.css" : "atom-light.css",
            printBackground: true,
            enableScriptExecution: true,
        },
    });

    const inputString = await readFile(inputFile, {
        encoding: "utf-8",
    });
    let html;
    let yamlConfig;
    ({ html, yamlConfig } = await engine.parseMD(inputString, {
        useRelativeFilePath: false,
        hideFrontMatter: true,
        isForPreview: false,
        runAllCodeChunks: true,
    }));
    html = await engine.generateHTMLTemplateForExport(html, yamlConfig, {
        isForPrint: false,
        isForPrince: false,
        offline: true,
        embedLocalImages: false,
    });

    await writeFile(outputFile, html);
}

async function server_handler(req, resp) {
    const q = url.parse(req.url, true).query;
    const inputFile = q.input_file;
    const outputFile = q.output_file;
    const darkMode = q.dark_mode == "true";

    resp.writeHead(200, {'Content-Type': 'text/plain'});
    try {
	await render(inputFile, outputFile, darkMode);
	resp.end('ok');
    } catch (err) {
	resp.end(err.message);
    }
}

async function main() {
    const argv = process.argv;
    const port = Number(argv[2]);
    http.createServer(server_handler).listen(port);
}

main()

