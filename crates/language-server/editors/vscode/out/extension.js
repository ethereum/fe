"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.deactivate = exports.activate = void 0;
const vscode = require("vscode");
const node_1 = require("vscode-languageclient/node");
const path_1 = require("path");
vscode.commands.registerCommand('fe-analyzer.helloWorld', () => {
    vscode.window.showInformationMessage('Hello World from fe-language-server!');
});
let client;
async function activate(context) {
    // todo: bundle binary with extension  
    const serverPath = (0, path_1.join)(__dirname, '..', '..', '..', '..', '..', 'target', 'debug', 'fe-language-server');
    const serverExecutable = {
        command: serverPath,
    };
    const serverOptions = {
        run: serverExecutable,
        debug: serverExecutable,
    };
    const clientOptions = {
        documentSelector: [{ scheme: "file", language: "Fe" }],
    };
    client = new node_1.LanguageClient("fe-language-server", "Fe Language Server", serverOptions, clientOptions);
    // Start the client. This will also launch the server
    client.start();
}
exports.activate = activate;
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map