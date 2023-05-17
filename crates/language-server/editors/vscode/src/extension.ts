import * as vscode from "vscode";
import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

import { join } from 'path';
import { clientLog, serverOutputChannel } from "./log";

vscode.commands.registerCommand('fe-analyzer.helloWorld', () => {
    vscode.window.showInformationMessage('Hello World from fe-analyzer extension!');
});

let client: LanguageClient;

export async function activate(
  context: vscode.ExtensionContext
): Promise<void> {
  // todo: bundle binary with extension  
  const serverPath = join(__dirname, '..', '..', '..', '..', '..', 'target', 'debug', 'fe-language-server');

  const serverExecutable: Executable = {
    command: serverPath,
  };

  const serverOptions: ServerOptions = {
    run: serverExecutable,
    debug: serverExecutable,
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "fe" }],
    traceOutputChannel: serverOutputChannel,
    // @ts-ignore
    outputChannel: clientLog.output,
  };

  client = new LanguageClient(
    "fe-language-server",
    "Fe Language Server",
    serverOptions,
    clientOptions
  );

  // Start the client. This will also launch the server
  await client.start();
  
  // console log all messages from the server
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}