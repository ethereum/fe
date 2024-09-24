import * as vscode from "vscode";
import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  StreamInfo,
  TransportKind,
} from "vscode-languageclient/node";

import { join } from "path";
import { clientLog, serverOutputChannel } from "./log";
import * as net from "net";

vscode.commands.registerCommand("fe-analyzer.helloWorld", () => {
  vscode.window.showInformationMessage(
    "Hello World from fe-analyzer extension!",
  );
});

let client: LanguageClient;

export async function activate(
  context: vscode.ExtensionContext,
): Promise<void> {
  // todo: bundle binary with extension (also: make this configurable?)
  const serverPath = join(
    __dirname,
    "..",
    "..",
    "..",
    "..",
    "..",
    "target",
    "debug",
    "fe-language-server",
  );

  let useTcp = false;

  let connectionInfo = {
    port: 4242,
    host: "127.0.0.1",
  };

  const serverOptionsTcp = () => {
    // Connect to language server via socket
    let socket = net.connect(connectionInfo);
    let result: StreamInfo = {
      writer: socket,
      reader: socket,
      // run: serverExecutable,
    };
    return Promise.resolve(result);
  };

  const serverExecutable: Executable = {
    command: serverPath,
  };

  const serverOptionsExecutable: ServerOptions = {
    run: serverExecutable,
    debug: serverExecutable,
    transport: TransportKind.socket,
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
    useTcp ? serverOptionsTcp : serverOptionsExecutable,
    clientOptions,
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
