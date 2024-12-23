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
import { spawn } from "child_process";
import * as os from "os";
import * as path from "path";

vscode.commands.registerCommand("fe-analyzer.helloWorld", () => {
  vscode.window.showInformationMessage(
    "Hello World from fe-analyzer extension!",
  );
});

let client: LanguageClient;
let serverProcess: any;

function getServerPath(): string {
  const isDev = process.env.NODE_ENV === "development";
  if (isDev) {
    return path.join(
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
  }

  const platform = os.platform();
  const binariesDir = path.join(__dirname, "..", "server");

  switch (platform) {
    case "win32":
      return path.join(binariesDir, "windows", "fe-language-server.exe");
    case "darwin":
      return path.join(binariesDir, "mac", "fe-language-server");
    case "linux":
      return path.join(
        binariesDir,
        "x86_64-unknown-linux-gnu",
        "fe-language-server",
      );
    default:
      throw new Error(`Unsupported platform: ${platform}`);
  }
}

export async function activate(
  context: vscode.ExtensionContext,
): Promise<void> {
  const config = vscode.workspace.getConfiguration("fe-analyzer");
  const customServerPath = config.get<string>("binaryPath", "");
  const serverPath = customServerPath || getServerPath();

  let useTcp = false;

  let connectionInfo = {
    port: 4242,
    host: "127.0.0.1",
  };

  if (useTcp) {
    try {
      // Spawn and manage the server process
      serverProcess = spawn(serverPath, [
        "tcp",
        "--port",
        connectionInfo.port.toString(),
      ]);

      serverProcess.stdout.on("data", (data: Buffer) => {
        console.log(`Server stdout: ${data}`);
      });

      serverProcess.stderr.on("data", (data: Buffer) => {
        console.error(`Server stderr: ${data}`);
      });

      serverProcess.on("close", (code: number) => {
        console.log(`Server process exited with code ${code}`);
      });
    } catch (error) {
      console.error(`Failed to spawn server process: ${error}`);
    }
  }

  const serverOptionsTcp = () => {
    let socket = net.connect(connectionInfo);
    let result: StreamInfo = {
      writer: socket,
      reader: socket,
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
