// adapted from rust-analyzer /editors/code/src/util.ts

import { inspect } from 'util';
import * as vscode from 'vscode';

const clientLog = new (class {
  private enabled = true;
  private readonly output = vscode.window.createOutputChannel("Fe Analyzer Client");

  setEnabled(yes: boolean): void {
      clientLog.enabled = yes;
  }

  // Hint: the type [T, ...T[]] means a non-empty array
  debug(...msg: [unknown, ...unknown[]]): void {
      if (!clientLog.enabled) return;
      clientLog.write("DEBUG", ...msg);
  }

  info(...msg: [unknown, ...unknown[]]): void {
      clientLog.write("INFO", ...msg);
  }

  warn(...msg: [unknown, ...unknown[]]): void {
      debugger;
      clientLog.write("WARN", ...msg);
  }

  error(...msg: [unknown, ...unknown[]]): void {
      debugger;
      clientLog.write("ERROR", ...msg);
      clientLog.output.show(true);
  }

  private write(label: string, ...messageParts: unknown[]): void {
      const message = messageParts.map(clientLog.stringify).join(" ");
      const dateTime = new Date().toLocaleString();
      clientLog.output.appendLine(`${label} [${dateTime}]: ${message}`);
  }

  private stringify(val: unknown): string {
      if (typeof val === "string") return val;
      return inspect(val, {
          colors: false,
          depth: 6, // heuristic
      });
  }
})();

const serverOutputChannel = vscode.window.createOutputChannel("Fe Analyzer Server");

export {
  clientLog,
  serverOutputChannel,  
};