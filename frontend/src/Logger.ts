type LogLevel = "debug" | "info" | "log" | "warning" | "error";

let levelsToLog = new Set<LogLevel>(["log", "warning", "error"]);

export function setLevel(level: LogLevel) {
  levelsToLog.clear();

  switch (level) {
    case "debug":
      levelsToLog.add("debug");
    case "info":
      levelsToLog.add("info");
    case "log":
      levelsToLog.add("log");
    case "warning":
      levelsToLog.add("warning");
    case "error":
      levelsToLog.add("error");
  }
}

export class Logger {
  constructor(readonly name: string) {}

  private prefix() {
    return `[${this.name}]`;
  }

  public log(...data: any[]) {
    if (levelsToLog.has("log")) {
      console.log(this.prefix(), ...data);
    }
  }

  public debug(...data: any[]) {
    if (levelsToLog.has("debug")) {
      console.debug(this.prefix(), ...data);
    }
  }

  public info(...data: any[]) {
    if (levelsToLog.has("info")) {
      console.info(this.prefix(), ...data);
    }
  }

  public warning(...data: any[]) {
    if (levelsToLog.has("warning")) {
      console.warn(this.prefix(), ...data);
    }
  }

  public error(...data: any[]) {
    if (levelsToLog.has("error")) {
      console.error(this.prefix(), ...data);
    }
  }
}

const loggers = new Map<string, Logger>();

export function getLogger(name: string): Logger {
  let logger = loggers.get(name);

  if (!logger) {
    logger = new Logger(name);
    loggers.set(name, logger);
  }

  return logger;
}
