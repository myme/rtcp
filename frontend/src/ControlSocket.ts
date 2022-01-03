const SIGNALING_SERVER = 'ws://localhost:3001';

interface RequestHandler {
  resolve(response: string): void;
  reject(error: string): void;
}

export default class ControlSocket {
  private requestId = 0;
  private requestMap = new Map<number, RequestHandler>();
  private socket?: WebSocket;

  public constructor() {
    console.log('new ControlSocket()');
  }

  private async getSocket(): Promise<WebSocket> {
    if (this.socket) {
      switch (this.socket.readyState) {
        case WebSocket.OPEN:
          return this.socket;
        case WebSocket.CONNECTING:
        case WebSocket.CLOSING:
        case WebSocket.CLOSED:
        default:
          throw new Error('Control socket not open');
      }
    }

    this.socket = await this.connect();
    return this.socket;
  }

  private connect(): Promise<WebSocket> {
    return new Promise((resolve, reject) => {
      console.log('ControlSocket::connect()');

      const socket = new WebSocket(SIGNALING_SERVER);

      socket.addEventListener('open', () => {
        resolve(socket);
      });

      socket.addEventListener('error', () => {
        reject();
      });

      socket.addEventListener('message', (event) => {
        this.handleMessage(event.data);
      });
    });
  }

  public close() {
    if (this.socket) {
      console.log('ControlSocket::close()');
      this.socket.close();
      delete this.socket;
    }
  }

  private handleMessage(data: string) {
    console.log(`Got message: ${data}`);
    const message = JSON.parse(data);
    if (!message.id) {
      throw new Error(`Missing response id`);
    }

    const handler = this.requestMap.get(message.id);
    if (!handler) {
      throw new Error(`Invalid response id: ${message.id}`);
    }

    this.requestMap.delete(message.id);

    if (message.error) {
      handler.reject(message.error);
    } else if (!message.result) {
      handler.reject(`Missing request result: ${data}`);
    } else {
      handler.resolve(message.result);
    }
  }

  private async request(method: string) {
    const socket = await this.getSocket();
    const requestId = ++this.requestId;

    socket.send(JSON.stringify({ id: requestId, method }));

    return new Promise((resolve, reject) => {
      this.requestMap.set(requestId, { resolve, reject });
    });
  }

  public async newSession(): Promise<string> {
    const result = await this.request('newSession');

    if (typeof result !== 'string') {
      throw new Error(`Invalid "newSession" response: ${result}`);
    }

    return result;
  }
}
