const SIGNALING_SERVER = 'ws://localhost:3001';

interface RequestHandler {
  resolve(response: string): void;
  reject(error: string): void;
}

export default class SignalingSocket {
  private requestId = 0;
  private requestMap = new Map<number, RequestHandler>();
  private socket?: WebSocket;

  private async getSocket(): Promise<WebSocket> {
    return new Promise((resolve, reject) => {
      if (this.socket) {
        switch (this.socket.readyState) {
          case WebSocket.OPEN:
            resolve(this.socket);
          case WebSocket.CONNECTING:
            break;
          case WebSocket.CLOSING:
          case WebSocket.CLOSED:
          default:
            reject(new Error('WebSocket closed or closing'));
        }
        return;
      }

      const socket = new WebSocket(SIGNALING_SERVER);
      this.socket = socket;

      socket.addEventListener('open', () => {
        resolve(socket);
      });

      socket.addEventListener('error', () => {
      });

      socket.addEventListener('message', (event) => {
        this.handleMessage(event.data);
      });
    });
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

  public async newSession() {
    return await this.request('newSession');
  }
}
