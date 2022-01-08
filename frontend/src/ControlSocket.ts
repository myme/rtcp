const protocol = location.protocol === 'http:' ? 'ws:' : 'wss:';
const SIGNALING_SERVER = `${protocol}//${location.host}`;

interface RequestHandler {
  resolve(response: string): void;
  reject(error: string): void;
}

interface Props {
  onPeerJoined(): void,
  onBroadcast(message: any): void,
}

export default class ControlSocket {
  private requestId = 0;
  private requestMap = new Map<number, RequestHandler>();
  private sessionId?: string;
  private socket?: WebSocket;

  public constructor(readonly props: Props) {
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
    console.log(`ControlSocket::handleMessage(): Got message: ${data}`);
    const message = JSON.parse(data);
    if (!message.id) {
      switch (message.method) {
        case 'peerJoined':
          console.log('ControlSocket::handleMessage(): Peer joined');
          this.props.onPeerJoined();
          return;
        case 'broadcast':
          console.log('ControlSocket::handleMessage(): Broadcast');
          this.props.onBroadcast(message.params);
          return;
        default:
          throw new Error(`Missing response id: ${data}`);
      }
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

  private async request(method: string, params?: object) {
    const socket = await this.getSocket();
    const requestId = ++this.requestId;

    const message = { id: requestId, method, ...params };
    const messageText = JSON.stringify(message);
    console.log(`ControlSocket::request(): Sending mesage: ${messageText}`);
    socket.send(messageText);

    return new Promise((resolve, reject) => {
      this.requestMap.set(requestId, { resolve, reject });
    });
  }

  public async broadcast(type: string, data: object) {
    const result = await this.request('broadcast', { params: { type, ...data } });
    console.log(`ControlSocket::broadcast(): response: ${result}`);
  }

  public async newSession(): Promise<string> {
    const result = await this.request('newSession');

    if (typeof result !== 'string') {
      throw new Error(`Invalid "newSession" response: ${result}`);
    }

    this.sessionId = result;
    return result;
  }

  public async joinSession(sessionId: string) {
    if (this.sessionId) {
      console.log(`ControlSocket::joinSession(): Already joined session: ${this.sessionId}`);
      return;
    }

    const result = await this.request('joinSession', { sessionId });

    if (result !== 'OK') {
      throw new Error(`Invalid "joinSession" response: ${result}`);
    }

    this.sessionId = sessionId;
  }

  public async leaveSession() {
    const result = await this.request('leaveSession');

    if (result !== 'OK') {
      throw new Error(`Invalid "leaveSession" response: ${result}`);
    }

    delete this.sessionId;
  }
}
