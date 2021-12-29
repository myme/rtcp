import io from 'socket.io-client';

const SIGNALING_SERVER_URL = 'http://localhost:9999';
const PC_CONFIG = {};

export type ConnectionState = 'connected' | 'disconnected';

export interface Props {
  onConnectionStateChange: (state: ConnectionState) => void,
  onMessage: (data: any) => void,
}

export class Socket {
  private channel: RTCDataChannel | null;
  private pc: RTCPeerConnection | null;
  private socket: ReturnType<typeof io>;

  constructor(readonly props: Props) {
    this.channel = null;
    this.pc = null;
    this.socket = io(SIGNALING_SERVER_URL, { autoConnect: false });

    this.socket.on('data', (data) => {
      console.log('Data received:', data);
      this.handleMessage(data);
    });

    this.socket.on('ready', () => {
      console.log('Ready');
      this.createPeerConnection();
      this.createDataChannel();
      this.sendOffer();
    });
  }

  connect() {
    this.socket.connect();
  }

  createPeerConnection() {
    try {
      this.pc = new RTCPeerConnection(PC_CONFIG);
      this.pc.onconnectionstatechange = (event) => {
        console.log('Peer connection state:', event);
      };
      this.pc.onicecandidate = (event) => { this.onIceCandidate(event); };
      this.pc.ondatachannel = (event) => {
        console.log('Data channel');
        this.setDataChannel(event.channel);
      };
      console.log('Peer connection created');
    } catch (error) {
      console.error('Peer connection failed:', error);
    }
  }

  handleMessage(message: any) {
    switch (message.type) {
      case 'offer':
        this.createPeerConnection();
        if (!this.pc) {
          throw new Error('No peer connection');
        }
        this.pc.setRemoteDescription(message);
        this.sendAnswer();
        break;
      case 'answer':
        if (!this.pc) {
          throw new Error('No peer connection');
        }
        this.pc.setRemoteDescription(message);
      case 'candidate':
        if (!this.pc) {
          throw new Error('No peer connection');
        }
        if (message.candidate) {
          this.pc.addIceCandidate(message.candidate);
        }
        break;
    }
  }

  setDataChannel(channel: RTCDataChannel) {
    console.log('setDataChannel()');
    this.channel = channel;
    this.channel.onmessage = (event) => { this.props.onMessage(event.data); };
    this.channel.onopen = () => { this.props.onConnectionStateChange('connected'); };
    this.channel.onclose = () => {
      this.props.onConnectionStateChange('disconnected');
      console.log('Closing Peer Connection');
      if (this.pc) {
        this.pc.close();
        this.pc = null;
      }
    };
  }

  onIceCandidate(event: RTCPeerConnectionIceEvent) {
    if (event.candidate) {
      console.log('ICE candidate');
      this.sendData({
        type: 'candidate',
        candidate: event.candidate,
      });
    }
  }

  async sendOffer() {
    console.log('Send offer');
    if (!this.pc) {
      throw new Error('No peer connection');
    }
    const sessionDescription = await this.pc.createOffer();
    this.pc.setLocalDescription(sessionDescription);
    this.sendData(sessionDescription);
  }

  async sendAnswer() {
    console.log('Send answer');
    if (!this.pc) {
      throw new Error('No peer connection');
    }
    const sessionDescription = await this.pc.createAnswer();
    this.pc.setLocalDescription(sessionDescription);
    this.sendData(sessionDescription);
  }

  sendData(data: object) {
    this.socket.emit('data', data);
  }

  send(message: any) {
    if (this.channel) {
      this.channel.send(message);
    }
  }

  createDataChannel() {
    if (!this.pc) {
      return;
    }
    const channel = this.pc.createDataChannel('chat-channel');
    this.setDataChannel(channel);
  }
}

export default function connect(props: Props) {
  const socket = new Socket(props);
  socket.connect();
  return socket;
}
