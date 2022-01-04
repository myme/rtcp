const PC_CONFIG = {};

export type ConnectionState = 'pending' | 'connected' | 'disconnected';

export type ItemType = 'text' | 'hidden' | 'file';

export interface Item {
  type: ItemType,
  value: string,
}

type SessionType = 'offer' | 'answer';

export interface Props {
  onConnectionStateChange: (state: ConnectionState) => void,
  onIceCandidate(candidate: RTCIceCandidate): void,
  onSessionDescription(type: SessionType, description: RTCSessionDescriptionInit): void,
  onItem: (item: Item) => void,
}

export default class PeerConnection {
  private channel?: RTCDataChannel;
  private pc?: RTCPeerConnection;

  constructor(readonly props: Props) {
  }

  private assertPeerConnection() {
    if (!this.pc) {
      throw new Error('PeerConnection: No peer connection');
    }
    return this.pc;
  }

  public close() {
    console.log('PeerConnection::close()');
    if (this.channel) {
      this.channel.close();
      delete this.channel;
    }
    if (this.pc) {
      this.pc.close();
      delete this.pc;
    }
  }

  private createPeerConnection() {
    try {
      this.pc = new RTCPeerConnection(PC_CONFIG);
      this.pc.onconnectionstatechange = (event) => {
        console.log('PeerConnection::createPeerConnection(): Peer connection state:', event);
      };
      this.pc.onicecandidate = (event) => { this.onIceCandidate(event); };
      this.pc.ondatachannel = (event) => {
        console.log('PeerConnection::createPeerConnection(): Data channel event');
        this.setDataChannel(event.channel);
      };
      console.log('PeerConnection::createPeerConnection(): Peer connection created');
    } catch (error) {
      console.error('PeerConnection::createPeerConnection(): Peer connection failed:', error);
    }
  }

  private createDataChannel() {
    if (!this.pc) {
      return;
    }
    const channel = this.pc.createDataChannel('chat-channel');
    this.setDataChannel(channel);
  }

  public handleControlMessage(message: any) {
    switch (message.type) {
      case 'offer': {
        this.createPeerConnection();
        const pc = this.assertPeerConnection();
        pc.setRemoteDescription(message.description);
        this.sendAnswer();
        break;
      }
      case 'answer': {
        const pc = this.assertPeerConnection();
        pc.setRemoteDescription(message.description);
      }
      case 'candidate':
        if (message.candidate) {
          const pc = this.assertPeerConnection();
          pc.addIceCandidate(message.candidate);
        }
        break;
    }
  }

  private setDataChannel(channel: RTCDataChannel) {
    console.log('PeerConnection::setDataChannel()');
    this.channel = channel;
    this.channel.onmessage = (event) => {
      this.props.onItem(JSON.parse(event.data));
    };
    this.channel.onopen = () => {
      this.props.onConnectionStateChange('connected');
    };
    this.channel.onclose = () => {
      this.props.onConnectionStateChange('disconnected');
      this.close();
    };
  }

  private onIceCandidate(event: RTCPeerConnectionIceEvent) {
    if (event.candidate) {
      console.log('PeerConnection::onIceCandidate(): ICE candidate');
      this.props.onIceCandidate(event.candidate);
    }
  }

  public async sendOffer() {
    console.log('PeerConnection::sendOffer()');
    this.createPeerConnection();
    this.createDataChannel();
    const pc = this.assertPeerConnection();
    const sessionDescription = await pc.createOffer();
    pc.setLocalDescription(sessionDescription);
    this.props.onSessionDescription('offer', sessionDescription);
    this.props.onConnectionStateChange('pending');
  }

  private async sendAnswer() {
    console.log('PeerConnection::sendAnswer()');
    const pc = this.assertPeerConnection();
    const sessionDescription = await pc.createAnswer();
    pc.setLocalDescription(sessionDescription);
    this.props.onSessionDescription('answer', sessionDescription);
  }

  public sendItem(item: Item) {
    if (this.channel) {
      const message = JSON.stringify(item);
      console.log(`PeerConnection::sendItem(): ${message}`);
      this.channel.send(message);
    }
  }
}
