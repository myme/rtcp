const PC_CONFIG = {};

export type ConnectionState = 'connected' | 'disconnected';

export type ItemType = 'text' | 'hidden' | 'file';

export interface Item {
  type: ItemType,
  value: string,
}

export interface Props {
  onConnectionStateChange: (state: ConnectionState) => void,
  onItem: (item: Item) => void,
}

export default class PeerConnection {
  private channel?: RTCDataChannel;
  private pc?: RTCPeerConnection;

  constructor(readonly props: Props) {
  }

  close() {
    if (this.channel) {
      this.channel.close;
      delete this.channel;
    }
    if (this.pc) {
      this.pc.close();
      delete this.pc;
    }
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
    this.channel.onmessage = (event) => {
      this.props.onItem(JSON.parse(event.data));
    };
    this.channel.onopen = () => { this.props.onConnectionStateChange('connected'); };
    this.channel.onclose = () => {
      this.props.onConnectionStateChange('disconnected');
      console.log('Closing Peer Connection');
      this.close();
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

  sendItem(item: Item) {
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
