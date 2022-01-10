import { Share } from "./Share";

const PC_CONFIG: RTCConfiguration = {};

import { getLogger } from "./Logger";
const logger = getLogger('PeerConnection');

export type ConnectionState = 'pending' | 'connected' | 'disconnected';

export type ItemType = 'text' | 'hidden'; // | 'file';

export interface Item {
  type: ItemType,
  value: string,
}

type MessageType = 'share' | 'removeShare';
type SessionType = 'offer' | 'answer';

export interface Props {
  onConnectionStateChange: (state: ConnectionState) => void,
  onIceCandidate(candidate: RTCIceCandidate): void,
  onSessionDescription(type: SessionType, description: RTCSessionDescriptionInit): void,
  onShare(share: Share): void,
  onRemoveShare(id: string): void,
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
    logger.log('PeerConnection::close()');
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
        logger.log('PeerConnection::createPeerConnection(): Peer connection state:', event);
      };
      this.pc.onicecandidate = (event) => { this.onIceCandidate(event); };
      this.pc.ondatachannel = (event) => {
        logger.log('PeerConnection::createPeerConnection(): Data channel event');
        this.setDataChannel(event.channel);
      };
      logger.log('PeerConnection::createPeerConnection(): Peer connection created');
    } catch (error) {
      logger.error('PeerConnection::createPeerConnection(): Peer connection failed:', error);
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

  private handleDataChannelMessage(message: string) {
    try {
      logger.log(`PeerConnection::handleDataChannelMessage(): ${message}`);
      const data = JSON.parse(message);

      if (!data || typeof data.type !== 'string') {
        throw new Error(`Invalid message: ${message}`);
      }

      const { type, ...payload } = data;

      switch (type as MessageType) {
        case 'share':
          if (typeof payload.share !== 'object') {
            throw new Error(`Invalid share object: ${payload.share}`);
          }
          this.props.onShare({ ...payload.share, 'direction': 'inbound' });
          break;
        case 'removeShare':
          if (typeof payload.id !== 'string') {
            throw new Error(`Invalid id: ${payload.id}`);
          }
          this.props.onRemoveShare(payload.id);
          break;
        default:
          throw new Error(`Invalid type: ${type}: ${message}`);
      }
    } catch (error) {
      if (error instanceof Error) {
        logger.error(`PeerConnection::handleDataChannelMessage(): ${error.message}`);
      } else {
        logger.error(`PeerConnection::handleDataChannelMessage(): Invalid message: ${message}`);
      }
    }
  }

  private setDataChannel(channel: RTCDataChannel) {
    logger.log('PeerConnection::setDataChannel()');
    this.channel = channel;
    this.channel.onmessage = (event) => {
      this.handleDataChannelMessage(event.data);
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
      logger.log('PeerConnection::onIceCandidate(): ICE candidate');
      this.props.onIceCandidate(event.candidate);
    }
  }

  public async sendOffer() {
    logger.log('PeerConnection::sendOffer()');
    this.createPeerConnection();
    this.createDataChannel();
    const pc = this.assertPeerConnection();
    const sessionDescription = await pc.createOffer();
    pc.setLocalDescription(sessionDescription);
    this.props.onSessionDescription('offer', sessionDescription);
    this.props.onConnectionStateChange('pending');
  }

  private async sendAnswer() {
    logger.log('PeerConnection::sendAnswer()');
    const pc = this.assertPeerConnection();
    const sessionDescription = await pc.createAnswer();
    pc.setLocalDescription(sessionDescription);
    this.props.onSessionDescription('answer', sessionDescription);
  }

  private send(type: MessageType, payload: object) {
    if (!this.channel) {
      logger.error('PeerConnection::send(): No data channel');
      return;
    }
    const message = JSON.stringify({ type, ...payload });
    logger.log(`PeerConnection::send(): ${message}`);
    this.channel.send(message);
  }

  public sendShare(share: Share) {
    this.send('share', { share });
  }

  public removeShare(id: string) {
    this.send('removeShare', { id });
  }
}
