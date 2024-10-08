import { Share } from "./share";

import { getLogger } from "./Logger";
const logger = getLogger("PeerConnection");

export type ConnectionStatus =
  | "pending"
  | "connected"
  | "disconnected"
  | "error";
export interface ConnectionState {
  status: ConnectionStatus;
  error?: string;
}

export type ItemType = "link" | "text"; // | 'file';

export interface Item {
  type: ItemType;
  value: string;
  hidden: boolean;
}

type MessageType = "share" | "removeShare";
type SessionType = "offer" | "answer";

export interface Props {
  onConnectionStateChange(state: ConnectionState): void;
  onIceCandidate(candidate: RTCIceCandidate): void;
  onSessionDescription(
    type: SessionType,
    description: RTCSessionDescriptionInit,
  ): void;
  onShare(share: Share): void;
  onRemoveShare(id: string): void;
}

export default class PeerConnection {
  private clientId?: string;
  private channel?: RTCDataChannel;
  private pc?: RTCPeerConnection;
  private rtcConfig: RTCConfiguration = {};

  constructor(readonly props: Props) {}

  private assertPeerConnection() {
    if (!this.pc) {
      throw new Error("PeerConnection: No peer connection");
    }
    return this.pc;
  }

  public close() {
    logger.debug("PeerConnection::close()");
    if (this.channel) {
      this.channel.close();
      delete this.channel;
    }
    if (this.pc) {
      this.pc.close();
      delete this.pc;
    }
  }

  public getClientId() {
    return this.clientId;
  }

  public setClientId(clientId: string) {
    this.clientId = clientId;
  }

  public setIceServers(iceServers: RTCIceServer[]) {
    this.rtcConfig.iceServers = iceServers;
  }

  private createPeerConnection() {
    try {
      this.pc = new RTCPeerConnection(this.rtcConfig);
      this.pc.onconnectionstatechange = (event) => {
        logger.debug(
          "PeerConnection::createPeerConnection(): Peer connection state:",
          event,
        );
      };
      this.pc.onicecandidate = (event) => {
        this.onIceCandidate(event);
      };
      this.pc.ondatachannel = (event) => {
        logger.debug(
          "PeerConnection::createPeerConnection(): Data channel event",
        );
        this.setDataChannel(event.channel);
      };
      logger.debug(
        "PeerConnection::createPeerConnection(): Peer connection created",
      );
    } catch (error) {
      logger.error(
        "PeerConnection::createPeerConnection(): Peer connection failed:",
        error,
      );
    }
  }

  private createDataChannel() {
    if (!this.pc) {
      return;
    }
    const channel = this.pc.createDataChannel("chat-channel");
    this.setDataChannel(channel);
  }

  public handleControlMessage(message: any) {
    switch (message.type) {
      case "offer": {
        this.createPeerConnection();
        const pc = this.assertPeerConnection();
        pc.setRemoteDescription(message.description);
        this.sendAnswer();
        break;
      }
      case "answer": {
        const pc = this.assertPeerConnection();
        pc.setRemoteDescription(message.description);
      }
      case "candidate":
        if (message.candidate) {
          const pc = this.assertPeerConnection();
          pc.addIceCandidate(message.candidate);
        }
        break;
    }
  }

  private handleDataChannelMessage(message: string) {
    try {
      logger.debug(`PeerConnection::handleDataChannelMessage(): ${message}`);
      const data = JSON.parse(message);

      if (!data || typeof data.type !== "string") {
        throw new Error(`Invalid message: ${message}`);
      }

      const { type, clientId, ...payload } = data;

      switch (type as MessageType) {
        case "share":
          if (typeof payload.share !== "object") {
            throw new Error(`Invalid share object: ${payload.share}`);
          }
          this.props.onShare({
            ...payload.share,
            direction: "inbound",
            clientId,
          });
          break;
        case "removeShare":
          if (typeof payload.id !== "string") {
            throw new Error(`Invalid id: ${payload.id}`);
          }
          this.props.onRemoveShare(payload.id);
          break;
        default:
          throw new Error(`Invalid type: ${type}: ${message}`);
      }
    } catch (error) {
      if (error instanceof Error) {
        logger.error(
          `PeerConnection::handleDataChannelMessage(): ${error.message}`,
        );
      } else {
        logger.error(
          `PeerConnection::handleDataChannelMessage(): Invalid message: ${message}`,
        );
      }
    }
  }

  private setDataChannel(channel: RTCDataChannel) {
    logger.debug("PeerConnection::setDataChannel()");
    this.channel = channel;
    this.channel.onmessage = (event) => {
      this.handleDataChannelMessage(event.data);
    };
    this.channel.onopen = () => {
      this.props.onConnectionStateChange({ status: "connected" });
    };
    this.channel.onclose = () => {
      this.props.onConnectionStateChange({ status: "disconnected" });
      this.close();
    };
  }

  private onIceCandidate(event: RTCPeerConnectionIceEvent) {
    if (event.candidate) {
      logger.debug("PeerConnection::onIceCandidate(): ICE candidate");
      this.props.onIceCandidate(event.candidate);
    }
  }

  public async sendOffer() {
    logger.debug("PeerConnection::sendOffer()");
    this.createPeerConnection();
    this.createDataChannel();
    const pc = this.assertPeerConnection();
    const sessionDescription = await pc.createOffer();
    pc.setLocalDescription(sessionDescription);
    this.props.onSessionDescription("offer", sessionDescription);
    this.props.onConnectionStateChange({ status: "pending" });
  }

  private async sendAnswer() {
    logger.debug("PeerConnection::sendAnswer()");
    const pc = this.assertPeerConnection();
    const sessionDescription = await pc.createAnswer();
    pc.setLocalDescription(sessionDescription);
    this.props.onSessionDescription("answer", sessionDescription);
  }

  private send(type: MessageType, payload: object) {
    if (!this.channel) {
      logger.error("PeerConnection::send(): No data channel");
      return;
    }
    const message = JSON.stringify({
      type,
      clientId: this.getClientId(),
      ...payload,
    });
    logger.debug(`PeerConnection::send(): ${message}`);
    this.channel.send(message);
  }

  public sendShare(share: Share) {
    this.send("share", { share });
  }

  public removeShare(id: string) {
    this.send("removeShare", { id });
  }
}
