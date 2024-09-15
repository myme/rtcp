import React, { useContext, useEffect, useMemo, useState } from "react";
import { Outlet } from "react-router";

import ControlSocket, { Session } from "../ControlSocket";
import { getLogger } from "../Logger";
import PeerConnection, { ConnectionState } from "../PeerConnection";
import { Share } from "../share";
import Header from './Header';

const logger = getLogger("ConnectionManager");

interface Props {
  onAddShare(share: Share): void;
  onShareRemoved(id: string): void;
  onReset(): void;
}

interface Context {
  connectionState: ConnectionState;
  controlSocket?: ControlSocket;
  peerConnection?: PeerConnection;
  session?: Session;
  user: { clientId?: string };
}

const ConnectionContext = React.createContext<Context>({
  connectionState: { status: "pending" },
  user: {},
});

export function useConnectionState() {
  const { connectionState } = useContext(ConnectionContext);
  return connectionState;
}

export function useControlSocket() {
  const { controlSocket } = useContext(ConnectionContext);
  return controlSocket;
}

export function usePeerConnection() {
  const { peerConnection } = useContext(ConnectionContext);
  return peerConnection;
}

export function useSession() {
  const { session } = useContext(ConnectionContext);
  return session;
}

export function useUser() {
  const { user } = useContext(ConnectionContext);
  return user;
}

export default function ConnectionManager(props: Props) {
  const { onAddShare, onShareRemoved, onReset } = props;
  const [clientId, setClientId] = useState<string>();
  const [connectionState, setConnectionState] = useState<ConnectionState>({
    status: "pending",
  });
  const [session, setSession] = useState<Session>();

  const controlSocket = useMemo(
    () =>
      new ControlSocket({
        onError(error: string) {
          setConnectionState({ status: "error", error });
        },
        onTokenData(tokenData) {
          setClientId(tokenData.token_clientId);
          peerConnection.setClientId(tokenData.token_clientId);
        },
        onIceServersUpdated(iceServers) {
          peerConnection.setIceServers(iceServers);
        },
        onPeerJoined(clientId) {
          logger.info(`Peer joined: ${clientId}`);
          peerConnection.sendOffer();
        },
        onBroadcast(message) {
          peerConnection.handleControlMessage(message);
        },
        setSession,
      }),
    []
  );

  const peerConnection = useMemo(
    () =>
      new PeerConnection({
        onConnectionStateChange: setConnectionState,
        onIceCandidate(candidate) {
          controlSocket.broadcast("candidate", { candidate });
        },
        onSessionDescription(type, description) {
          controlSocket.broadcast(type, { description });
        },
        onShare: onAddShare,
        onRemoveShare: onShareRemoved,
      }),
    []
  );

  // Cleanup
  useEffect(
    () => () => {
      // Next tick to let child components send leave message
      setTimeout(() => {
        controlSocket.close();
        peerConnection.close();
        setConnectionState({ status: "pending" });
        onReset();
      }, 0);
    },
    []
  );

  const context = {
    connectionState,
    controlSocket,
    peerConnection,
    session,
    user: { clientId },
  };

  return (
    <ConnectionContext.Provider value={context}>
      <Header />
      <small>{`ClientID: ${clientId}`}</small>
      <Outlet />
    </ConnectionContext.Provider>
  );
}
