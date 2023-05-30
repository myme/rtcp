import React, { useContext, useEffect, useMemo, useState } from "react";
import { Outlet } from "react-router";

import ControlSocket, { Session } from "../ControlSocket";
import PeerConnection, { ConnectionState } from "../PeerConnection";
import { Share } from "../share";

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
}

const ConnectionContext = React.createContext<Context>({
  connectionState: { status: "pending" },
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

export default function ConnectionManager(props: Props) {
  const { onAddShare, onShareRemoved, onReset } = props;
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
        onIceServersUpdated(iceServers) {
          peerConnection.setIceServers(iceServers);
        },
        onPeerJoined() {
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
  };

  return (
    <ConnectionContext.Provider value={context}>
      <Outlet />
    </ConnectionContext.Provider>
  );
}
