import React from "react";
import { Link } from "react-router-dom";
import { QRCodeSVG } from "qrcode.react";

import { Session } from "../ControlSocket";
import { prettifyShareId } from "../utils";
import { getLogger } from "../Logger";

import { useControlSocket } from "./ConnectionManager";
import PinForm from "./PinForm";

const logger = getLogger('Pending');

interface PendingProps {
  session?: Session;
  shareId: string;
}

export default function Pending({ session, shareId }: PendingProps) {
  const controlSocket = useControlSocket();

  const onConnectWrapper = (sharePin: string) => {
    if (!controlSocket) {
      logger.error("No control socket!");
      return;
    }
    controlSocket.joinSession({ id: shareId, pin: sharePin });
  };

  if (!session) {
    return (
      <>
        <h1>{`Connect to share: ${prettifyShareId(shareId)}`}</h1>
        <PinForm onSubmit={onConnectWrapper} />
      </>
    );
  }

  return (
    <>
      <h1>
        Share ID:
        <Link to={`/${session.id}`}>{prettifyShareId(session.id)}</Link>
      </h1>
      <h1>{`PIN: ${session.pin}`}</h1>
      <p>
        <QRCodeSVG value={`${location}`} />
      </p>
    </>
  );
}
