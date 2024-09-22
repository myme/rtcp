import React, { useCallback, useState } from "react";

import { Item as IItem } from "../PeerConnection";
import Icon from "./Icon";

interface Props {
  clientId: string;
  item: IItem;
  onCopyItem(): void;
  onRemoveItem(): void;
}

export default function Item(props: Props) {
  const [showHidden, setShowHidden] = useState(false);
  const { clientId, item, onCopyItem, onRemoveItem } = props;

  const toggleShowHidden = useCallback(() => {
    setShowHidden((v) => !v);
  }, [setShowHidden]);

  const hideHiddenBtnClass = !item.hidden ? "hidden" : "";

  return (
    <>
      <Output item={item} showHidden={showHidden} />
      <small
        title={`From client ID: ${clientId}`}
      >{`${clientId.substring(0, 9)}…`}</small>
      <span>
        <button
          className={`unstyled ${hideHiddenBtnClass}`}
          onClick={toggleShowHidden}
          title="Show item"
        >
          {showHidden ? <Icon icon="eye-slash" /> : <Icon icon="eye" />}
        </button>
        <button className="unstyled" onClick={onCopyItem} title="Copy item">
          <Icon icon="clipboard" />
        </button>
        <button className="unstyled" onClick={onRemoveItem} title="Remove item">
          <Icon icon="trash" />
        </button>
      </span>
    </>
  );
}

function Output(props: { item: IItem; showHidden: boolean }) {
  const { item, showHidden } = props;
  const { value, type, hidden } = item;

  if (hidden && !showHidden) {
    return <span>***</span>;
  }

  if (type === "link") {
    const href = value.match(/https?:\/\//) ? value : `https://${value}`;
    return (
      <a href={href} target="_blank">
        {value}
      </a>
    );
  }

  return <span>{value}</span>;
}
