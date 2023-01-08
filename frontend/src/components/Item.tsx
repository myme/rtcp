import React, { useCallback, useState } from 'react';

import { Item as IItem } from '../PeerConnection';
import Icon from './Icon';

interface Props {
  item: IItem,
  onCopyItem(): void,
  onRemoveItem(): void,
}

export default function Item(props: Props) {
  const [showHidden, setShowHidden] = useState(false);
  const { item: { type, value }, onCopyItem, onRemoveItem } = props;

  const output = type === 'hidden' && !showHidden ? value.replace(/./g, '*') : value;

  const toggleShowHidden = useCallback(() => {
    setShowHidden(v => !v);
  }, [setShowHidden]);

  const hideHiddenBtnClass = type !== 'hidden' ? 'hidden' : '';

  return (
    <>
      <span>{output}</span>
      <span>
        <button className={`unstyled ${hideHiddenBtnClass}`} onClick={toggleShowHidden} title="Show item">
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
