import React, { useCallback } from 'react';

import { Item } from './PeerConnection';

function Clipboard() {
  return (
    <svg width="1em" className="icon" aria-hidden="true" focusable="false" data-prefix="far" data-icon="clipboard" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 384 512">
      <path fill="currentColor" d="M336 64h-80c0-35.3-28.7-64-64-64s-64 28.7-64 64H48C21.5 64 0 85.5 0 112v352c0 26.5 21.5 48 48 48h288c26.5 0 48-21.5 48-48V112c0-26.5-21.5-48-48-48zM192 40c13.3 0 24 10.7 24 24s-10.7 24-24 24-24-10.7-24-24 10.7-24 24-24zm144 418c0 3.3-2.7 6-6 6H54c-3.3 0-6-2.7-6-6V118c0-3.3 2.7-6 6-6h42v36c0 6.6 5.4 12 12 12h168c6.6 0 12-5.4 12-12v-36h42c3.3 0 6 2.7 6 6z" >
      </path>
    </svg>
  );
}

function Trash() {
  return (
    <svg width="1.1em" className="icon" aria-hidden="true" focusable="false" data-prefix="far" data-icon="trash" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512">
      <path fill="currentColor" d="M432 80h-82.4l-34-56.7A48 48 0 0 0 274.4 0H173.6a48 48 0 0 0-41.2 23.3L98.4 80H16A16 16 0 0 0 0 96v16a16 16 0 0 0 16 16h16l21.2 339a48 48 0 0 0 47.9 45h245.8a48 48 0 0 0 47.9-45L416 128h16a16 16 0 0 0 16-16V96a16 16 0 0 0-16-16zM173.6 48h100.8l19.2 32H154.4zm173.3 416H101.11l-21-336h287.8z">
      </path>
    </svg>
  )
}

interface Props {
  item: Item,
  onCopyItem(): void,
  onRemoveItem(): void,
}

export default function Item(props: Props) {
  const { item: { type, value }, onCopyItem, onRemoveItem } = props;

  const output = type === 'hidden' ? value.replace(/./g, '*') : value;

  return (
    <>
      <span>{output}</span>
      <span className="pull-right">
        <button className="unstyled" onClick={onCopyItem} title="Copy item">
          <Clipboard />
        </button>
        <button className="unstyled" onClick={onRemoveItem} title="Remove item">
          <Trash />
        </button>
      </span>
    </>
  );
}
