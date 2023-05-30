import React, { useState } from "react";
import { Item, ItemType } from "../PeerConnection";
import Icon from "./Icon";

interface Props {
  onSubmit(item: Item): void,
}

function capitalize(input: string) {
  return input.substring(0, 1).toUpperCase() + input.substring(1);
}

export default function Form(props: Props): JSX.Element {
  const { onSubmit } = props;
  const [input, setInput] = useState('');
  const [autoType, setAutoType] = useState(true);
  const [hidden, setHidden] = useState(false);
  const [type, setType] = useState<ItemType>('text');

  const submit: React.FormEventHandler = (event) => {
    event.preventDefault();
    onSubmit({ type, value: input, hidden });
    setInput('');
    setAutoType(true);
    setHidden(false);
    setType('text');
  };

  const inputChange: React.ChangeEventHandler<HTMLInputElement> = (event) => {
    const { value } = event.target;
    setInput(value);
    if (autoType) {
      if (value.match(/^https?:\/\//) || value.match(/\w+\.\w{2,}/)) {
        setType('link');
      } else {
        setType('text');
      }
    }
  };

  const typeChange: React.ChangeEventHandler<HTMLSelectElement> = (event) => {
    setAutoType(false);
    const value = event.target.value.toLowerCase();
    switch (value) {
      case 'link':
      case 'text':
        // case 'file':
        setType(value);
    }
  };

  const types: ItemType[] = ['link', 'text', /* 'file' */];
  const secretIcon = hidden ? "eye-slash" : "eye";

  return (
    <form id="client" className="inline" onSubmit={submit}>
      <div className="group">
        <select value={type} onChange={typeChange}>
          {types.map(type => (
            <option key={type} value={type}>{capitalize(type)}</option>
          ))}
        </select>
        <button
          type="button"
          onClick={() => setHidden(h => !h)}
          title="Toggle hidden input"
        >
          <Icon icon={secretIcon} />
        </button>
        <input
          type={hidden ? 'password' : type}
          placeholder="Input"
          value={input}
          onChange={inputChange}
          autoFocus
        />
        <button type="submit" disabled={!input.trim().length}>Share</button>
      </div>
    </form>
  );
}
