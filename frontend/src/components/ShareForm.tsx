import React, { useCallback, useState } from "react";
import { Item, ItemType } from "../PeerConnection";

interface Props {
  onSubmit(item: Item): void,
}

function capitalize(input: string) {
  return input.substring(0, 1).toUpperCase() + input.substring(1);
}

export default function Form(props: Props): JSX.Element {
  const { onSubmit } = props;
  const [input, setInput] = useState('');
  const [type, setType] = useState<ItemType>('text');

  const submit: React.FormEventHandler = useCallback((event) => {
    event.preventDefault();
    onSubmit({ type, value: input });
    setInput('');
    setType('text');
  }, [input, type, setInput, setType, onSubmit]);

  const inputChange: React.ChangeEventHandler<HTMLInputElement> = useCallback((event) => {
    setInput(event.target.value);
  }, [setInput]);

  const typeChange: React.ChangeEventHandler<HTMLSelectElement> = useCallback((event) => {
    const value = event.target.value.toLowerCase();
    switch (value) {
      case 'text':
      case 'hidden':
      // case 'file':
        setType(value);
    }
  }, [setType]);

  const types: ItemType[] = ['text', 'hidden', /* 'file' */];

  return (
    <form id="client" className="inline" onSubmit={submit}>
      <div className="group">
        <select value={type} onChange={typeChange}>
          {types.map(type => (
            <option key={type} value={type}>{capitalize(type)}</option>
          ))}
        </select>
        <input
          type={type === 'hidden' ? 'password' : type}
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
