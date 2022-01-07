import React, { useCallback, useState } from "react";
import { Item, ItemType } from "./PeerConnection";

interface Props {
  onSubmit(item: Item): void,
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
      case 'file':
        setType(value);
    }
  }, [setType]);

  return (
    <form id="client" className="inline" onSubmit={submit}>
      <span className="group">
        <select value={type} onChange={typeChange}>
          <option value="text">Text</option>
          <option value="hidden">Hidden</option>
          <option value="file">File</option>
        </select>
        <input type={type === 'hidden' ? 'password' : type} value={input} onChange={inputChange} />
      </span>
      <button type="submit" disabled={!input.trim().length}>Share</button>
    </form>
  );
}
