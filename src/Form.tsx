import React, { useCallback, useState } from "react";

type ItemType = 'text' | 'hidden' | 'file';

export interface Item {
  type: ItemType,
  value: string,
}

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
    <form id="client" onSubmit={submit}>
      <input type="text" value={input} onChange={inputChange} />
      <select value={type} onChange={typeChange}>
        <option>Text</option>
        <option>Hidden</option>
        <option>File</option>
      </select>
      <button type="submit">Share</button>
    </form>
  );
}
