import React from 'react';
import { Link } from 'react-router-dom';

import logo from '../favicon.svg';

interface Props {
  small?: boolean;
}

export default function Header(props: Props) {
  const { small } = props;

  if (small) {
    return (
      <Link to="/">
        <h3>
          <img src={logo} height="30px" className="App-logo" alt="logo" />
          {' '}
          <span>xchg</span>
        </h3>
      </Link>
    );
  }

  return (
    <Link to="/">
      <img src={logo} height="60px" className="App-logo" alt="logo" />
      <h1>xchg</h1>
    </Link>
  )
}
