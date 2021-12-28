import React from 'react';
import ReactDOM from 'react-dom';
import { BrowserRouter, Route, Routes } from 'react-router-dom';

import './style.css';
import Share from './Share';
import Start from './Start';

ReactDOM.render(
  <React.StrictMode>
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<Start />} />
        <Route path="/share" element={<Share />} />
      </Routes>
    </BrowserRouter>
  </React.StrictMode>,
  document.getElementById('app')
);
