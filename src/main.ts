import './style.css'

import io from 'socket.io-client';

const SIGNALING_SERVER_URL = 'http://localhost:9999';
const PC_CONFIG = {};

function setupSendMessageHandler(client: string, channel: RTCDataChannel) {
  const formEl = document.querySelector<HTMLFormElement>(`#${client}`);
  if (!formEl) {
    throw new Error('No form element in DOM');
  }
  const handler = (event: Event) => {
    console.log('submit');
    event.preventDefault();

    const textInputEl = formEl.querySelector<HTMLInputElement>('input');
    if (!textInputEl) {
      throw new Error('No input element in DOM');
    }
    const message = textInputEl.value;

    textInputEl.value = '';
    textInputEl.focus();

    channel.send(message);

    const messageEl = document.createElement('p');
    messageEl.appendChild(document.createTextNode(`> ${message}`));

    const messagesArea = formEl.querySelector('pre');
    messagesArea?.appendChild(messageEl);
  };
  formEl.addEventListener('submit', handler);
  return handler;
}

const handleChannelStatusChange = (client: string, channel: RTCDataChannel) => () => {
  const statusEl = document.querySelector(`#${client} .status`);
  if (!statusEl) {
    throw new Error('No status element in DOM');
  }
  let state = channel.readyState;
  if (state === 'open') {
    statusEl.textContent = 'Connected';
  } else {
    statusEl.textContent = 'Disconnected';
  }
}

function main() {
  let pc: RTCPeerConnection | null; // Peer connection

  let socket = io(SIGNALING_SERVER_URL, { autoConnect: true });

  socket.on('data', (data) => {
    console.log('Data received:', data);
    switch (data.type) {
      case 'offer':
        createPeerConnection();
        if (!pc) {
          throw new Error('No peer connection');
        }
        pc.setRemoteDescription(data);
        sendAnswer();
        break;
      case 'answer':
        if (!pc) {
          throw new Error('No peer connection');
        }
        pc.setRemoteDescription(data);
      case 'candidate':
        if (!pc) {
          throw new Error('No peer connection');
        }
        if (data.candidate) {
          pc.addIceCandidate(data.candidate);
        }
        break;
    }
  });

  socket.on('ready', () => {
    console.log('Ready');
    createPeerConnection();
    createDataChannel();
    sendOffer();
  });

  function createPeerConnection() {
    try {
      pc = new RTCPeerConnection(PC_CONFIG);
      pc.onconnectionstatechange = (event) => {
        console.log('Peer connection state:', event);
      };
      pc.onicecandidate = onIceCandidate;
      pc.ondatachannel = (event) => {
        console.log('Data channel');
        setDataChannel(event.channel);
      };
      console.log('Peer connection created');
    } catch (error) {
      console.error('Peer connection failed:', error);
    }
  }

  function createDataChannel() {
    if (!pc) {
      return;
    }
    const channel = pc.createDataChannel('chat-channel');
    setDataChannel(channel);
  }

  function setDataChannel(channel: RTCDataChannel) {
    console.log('setDataChannel()');
    channel.onmessage = (event) => { handleMessage('client', event) };
    const statusChangeHandler = handleChannelStatusChange('client', channel);
    channel.onopen = statusChangeHandler;
    let handler: (event: Event) => void;
    channel.onclose = () => {
      statusChangeHandler();
      console.log('Closing Peer Connection');
      if (pc) {
        pc.close();
        pc = null;
      }
      const formEl = document.getElementById('client');
      if (!formEl) {
        throw new Error('No form element in DOM');
      }
      formEl.removeEventListener('submit', handler);
    };
    handler = setupSendMessageHandler('client', channel);
  }

  function onIceCandidate(event: RTCPeerConnectionIceEvent) {
    if (event.candidate) {
      console.log('ICE candidate');
      sendData({
        type: 'candidate',
        candidate: event.candidate,
      });
    }
  }

  async function sendOffer() {
    console.log('Send offer');
    if (!pc) {
      throw new Error('No peer connection');
    }
    const sessionDescription = await pc.createOffer();
    pc.setLocalDescription(sessionDescription);
    sendData(sessionDescription);
  }

  async function sendAnswer() {
    console.log('Send answer');
    if (!pc) {
      throw new Error('No peer connection');
    }
    const sessionDescription = await pc.createAnswer();
    pc.setLocalDescription(sessionDescription);
    sendData(sessionDescription);
  }

  function sendData(data: object) {
    socket.emit('data', data);
  }

  function handleMessage(client: string, event: MessageEvent<any>) {
    const messageEl = document.createElement('p');
    messageEl.appendChild(document.createTextNode(`< ${event.data}`));

    const messagesArea = document.querySelector(`#${client} pre`);
    if (!messagesArea) {
      throw new Error('No messages area in DOM');
    }
    messagesArea.appendChild(messageEl);
  }
}

document.addEventListener('DOMContentLoaded', () => main());
