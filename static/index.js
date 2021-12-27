(function () {
  'use strict';

  const SIGNALING_SERVER_URL = 'http://localhost:9999';
  const PC_CONFIG = {};

  function setupSendMessageHandler(client, channel) {
    const formEl = document.getElementById(client);
    const handler = (event) => {
      console.log('submit');
      event.preventDefault();

      const textInputEl = formEl.querySelector('input');
      const message = textInputEl.value;

      textInputEl.value = '';
      textInputEl.focus();

      channel.send(message);

      const messageEl = document.createElement('p');
      messageEl.appendChild(document.createTextNode(`> ${message}`));

      const messagesArea = formEl.querySelector('pre');
      messagesArea.appendChild(messageEl);
    };
    formEl.addEventListener('submit', handler);
    return handler;
  }

  const handleChannelStatusChange = (client, channel) => () => {
    const statusEl = document.querySelector(`#${client} .status`);
    let state = channel.readyState;
    if (state === 'open') {
      statusEl.textContent = 'Connected';
    } else {
      statusEl.textContent = 'Disconnected';
    }
  }

  function main() {
    let pc; // Peer connection

    let socket = io(SIGNALING_SERVER_URL, { autoConnect: true });

    socket.on('data', (data) => {
      console.log('Data received:', data);
      switch (data.type) {
        case 'offer':
          createPeerConnection();
          pc.setRemoteDescription(data);
          sendAnswer();
          break;
        case 'answer':
          pc.setRemoteDescription(data);
        case 'candidate':
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
      const channel = pc.createDataChannel('chat-channel');
      setDataChannel(channel);
    }

    function setDataChannel(channel) {
      console.log('setDataChannel()');
      channel.onmessage = (event) => { handleMessage('client', event) };
      const statusChangeHandler = handleChannelStatusChange('client', channel);
      channel.onopen = statusChangeHandler;
      let handler;
      channel.onclose = () => {
        statusChangeHandler();
        console.log('Closing Peer Connection');
        pc.close();
        pc = null;
        const formEl = document.getElementById('client');
        formEl.removeEventListener('submit', handler);
      };
      handler = setupSendMessageHandler('client', channel);
    }

    function onIceCandidate(event) {
      if (event.candidate) {
        console.log('ICE candidate');
        sendData({
          type: 'candidate',
          candidate: event.candidate,
        });
      }
    }

    function sendOffer() {
      console.log('Send offer');
      pc.createOffer()
        .then((sessionDescription) => {
          pc.setLocalDescription(sessionDescription);
          sendData(sessionDescription);
        })
    }

    function sendAnswer() {
      pc.createAnswer()
        .then((sessionDescription) => {
          pc.setLocalDescription(sessionDescription);
          sendData(sessionDescription);
        })
    }

    function sendData(data) {
      socket.emit('data', data);
    }

    function handleMessage(client, event) {
      const messageEl = document.createElement('p');
      messageEl.appendChild(document.createTextNode(`< ${event.data}`));

      const messagesArea = document.querySelector(`#${client} pre`);
      messagesArea.appendChild(messageEl);
    }
  }

  document.addEventListener('DOMContentLoaded', () => main());
})();
