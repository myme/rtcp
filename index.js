(function () {
  'use strict';

  function setupSendMessageHandler(client, channel) {
    const formEl = document.getElementById(client);
    formEl.addEventListener('submit', (event) => {
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
    });
  }

  function main() {
    const sendStatusEl = document.getElementById('send-connection-status');
    const receiveStatusEl = document.getElementById('receive-connection-status');
    let receiveChannel;

    let localConnection = new RTCPeerConnection();

    let sendChannel = localConnection.createDataChannel('chat-channel');
    sendChannel.onmessage = (event) => { handleMessage('client1', event) };
    sendChannel.onopen = handleSendChannelStatusChange;
    sendChannel.onclose = handleSendChannelStatusChange;
    setupSendMessageHandler('client1', sendChannel);

    let remoteConnection = new RTCPeerConnection();
    remoteConnection.ondatachannel = (event) => {
      receiveChannel = event.channel;
      receiveChannel.onmessage = (event) => { handleMessage('client2', event); };
      receiveChannel.onopen = handleReceiveChannelStatusChange;
      receiveChannel.onclose = handleReceiveChannelStatusChange;
      setupSendMessageHandler('client2', receiveChannel);
    };

    localConnection.onicecandidate = e => !e.candidate
      || remoteConnection.addIceCandidate(e.candidate).catch(handleAddCandidateError);

    remoteConnection.onicecandidate = e => !e.candidate
      || localConnection.addIceCandidate(e.candidate).catch(handleAddCandidateError);

    localConnection
      .createOffer()
      .then(offer => localConnection.setLocalDescription(offer))
      .then(() => remoteConnection.setRemoteDescription(localConnection.localDescription))
      .then(() => remoteConnection.createAnswer())
      .then(answer => remoteConnection.setLocalDescription(answer))
      .then(() => localConnection.setRemoteDescription(remoteConnection.localDescription))
      .catch(handleCreateDescriptionError);

    function handleSendChannelStatusChange() {
      if (sendChannel) {
        let state = sendChannel.readyState;

        if (state === 'open') {
          sendStatusEl.textContent = 'Connected'
        } else {
          sendStatusEl.textContent = 'Disconnected'
        }
      }
    }

    function handleReceiveChannelStatusChange() {
      if (receiveChannel) {
        let state = receiveChannel.readyState;

        if (state === 'open') {
          receiveStatusEl.textContent = 'Connected'
        } else {
          receiveStatusEl.textContent = 'Disconnected'
        }
      }
    }

    function handleMessage(client, event) {
      const messageEl = document.createElement('p');
      messageEl.appendChild(document.createTextNode(`< ${event.data}`));

      const messagesArea = document.querySelector(`#${client} pre`);
      messagesArea.appendChild(messageEl);
    }

    function handleAddCandidateError() {
      sendStatusEl.textContent = 'Error!';
    }

    function handleCreateDescriptionError() {
      sendStatusEl.textContent = 'Error!';
    }
  }

  document.addEventListener('DOMContentLoaded', () => main());
})();
