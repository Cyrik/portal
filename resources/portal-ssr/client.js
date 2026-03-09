(function() {
  var session = window.SSR_SESSION;
  var proto = location.protocol === 'https:' ? 'wss:' : 'ws:';
  var ws;
  var reconnectDelay = 1000;

  function connect() {
    ws = new WebSocket(proto + '//' + location.host + '/ssr/ws?s=' + session);

    ws.onopen = function() {
      reconnectDelay = 1000;
    };

    ws.onmessage = function(e) {
      var msg = JSON.parse(e.data);
      switch (msg.op) {
        case 'replace':
          var el = document.getElementById(msg.id);
          if (el) el.outerHTML = msg.html;
          break;
        case 'prepend':
          var container = document.getElementById(msg.id);
          if (container) {
            // Remove the "waiting" placeholder if present
            var placeholder = container.querySelector('[data-placeholder]');
            if (placeholder) placeholder.remove();
            container.insertAdjacentHTML('afterbegin', msg.html);
          }
          break;
        case 'full':
          var target = document.getElementById(msg.id);
          if (target) target.outerHTML = msg.html;
          break;
      }
    };

    ws.onclose = function() {
      setTimeout(function() {
        reconnectDelay = Math.min(reconnectDelay * 2, 10000);
        connect();
      }, reconnectDelay);
    };
  }

  // Event delegation for toggle clicks
  document.addEventListener('click', function(e) {
    var el = e.target.closest('[data-action]');
    if (!el) return;
    e.stopPropagation();
    if (ws && ws.readyState === WebSocket.OPEN) {
      var msg = {
        op: el.dataset.action,
        valueId: el.dataset.valueId,
        path: el.dataset.path
      };
      if (el.dataset.index != null) msg.index = el.dataset.index;
      ws.send(JSON.stringify(msg));
    }
  });

  connect();
})();
