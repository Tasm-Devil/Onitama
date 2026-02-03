// SSE (Server-Sent Events) helper for Onitama real-time updates
(function () {
    'use strict';

    let lobbyEventSource = null;
    let gameEventSource = null;

    // Open SSE connection for lobby updates
    function openLobbyStream(app) {
        closeLobbyStream();

        console.log('Opening lobby SSE stream...');
        lobbyEventSource = new EventSource('/1/onitama/games/stream');

        lobbyEventSource.onopen = function () {
            console.log('Lobby SSE stream connected');
        };

        lobbyEventSource.onmessage = function (event) {
            try {
                const data = JSON.parse(event.data);
                console.log('Lobby event received:', data);
                if (app.ports && app.ports.lobbyEventReceived) {
                    app.ports.lobbyEventReceived.send(data);
                }
            } catch (e) {
                console.error('Error parsing lobby event:', e);
            }
        };

        lobbyEventSource.onerror = function (err) {
            console.error('Lobby SSE error:', err);
            // EventSource will automatically reconnect
        };
    }

    // Close lobby SSE connection
    function closeLobbyStream() {
        if (lobbyEventSource) {
            console.log('Closing lobby SSE stream');
            lobbyEventSource.close();
            lobbyEventSource = null;
        }
    }

    // Open SSE connection for game updates
    function openGameStream(gameId, app) {
        closeGameStream();

        console.log('Opening game SSE stream for game:', gameId);
        gameEventSource = new EventSource('/1/onitama/games/' + gameId + '/stream');

        gameEventSource.onopen = function () {
            console.log('Game SSE stream connected for game:', gameId);
        };

        gameEventSource.onmessage = function (event) {
            try {
                const data = JSON.parse(event.data);
                console.log('Game event received:', data);
                if (app.ports && app.ports.gameEventReceived) {
                    app.ports.gameEventReceived.send(data);
                }
            } catch (e) {
                console.error('Error parsing game event:', e);
            }
        };

        gameEventSource.onerror = function (err) {
            console.error('Game SSE error:', err);
            // EventSource will automatically reconnect
        };
    }

    // Close game SSE connection
    function closeGameStream() {
        if (gameEventSource) {
            console.log('Closing game SSE stream');
            gameEventSource.close();
            gameEventSource = null;
        }
    }

    // Initialize SSE when Elm app is ready
    window.initSSE = function (app) {
        // Subscribe to lobby stream commands from Elm
        if (app.ports && app.ports.openLobbyStream) {
            app.ports.openLobbyStream.subscribe(function () {
                openLobbyStream(app);
            });
        }

        if (app.ports && app.ports.closeLobbyStream) {
            app.ports.closeLobbyStream.subscribe(function () {
                closeLobbyStream();
            });
        }

        // Subscribe to game stream commands from Elm
        if (app.ports && app.ports.openGameStream) {
            app.ports.openGameStream.subscribe(function (gameId) {
                openGameStream(gameId, app);
            });
        }

        if (app.ports && app.ports.closeGameStream) {
            app.ports.closeGameStream.subscribe(function () {
                closeGameStream();
            });
        }

        console.log('SSE module initialized');
    };
})();
