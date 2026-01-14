// LocalStorage helper for Onitama player identity
(function() {
    'use strict';

    const STORAGE_KEY = 'onitama-player';

    // Load player identity from localStorage and send to Elm on startup
    function loadPlayer(app) {
        try {
            const stored = localStorage.getItem(STORAGE_KEY);
            let player = stored ? JSON.parse(stored) : null;

            // Validate player object
            if (player && (!player.playerName || !player.token)) {
                console.log('Invalid player object, clearing:', player);
                player = null;
                localStorage.removeItem(STORAGE_KEY);
            }

            // Send to Elm
            if (app.ports && app.ports.loadPlayer) {
                app.ports.loadPlayer.send(player || {});
            }
        } catch (e) {
            console.error('Error loading player from localStorage:', e);
            // Send empty object on error
            if (app.ports && app.ports.loadPlayer) {
                app.ports.loadPlayer.send({});
            }
        }
    }

    // Save player identity to localStorage
    function savePlayer(player) {
        try {
            // Validate player object
            if (!player.playerName || !player.token) {
                console.error('Cannot save invalid player:', player);
                return;
            }

            // Save to localStorage
            localStorage.setItem(STORAGE_KEY, JSON.stringify(player));

            console.log('Saved player to localStorage:', player.playerName);
        } catch (e) {
            console.error('Error saving player to localStorage:', e);
        }
    }

    // Clear player identity (logout)
    function clearPlayer() {
        try {
            localStorage.removeItem(STORAGE_KEY);
            console.log('Cleared player from localStorage');
        } catch (e) {
            console.error('Error clearing player from localStorage:', e);
        }
    }

    // Initialize when Elm app is ready
    window.initLocalStorage = function(app) {
        // Load player on startup
        loadPlayer(app);

        // Subscribe to save commands from Elm
        if (app.ports && app.ports.savePlayer) {
            app.ports.savePlayer.subscribe(function(player) {
                savePlayer(player);
            });
        }

        // Expose clear function globally for manual logout
        window.clearOnitamaPlayer = clearPlayer;
    };
})();
