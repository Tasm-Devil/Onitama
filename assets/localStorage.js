// LocalStorage helper for Onitama player identities
(function() {
    'use strict';

    const STORAGE_KEY = 'onitama-players';

    // Load all player identities from localStorage and send to Elm on startup
    function loadPlayers(app) {
        try {
            const stored = localStorage.getItem(STORAGE_KEY);
            let players = stored ? JSON.parse(stored) : [];

            // Validate it's an array
            if (!Array.isArray(players)) {
                console.log('Invalid players data, clearing:', players);
                players = [];
                localStorage.removeItem(STORAGE_KEY);
            }

            // Validate each player object and filter out invalid ones
            players = players.filter(function(player) {
                if (!player.playerName || !player.token) {
                    console.log('Invalid player object, skipping:', player);
                    return false;
                }
                return true;
            });

            // Send to Elm
            if (app.ports && app.ports.loadPlayers) {
                app.ports.loadPlayers.send(players);
            }
        } catch (e) {
            console.error('Error loading players from localStorage:', e);
            // Send empty list on error
            if (app.ports && app.ports.loadPlayers) {
                app.ports.loadPlayers.send([]);
            }
        }
    }

    // Save player identity to localStorage (add or update)
    // After saving, notifies Elm with updated players list to keep everything in sync
    function savePlayer(player, app) {
        try {
            // Validate player object
            if (!player.playerName || !player.token) {
                console.error('Cannot save invalid player:', player);
                return;
            }

            // Load existing players
            const stored = localStorage.getItem(STORAGE_KEY);
            let players = stored ? JSON.parse(stored) : [];

            if (!Array.isArray(players)) {
                players = [];
            }

            // Find if player already exists (by name)
            const existingIndex = players.findIndex(function(p) {
                return p.playerName === player.playerName;
            });

            if (existingIndex >= 0) {
                // Update existing player
                players[existingIndex] = player;
                console.log('Updated player in localStorage:', player.playerName);
            } else {
                // Add new player
                players.push(player);
                console.log('Added new player to localStorage:', player.playerName);
            }

            // Save to localStorage
            localStorage.setItem(STORAGE_KEY, JSON.stringify(players));

            // Notify Elm of updated players list (keeps Elm in sync with localStorage)
            if (app.ports && app.ports.loadPlayers) {
                app.ports.loadPlayers.send(players);
            }
        } catch (e) {
            console.error('Error saving player to localStorage:', e);
        }
    }

    // Clear all player identities (logout)
    function clearPlayers() {
        try {
            localStorage.removeItem(STORAGE_KEY);
            console.log('Cleared all players from localStorage');
        } catch (e) {
            console.error('Error clearing players from localStorage:', e);
        }
    }

    // Initialize when Elm app is ready
    window.initLocalStorage = function(app) {
        // Load players on startup
        loadPlayers(app);

        // Subscribe to save commands from Elm
        if (app.ports && app.ports.savePlayer) {
            app.ports.savePlayer.subscribe(function(player) {
                savePlayer(player, app);
            });
        }

        // Expose clear function globally for manual logout
        window.clearOnitamaPlayers = clearPlayers;
    };
})();
