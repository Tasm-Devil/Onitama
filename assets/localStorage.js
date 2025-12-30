// LocalStorage helper for Onitama sessions
(function() {
    'use strict';

    const STORAGE_KEY = 'onitama-sessions';

    // Clean up duplicate or invalid sessions
    function cleanupSessions(sessions) {
        const cleaned = [];
        const seen = new Set();
        
        for (const session of sessions) {
            // Skip sessions with empty playerName
            if (!session.playerName || session.playerName === '') {
                console.log('Removing session with empty playerName:', session);
                continue;
            }
            
            // Use gameId+token as unique key
            const key = `${session.gameId}-${session.token}`;
            
            if (!seen.has(key)) {
                seen.add(key);
                cleaned.push(session);
            } else {
                console.log('Removing duplicate session:', session);
            }
        }
        
        return cleaned;
    }

    // Load sessions from localStorage and send to Elm on startup
    function loadSessions(app) {
        try {
            const stored = localStorage.getItem(STORAGE_KEY);
            let sessions = stored ? JSON.parse(stored) : [];
            
            // Clean up duplicates and invalid entries
            sessions = cleanupSessions(sessions);
            
            // Save cleaned sessions back
            if (sessions.length !== (stored ? JSON.parse(stored).length : 0)) {
                localStorage.setItem(STORAGE_KEY, JSON.stringify(sessions));
                console.log('Cleaned up sessions, saved:', sessions);
            }
            
            // Send to Elm
            if (app.ports && app.ports.loadSession) {
                app.ports.loadSession.send(sessions);
            }
        } catch (e) {
            console.error('Error loading sessions from localStorage:', e);
            // Send empty array on error
            if (app.ports && app.ports.loadSession) {
                app.ports.loadSession.send([]);
            }
        }
    }

    // Save a session to localStorage
    function saveSession(session) {
        try {
            const stored = localStorage.getItem(STORAGE_KEY);
            let sessions = stored ? JSON.parse(stored) : [];
            
            // Remove any existing session for this game (by gameId and token)
            // This prevents duplicates even if playerName changes
            sessions = sessions.filter(s => 
                !(s.gameId === session.gameId && s.token === session.token)
            );
            
            // Also remove any session with same gameId but empty playerName
            // (cleanup from incomplete joins)
            sessions = sessions.filter(s =>
                !(s.gameId === session.gameId && (!s.playerName || s.playerName === ''))
            );
            
            // Add the new session
            sessions.push(session);
            
            // Save back to localStorage
            localStorage.setItem(STORAGE_KEY, JSON.stringify(sessions));
            
            console.log('Saved session to localStorage:', session);
            console.log('Total sessions:', sessions.length);
        } catch (e) {
            console.error('Error saving session to localStorage:', e);
        }
    }

    // Clear a specific session
    function clearSession(session) {
        try {
            const stored = localStorage.getItem(STORAGE_KEY);
            let sessions = stored ? JSON.parse(stored) : [];
            
            // Remove the session
            sessions = sessions.filter(s => 
                !(s.gameId === session.gameId && s.playerName === session.playerName)
            );
            
            // Save back to localStorage
            localStorage.setItem(STORAGE_KEY, JSON.stringify(sessions));
            
            console.log('Cleared session from localStorage:', session);
        } catch (e) {
            console.error('Error clearing session from localStorage:', e);
        }
    }

    // Initialize when Elm app is ready
    window.initLocalStorage = function(app) {
        // Load sessions on startup
        loadSessions(app);
        
        // Subscribe to save commands from Elm
        if (app.ports && app.ports.saveSession) {
            app.ports.saveSession.subscribe(function(session) {
                saveSession(session);
            });
        }
        
        // Subscribe to clear commands from Elm
        if (app.ports && app.ports.clearSession) {
            app.ports.clearSession.subscribe(function(session) {
                clearSession(session);
            });
        }
    };
})();
