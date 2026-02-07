// Sound effects for Onitama move notifications
(function () {
    'use strict';

    var sounds = [];

    // Preload all move sounds
    for (var i = 1; i <= 5; i++) {
        var num = i < 10 ? '0' + i : '' + i;
        var audio = new Audio('/mp3/move_' + num + '.mp3');
        audio.preload = 'auto';
        sounds.push(audio);
    }

    function playRandomMoveSound() {
        var index = Math.floor(Math.random() * sounds.length);
        var clone = sounds[index].cloneNode();
        clone.play().catch(function () {
            // Ignore autoplay policy errors
        });
    }

    window.initSound = function (app) {
        if (app.ports && app.ports.playSound) {
            app.ports.playSound.subscribe(function (command) {
                if (command === 'move') {
                    playRandomMoveSound();
                }
            });
        }
        console.log('Sound module initialized');
    };
})();
