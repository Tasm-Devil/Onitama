# Onitama Multiplayer Game made with ELM and Haskell

## Makefile
There's a `Makefile` included with the following targets:

- `all`   -- Calls first `setup` and then `build`
- `setup` -- Set up everything: install ghc and dependencies. (Needs `stack`, `elm`
  and `elm-test`.)
- `build` -- Build the server and the client.
- `server-start` -- Start the server here: <http://localhost:3000/>. Requests sent
  to this server will trigger a recompilation of the client code (not the server
  code).

## JSON API
JSON Message from Server to Client after two game moves. Last move comes first, thats fp style (Head of the List).
```
{
  "cards": ["Eel", "Rabbit", "Tiger", "Rooster", "Horse"],
  "history": [
    { "color": "Black", "card": "Rooster", "from": [1, 4], "move": [-1, -1] },
    { "color": "White", "card": "Eel", "from": [2, 0], "move": [-1, 1] }
  ]
}
```

JSON Message from Client to server after third game move.
```
{"color":"White","card":"Rabbit","from":[3,0],"move":[1,1]}
```


## Client
The Game logic is implemented entirely on the client side only.
So there's no checking for cheating on the server side currently!
This is on purpose because I want to use it for demonstration.
You can play the older [non-multiplayer version here ](https://tasm-devil.github.io/Onitama/) since it doesn't require a backend server.

### Thanks
- Inspiration came from [here](http://onitama.lannysport.net/)
- Http-Server HowTo from [here](https://elmprogramming.com/decoding-json-part-1.html)

## Server
The Server is my first haskell project. So don't expect very much. I still do not understand monads ;)

### ToDos
In the order in which I would like to tackle them.

- Implement http polling temporarily.
- New http GET-Request to see all open games.
- landing page for game creation and selection.
- API should return error code if game with gameid is not created.
- JSON for GameMove is to verbose. Simplyfy it to something like `{"move":"white:c1b2:elephant"}`
- Use WebSocket to get new moves without the need to do http polling.
- Implement Chat feature
- Authetification by player name at least.
- More Cards from the explansion.


### Test with curl

You can easily test the API with some simple curl commands.

```
curl -X POST http://localhost:3000/game -w "\n"
curl http://localhost:3000/game/1 -w "\n"
curl -X POST -d '{"color": "White","card": "Ox","from": [3,0],"move": [0,1]}' -H 'Content-Type: application/json' http://localhost:5019/game/1 -w "\n"
```

### Expansion Cards

- Senseis Path [Cards from Addon: Senseis Path](https://www.gadgetsville.store/wp-content/uploads/2017/12/16096-c.jpg)

```
FOX = Card("fox", Player.RED, [Pos(1, 1), Pos(1, 0), Pos(1, -1)])
DOG = Card("dog", Player.BLUE, [Pos(-1, 1), Pos(-1, 0), Pos(-1, -1)])
GIRAFFE = Card("giraffe", Player.BLUE, [Pos(-2, 1), Pos(0, -1), Pos(2, 1)])
PANDA = Card("panda", Player.RED, [Pos(-1, -1), Pos(0, 1), Pos(1, 1)])
BEAR = Card("bear", Player.BLUE, [Pos(-1, 1), Pos(0, 1), Pos(1, -1)])
KIRIN = Card("kirin", Player.RED, [Pos(-1, 2), Pos(0, -2), Pos(1, 2)])
SEA_SNAKE = Card("sea_snake", Player.BLUE, [Pos(-1, -1), Pos(0, 1), Pos(2, 0)])
VIPER = Card("viper", Player.RED, [Pos(-2, 0), Pos(0, 1), Pos(1, -1)])
PHOENIX = Card("phoenix", Player.BLUE, [Pos(-2, 0), Pos(-1, 1), Pos(1, 1), Pos(2, 0)])
MOUSE = Card("mouse", Player.BLUE, [Pos(-1, -1), Pos(0, 1), Pos(1, 0)])
RAT = Card("rat", Player.RED, [Pos(-1, 0), Pos(0, 1), Pos(1, -1)])
TURTLE = Card("turtle", Player.RED, [Pos(-2, 0), Pos(-1, -1), Pos(1, -1), Pos(2, 0)])
TANUKI = Card("tanuki", Player.BLUE, [Pos(-1, -1), Pos(0, 1), Pos(2, 1)])
IGUANA = Card("iguana", Player.RED, [Pos(-2, 1), Pos(0, 1), Pos(1, -1)])
SABLE = Card("sable", Player.BLUE, [Pos(-2, 0), Pos(-1, -1), Pos(1, 1)])
OTTER = Card("otter", Player.RED, [Pos(-1, 1), Pos(1, -1), Pos(2, 0)])
```

- Promo Cards [from here](https://www.arcanewonders.com/product/onitama-promo-cards/)

### Thanks
Thanks to <https://github.com/haskell-servant/example-servant-elm>
