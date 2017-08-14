import SallyGame from "./sallygame.js"

window.onload = function () {

    var gameElts = {
          form     : document.getElementById('guess__form')
        , likes    : document.getElementById('guess.likes')
        , notlikes : document.getElementById('guess.notlikes')
    }

    console.log(gameElts.form);

    const messageUl = document.getElementById('message__list');
    const gameUl = document.getElementById('game__list');

    var game = new SallyGame("ws://localhost:8080", gameUl, messageUl, gameElts);

    return true;
}
