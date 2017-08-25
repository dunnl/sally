import SallyGame from "./sallygame.js"

window.onload = function () {

    var gameElts = {
          form     : document.getElementById('guess__form')
        , likes    : document.getElementById('guess.likes')
        , notlikes : document.getElementById('guess.notlikes')
        , guessHeader : document.getElementById('guess__header')
    }


    const messageUl = document.getElementById('message__list');
    const gameUl = document.getElementById('game__list');
    const subField = document.getElementById('game__subform');

    var game = new SallyGame("ws://sally.dunnl.io", gameUl, messageUl, gameElts, subField);

    return true;
}
