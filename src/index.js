import { Elm } from "./Main.elm";
import { Howl, Howler } from "howler";

const app = Elm.Main.init({
  node: document.querySelector("main")
});

// Howler.volume(0.5);

app.ports.emitSound((soundToPlay) => {
  console.log('playing sound', soundToPlay);
  const sound = new Howl({
    src: ['interface3.wav']
  })
  sound.play();
})
