import {RequestManager, WebSocketTransport, Client} from "@open-rpc/client-js";

const transport = new WebSocketTransport("http://localhost:8080");
const client = new Client(new RequestManager([transport]));

let heartbeatInterval: number;

// Add connection event handlers
transport.connection.addEventListener("open", () => {
  console.log("WebSocket connected");
  // Start heartbeat every 30 seconds
  heartbeatInterval = setInterval(() => {
    try {
      client.notify({ method: "ping", params: {} });
    } catch (e) {
      console.log("Failed to send ping:", e);
    }
  }, 30000);
});

transport.connection.addEventListener("close", () => {
  console.log("WebSocket disconnected");
  clearInterval(heartbeatInterval);
  // Attempt to reconnect after 2 seconds
  setTimeout(() => {
    location.reload();
  }, 2000);
});

transport.connection.addEventListener("error", (error) => {
  console.log("WebSocket error:", error);
});

client.onNotification(lcdUpdate);

interface KeyInfo {
  normal: HTMLImageElement;
  pressed: HTMLImageElement;
  xpos: number;
}

const keyMap = new Map<number, KeyInfo>()

const keyON = 0x18;
const keyUSER = 0xC6;
const keyPRGM = 0xC5;
const keyALPHA = 0xC4;
const keySIGMA = 0x10;
const keyINV = 0x30;
const keySQRT = 0x70;
const keyLOG = 0x80;
const keyLN = 0xC0;
const keySWAP = 0x11;
const keyRDN = 0x31;
const keySIN = 0x71;
const keyCOS = 0x81;
const keyTAN = 0xC1;
const keySHIFT = 0x12;
const keyXEQ = 0x32;
const keySTO = 0x72;
const keyRCL = 0x82;
const keySST = 0xC2;
const keyENTER = 0x13;
const keyCHS = 0x73;
const keyEEX = 0x83;
const keyARROW = 0xC3;
const keyMINUS = 0x14;
const key7 = 0x34;
const key8 = 0x74;
const key9 = 0x84;
const keyPLUS = 0x15;
const key4 = 0x35;
const key5 = 0x75;
const key6 = 0x85;
const keyMUL = 0x16;
const key1 = 0x36;
const key2 = 0x76;
const key3 = 0x86;
const keyDIV = 0x17;
const key0 = 0x37;
const keyDOT = 0x77;
const keyRUN = 0x87;

function preventBackspaceHandler(evt) {
  evt = evt || window.event;
  if (evt.keyCode == 8 || evt.keyCode == 9) {
      return false;
  }
}

let rocker = new Map()

function rockerKey(xpos, name, code, direction, buddyCode) {
  const image = document.createElementNS('http://www.w3.org/2000/svg','image');
  image.setAttribute('width', "52");
  image.setAttribute('height', "19");
  image.setAttribute('x', xpos);
  image.setAttribute('y', "90");
  image.setAttribute('href', `image/key_${name}.png`);
  image.setAttribute('id', name);
  addKey(image, undefined, code, xpos);
  rocker.set(code, { buddy: buddyCode, x: xpos, direction: direction });
}

function normalKey(xpos, ypos, name, code) {
  keyImage(33, xpos, ypos, name, code);
}

function wideKey(xpos, ypos, name, code) {
  keyImage(80, xpos, ypos, name, code);
}

function keyImage(width: number, xpos: number, ypos: number, name: string, code: number): void {
  const image = document.createElementNS('http://www.w3.org/2000/svg','image');
  image.setAttribute('width', width.toString());
  image.setAttribute('height', "31");
  image.setAttribute('x', xpos.toString());
  image.setAttribute('y', ypos.toString());
  image.setAttribute('href', `image/key_${name}.png`);
  const imagePressed = document.createElementNS('http://www.w3.org/2000/svg','image');
  imagePressed.setAttribute('width', width.toString());
  imagePressed.setAttribute('height', "31");
  imagePressed.setAttribute('x', (xpos + 2).toString());
  imagePressed.setAttribute('y', (ypos - 2).toString());
  imagePressed.setAttribute('href', `image/key_${name}_pressed.png`);
  addKey(image, imagePressed, code, xpos);
}

function addKey(image, imagePressed, code, xpos): void {
  const calculator = document.getElementById("calculator");
  calculator.appendChild(image);
  keyMap.set(code, { normal: image, pressed: imagePressed, xpos: xpos });
  image.onmousedown = function (event: MouseEvent) {
    mouseEvent("key_press", event, code);
  }
  image.onmouseup = function (event: MouseEvent) {
    mouseEvent("key_release", event, code); }

  if (imagePressed) {
    imagePressed.onmousedown = function (event: MouseEvent) {
      mouseEvent("key_press", event, code);
    }
    imagePressed.onmouseup = function (event: MouseEvent) {
      mouseEvent("key_release", event, code); }
  }
}

function mouseEvent(what, event, code): void {
  if (event.button === 0) {  // left mouse button pressed
    keyMouseEvent(what, code)
  }
}

function positionRockerKey(what, rockerInfo, keyInfo, direction) {
  if (what === "key_press") {
    const dx = 2 * direction;
    keyInfo.normal.style.x = rockerInfo.x + dx;
  } else {
    keyInfo.normal.style.x = rockerInfo.x;
  }
}

function keyMouseEvent(what, code): void {
  client.notify( { method: what, params: { code: code, timestamp: Date.now() } });
  const rockerInfo = rocker.get(code);
  const info = keyMap.get(code);
  const calculator = document.getElementById("calculator");
  if (rockerInfo) {
    // Rocker key is actually two that move left or right together
    const buddy = keyMap.get(rockerInfo.buddy);
    positionRockerKey(what, rockerInfo, info, rockerInfo.direction);
    positionRockerKey(what, rocker.get(rockerInfo.buddy), keyMap.get(rockerInfo.buddy), rockerInfo.direction);
  } else {
    // Ordinary key
    if (what === "key_press") {
      calculator.replaceChild(info.pressed, info.normal);
    } else {
      calculator.replaceChild(info.normal, info.pressed);
    }
  }
}

function render() {
  //console.log("Hello hp41");
  const lcdText = document.getElementById("lcdText");
  lcdText.textContent = "MEMORY LOST";
  rockerKey(20, "ON", keyON, -1, keyUSER);
  rockerKey(77, "USER", keyUSER, 1, keyON);
  rockerKey(169, "PRGM", keyPRGM, -1, keyALPHA);
  rockerKey(226, "ALPHA", keyALPHA, 1, keyPRGM);
  normalKey(35, 148, "SIGMA", keySIGMA);
  normalKey(82, 148, "INV", keyINV);
  normalKey(129, 148, "SQRT", keySQRT);
  normalKey(176, 148, "LOG", keyLOG);
  normalKey(223, 148, "LN", keyLN);
  normalKey(35, 197, "SWAP", keySWAP);
  normalKey(82, 197, "RDN", keyRDN);
  normalKey(129, 197, "SIN", keySIN);
  normalKey(176, 197, "COS", keyCOS);
  normalKey(223, 197, "TAN", keyTAN);
  normalKey(35, 246, "SHIFT", keySHIFT);
  normalKey(82, 246, "XEQ", keyXEQ);
  normalKey(129, 246, "STO", keySTO);
  normalKey(176, 246, "RCL", keyRCL);
  normalKey(223, 246, "SST", keySST);
  wideKey(35, 295, "ENTER", keyENTER);
  normalKey(129, 295, "CHS", keyCHS);
  normalKey(176, 295, "EEX", keyEEX);
  normalKey(223, 295, "ARROW", keyARROW);
  normalKey(35, 344, "MINUS", keyMINUS);
  normalKey(98, 344, "7", key7);
  normalKey(161, 344, "8", key8);
  normalKey(224, 344, "9", key9);
  normalKey(35, 393, "PLUS", keyPLUS);
  normalKey(98, 393, "4", key4);
  normalKey(161, 393, "5", key5);
  normalKey(224, 393, "6", key6);
  normalKey(35, 442, "MUL", keyMUL);
  normalKey(98, 442, "1", key1);
  normalKey(161, 442, "2", key2);
  normalKey(224, 442, "3", key3);
  normalKey(35, 491, "DIV", keyDIV);
  normalKey(98, 491, "0", key0);
  normalKey(161, 491, "DOT", keyDOT);
  normalKey(224, 491, "RUN", keyRUN);

  document.onkeydown = function (event) {
    const code = keyCodes[event.key];
    if (code) {
      keyMouseEvent("key_press", code);
    }
  }
  document.onkeyup = function (event) {
    const code = keyCodes[event.key];
    if (code) {
      keyMouseEvent("key_release", code);
    }
  }
}

const keyCodes = {
  "F1": keyON,
  "F2": keyUSER,
  "F3": keyPRGM,
  "F4": keyALPHA,
  "F5": keyRUN,
  "a": keySIGMA,
  "b": keyINV,
  "c": keySQRT,
  "d": keyLOG,
  "e": keyLN,
  "f": keySWAP,
  "g": keyRDN,
  "h": keySIN,
  "i": keyCOS,
  "j": keyTAN,
  "Shift": keySHIFT,
  "k": keyXEQ,
  "l": keySTO,
  "m": keyRCL,
  "Tab": keySST,
  "n": keyENTER,
  "Enter": keyENTER,
  "o": keyCHS,
  "p": keyEEX,
  "Backspace": keyARROW,
  "q": keyMINUS,
  "r": key7,
  "s": key8,
  "t": key9,
  "u": keyPLUS,
  "v": key4,
  "w": key5,
  "x": key6,
  "y": keyMUL,
  "z": key1,
  "Space": key0,
  "0": key0,
  "1": key1,
  "2": key2,
  "3": key3,
  "4": key4,
  "5": key5,
  "6": key6,
  "7": key7,
  "8": key8,
  "9": key9,
  "/": keyDIV,
  "=": key2,
  ".": keyDOT,
  ",": keyDOT,
}

document.onkeydown = preventBackspaceHandler;
render();

function lcdUpdate(notification): void {
  if (notification.method === "lcd-update") {
    const lcdText = document.getElementById("lcdText");
    lcdText.textContent = notification.params.lcd;
    const lcdAnn = document.getElementById("lcdAnnunciators");
    lcdAnn.textContent = notification.params.ann;
  }
}

// async function askServer() {
//   const result = await client.request({method: "foo", params: [2, 2]});
//   console.log(result)
//   await client.notify({method: "say", params: ["This is from your new client"]});
// }


// askServer();
