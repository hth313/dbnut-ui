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

function rockerKey(xpos, name, code, dir, buddyCode) {
  const image = document.createElementNS('http://www.w3.org/2000/svg','image');
  image.setAttribute('width', 52);
  image.setAttribute('height', 19);
  image.setAttribute('x', xpos);
  image.setAttribute('y', 90);
  image.setAttribute('href', `image/key_${name}.png`);
  image.setAttribute('id', name);
  addKey(image);
}

function normalKey(xpos, ypos, name, code) {
  keyImage(33, xpos, ypos, name, code);
}

function wideKey(xpos, ypos, name, code) {
  keyImage(80, xpos, ypos, name, code);
}

function keyImage(width, xpos, ypos, name, code) {
  const image = document.createElementNS('http://www.w3.org/2000/svg','image');
  image.setAttribute('width', width);
  image.setAttribute('height', 31);
  image.setAttribute('x', xpos);
  image.setAttribute('y', ypos);
  image.setAttribute('href', `image/key_${name}.png`);
  addKey(image);
}

function addKey(image) {
  const calculator = document.getElementById("calculator");
  calculator.appendChild(image);
}

function render() {
  console.log("Hello hp41");
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
}

document.onkeydown = preventBackspaceHandler;
render();
