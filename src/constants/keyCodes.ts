// HP-41 Key codes
export const keyON = 0x18;
export const keyUSER = 0xC6;
export const keyPRGM = 0xC5;
export const keyALPHA = 0xC4;
export const keySIGMA = 0x10;
export const keyINV = 0x30;
export const keySQRT = 0x70;
export const keyLOG = 0x80;
export const keyLN = 0xC0;
export const keySWAP = 0x11;
export const keyRDN = 0x31;
export const keySIN = 0x71;
export const keyCOS = 0x81;
export const keyTAN = 0xC1;
export const keySHIFT = 0x12;
export const keyXEQ = 0x32;
export const keySTO = 0x72;
export const keyRCL = 0x82;
export const keySST = 0xC2;
export const keyENTER = 0x13;
export const keyCHS = 0x73;
export const keyEEX = 0x83;
export const keyARROW = 0xC3;
export const keyMINUS = 0x14;
export const key7 = 0x34;
export const key8 = 0x74;
export const key9 = 0x84;
export const keyPLUS = 0x15;
export const key4 = 0x35;
export const key5 = 0x75;
export const key6 = 0x85;
export const keyMUL = 0x16;
export const key1 = 0x36;
export const key2 = 0x76;
export const key3 = 0x86;
export const keyDIV = 0x17;
export const key0 = 0x37;
export const keyDOT = 0x77;
export const keyRUN = 0x87;

// Keyboard mapping
export const keyCodes: Record<string, number> = {
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
};
