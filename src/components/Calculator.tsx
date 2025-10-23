import { useEffect, useRef, useCallback } from 'react';
import * as keys from '../constants/keyCodes';

interface CalculatorProps {
  onKeyEvent: (method: string, code: number) => void;
  lcdText: string;
  annunciators: string;
}

interface KeyInfo {
  normal: SVGImageElement;
  pressed: SVGImageElement | null;
  xpos: number;
}

interface RockerInfo {
  buddy: number;
  x: number;
  direction: number;
}

export function Calculator({ onKeyEvent, lcdText, annunciators }: CalculatorProps) {
  const svgRef = useRef<SVGSVGElement>(null);
  const keyMapRef = useRef(new Map<number, KeyInfo>());
  const rockerRef = useRef(new Map<number, RockerInfo>());

  // Handle keyboard events
  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      const code = keys.keyCodes[event.key];
      if (code !== undefined) {
        event.preventDefault();
        handleKeyPress(code);
      }
    };

    const handleKeyUp = (event: KeyboardEvent) => {
      const code = keys.keyCodes[event.key];
      if (code !== undefined) {
        event.preventDefault();
        handleKeyRelease(code);
      }
    };

    // Prevent backspace navigation
    const preventBackspace = (evt: KeyboardEvent) => {
      if (evt.keyCode === 8 || evt.keyCode === 9) {
        evt.preventDefault();
        return false;
      }
    };

    document.addEventListener('keydown', handleKeyDown);
    document.addEventListener('keyup', handleKeyUp);
    document.addEventListener('keydown', preventBackspace);

    return () => {
      document.removeEventListener('keydown', handleKeyDown);
      document.removeEventListener('keyup', handleKeyUp);
      document.removeEventListener('keydown', preventBackspace);
    };
  }, []);

  const updateKeyVisuals = useCallback((code: number, isPressed: boolean) => {
    const rockerInfo = rockerRef.current.get(code);
    const keyInfo = keyMapRef.current.get(code);
    const calculator = svgRef.current;

    if (!keyInfo || !calculator) return;

    if (rockerInfo) {
      // Rocker key - move both keys together
      const buddy = keyMapRef.current.get(rockerInfo.buddy);
      const dx = isPressed ? 2 * rockerInfo.direction : 0;
      keyInfo.normal.setAttribute('x', (rockerInfo.x + dx).toString());

      const buddyRockerInfo = rockerRef.current.get(rockerInfo.buddy);
      if (buddy && buddyRockerInfo) {
        const buddyDx = isPressed ? 2 * rockerInfo.direction : 0;
        buddy.normal.setAttribute('x', (buddyRockerInfo.x + buddyDx).toString());
      }
    } else {
      // Normal key - swap images
      if (keyInfo.pressed) {
        if (isPressed) {
          calculator.replaceChild(keyInfo.pressed, keyInfo.normal);
        } else {
          calculator.replaceChild(keyInfo.normal, keyInfo.pressed);
        }
      }
    }
  }, []);

  const handleKeyPress = useCallback((code: number) => {
    onKeyEvent("key_press", code);
    updateKeyVisuals(code, true);
  }, [onKeyEvent, updateKeyVisuals]);

  const handleKeyRelease = useCallback((code: number) => {
    onKeyEvent("key_release", code);
    updateKeyVisuals(code, false);
  }, [onKeyEvent, updateKeyVisuals]);

  const createRockerKey = (xpos: number, name: string, code: number, direction: number, buddyCode: number) => {
    const image = document.createElementNS('http://www.w3.org/2000/svg', 'image');
    image.setAttribute('width', "52");
    image.setAttribute('height', "19");
    image.setAttribute('x', xpos.toString());
    image.setAttribute('y', "90");
    image.setAttribute('href', `image/key_${name}.png`);
    image.setAttribute('id', name);

    rockerRef.current.set(code, { buddy: buddyCode, x: xpos, direction: direction });
    keyMapRef.current.set(code, { normal: image, pressed: null, xpos: xpos });

    image.onmousedown = (event: MouseEvent) => {
      if (event.button === 0) handleKeyPress(code);
    };
    image.onmouseup = (event: MouseEvent) => {
      if (event.button === 0) handleKeyRelease(code);
    };

    return image;
  };

  const createNormalKey = (xpos: number, ypos: number, name: string, code: number) => {
    return createKeyImage(33, xpos, ypos, name, code);
  };

  const createWideKey = (xpos: number, ypos: number, name: string, code: number) => {
    return createKeyImage(80, xpos, ypos, name, code);
  };

  const createKeyImage = (width: number, xpos: number, ypos: number, name: string, code: number) => {
    const image = document.createElementNS('http://www.w3.org/2000/svg', 'image');
    image.setAttribute('width', width.toString());
    image.setAttribute('height', "31");
    image.setAttribute('x', xpos.toString());
    image.setAttribute('y', ypos.toString());
    image.setAttribute('href', `image/key_${name}.png`);

    const imagePressed = document.createElementNS('http://www.w3.org/2000/svg', 'image');
    imagePressed.setAttribute('width', width.toString());
    imagePressed.setAttribute('height', "31");
    imagePressed.setAttribute('x', (xpos + 2).toString());
    imagePressed.setAttribute('y', (ypos - 2).toString());
    imagePressed.setAttribute('href', `image/key_${name}_pressed.png`);

    keyMapRef.current.set(code, { normal: image, pressed: imagePressed, xpos: xpos });

    image.onmousedown = (event: MouseEvent) => {
      if (event.button === 0) handleKeyPress(code);
    };
    image.onmouseup = (event: MouseEvent) => {
      if (event.button === 0) handleKeyRelease(code);
    };

    imagePressed.onmousedown = (event: MouseEvent) => {
      if (event.button === 0) handleKeyPress(code);
    };
    imagePressed.onmouseup = (event: MouseEvent) => {
      if (event.button === 0) handleKeyRelease(code);
    };

    return image;
  };

  // Initialize calculator display
  useEffect(() => {
    if (!svgRef.current) return;

    const svg = svgRef.current;

    // Clear any existing keys
    while (svg.firstChild) {
      svg.removeChild(svg.firstChild);
    }

    // Add background
    const background = document.createElementNS('http://www.w3.org/2000/svg', 'image');
    background.setAttribute('width', "298");
    background.setAttribute('height', "557");
    background.setAttribute('href', 'image/hp41.png');
    svg.appendChild(background);

    // Add LCD text
    const lcdTextElement = document.createElementNS('http://www.w3.org/2000/svg', 'text');
    lcdTextElement.setAttribute('id', 'lcdText');
    lcdTextElement.setAttribute('font-size', '23');
    lcdTextElement.setAttribute('font-family', 'HP41');
    lcdTextElement.setAttribute('x', '30');
    lcdTextElement.setAttribute('y', '48');
    lcdTextElement.setAttribute('xml:space', 'preserve');
    lcdTextElement.setAttribute('style', 'white-space: pre');
    // Replace regular spaces with non-breaking spaces so they render in SVG
    const initialText = (lcdText || "MEMORY LOST").replace(/ /g, '\u00A0');
    lcdTextElement.textContent = initialText;
    svg.appendChild(lcdTextElement);

    // Add annunciators
    const annElement = document.createElementNS('http://www.w3.org/2000/svg', 'text');
    annElement.setAttribute('id', 'lcdAnn');
    annElement.setAttribute('font-size', '11');
    annElement.setAttribute('x', '30');
    annElement.setAttribute('y', '62');
    annElement.setAttribute('xml:space', 'preserve');
    annElement.setAttribute('style', 'white-space: pre');
    annElement.setAttribute('font-family', 'Andale Mono');
    annElement.textContent = annunciators || "";
    svg.appendChild(annElement);

    // Add all keys
    svg.appendChild(createRockerKey(20, "ON", keys.keyON, -1, keys.keyUSER));
    svg.appendChild(createRockerKey(77, "USER", keys.keyUSER, 1, keys.keyON));
    svg.appendChild(createRockerKey(169, "PRGM", keys.keyPRGM, -1, keys.keyALPHA));
    svg.appendChild(createRockerKey(226, "ALPHA", keys.keyALPHA, 1, keys.keyPRGM));

    svg.appendChild(createNormalKey(35, 148, "SIGMA", keys.keySIGMA));
    svg.appendChild(createNormalKey(82, 148, "INV", keys.keyINV));
    svg.appendChild(createNormalKey(129, 148, "SQRT", keys.keySQRT));
    svg.appendChild(createNormalKey(176, 148, "LOG", keys.keyLOG));
    svg.appendChild(createNormalKey(223, 148, "LN", keys.keyLN));
    svg.appendChild(createNormalKey(35, 197, "SWAP", keys.keySWAP));
    svg.appendChild(createNormalKey(82, 197, "RDN", keys.keyRDN));
    svg.appendChild(createNormalKey(129, 197, "SIN", keys.keySIN));
    svg.appendChild(createNormalKey(176, 197, "COS", keys.keyCOS));
    svg.appendChild(createNormalKey(223, 197, "TAN", keys.keyTAN));
    svg.appendChild(createNormalKey(35, 246, "SHIFT", keys.keySHIFT));
    svg.appendChild(createNormalKey(82, 246, "XEQ", keys.keyXEQ));
    svg.appendChild(createNormalKey(129, 246, "STO", keys.keySTO));
    svg.appendChild(createNormalKey(176, 246, "RCL", keys.keyRCL));
    svg.appendChild(createNormalKey(223, 246, "SST", keys.keySST));
    svg.appendChild(createWideKey(35, 295, "ENTER", keys.keyENTER));
    svg.appendChild(createNormalKey(129, 295, "CHS", keys.keyCHS));
    svg.appendChild(createNormalKey(176, 295, "EEX", keys.keyEEX));
    svg.appendChild(createNormalKey(223, 295, "ARROW", keys.keyARROW));
    svg.appendChild(createNormalKey(35, 344, "MINUS", keys.keyMINUS));
    svg.appendChild(createNormalKey(98, 344, "7", keys.key7));
    svg.appendChild(createNormalKey(161, 344, "8", keys.key8));
    svg.appendChild(createNormalKey(224, 344, "9", keys.key9));
    svg.appendChild(createNormalKey(35, 393, "PLUS", keys.keyPLUS));
    svg.appendChild(createNormalKey(98, 393, "4", keys.key4));
    svg.appendChild(createNormalKey(161, 393, "5", keys.key5));
    svg.appendChild(createNormalKey(224, 393, "6", keys.key6));
    svg.appendChild(createNormalKey(35, 442, "MUL", keys.keyMUL));
    svg.appendChild(createNormalKey(98, 442, "1", keys.key1));
    svg.appendChild(createNormalKey(161, 442, "2", keys.key2));
    svg.appendChild(createNormalKey(224, 442, "3", keys.key3));
    svg.appendChild(createNormalKey(35, 491, "DIV", keys.keyDIV));
    svg.appendChild(createNormalKey(98, 491, "0", keys.key0));
    svg.appendChild(createNormalKey(161, 491, "DOT", keys.keyDOT));
    svg.appendChild(createNormalKey(224, 491, "RUN", keys.keyRUN));

  }, []); // Only run once on mount

  // Update LCD when props change
  useEffect(() => {
    if (!svgRef.current) return;
    const lcdTextElement = svgRef.current.querySelector('#lcdText');
    if (lcdTextElement) {
      // Replace regular spaces with non-breaking spaces so they render in SVG
      const displayText = lcdText.replace(/ /g, '\u00A0');
      lcdTextElement.textContent = displayText;
    }
  }, [lcdText]);

  useEffect(() => {
    if (!svgRef.current) return;
    const annElement = svgRef.current.querySelector('#lcdAnn');
    if (annElement) {
      annElement.textContent = annunciators;
    }
  }, [annunciators]);

  return (
    <svg
      ref={svgRef}
      width="298"
      height="557"
      id="calculator"
    />
  );
}
