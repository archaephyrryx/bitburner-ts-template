import { CodingContractData } from "@ns";
import { Solver } from "coding_contracts/common";

export const caesarSolver: Solver = (ns, cc, file, host) => {
    const data = cc.getData(file, host);
    const answer = solveCaesar(data);
    const res = cc.attempt(answer, file, host);
    if (res != "") {
        ns.tprint(`SUCCESS: Solved ${file}: ${res}`);
        return true;
    } else {
        ns.tprint(`WARNING: Script-Based solver failed to solve ${cc.getContractType(file, host)}, please ensure contract type is correct and logic is correctly implemented`);
        return false;
    }
}

type CaesarShiftData = [string, number];

function sanitize(raw: CodingContractData): CaesarShiftData {
    if (Array.isArray(raw) && raw.length == 2 && typeof raw[0] === 'string' && typeof raw[1] === 'number') {
        return [raw[0], raw[1]];
    }
    throw new Error(`Invalid Caesar Shift Data: ${JSON.stringify(raw)}`);
}


function solveCaesar(data: CodingContractData) {
    const [plaintext, lShift] = sanitize(data);

    return caesarShift(plaintext, lShift);
}

function caesarShift(plaintext: string, lShift: number): string {
    const rawCipher: number[] = [];

    for (let i = 0; i < plaintext.length; i++) {
        const rawCode = plaintext.charCodeAt(i);
        const shiftCode = (rawCode == 0x20) ? rawCode : (((rawCode - 0x41 + 26 - lShift) % 26) + 0x41);
        rawCipher.push(shiftCode);
    }

    return String.fromCharCode(...rawCipher);
}
