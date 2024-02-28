import { CodingContract, CodingContractData, NS } from '@ns';

type LPFData = number;

export function lpfSolver(ns: NS, cc: CodingContract, file: string, host: string | undefined): boolean {
    const data = cc.getData(file, host);
    const answer = solve_lpf(data);
    const res = cc.attempt(answer, file, host);
    if (res != "") {
        ns.tprint(`SUCCESS: Solved ${file}: ${res}`);
        return true;
    } else {
        ns.tprint(`WARNING: Script-Based solver failed to solve ${cc.getContractType(file, host)}, please ensure contract type is correct and logic is correctly implemented`);
        return false;
    }
}

function sanitize(data: unknown): asserts data is LPFData {
    if (typeof data === 'number') {
        if (isNaN(Number(data))) {
            throw new Error(`Invalid Largest Prime Factor contract data: ${JSON.stringify(data)}`);
        }
        return;
    } else {
        throw new Error(`Invalid Largest Prime Factor contract data: ${JSON.stringify(data)}`);
    }
}

export function solve_lpf(dat: CodingContractData): number {
    sanitize(dat);

    const answer = lpf(dat);
    return answer;
}

function lpf(m: number): number {
    let p = 2;
    let n = m;
    while (n > (p - 1) * (p - 1)) {
        while (n % p === 0) {
            n = Math.round(n / p);
        }
        ++p;
    }
    return n === 1 ? p - 1 : n;
}
