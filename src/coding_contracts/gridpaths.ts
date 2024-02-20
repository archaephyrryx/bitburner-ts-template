import { CodingContract, CodingContractData, NS } from '@ns';

type GridPathData = Array<number>;

export function gridpathsSolver(ns: NS, cc: CodingContract, file: string, host: string | undefined): boolean {
    const data = cc.getData(file, host);
    const answer = solve_gridpaths(data);
    if (cc.attempt(answer, file, host)) {
        return true;
    } else {
        ns.tprint(`WARNING: Script-Based solver failed to solve GridPaths problem, please ensure contract type is correct and logic is correctly implemented`);
        return false;
    }
}

function sanitize(data: unknown): asserts data is GridPathData {
    if (Array.isArray(data)) {
        for (const elem of data) {
            if (typeof elem !== 'number') {
                throw new Error(`Invalid GridPaths contract data: ${JSON.stringify(data)}`);
            }
        }
        return;
    }
    throw new Error(`Invalid GridPaths contract data: ${JSON.stringify(data)}`);
}

export function solve_gridpaths(dat: CodingContractData): number {
    sanitize(dat);
    const n = dat[0];
    const m = dat[1];

    const steps = n + m - 2;
    const picks = Math.min(n, m) - 1;

    let numer = steps;
    let denom = 1;

    for (let i = 1; i < picks; i++) {
        numer *= (steps - i);
        denom *= (i + 1);
    }

    const answer = Math.round(numer / denom);

    return answer;
}
