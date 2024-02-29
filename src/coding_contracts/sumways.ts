import { CodingContract, CodingContractData, NS } from '@ns';

type SumWaysData = number;

export function sumwaysSolver(ns: NS, cc: CodingContract, file: string, host: string | undefined): boolean {
    const data = cc.getData(file, host);
    const answer = solve_sumways(data);
    const res = cc.attempt(answer, file, host);
    if (res != "") {
        ns.tprint(`SUCCESS: Solved ${file}: ${res}`);
        return true;
    } else {
        ns.tprint(`WARNING: Script-Based solver failed to solve ${cc.getContractType(file, host)}, please ensure contract type is correct and logic is correctly implemented`);
        return false;
    }
}

function sanitize(data: unknown): asserts data is SumWaysData {
    if (typeof data === 'number') {
        if (isNaN(Number(data))) {
            throw new Error(`Invalid Total Ways to Sum contract data: ${JSON.stringify(data)}`);
        }
        return;
    } else {
        throw new Error(`Invalid Total Ways to Sum contract data: ${JSON.stringify(data)}`);
    }
}

export function solve_sumways(dat: CodingContractData): number {
    sanitize(dat);

    const answer = sumways(dat);
    return answer;
}

function sumways(m: number): number {
    const ways = [1];
    ways.length = m + 1;
    ways.fill(0, 1);
    for (let i = 1; i < m; ++i) {
        for (let j = i; j <= m; ++j) {
            ways[j] += ways[j - i]
        }
    }
    return ways[m];
}
