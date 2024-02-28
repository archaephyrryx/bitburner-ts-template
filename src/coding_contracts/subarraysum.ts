import { CodingContract, CodingContractData, NS } from '@ns';

type SubarraySumData = Array<number>;

export function subarraySumSolver(ns: NS, cc: CodingContract, file: string, host: string | undefined): boolean {
    const data = cc.getData(file, host);
    const answer = solve_subarray_sum(data);
    const res = cc.attempt(answer, file, host);
    if (res != "") {
        ns.tprint(`SUCCESS: Solved ${file}: ${res}`);
        return true;
    } else {
        ns.tprint(`WARNING: Script-Based solver failed to solve ${cc.getContractType(file, host)}, please ensure contract type is correct and logic is correctly implemented`);
        ns.tprint(`Incorrect Answer: ${answer} (Data: ${cc.getData(file, host)})`);
        return false;
    }
}

function sanitize(data: unknown): asserts data is SubarraySumData {
    if (Array.isArray(data)) {
        for (const elem of data) {
            if (typeof elem !== 'number' || Number.isNaN(elem)) {
                throw new Error(`Invalid Subarray-Sum contract data: ${JSON.stringify(data)}`);
            }
        }
        return;
    }
    throw new Error(`Invalid Subarray-Sum contract data: ${JSON.stringify(data)}`);
}

export function solve_subarray_sum(dat: CodingContractData): number {
    sanitize(dat);

    return bestSum(dat);
}

function bestSum(xs: number[]): number {
    const worstcase = xs.toSorted()[0] - 1;

    function go(total: number, accum: number, xs: number[]): number {
        if (xs.length == 0) return total;
        const [x, ...xt] = [...xs];
        const newAccum = Math.max(x, accum + x);
        const newTotal = Math.max(total, newAccum);
        return go(newTotal, newAccum, xt);
    }

    return go(worstcase, 0, xs);
}
