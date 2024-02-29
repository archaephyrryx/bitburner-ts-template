import { NS, CodingContract, CodingContractData } from '@ns';
import { Solver } from 'coding_contracts/common';

export const pathsumSolver: Solver = (ns: NS, cc: CodingContract, file: string, host: string | undefined): boolean => {
    const data = cc.getData(file, host);
    const answer = solve_pathsum(data);
    const res = cc.attempt(answer, file, host);
    if (res != "") {
        ns.tprint(`SUCCESS: Solved ${file}: ${res}`);
        return true;
    } else {
        ns.tprint(`WARNING: Script-Based solver failed to solve ${cc.getContractType(file, host)}, please ensure contract type is correct and logic is correctly implemented`);
        return false;
    }
};

type PathSumData = Array<number[]>;

function sanitize(data: CodingContractData): asserts data is PathSumData {
    if (Array.isArray(data)) {
        for (const row of data) {
            if (Array.isArray(row)) {
                for (const elt of row) {
                    if (typeof elt !== 'number') {
                        throw new Error(`Invalid Path Sum Data: ${JSON.stringify(data)}`);
                    }
                }
            }
        }
    }
    return;
}


function solve_pathsum(dat: CodingContractData): number {
    sanitize(dat);

    const answer = pathSum(0, 0, dat);
    return answer;
}

function pathSum(ix: number, accum: number, rows: number[][]): number {
    if (rows.length === 0) return accum;

    const [row, ...rest] = rows;
    const xs = row.slice(ix);
    if (xs.length === 0) throw new Error(`bad pathSum args: ${ix} @ ${JSON.stringify(rows)}`);
    if (xs.length === 1) return pathSum(ix, accum + xs[0], rest);
    const [l, r] = xs;
    const left = pathSum(ix, accum + l, rest);
    const right = pathSum(ix + 1, accum + r, rest);
    return Math.min(left, right);
}
