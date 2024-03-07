import { NS, CodingContract, CodingContractData } from '@ns';
import { Solver } from 'coding_contracts/common';

export const imergeSolver: Solver = (ns: NS, cc: CodingContract, file: string, host: string | undefined): boolean => {
    const data = cc.getData(file, host);
    const answer = solve_imerge(data);
    const res = cc.attempt(answer, file, host);
    if (res != "") {
        ns.tprint(`SUCCESS: Solved ${file}: ${res}`);
        return true;
    } else {
        ns.tprint(`Incorrect Answer: ${JSON.stringify(answer)} (Data: ${JSON.stringify(cc.getData(file, host))})`);
        ns.tprint(`WARNING: Script-Based solver failed to solve ${cc.getContractType(file, host)}, please ensure contract type is correct and logic is correctly implemented`);
        return false;
    }
};

type IntervalMergeData = Array<[number, number]>;

function sanitize(data: CodingContractData): asserts data is IntervalMergeData {
    if (Array.isArray(data)) {
        for (const row of data) {
            if (Array.isArray(row)) {
                for (const elt of row) {
                    if (typeof elt !== 'number') {
                        throw new Error(`Invalid IntervalMergeData: ${JSON.stringify(data)}`);
                    }
                }
            }
            if (row.length !== 2) {
                throw new Error(`Invalid IntervalMergeData: ${JSON.stringify(data)}`);
            }
        }
    }
    return;
}

type Interval = [number, number];

function solve_imerge(dat: CodingContractData): number[][] {
    sanitize(dat);

    const answer = imerge(dat);
    return answer;
}

function imerge(intervals: Interval[]): Interval[] {
    const l = intervals.length;
    const sorted = intervals.toSorted(compareInterval);
    const output = merge_intervals(sorted);
    if (output.length === l) {
        return output;
    } else {
        return imerge(output);
    }
}

function span(interval: Interval): number {
    const [lo, hi] = interval;
    return hi - lo;
}

function compareInterval(lhs: Interval, rhs: Interval): number {
    if (lhs[1] < rhs[1]) return -1;
    if (lhs[1] > rhs[1]) return 1;
    if (span(lhs) < span(rhs)) return -1;
    if (span(lhs) > span(rhs)) return 1;
    return 0;
}

function overlaps([lo0, hi0]: Interval, [lo1, hi1]: Interval): boolean {
    return (lo0 <= hi1 && hi0 >= hi1) || (lo1 <= hi0 && hi1 >= hi0);
}

function merge([lo0, hi0]: Interval, [lo1, hi1]: Interval): Interval {
    return [Math.min(lo0, lo1), Math.max(hi0, hi1)];
}

function merge_intervals(ints: Interval[]): Interval[] {
    if (ints.length === 0) return [];

    let tmp: Interval | undefined = undefined;
    const ret: Interval[] = [];
    for (const ival of ints) {
        if (tmp === undefined) {
            tmp = ival;
            continue;
        } else {
            if (overlaps(ival, tmp)) {
                tmp = merge(ival, tmp);
                continue;
            } else {
                ret.push(tmp);
                tmp = ival;
            }
        }
    }
    if (tmp !== undefined) {
        ret.push(tmp);
    }
    return ret;
}
