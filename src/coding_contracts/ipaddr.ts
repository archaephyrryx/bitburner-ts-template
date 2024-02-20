import { CodingContract, CodingContractData, NS } from '@ns';

type IPAddrData = string;

export function ipAddrSolver(ns: NS, cc: CodingContract, file: string, host: string | undefined): boolean {
    const data = cc.getData(file, host);
    const answer = solve_ipaddr(data);
    const res = cc.attempt(answer, file, host);
    if (res != "") {
        ns.tprint(`SUCCESS: Solved ${file}: ${res}`);
        return true;
    } else {
        ns.tprint(`WARNING: Script-Based solver failed to solve ${cc.getContractType(file, host)}, please ensure contract type is correct and logic is correctly implemented`);
        return false;
    }
}

function sanitize(data: unknown): asserts data is IPAddrData {
    if (typeof data === 'string') {
        if (isNaN(Number(data))) {
            throw new Error(`Invalid IP Address contract data: ${JSON.stringify(data)}`);
        }
        return;
    } else {
        throw new Error(`Invalid IP Address contract data: ${JSON.stringify(data)}`);
    }
}

export function solve_ipaddr(dat: CodingContractData): string[] {
    sanitize(dat);

    const answer = ip(dat);
    return answer;
}

function ip(remainder: string): string[] {
    const res = splitIp(0, remainder);
    if (res.just) {
        return res.value.map((segs) => segs.join('.'));
    } else {
        return [];
    }
}

function splitIp(splits: number, remainder: string): Maybe<string[][]> {
    function go(l: number, s: string): Maybe<string[][]> {
        const seg = s.substring(0, l);
        const rest = s.substring(l);
        if (isIpSeg(seg)) {
            const rems = splitIp(splits + 1, rest);
            if (!rems.just) {
                return { just: false };
            } else {
                return { just: true, value: rems.value.map((segs) => [seg, ...segs]) };
            }
        } else {
            return { just: false };
        }
    }

    if (splits >= 4) {
        return { just: false };
    } else if (splits == 3) {
        if (isIpSeg(remainder)) {
            return { just: true, value: [[remainder]] };
        } else {
            return { just: false };
        }
    } else {
        const res = [go(1, remainder), go(2, remainder), go(3, remainder)].filter((v) => v.just);
        if (res.length == 0) {
            return { just: false };
        } else {
            return fmap((x) => x.flat(1), sequence(res));
        }
    }
}

function isIpSeg(x: string): boolean {
    return (!(x.startsWith('0') && x.length > 1) && Number(x) <= 255);
}

type Just<T> = { just: true, value: T };
type Nothing = { just: false };

type Maybe<T> = Nothing | Just<T>;

function fmap<T, U>(f: (x: T) => U, mx: Maybe<T>): Maybe<U> {
    if (mx.just) {
        return { just: true, value: f(mx.value) };
    } else {
        return { just: false };
    }
}

function sequence<T>(xs: Array<Maybe<T>>): Maybe<Array<T>> {
    const accum: Array<T> = [];
    for (const mx of xs) {
        if (mx.just) {
            accum.push(mx.value);
        } else {
            return { just: false };
        }
    }
    return { just: true, value: accum };
}
