import { NS, CodingContract, CodingContractData } from '@ns';



type LZData = string;

export function lzSolver(ns: NS, cc: CodingContract, file: string, host: string | undefined): boolean {
    const data = cc.getData(file, host);
    let answer;
    const ccType = cc.getContractType(file, host);
    switch (ccType) {
        case 'Compression II: LZ Decompression':
            answer = solve_lz_decompress(data);
            break;
        default:
            ns.tprint(`WARNING: No logic, or bad contract type ${ccType} for ${file} on ${host}!`);
            return false;
    }
    if (typeof answer === 'number') {
        ns.tprint(`ERROR: No LZ decompression for contract ${file} input '${data}' at index ${answer}`);
        return false;
    }

    const res = cc.attempt(answer, file, host);
    if (res != "") {
        ns.tprint(`SUCCESS: Solved ${file}: ${res}`);
        return true;
    } else {
        ns.tprint(`WARNING: Script-Based solver failed to solve ${cc.getContractType(file, host)}, please ensure contract type is correct and logic is correctly implemented`);
        return false;
    }
}

function sanitize(data: unknown): asserts data is LZData {
    if (typeof data === 'string') {
        return;
    } else {
        throw new Error(`Invalid LZ contract data: ${JSON.stringify(data)}`);
    }
}

export function solve_lz_decompress(dat: CodingContractData): string | number {
    sanitize(dat);

    const answer = lz_decompress(dat);
    return answer;
}

// export function solve_lz_compress(dat: CodingContractData): string {
//     sanitize(dat);

//     const answer = lz_compress(dat);
//     return answer;
// }

class Buffer {
    private data: string;
    private cursor: number;

    constructor() {
        this.data = "";
        this.cursor = 0;
    }

    public pushText(text: string) {
        this.data += text;
        this.cursor = this.data.length;
    }

    public get length(): number {
        return this.data.length;
    }

    public pushOffset(len: number, dist: number) {
        if (dist > this.cursor) {
            throw new Error("cannot decompress before start of buffer");
        }
        this.cursor = this.cursor - dist;

        for (let i = 0; i < len; i++) {
            this.data += this.data.charAt(this.cursor + i);
        }

        this.cursor = this.data.length;
    }

    public toString(): string {
        return this.data;
    }
}

function lz_decompress(compr: string): string | number {
    const buf = new Buffer();
    for (let i = 0; i < compr.length;) {
        const text_len = compr.charCodeAt(i) - 0x30;

        if (text_len < 0 || text_len > 9 || i + 1 + text_len > compr.length) {
            return i;
        }

        buf.pushText(compr.substring(i + 1, i + 1 + text_len));

        i += text_len + 1;

        if (i >= compr.length) {
            break;
        }

        const ref_len = compr.charCodeAt(i) - 0x30;
        if (ref_len < 0 || ref_len > 9) {
            return i;
        } else if (ref_len === 0) {
            ++i;
        } else {
            if (i + 1 >= compr.length) {
                return i;
            }

            const offset = compr.charCodeAt(i + 1) - 0x30;
            if ((ref_len > 0 && (offset < 1 || offset > 9)) || offset > buf.length) {
                return i;
            }

            buf.pushOffset(ref_len, offset);
            i += 2;
        }
    }
    return buf.toString();
}
