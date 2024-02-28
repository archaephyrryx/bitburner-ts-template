import { CodingContract, CodingContractData, NS } from '@ns';

type ArrayJumpData = Array<number>;

export function arrayJumpSolverBinary(ns: NS, cc: CodingContract, file: string, host: string | undefined): boolean {
    const data = cc.getData(file, host);
    const answer = solve_arrayjump(data, true);
    const res = cc.attempt(answer, file, host);
    if (res != "") {
        ns.tprint(`SUCCESS: Solved ${file}: ${res}`);
        return true;
    } else {
        ns.tprint(`WARNING: Script-Based solver failed to solve ${cc.getContractType(file, host)}, please ensure contract type is correct and logic is correctly implemented`);
        return false;
    }
}
export function arrayJumpSolver(ns: NS, cc: CodingContract, file: string, host: string | undefined): boolean {
    const data = cc.getData(file, host);
    const answer = solve_arrayjump(data);
    const res = cc.attempt(answer, file, host);
    if (res != "") {
        ns.tprint(`SUCCESS: Solved ${file}: ${res}`);
        return true;
    } else {
        ns.tprint(`WARNING: Script-Based solver failed to solve ${cc.getContractType(file, host)}, please ensure contract type is correct and logic is correctly implemented`);
        return false;
    }
}

function sanitize(data: unknown): asserts data is ArrayJumpData {
    if (Array.isArray(data)) {
        for (const elem of data) {
            if (typeof elem !== 'number') {
                throw new Error(`Invalid ArrayJump contract data: ${JSON.stringify(data)}`);
            }
        }
        return;
    }
    throw new Error(`Invalid ArrayJump contract data: ${JSON.stringify(data)}`);
}

export function solve_arrayjump(dat: CodingContractData, binary = false): number {
    sanitize(dat);
    const graph = new Graph(dat.length);
    dat.forEach((jumpdist, ix) => {
        graph.initNode(ix);
        for (let j = 1; j <= jumpdist && ix + j < graph.size; j++) {
            graph.addEdge(ix, ix + j)
        }
    })
    const res = graph.dijkstra();
    return (binary && res > 0) ? 1 : res;
}

class Graph {
    public readonly size: number;
    private edges: Map<number, Set<number>>;

    constructor(size: number) {
        this.size = size;
        this.edges = new Map();
    }

    public addEdge(source: number, target: number) {
        const toEdges = this.edges.get(source);
        if (typeof toEdges === 'undefined') {
            const edges = new Set<number>();
            edges.add(target);
            this.edges.set(source, edges);
        } else {
            toEdges.add(target);
        }
    }

    protected directNeighbors(node: number): number[] {
        const ret = this.edges.get(node)?.values();
        return [...(ret ?? [])];
    }

    public dijkstra(): number {
        const START = 0;
        const END = this.size - 1;
        const dist: Array<number> = Array(this.size);
        const prev: Array<number | undefined> = Array(this.size);
        const Q = new Set<number>();

        for (const u of this.edges.keys()) {
            dist[u] = Number.MAX_SAFE_INTEGER;
            prev[u] = undefined;
            Q.add(u);
        }
        dist[START] = 0;

        while (Q.size > 0) {
            const u = [...Q.values()].toSorted((a, b) => dist[a] - dist[b])[0];
            Q.delete(u);

            for (const v of this.directNeighbors(u)) {
                const alt = dist[u] + 1;
                if (alt < dist[v]) {
                    dist[v] = alt;
                    prev[v] = u;
                }
            }
        }

        if (prev[END] === undefined) {
            return 0;
        } else {
            return dist[END];
        }
    }

    public initNode(node: number) {
        if (this.edges.has(node)) {
            return;
        } else {
            this.edges.set(node, new Set<number>());
        }
    }
}
