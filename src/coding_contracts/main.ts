import { CodingContract, NS } from '@ns';
import { mimic } from '/util/stringtools';
import { gridpathsSolver } from 'coding_contracts/gridpaths';
import { ipAddrSolver } from 'coding_contracts/ipaddr';
import { Solver } from 'coding_contracts/common';
import { lzSolver } from 'coding_contracts/lz';

export function attemptSolution(solverFn: Solver, ns: NS, cc: CodingContract, filename: string, hostMachine: string): boolean {
    if (cc.getNumTriesRemaining(filename, hostMachine) > 1) {
        if (solverFn(ns, cc, filename, hostMachine)) {
            return true;
        } else {
            ns.tprint(`INFO: ${filename} (${cc.getContractType(filename, hostMachine)} @ ${hostMachine}) attempted but failed, ${cc.getNumTriesRemaining(filename, hostMachine)} tries remaining...`);
        }
    } else {
        ns.tprint(`WARNING: ${filename} (${cc.getContractType(filename, hostMachine)} @ ${hostMachine}) has only 1 try remaining, will not attempt script-based solution.`);
    }
    return false;
}

export function solve_contract(ns: NS, cc: CodingContract, filename: string, hostMachine: string): boolean {
    const ccType = cc.getContractType(filename, hostMachine);
    ns.tprint(`INFO: Coding Contract Type: ${ccType}`);
    switch (ccType) {
        case 'Unique Paths in a Grid I':
            return attemptSolution(gridpathsSolver, ns, cc, filename, hostMachine);
        case "Generate IP Addresses":
            return attemptSolution(ipAddrSolver, ns, cc, filename, hostMachine);
        case "Compression II: LZ Decompression":
            return attemptSolution(lzSolver, ns, cc, filename, hostMachine);
        default:
            ns.tprint(`WARNING: No auto-solver implemented for contract-type '${ccType}' (${filename} on ${hostMachine})`);
            break;
    }
    return false;
}

function scan(ns: NS, parent: string, server: string, list: string[]): void {
    const children = ns.scan(server);
    for (const child of children) {
        if (parent == child) {
            continue;
        }
        list.push(child);

        scan(ns, server, child, list);
    }
}

export function list_servers(ns: NS): string[] {
    const list: string[] = [];
    scan(ns, '', 'home', list);
    return list;
}

export async function main(ns: NS): Promise<void> {
    const servers = list_servers(ns);
    const hostnames = servers.filter(s => ns.ls(s).some(f => f.endsWith(".cct")))
    if (hostnames.length == 0) {
        ns.tprint("No coding contract found.");
        return;
    } else {
        const cc = ns.codingcontract;
        const types = cc.getContractTypes();
        const header = `=== Coding Contract Types ===`;
        ns.tprint(header);
        for (const type of types) {
            ns.tprint(`\t${type}`);
        }
        ns.tprint(mimic(header, "="));


        for (const hostname of hostnames) {
            const contracts = ns.ls(hostname, '.cct');
            if (contracts.length == 0) {
                continue;
            } else {
                for (const contract of contracts) {
                    solve_contract(ns, cc, contract, hostname);
                }
            }
        }
    }
}
