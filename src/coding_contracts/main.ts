import { AutocompleteData, CodingContract, NS } from '@ns';
import { mimic } from '/util/stringtools';
import { gridpathsSolver } from 'coding_contracts/gridpaths';
import { ipAddrSolver } from 'coding_contracts/ipaddr';
import { Solver } from 'coding_contracts/common';
import { lzSolver } from 'coding_contracts/lz';
import { arrayJumpSolver, arrayJumpSolverBinary } from 'coding_contracts/arrayjump';
import { lpfSolver } from 'coding_contracts/lpf';
import { subarraySumSolver } from 'coding_contracts/subarraysum';
import { pathsumSolver } from 'coding_contracts/pathsum';
import { sumwaysSolver } from 'coding_contracts/sumways';
import { caesarSolver } from 'coding_contracts/encryption';

export function attemptSolution(solverFn: Solver, ns: NS, cc: CodingContract, filename: string, hostMachine: string, force = false): boolean {
    const triesRemaining = cc.getNumTriesRemaining(filename, hostMachine);

    if (triesRemaining <= 1 && !force) {
        ns.tprint(`ERROR: ${filename} (${cc.getContractType(filename, hostMachine)} @ ${hostMachine}) has only 1 try remaining, will not attempt script-based solution.`);
        return false;
    }

    if (triesRemaining <= 1) {
        ns.tprint(`WARNING: ${filename} (${cc.getContractType(filename, hostMachine)} @ ${hostMachine}) has only 1 try remaining, but will be attempted due to --force flag`);
    }
    if (solverFn(ns, cc, filename, hostMachine)) {
        return true;
    } else {
        const triesRemaining = cc.getNumTriesRemaining(filename, hostMachine);
        if (triesRemaining > 0) {
            ns.tprint(`INFO: ${filename} (${cc.getContractType(filename, hostMachine)} @ ${hostMachine}) attempted but failed, ${cc.getNumTriesRemaining(filename, hostMachine)} tries remaining...`);
        } else {
            ns.tprint(`WARNING: ${filename} (${cc.getContractType(filename, hostMachine)} @ ${hostMachine}) attempted but failed, and is now destroyed!`);
        }
        return false;
    }
}

export function solve_contract(ns: NS, cc: CodingContract, filename: string, hostMachine: string, force = false): boolean {
    const ccType = cc.getContractType(filename, hostMachine);
    switch (ccType) {
        case "Array Jumping Game":
            return attemptSolution(arrayJumpSolverBinary, ns, cc, filename, hostMachine, force);
        case "Array Jumping Game II":
            return attemptSolution(arrayJumpSolver, ns, cc, filename, hostMachine, force);
        case "Compression II: LZ Decompression":
            return attemptSolution(lzSolver, ns, cc, filename, hostMachine, force);
        case "Encryption I: Caesar Cipher":
            return attemptSolution(caesarSolver, ns, cc, filename, hostMachine, force);
        case "Find Largest Prime Factor":
            return attemptSolution(lpfSolver, ns, cc, filename, hostMachine, force);
        case "Generate IP Addresses":
            return attemptSolution(ipAddrSolver, ns, cc, filename, hostMachine, force);
        case "Minimum Path Sum in a Triangle":
            return attemptSolution(pathsumSolver, ns, cc, filename, hostMachine, force);
        case "Subarray with Maximum Sum":
            return attemptSolution(subarraySumSolver, ns, cc, filename, hostMachine, force);
        case "Total Ways to Sum":
            return attemptSolution(sumwaysSolver, ns, cc, filename, hostMachine, force);
        case "Unique Paths in a Grid I":
            return attemptSolution(gridpathsSolver, ns, cc, filename, hostMachine, force);
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
    const flags = ns.flags([['force', false]]);
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
                    solve_contract(ns, cc, contract, hostname, flags.force as boolean);
                }
            }
        }
    }
}

export function autocomplete(data: AutocompleteData, args: string[]) {
    data.flags([['force', false]]);
    return [];
}
