import { AutocompleteData, NS } from "@ns";

const scripts = [
    "bootstrap.js",
    "server.js",
    "rip.js",
];

export async function main(ns: NS): Promise<void> {
    const hostName = ns.getHostname();
    const scriptName = (ns.args[0] ?? (await ns.prompt("Pick script to run", { type: "select", choices: scripts }))) as string;
    const args = [...ns.args.slice(1)];
    if (!ns.fileExists(scriptName, hostName)) {
        ns.tprintf("ERROR: %s does not exist on %s", scriptName, hostName);
        return;
    }
    const memPerThread = ns.getScriptRam(scriptName);
    const freeMemory = ns.getServerMaxRam(hostName) - ns.getServerUsedRam(hostName);
    const maxThreads = Math.floor(freeMemory / memPerThread);
    const pid = ns.run(scriptName, maxThreads, ...args);
    if (pid === 0) {
        ns.tprintf("ERROR: Failed to run %s on %s", scriptName, hostName);
    } else {
        ns.tprintf("Running %s on %s with %d threads (pid: %d)", scriptName, hostName, maxThreads, pid);
    }
    return;
}

export function autocomplete(data: AutocompleteData, args: string[]) {
    return [...scripts];
}
