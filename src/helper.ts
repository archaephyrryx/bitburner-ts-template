import { NS } from "@ns";

export type ExecParams =
    {
        scriptName: string;
        servName: string;
    } & (
        { canRun: false } | {
            canRun: true;
            servRamMax: number;
            servRamUsed: number;
            serverRamFree: number;
            scriptRam: number;
            threads: number;
        })

export function idealThreads(ns: NS, scriptName: string, servName: string, deps: string[] = []): ExecParams {
    const maxRam = ns.getServerMaxRam(servName);
    const usedRam = ns.getServerUsedRam(servName);
    const availRam = maxRam - usedRam;
    if (availRam === 0) {
        return { scriptName, servName, canRun: false };
    }
    for (const scriptFile of [scriptName, ...deps]) {
        if (!ns.fileExists(scriptFile, servName)) {
            if (!ns.fileExists(scriptFile, "home")) {
                ns.toast(`Script (or dependency) ${scriptFile} not found on ${servName} or home`, "error", 1500);
                return { scriptName, servName, canRun: false };
            } else {
                if (!ns.scp(scriptFile, servName, "home")) {
                    ns.toast(`Script (or dependency) ${scriptFile} not found on ${servName} or home`, "error", 1500);
                    return { scriptName, servName, canRun: false }
                }
            }
        }
    }
    const runCost = ns.getScriptRam(scriptName, servName);
    if (runCost === 0) {
        ns.toast(`Script ${scriptName} not found on ${servName}`, "error", 1500);
        return { scriptName, servName, canRun: false };
    }
    return {
        scriptName,
        servName,
        canRun: true,
        servRamMax: maxRam,
        servRamUsed: usedRam,
        serverRamFree: availRam,
        scriptRam: runCost,
        threads: Math.floor(availRam / runCost)
    }
}