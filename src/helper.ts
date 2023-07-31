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
    const runCost = ns.getScriptRam(scriptName, servName);
    if (runCost === 0) {
        ns.toast(`Script ${scriptName} may have missing dependencies on server ${servName}`, "error", 1500);
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

/**
 * Obtains root access on a server, assuming it is possible.
 * @param ns NetScript instance to use
 * @param server server to hack
 * @returns true if server is rooted, false if it could not be rooted
 */
export function autoHack(ns: NS, server: string): boolean {
    if (!ns.serverExists(server)) {
        ns.printf("helper.autoHack: server %s does not exist", server);
        return false;
    }
    if (ns.hasRootAccess(server)) {
        return true;
    }
    if (!canHack(ns, server)) {
        return false;
    }

    if (ns.fileExists("SQLInject.exe", "home")) ns.sqlinject(server);
    if (ns.fileExists("HTTPWorm.exe", "home")) ns.httpworm(server);
    if (ns.fileExists("relaySMTP.exe", "home")) ns.relaysmtp(server);
    if (ns.fileExists("FTPCrack.exe", "home")) ns.ftpcrack(server);
    if (ns.fileExists("BruteSSH.exe", "home")) ns.brutessh(server);
    ns.nuke(server);
    return true;
}

/**
 * Checks if a server can be hacked by the player at the moment.
 * @param ns NetScript instance to use
 * @param server server to check for hackability
 * @returns true if server can be hacked, false otherwise
 */
export function canHack(ns: NS, server: string): boolean {
    if (server === "home" || server.startsWith("pserv")) {
        return false;
    }
    const requiredSkill = ns.getServerRequiredHackingLevel(server);
    const level = ns.getHackingLevel();
    const threshold = (level <= 1 ? 1 : level / 2);
    if (requiredSkill > threshold) {
        return false;
    }
    const portsNeeded = ns.getServerNumPortsRequired(server);
    if (portsNeeded >= 5 && !ns.fileExists("SQLInject.exe", "home")) {
        return false;
    }
    if (portsNeeded >= 4 && !ns.fileExists("HTTPWorm.exe", "home")) {
        return false;
    }
    if (portsNeeded >= 3 && !ns.fileExists("relaySMTP.exe", "home")) {
        return false;
    }
    if (portsNeeded >= 2 && !ns.fileExists("FTPCrack.exe", "home")) {
        return false;
    }
    if (portsNeeded >= 1 && !ns.fileExists("BruteSSH.exe", "home")) {
        return false;
    }
    return true;
}
