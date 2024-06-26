import { BitNodeMultipliers, NS } from "@ns";

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
 * @param handicap multiplier for effective hacking level, default 0.5
 * @returns true if server can be hacked, false otherwise
 */
export function canCrack(ns: NS, server: string): boolean {
    if (server === "home" || server.startsWith("pserv")) {
        return true;
    }
    const requiredSkill = ns.getServerRequiredHackingLevel(server);
    const level = ns.getHackingLevel();
    if (requiredSkill > level) {
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

/**
 * Checks if a server can be hacked by the player at the moment.
 * @param ns NetScript instance to use
 * @param server server to check for hackability
 * @param handicap multiplier for effective hacking level, default 0.5
 * @returns true if server can be hacked, false otherwise
 */
export function canHack(ns: NS, server: string, handicap = 0.5): boolean {
    if (server === "home" || server.startsWith("pserv") || server.startsWith("hacknet-server")) {
        return false;
    }
    const requiredSkill = ns.getServerRequiredHackingLevel(server);
    const level = ns.getHackingLevel();
    const threshold = (level <= 1 ? 1 : level * Math.min(1, Math.max(0.5, handicap)));
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

export function printWaitingMoney(ns: NS, moneyAvailable: number, moneyNeeded: number, verb: string): void {
    if (moneyAvailable < moneyNeeded)
        ns.print(`Waiting for money to ${verb}: $${ns.formatNumber(moneyAvailable)}/$${ns.formatNumber(moneyNeeded)} (${ns.formatPercent(moneyAvailable / moneyNeeded)}, Need $${ns.formatNumber(moneyNeeded - moneyAvailable)} more)`);
}

export function formatTime(seconds: number): string {
    const s = Math.ceil(seconds);
    if (s < M) {
        return `${s}s`;
    } else if (s < H) {
        return `${Math.floor(s / M)}m, ${s % M}s`;
    } else if (seconds < D) {
        return `${Math.floor(s / H)}h ${Math.floor((s % H) / M)}m ${(s % M)}s`;
    } else {
        return `${Math.floor(s / D)}d ${Math.floor((s % D) / H)}h ${Math.floor((s % H) / M)}m ${(s % M)}s`;
    }
}

export const D = 86400;
export const H = 3600;
export const M = 60;

export function isOnBitnode(ns: NS, n: number): boolean {
    const currentMults = ns.getBitNodeMultipliers();
    for (let i = 1; i <= 3; ++i) {
        const nMults = ns.getBitNodeMultipliers(n, i);
        if (equalMultipliers(currentMults, nMults)) {
            return true;
        }
    }
    return false;
}

function equalMultipliers(currentMults: BitNodeMultipliers, nMults: BitNodeMultipliers) {
    for (const entry of Object.entries(currentMults)) {
        const [key, value] = entry;
        if (nMults[key as keyof BitNodeMultipliers] === value) {
            continue;
        } else {
            return false;
        }
    }
    return true;
}
export const bold = "\u001b[01m";
export const reset = "\u001b[0m";
export const red = "\u001b[31m";

export function formatRatio(x: number, y: number): string {
    const lhs = `${x == 0 ? 0 : (x <= y || y == 0) ? 1 : x % y == 0 ? x / y : (x / y).toFixed(2)}`;
    const rhs = `${y == 0 ? 0 : (y <= x || x == 0) ? 1 : y % x == 0 ? y / x : (y / x).toFixed(2)}`;
    return `${lhs}:${rhs}`;
}
